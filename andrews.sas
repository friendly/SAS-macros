 /*--------------------------------------------------------------*
  *    Name: andrews.sas                                         *
  *   Title: Andrews function plots for multivariate data        *
  *     Doc: http://datavis.ca/sasmac/andrews.html    
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 19 Jul 2001 13:30:02                                *
  * Revised: 30 Sep 2011 09:31:49                                *
  * Version: 1.1-0                                               *
  * 1.1  Added IDLOC= (label curves in plot, not legend),        *
  *      YLABEL=, XLABEL=; inlined %words macro.                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The ANDREWS macro calculates a periodic function, z(t), composed of sin
 and cosine components to represent each observation in a multivariate
 sample.  The macro plots this function vs. t, from -pi to +pi.
 
 Two function types may be calculate and plotted.  The original version,
 from Andrews (1972) defines
 
   z(t) = Y1/sqrt(2) + Y2 sin(t) + Y3 cos(t) + Y4 sin(2t) + Y5 cos (2t) + ...

 A modified version, suggested by Khattree and Naik (2001) defines

   z(t) = [Y1 + Y2 (sin(t)+cos(t)) + Y3 (sin(t)-cos(t)) +
           Y4 (sin(2t)+cos(2t)) + Y5 (sin(2t)-cos(2t)) + ...]/sqrt(2)

 and appears to separate distinct observations better than the
 original formulation.

 These plots assume that the variables are measured on the same scale.
 If not, it is usually worthwhile to standardize them first, using
 either PROC STANDARD M=0 S=1, or scaling each variable to a [0,1]
 range with PROC STDIZE METHOD=RANGE.

=Usage:

 The ANDREWS macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
   %andrews(var=x1-x8, id=name);
 
==Parameters:

* DATA=       Name of input data set [Default: DATA=_LAST_]

* VAR=        List of variables to plot.  You can list variables
              individually, or use any of the SAS shorthand notations,
              such as VAR=X1-X10 or VAR=INCOME--STATUS.
              [Default: VAR=_NUMERIC_]

* ID=         Name of an ID variable (character or numeric), used in the 
              legend for the curves, or as a curve label  [Default: ID=_N_]

* TYPE=       Type of function: ORIGinal or MODified [Default: TYPE=ORIG]

* NUMPTS=     Number of function points (-1) calculated for each
              observation, on the range -pi to pi. [Default: NUMPTS=80]

* ANNO=       Name of an optional input annotate data set.   

* OUT=        Name of the output data set.  The output data set
              contains the variables Z, T, and ID, and (NUMPTS+1)*nobs
              observations. [Default: OUT=ANDREWS]

* PLOT=       Draw the plot? [Default: PLOT=Y]

* VAXIS=      Custom vertical axis statement, e.g., VAXIS=AXIS1,
              where you have defined AXIS1 before calling the macro.
              If not specified, the macro uses

                   axis98 offset=(2) label=(angle=90 rotate=0);

* HAXIS=      Custom horizontal axis statement, e.g., HAXIS=AXIS2.
              If not specified, the macro uses

                  axis99 order=(-3.1416 to 3.1416 by 1.5708)
                     value=(font=greek '-p' '-p/2' '0' 'p/2' 'p')
                     offset=(2);

* SYMBOLS=    Plotting symbols for the observations. [Default: SYMBOLS=NONE]

* COLORS=     List of colors to be used for the observations.
              [Default: COLORS=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE]

* LINES=      List of line styles for observations.
              [Default: LINES=1 20 41 21 7 14 33 12 5]

* LEGEND=     Name of a LEGEND statement, to alter the default placement
              or characteristics of the legend for observations.

* IDLOC=      If non-blank, the legend is suppressed, and the curves are
              labeled using the ID= variable to the right of the last
              point plotted for each observation.

* YLABEL=     Y-axis label. [Default: YLABEL=z(t)]

* XLABEL=     X-axis label. [Default: XLABEL=t]

* NAME=       Name of graphics catalog entry. [Default: NAME=ANDREWS]

* GOUT=       Name of graphics catalog. [Default: GOUT=GSEG]
                
=References:

 Andrews, D. F. (1972).  Plots of high dimensional data.  Biometrics,
   28, 125-136.

 Khattree, R. and Naik, D. N. (2001).  Andrews plots for multivariate data:
   Some new suggestions and applications. J. Stat. Planning and Inference,
	100(2), 411-425.

 
 =*/
%macro andrews(
	data=_last_,              /* name of input data set           */
	var=_numeric_,            /* list of variables to plot        */
	id=,                      /* name of ID variable              */
	type=orig,                /* type: ORIGinal or MODified       */
	numpts=80,                /* number of function points (-1)   */
	anno=,                    /* input annotate data set          */   
	out=andrews,              /* output data set                  */
	plot=Y,                   /* Draw the plot?                   */
	vaxis=,                   /* Custom vertical axis statement   */
	haxis=,                   /* Custom horizontal axis statement */
   symbols=none,             /* plotting symbols                 */
   colors=black red green blue brown yellow orange purple,
   lines=1 20 41 21 7 14 33 12 5,     /* line styles for obs     */
	legend=,                  /* Name of a LEGEND statement       */
	idloc=,                   /* label ID in plot?                */
	ylabel=z(t),              /* Y-axis label                     */
	xlabel=t,                 /* X-axis label                     */             
	name=andrews,             /* Name of graphics catalog entry   */
	gout=                     /* Name of graphics catalog         */
	);

%local i nv nc;
%let type=%substr(%upcase(&type),1,1);
%if %upcase(&data)=_LAST_ %then %let data = &syslast;
%if &gout^=%str()  %then %let gout=GOUT=&gout;
%if &anno^=%str()  %then %let anno=ANNO=&anno;

*-- Parse shorthand variables lists, creating expanded explicit list;
 data _null_;
	set &data (obs=1) nobs=nobs;
 	call symput('nobs', trim(left(put(nobs,8.))) );
    %if %index(&var,-) > 0 or %upcase("&var")="_NUMERIC_" %then %do;
 
       %* convert shorthand variable list to long form;
     length _vname_ $ 8 _vlist_ $ 200;
     array _xx_ &var;
     _vname_ = ' ';
     do over _xx_;
        call vname(_xx_,_vname_);
        if _vname_ ne "&group" then do;
           nvar + 1;
	        _vlist_ = trim(_vlist_)|| ' ' || trim(_vname_);
        end;
     end;
     call symput( 'VAR', trim(_vlist_) );
   %end;
run;
%put var=&var;
%let nv=%words(&var, root=v);

*options symbolgen;
data &out;
	set &data end=eof nobs=nobs;
	
	%if %length(&id)=0 %then %do;
		%let id=id;
		id = _n_;
		%end;

	keep z t &id;
	label z="&ylabel"
		t="&xlabel";
	
	*--apply the orthonormal functions of t from -pi to pi;
	pi=3.14159265;
	s=1/sqrt(2);
	inc=2*pi/&numpts;

  do t=-pi to pi by inc;

	%if &type=O %then %do;            %* Original Andrews formula;
		z = &v1 * s
		%do i=2 %to &nv ;
			%let k = %eval(1 + (&i/2));
			%let odd = %eval(&i - 2 * (&i/2));
			%if &odd %then %let fun=cos;
						%else %let fun=sin;
			%if &i < &nv %then   + &fun(&k * t) * &&v&i ;
			%end;
			;
		%end;  %*-- type=ORIG;

	%else %do;                          %* Khattre/Naik (1998) formula;
		z= s * ( &v1
		%do i=2 %to &nv ;
			%let k = %eval(1 + (&i/2));
			%let odd = %eval(&i - 2 * (&i/2));
			%if &odd %then %let fun=-;
						%else %let fun=+;
			%if &i < &nv %then  + ( sin(&k * t) * &&v&i &fun cos(&k * t) * &&v&i);
			%end;
			);
		%end;  %*-- type=MOD;
		
	output;
	end;

	if eof then do;
		call symput('nobs', left(put(nobs, 6.)));
		end;
run;
%put nobs=&nobs;

%if &plot=Y %then %do;
	%gensym(n=&nobs, line=&lines, symbols=&symbols, colors=&colors, interp=join);
	
	%let offset=2;
	%if %length(&legend)>0 %then %do;
		%if %upcase(&legend) = NONE 
			%then %let legend=NOLEGEND;
			%else %let legend=LEGEND=&legend;
		%end;

	%if %length(&idloc)>0 %then %do;
		%let nc = %words(&colors);
		data _anno_;
			set &out;
			by &id notsorted;
			length text $16 color function $8;
			drop z t;
			if last.&id then do;
				xsys='2'; ysys='2';
				y=z;
				x=t+.1;  position='6';
				function='label';
				text = left(&id);
				c = 1+mod(_n_-1,&nc);
				color = scan("&colors",c);
				output;
				end;

		%if %length(&anno) %then %do;
			data _anno_;
				set _anno_ &anno;
			%end;
		%let anno=ANNO=_anno_;
		%let offset=2 10;
		%let legend=nolegend;
		%end;

	%if %length(&vaxis)=0 %then %do;
		%let vaxis=axis98;
		axis98 offset=(2) label=(angle=90 rotate=0);
		%end;
	%if %length(&haxis)=0 %then %do;
		%let haxis=axis99;
		axis99 order=(-3.1416 to 3.1416 by 1.5708)
			value=(font=greek '-p' '-p/2' '0' 'p/2' 'p')
			offset=(&offset);
		%end;

	proc gplot data=&out &gout;
		plot z * t = &id /
			frame hm=1 vm=1 &anno
			vaxis=&vaxis
			haxis=&haxis
			&legend
			name="&name"
			des="Andrews plot of &data";
		
		run; quit;
	%*-- Reset symbol statements;
	goptions reset=symbol;
	%end;

%done:

%mend;

%macro words(string,root=);
%*--------------------------------------------------;
%* Return number of words in string. If root ^' ',  ;
%* then create global variables starting with root. ;
%*--------------------------------------------------;
   %local count word;
   %let count=1;
   %let word = %scan(&string,&count,%str( ));
   %do %while(&word^= );
	%*put WORDS: word=&word;
       %if &root^=  %then %do;
          %global &root&count;
          %let &root&count=&word;
       %end;
       %let count = %eval(&count+1);
       %let word = %scan(&string,&count,%str( ));
   %end;
   %eval(&count-1)
%mend words;
