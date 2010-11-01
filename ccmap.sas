 /*--------------------------------------------------------------*
  *    Name: ccmap.sas                                           *
  *   Title: Produce a conditioned choropleth map                *
        Doc: http://www.datavis.ca/sasmac/ccmap.html       
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  2 Dec 2005 10:23:56                                *
  * Revised: 13 Dec 2005 16:36:43                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The CCMAP macro produces a set of choropleth maps for one response
 variable where the geographic units are partitioned into groups
 by two conditioning variables.  Each region appears in all maps,
 but is only shaded (in relation the the RESPONSE= variable) in those
 views defined by the ranges of the conditioning (GIVEN=) variable.
 This makes it possible to see how the geographic distribution of the
 response changes with the conditioning variables.
 
 See: Dan Carr's CCmaps page:  http://www.galaxy.gmu.edu/~dcarr/CCmaps/
 for information on CCmaps.

=Usage:

 The CCMAP macro is defined with keyword parameters.  The RESPONSE=
 (or VAR=) variable is required, as are two GIVEN= variables. The ID=
 variable must name the geographic region in the MAP= and DATA= data
 sets.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%ccmap();

 For useful results, you should also specify the MIDPOINTS= values of the
 RESPONSE= variable and define a corresponding set of PATTERN statements.

==Parameters:

* DATA=       The name of the input data set

* MAP=        Name of the input map data set [Default: MAP=MYMAPS.USVISMAP]

* RESPONSE=   Response variable shown on maps

* VAR=        Synonym for RESPONSE=

* ID=         The name of the map ID variable indicating geographic regions in the
              MAP= and DATA= data sets.

* WEIGHT=     Region weight variable (e.g. population), used for calculating weighted
              means/medians for the levels of the conditioning variables.

* GIVEN=      The names of 2 Conditioning variables, X, Y

* SLICES=     Number of slices for X,Y [Default: SLICES=2 2]

* OVERLAP=    Allowed overlap between slices of the GIVEN= variables, in percent. Ignored if
              the given variables are character. [Default: OVERLAP=0.10]

* LEVELS=     Number of levels for the VAR= variable in the maps [Default: LEVELS=7]

* MIDPOINTS=  Midpoints for the VAR= variable. You should use this to
              specify a common set of midpoints for the response variable
			  in the maps, so they all use the same midpoints.  You can
			  append PCT to a list of numeric midpoint values to have the
			  macro calculate the corresponding percentiles of the VAR=
			  variable.  Specifying MIDPOINTS= overrides the LEVELS=
			  parameter.

* COUTLINE=   Outline color for the map regions. Specify COUTLINE=, (null) to suppress
              the region outlines. [Default: COUTLINE=GRAYCC]

* LEGEND=     NONE, or name of a LEGEND statement, e.g., LEGEND=LEGEND1.  If
              you do not specify LEGEND=, you will get a default PROC GMAP
			  legend in each map, probably not what you want.

* GMAPOPTIONS=  Other GMAP options

* ANNO=       Name of an additional input annotate dataset. If supplied, this is
              used in all of the maps.

* HSTRIP=     Height (in %) for marginal label strip used to identify the
              values of the GIVEN= variables. [Default: HSTRIP=6]

* STRIPBG=    Strip background color [Default: STRIPBG=YELLOW]

* STRIPFG=    Strip foreground color [Default: STRIPFG=BLACK]

* GTEMP=      Temporary graphics catalog used to generate the individual
              maps. [Default: GTEMP=GTEMP]

* KILL=       Delete gtemp when done? [Default: KILL=Y]

* GOUT=       The name of the graphics catalog [Default: GOUT=GSEG]

* NAME=       The name of the graph in the graphic catalog [Default: NAME=CCMAP]
                

==Dependencies:

 CCMAP requires the following macros:
	
	gdispla.sas     Device-independent DISPLAY/NODISPLAY control
	panels.sas      Display a set of plots in rectangular panels
	slice.sas       Divide a variable into slices

=Examples:

  %include macros(ccmap);
  %include data(guerry);

  *-- Define a palette of 7 colors from Red to Blue thru Yellow;
  %brewerpal(n=7, palette=RdYlBu, out=palette);
  %genpat(data=palette, n=nobs, colors=color);

  legend1
	  mode=share
	  position=(bottom inside left)
	  across=4
	  label=(position=middle j=c 'Pop/Crime' j=c '(00s)')
	  shape=bar(3,1);	

  %ccmap(data=guerry, map=mymaps.gfrance(where=(density<5)),
	  var=Crime_pers, given=Literacy Wealth,
	  id=dept, weight=Pop1831,
	  midpoints = 20 30 40 50 60 70 80 PCT,
	  legend=legend1,
	  slices=2 2, overlap=0.10);

% =*/

%macro ccmap(
	data=_last_,         /* name of input data set                 */
	map=mymaps.usvismap, /* name of the input map data set         */  
	response=,           /* response variable shown on maps        */
	var=,                /* response variable (synonym)            */
	id=,                 /* map region id variable                 */
	weight=,             /* region weight variable (e.g. pop)      */
	given=,              /* 2 Conditioning variables, X, Y         */
	slices=2 2,          /* Number of slices for X,Y               */
	overlap=0.10,        /* Overlap between slices                 */
	levels=7,            /* Number of levels for the VAR= variable */
	midpoints=,          /* Midpoints for the VAR= variable        */
	coutline=graycc,     /* Outline color for the map regions      */
	legend=,             /* NONE, or name of a LEGEND statement    */
	gmapOptions=,        /* Other GMAP options                     */
	anno=,               /* Input annotate dataset                 */
	hstrip=6,            /* Height (%) for marginal label strip    */
	stripbg=yellow,      /* Strip background color                 */
	stripfg=black,       /* Strip foreground color                 */
	gtemp=gtemp,         /* temporary graphics catalog             */
	kill=Y,              /* delete gtemp when done?                */
	gout=GSEG,           /* graphic catalog for plot matrix        */
	name=ccmap
	);

%if %length(&response)=0 %then
	%if %length(&var) %then %let response=&var;
	%else %do;
		%put ERROR: You must supply a RESPONSE= (or VAR=) response variable;
		%goto done;
		%end;
	
%let given1 = %scan(&given,1,%str( ));
%let given2 = %scan(&given,2,%str( ));
%let slices1 = %scan(&slices,1,%str( ));
%let slices2 = %scan(&slices,2,%str( ));
%if %length(slices2)=0 %then %let slices2=&slices1;

%let legend = %upcase(&legend);

%if %length(&given2)=0 %then %do;
   %put ERROR: You must supply two GIVEN= variables;
   %goto done;
   %end;

%if %length(&id)=0 %then %do;
   %put ERROR: You must supply a map ID= variable variable;
   %goto done;
   %end;

/*
*-- doesnt work if data set options are specified
%if %sysfunc(exist(&map))=0 %then %do;
   %put ERROR: The MAP= &map data set cannot be found;
   %goto done;
   %end;
*/

%slice(data=&data, var=&given1, slices=&slices1, overlap=&overlap,
	out=out1, outs=_slice1_, slicevar=slice1);
%slice(data=&data, var=&given2, slices=&slices2, overlap=&overlap,
	out=out2, outs=_slice2_, slicevar=slice2);

proc sort data=out1;
	by &id;
proc sort data=out2;
	by &id;
	
data _combine_;
	merge out1(drop=lowerx upperx)
		out2(drop=lowerx upperx);
	by &id;

*-- Find (weighted) means of response for each slice;
proc means noprint nway data=_combine_;
	var &response;
	class slice1 slice2;
	%if %length(&weight) %then %do;
		weight &weight;
		%end;
	output out=_stats_(drop=_type_ _freq_) mean=mean median=median n=n;
proc print data=_stats_;
	id slice1 slice2;

%if %length(&midpoints) %then %do;
*options mprint;
	%let midpoints = %upcase(&midpoints);
	%if %index(&midpoints, PCT) %then %do;
		%let pctiles = %substr(&midpoints, 1,%eval(%index(&midpoints, PCT)-1);
		*-- Find the midpoints as percentiles of the respopnse variable;
		proc univariate data=&data noprint;
			var &response;
			output out=_pctiles_
				pctlpts=&pctiles
				pctlpre=p;
		proc transpose data=_pctiles_ out=_pctiles_;
			var p:;

		proc sql noprint;
			select col1
				into :midpoints separated by ' '
				from _pctiles_;
			select count(col1)
					into :levels
				from _pctiles_;
			quit;
		%put CCMAP: Percentile midpoints(&pctiles) = &midpoints;
		%put CCMAP: Using &levels levels for &response;

		%end;
*options nomprint;
	%end;

%gdispla(OFF);

%let panel=0;
%do j=1 %to &slices2;
	data _strip2_;
		set _slice2_ end=last;
    	where (slice2=&j);
		drop lowerx upperx lopct uppct;
		length text $12 function $8;
    	retain xsys '3' ysys '3' style 'solid' when 'b';

		color="&stripbg"; line=0;
    	x=lopct; y=100;            function='MOVE   '; output;
    	x=uppct; y=100-&hstrip;    function='BAR    '; output;
		/*
		color='grayb0';  line=1;
		y =100-&hstrip;
		x =  0;    function='MOVE   '; output;
		x =100;    function='DRAW   '; output;
		*/
		
		style=''; color="&stripfg";
		x=mean(of lopct uppct); 
		y=100-(&hstrip/2); hsys='1'; size=&hstrip;
		function='label';
		text = "&given2";
		output;
		size=&hstrip/2;
		x = lopct; position='6'; text=put(lowerx,3.0); output;
		x = uppct; position='4'; text=put(upperx,3.0); output;
		
	%do i=1 %to &slices1;
	%let panel=%eval(&panel+1);
	data _strip1_;
		set _slice1_ end=last;
    	where (slice1=&i);
		drop lowerx upperx lopct uppct shrink;
		length text $12 function $8;
    	retain xsys '3' ysys '3' style 'solid' when 'b';

		color="&stripbg"; line=0;
		shrink = (100-&hstrip)/100;
    	y=lopct*shrink; x=0;          function='MOVE   '; output;
    	y=uppct*shrink; x=&hstrip;    function='BAR    '; output;
		
		style=''; color="&stripfg"; angle=90;
		y=shrink*mean(of lopct uppct); 
		x=(&hstrip/2); hsys='1'; size=&hstrip-1;
		function='label';
		text = "&given1";
		output;
		size=&hstrip/2;
		y = lopct*shrink; position='6'; text=put(lowerx,3.0); output;
		y = uppct*shrink; position='4'; text=put(upperx,3.0); output;

	data _inset_;
		set _stats_;
		where (slice1=&i) and (slice2=&j);
		retain xsys ysys '1' size 2;
		x = 98; y = 94; 
		function = 'label   ';
		text = 'Mdn: ' ||put(median, best6.);
		position='A'; output;
		text = 'n:   ' ||put(n, best6.);
		position='D'; output;

	data _strip_;
		set _strip1_ _strip2_ _inset_ &anno;

	*-- Leave some space at top and left for strip labels;
	title h=%eval(&hstrip+2) pct;
	title h=&hstrip pct a=90;

	goptions border;
	proc gmap
		all
		map=&map
		data=_combine_
		gout=&gtemp;
		where (slice1=&i and slice2=&j);
		id &id;
		choro &response / 
            levels=&levels 
			%if %length(&midpoints) %then %do;
			midpoints=&midpoints
			%end;
			%if %length(&coutline) %then %do;
            coutline=&coutline
			%end;
			anno=_strip_ 
		%if &legend=NONE %then nolegend;
		%else %if %length(&legend)>0 %then %str(legend=&legend);
		name = "&name.&i.&j"
		&gmapOptions
			;
		run; quit;
	%end;
%end;
%gdispla(ON);

%panels(rows=&slices2, cols=&slices1, order=down byrows, gin=&gtemp );
	
%if %substr(%upcase(&kill),1,1)=Y %then %do;
  proc catalog kill catalog=&gtemp et=grseg;
run; quit;
%end;

 /*------------------------------------*
  | Clean up datasets no longer needed |
  *------------------------------------*/
proc datasets nofs nolist library=work memtype=(data);
    delete _strip1_ _strip2_ _strip_ combine_ _stats_;
     run; quit;

%DONE:

%mend;

