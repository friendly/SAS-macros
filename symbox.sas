 /*--------------------------------------------------------------*
  *    Name: symbox.sas                                          *
  *   Title: Show transformations of a variable as boxplots      *
        Doc: http://www.datavis.ca/sasmac/symbox.html      
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  24 Sep 1997 06:55:08                               *
  * Revised:  07 Apr 2005 16:03:20                               *
  * Version:  1.2                                                *
  * 1.1  - Use variable label in the boxplot; note dropped obs.  *
  *    Fixed buglet with null DATA=                              *
  * 1.2  - Keep original variable name. Handle multiple VAR=     *
  *    Added %gskip for multiple EPS/GIF plots                   *
  *    Added inline documentation                                *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 Various graphical displays for diagnosing symmetry of a distribution
 are available in the SYMPLOT macro.  SYMBOX takes a more direct
 approach.  The variable is first transformed to each of the selected
 powers, then each transform is standardized to mean=0, std=1, and
 then displayed as side-by-side boxplots, allowing direct visual
 comparison of various transformations.
	
 Missing and non-positive observations are excluded.
	
=Usage:

 The SYMBOX macro is defined with keyword parameters.  The VAR=
 parameter must be specified.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%symbox();
 
==Parameters:

* DATA=       Name of input data set [Default: DATA=_LAST_]

* VAR=        A blank-separated list of the name(s) of the variable(s) to examine.  

* ID=         Name of an ID variable, used to label outside observations

* OUT=        Name of the output data set [Default: OUT=SYMOUT]

* ORIENT=     Orientation of the boxplots [Default: ORIENT=V]

* POWERS=     List of powers to consider [Default: POWERS=-1 -0.5 0 .5 1]

* NAME=       Name for graphics catalog entry [Default: NAME=SYMBOX]
                
==Dependencies:
	%gskip; %boxplot
 
 =*/
 
%macro symbox(
	data=_last_,     /* name of input data set                    */
	var=,            /* name(s) of the variable(s) to examine     */
	id=,             /* name of ID variable                       */
	out=symout,      /* name of output data set                   */
	orient=V,        /* orientation of boxplots                   */
	powers=-1 -0.5 0 .5 1,  /* list of powers to consider         */
	name=symbox
	);

%local abort;
%let abort=0;
%if %length(&var)=0
	%then %do;
		%put ERROR: You must specify a VAR= variable;
		%let abort=1;
		%goto DONE;
	%end;

%local i npow nv p1 p2;
%let npow = %numwords(&powers);
%let nv = %numwords(&var);

%let p1 = %scan(&powers,1,%str( ));
%let p2 = %scan(&powers,&npow,%str( ));
%*put npow = &npow p1=&p1 p2=&p2 p3=&p3;

%if %upcase(&data)=_LAST_ %then %let data = &syslast;

proc format;
   value pow -1 ='-1/X' -0.5= '-1/Sqrt' 0='Log' .5='Sqrt' 1='Raw';

%*-- Loop over variables in the VAR= list;
%let varlist = &var;
%do vi = 1 %to &nv;
	%let var=%scan(&varlist, &vi);
	%put NOTE: Computing &npow power transforms for &var ...;
		
	data _null_;
		set &data(obs=1);
		length _label_ $40;
		call label(&var, _label_);
		_label_= trim(_label_) || ' (Std.)';
		call symput('label', _label_);
		run;
	%*put VAR=&var LABEL=&LABEL;
	%if &syserr > 4 %then %let abort=1; %if &abort>0 %then %goto DONE;
	
	%let pstep=0.5;
	data _bb_;
		set &data end=eof;
		keep &var power &id;
		label &var="&label";
	
		step=1;
		_var_ = &var;
		if &var > 0 then do;
			%do i=1 %to &npow;
				power = %scan(&powers, &i, %str( ));
				if int(power) ^= power then step=0.5;
				if power=0 then &var = log10(_var_);
								else &var =  (_var_)**power / power;
			output;
			%end;
		end;
		else do;
			dropped+1;
			end;
	
		if eof then do;
			if dropped>0 
				then put 'NOTE: ' dropped ' observatations were missing or negative,'
				' and were excluded from the analysis.';
	*		call symput('pstep', left(put(step,best8.)));
			end;
	run;
	%if &syserr > 4 %then %let abort=1; %if &abort>0 %then %goto DONE;
	
	proc sort data=_bb_; 
		by power;
	proc standard m=0 s=1 out=&out;
		var &var;
		by power;
	run;
	
	%boxplot(data=&out, var=&var, id=&id,
			class=Power, classfmt=pow., orient=&orient, name=&name,
			xorder=&P1 to &P2 by &pstep, varlab=&label, print=);
	
	%gskip;
	%end;

  *-- Clean up datasets no longer needed;
proc datasets nofs nolist library=work memtype=(data);
    delete _bb_;
	 run; quit;

%done:
%if &abort %then %put ERROR: The SYMBOX macro ended abnormally.;
%mend;

%macro numwords(lst);
   %local i;
   %let i = 1;
   %let v = %scan(&lst,&i);
   %do %while (%length(&v) > 0);
      %let i = %eval(&i + 1);
      %let v = %scan(&lst,&i, %str( ));
      %end;
   %eval(&i - 1)
%mend;
