/*--------------------------------------------------------------*
  *    Name: regline.sas                                         *
  *   Title: Create annotate data set to draw a regression line  *
        Doc: http://www.datavis.ca/sasmac/regline.html     
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 20 Aug 2002 11:15:43                                *
  * Revised: 12 May 2006 16:58:57                                *
  * Version: 1.1                                                 *
  *  1.1  Added BY= for multiple lines                           *
  *       Added WHEN=                                            *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The REGLINE macro creates an ANNOTATE dataset to draw a regression
 line in a plot.  It is useful for situations where the SYMBOL statement
 with INTERPOL=RL cannot be used.

=Usage:

 The REGLINE macro is defined with keyword parameters.
 The X= and Y= parameters are required; all others are optional.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
    %regline(data=iris, x=petalwid, y=petallen, width=3, xmin=0, xmax=26);

==Parameters:

* DATA=       Input data set [Default: DATA=_LAST_]

* Y=          Ordinate variable

* X=          Abscissa variable

* Z=          Z value or variable name for use with proc G3D

* OTHERX=     Other predictors in the model

* BY=         Multiple reglines for each level of the BY= variable

* XMIN=       Override xmin in data?

* XMAX=       Override xmax in data?

* WEIGHT=     Name of a weight or frequency variable if the data
              are grouped or you need to perform a weighted regression.

* COLOR=      Line color [Default: COLOR=BLACK]

* LINE=       Line style [Default: LINE=1]

* WIDTH=      Line width [Default: WIDTH=1]

* IN=         Input annotate data set(s)

* OUT=        Output annotate data set [Default: OUT=REGLINE]

* NOTES=      Suppress notes?
                

 =*/
 
%macro regline(
    data=_last_,      /* input data set                */
    y=,               /* ordinate variable             */
    x=,               /* abscissa variable             */
	otherx=,          /* other predictors in the model */
    by=,              /* multiple reglines */
    xmin=,            /* override xmin in data?        */
    xmax=,            /* override xmax in data?        */
    z=,               /* Z value for use with proc G3D */
    weight=,          /* weighted regression?          */
    color=black,      /* line color                    */
    line=1,           /* line style                    */
    width=1,          /* line width                    */
    when=,            /* when to draw the line: B or A */
    in=,              /* input annotate data set       */
    out=regline,      /* output annotate data set      */
    notes=nonotes     /* suppress notes?               */
    );
    
%if %upcase(&data)=_LAST_ %then %let data = &syslast;
%let abort=0;
%if &x=%str() or &y=%str() %then %do;
   %put REGLINE:  The X= and Y= variables must be specified;
    %let abort=1;
   %goto DONE;
   %end;

%if %upcase(&x)=X or %upcase(&x)=Y %then %do;
   %put REGLINE:  The X= variable may not be named "&x";
    %let abort=1;
   %goto DONE;
   %end;

%if %upcase(&y)=Y or %upcase(&y)=X %then %do;
   %put REGLINE:  The Y= variable may not be named "&y";
    %let abort=1;
   %goto DONE;
   %end;

%if %substr(%upcase(&notes),1,3)=NON %then %str(options nonotes;);
proc reg data=&data outest=_parms_ noprint;
    %if %length(&weight) %then %do;
        weight &weight;
        %end;
   model &y =  &x &otherx;
    %if %length(&by) %then %do;
        by &by;
        %end;

proc print data=_parms_;

proc means data=&data noprint;
   var &x;
   output out=_sum_  min=xmin max=xmax;

data &out;
    set _parms_(drop = _MODEL_ _TYPE_ _DEPVAR_ _RMSE_);
    if _n_=1 then set _sum_   (keep=xmin xmax);
    retain xmin xmax;
   length function $8;

    color="&color";
    line=&line;
    size=&width;
    %if %length(&when) %then %str(when="&when";);
    %if %length(&xmin) %then %str(xmin=&xmin;);
    %if %length(&xmax) %then %str(xmax=&xmax;);

   *-- Draw the regression line;
    %if %sysevalf(&sysver  >= 7) %then %let int=Intercept;
    %else %let int=intercep;
   xsys='2'; ysys='2';
    %if %length(&z)>0 %then %do;
        zsys='2';
        z=&z;
        %end;
   x=xmin; y = &int + &x * x; function='MOVE'; output;
   x=xmax; y = &int + &x * x; function='DRAW'; output;
drop xmin xmax &int &x &y;
run;

%if %length(&in)>0 %then %do;
    data &out;
        set &in &out;
    %end;
    
%done:
%if %substr(%upcase(&notes),1,3)=NON %then %str(options notes;);
%if &abort %then %put ERROR: The REGLINE macro ended abnormally.;
%mend;

    
