%macro boxplot(                /* -------------------------          */
       data=_LAST_,            /* Input dataset                      */
       where=,                 /* WHERE clause to subset data        */
       var=,                   /* Analysis variable(s)               */
       class=,                 /* Grouping variable                  */
	   block=,                 /* Block variable(s)                  */
       id=,                    /* Observation ID variable            */
	   by=,                    /* BY variable(s)                     */
       sortby=,                /* Order classes by var, _mean_,...   */
       ofactor=1.5,            /* IQR multiplier for outside values  */
	   idsymbol=square,        /* symbol for outside observations    */
       width=,                 /* Box width (screen percent)         */
       notch=0,                /* =0|1, 1=draw notched boxes         */
       cbox=BLACK,             /* color of box outline               */
       cfill=,                 /* color for box fill                 */
       cnotch=,                /* color for notch fill               */
       lbox=1,                 /* line style for box outline         */
       sbox=1,                 /* line thickness for box outline     */
       orient=V,               /* box orientation: H or V            */
       connect=,               /* =0 or line style to connect medians*/
       f=0.5,                  /* Notch depth, fraction of halfwidth */
       fn=1,                   /* Box width proportional to &FN      */
       varfmt=,                /* Format for analysis variable       */
       classfmt=,              /* Format for class variable(s)       */
       vaxis=,
       haxis=,
       yorder=,                /* Tick marks, range for ordinate     */
       xorder=,                /* Tick marks, range for abscissa     */
       anno=,                  /* Addition to ANNOTATE set           */
       out=boxstat,            /* Output data set: quartiles, etc.   */
       options=,               /* Additional options for PROC BOXPLOT */
       name=BOXPLOT,           /* Name for graphic catalog entry     */
       gout=                   /* The name of the graphics catalog   */
       );
 

    %*-- Reset required global options;
    %if &sysver >= 8 %then %do;
        %local o1 o2;
        %let o1 = %sysfunc(getoption(notes));
        *let o2 = %sysfunc(getoption(validvarname,keyword));
        options nonotes /* validvarname=upcase */;
        %end;
    %else %do;
       %put This version requires SAS Version 8+;
        %end;

%let abort=0;
%if %length(&var)=0 | %length(&class)=0
   %then %do;
      %put ERROR: The VAR= and CLASS= parameters must be specified;
      %let abort=1;
      %goto DONE;
   %end;

%if &gout^=%str()  %then %let gout=GOUT=&gout;
*goptions reset=symbol;  *-- cancel prior SYMBOL stmts;

%if %upcase(&data)=_LAST_ %then %let data = &syslast;
%let _DSN_ = %upcase(&DATA);

%let CLASS = %upcase(&CLASS);
*let PRINT = %upcase(&PRINT);
%let ORIENT= %upcase(&ORIENT);

%if &orient = H %then %do;
    %put ORIENT=H is not implemented in this version.;
   %end;

%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
%local clvars vars;
%let clvars = %nvar(&class);
%let vars = %nvar(&var);

 
%if %length(&vaxis)=0 %then %do;
    %let vaxis=axis98;
    axis98 minor=(number=1)
    %if %length(&yorder) > 0 %then order=(&yorder);
    ;
    %end;

%if %length(&haxis)=0 %then %do;
    %let haxis=axis99;
    axis99 minor=none
    %if %length(&xorder) > 0 %then order=(&xorder);
    ;
    %end;

proc boxplot data=&data &gout;
     %if %length(&where)>0 %then %do;
     where (&where);
     %end;

	%if &vars=1 %then %do;
      plot &var * &class
	  %end;
	%else %do; 
      plot (&var) * &class
	  %end;

	 %if %length(&block) %then %do; (&block) %end;
 
        /
        boxstyle=schematicidfar
        vaxis=&vaxis haxis=&haxis
		blockpos=3
        %if %length(&anno) %then  annotate=&anno ;
        %if %length(&cbox) %then  cboxes=&cbox ;
        %if %length(&lbox) %then  lboxes=&lbox ;
        %if %length(&cfill) %then  cboxfill=&cfill ;
        %if %length(&connect)  %then  boxconnect=median ;
        %if %length(&idsymbol) %then  idsymbol=&idsymbol ;
        %if &notch ^= 0  %then  notches ;
        %if %length(&options) %then  &options ;
         name="&name"
            des="Boxplot of &var in &_dsn_" 
        ;
    %if %length(&id) %then %do;   
		id &id;  
		%end;
    %if %length(&by) %then %do;   
		by &by;  
		%end;
  %if &varfmt ^=  %str() %then %do;
  		format &var   &varfmt ;
       %end;
  %if %length(&classfmt) & &clvars=1 %then %do;
  		format &CLASS &classfmt ;    %end;
  run; quit;
*goptions reset=symbol;  *-- cancel prior SYMBOL stmts;

%done:
%if &abort %then %put ERROR: The BOXPLOT macro ended abnormally.;


    %*-- Restore global options;
    %if &sysver >= 8 %then %do;
        options &o1 &o2;
        %end;
    %else %do;
       options notes;
        %end;

%mend;

 /*----------------------------------*
  | Count number of &CLASS variables |
  *----------------------------------*/
%macro nvar(varlist);
   %local wvar result;
   %let result = 1;
   %let wvar = %nrbquote(%scan( &varlist, &result));
   %do %until ( &wvar= );
       %let result = %eval( &result + 1);
       %let wvar = %nrbquote(%scan( &varlist, &result));
   %end;
   %eval( &result - 1)
%mend nvar;
