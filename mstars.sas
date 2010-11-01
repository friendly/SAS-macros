 /*--------------------------------------------------------------*
  *    Name: MSTARS.SAS                                          *
  *   Title: Star plot of Multivariate means                     *
  *--------------------------------------------------------------*/
 

%macro MSTARS( /* Star plot of Multivariate data                           */
/** Param    Default    Req Description ************************************/
    Data    =_LAST_ , /* N  Input SAS Data Set                             */
    Class   =       , /* N  Observation identifier (Must uniquely id obs)  */
    Var     =       , /* Y  List of (numeric) variables                    */
    ErrorBar=       , /* N  List of error bar lengths                      */
/***********************    Tuning Parameters ******************************/
    Minimum =DATA   , /* N  Minimum data value ('DATA' or number)          */
    Maximum =DATA   , /* N  Maximum data value ('DATA' or number)          */
    MinRay  =.1     , /* N  Offset of minimum data value from origin (>=0) */
    NTicks  =2      , /* N  Number of tick marks (0=none, >=2)             */
    RayThick=1      , /* N  Thickness of the rays, errorbars and tickmarks */
    RayLine =1      , /* N  Line style of rays, errorbars and tickmarks    */
    LabSize =1      , /* N  Character size of the variable labels          */
    Circle  =No     , /* N  Draw a circle around the plot                  */
    Symbols =Yes    , /* N  Generate SYMBOL statements                     */
    Symbol_C=       , /* N   Colors on generated SYMBOL statements         */
    Symbol_V=DOT    , /* N   Symbols on generated SYMBOL statements        */
    Symbol_F=NONE   , /* N   Symbol fonts on generated SYMBOL statements   */
    Symbol_H=1      , /* N   Symbol sizes on generated SYMBOL statements   */
    Symbol_L=1      , /* N   Line types on generated SYMBOL statements     */
    Symbol_W=1      , /* N   Line widths on generated SYMBOL statements    */
    AdjHoriz=1.6    , /* N  Adjustment of horizontal plot width (>=1)      */
    AdjVert =1.1    , /* N  Adjustment of vertical plot width (>=1)        */
    AdjLabel=1.02   , /* N  Closeness of label to ray endpoints (>=1)      */
    AdjEBar =4      , /* N  Closeness of error bar to ray (in degrees)     */
    AdjTick =1      , /* N  Width of Tick mark (in degrees)                */
	 EBarLoc = 1       /* N  Error bar location: 1=max, 0=min               */
    );
 /***************************************************************************
 /* Create a star plot of multivariate data.                                *
 /* Each observation is represented by a separate star figure with one      *
 /*   ray for each variable.  An observations position along a ray (obs) is *
 /*   proportional to the value of the observation.                         *
 /* You may need the GOPTIONS  HSIZE= VSIZE= HORIGIN= VORIGIN= to modify    *
 /*    the aspect of the plot.                                              *
 /* Description of the paramerers                                           *
 /* DATA     specifies the SAS Data Set containing the data.                *
 /* CLASS    specifies a single variable that uniquely determines the       *
 /*          observation.  The variable's label and formatted values        *
 /*          are printed below the plot as the plot legend. If omitted the  *
 /*          observation number is used.                                    *
 /* VAR      specifies a list of numeric variables representing the rays    *
 /*          of the star plot.  The variables are spaced equi-angularly     *
 /*          in order around the unit circle, counter-clockwise from the    *
 /*          horizontal (3 o'clock) position.  The variables' labels are    *
 /*          printed next to each ray.                                      *
 /* ERRORBAR specifies a numeric list of error bar lengths (one per VAR     *
 /*          variable.  You may optionally specify a 1 observation SAS Data *
 /*          Set containing the error bar lengths.  This SAS Data Set must  *
 /*          have the same variable names as those listed on the VAR        *
 /*          statement.  If omitted, no error bars are produced.            *
 /* MINUMUM  specifies a minimum and maximum value for the length of a ray. *
 /* MAXIMUM  If omitted,the minimum and maximum observed values are are used*
 /*          These values will be listed in the SASLOG but not indicated    *
 /*          on the plot itself.                                            *
 /* MINRAY   specifies the offset from the origin for the MINIMUM= value.   *
 /*          A value of 0 specifies that each ray starts at the origin.     *
 /*          A small positive value (default=0.1) displaces this minimum    *
 /*          from the origin.                                               *
 /* NTICKS   specifies whether tick marks are placed on the rays.  The      *
 /*          value may be either 0 (for no tick marks) or an integer larger *
 /*          than 1.                                                        *
 /* RAYTHICK specifies the line widths of the lines forming the rays, error *
 /*          bars and tick marks. The value should be a positive integer    *
 /* RAYLINE  specifies the line style of the lines forming the rays         *
 /* LABSIZE  specifies the character size of the variable labels attached   *
 /*          to the end of the rays.  The value should be a positive number.*
 /*          The units are in character cells                               *
 /* CIRCLE   specifies whether the unit circle is plotted. Valid values are *
 /*          NO, YES (a circle around the outer endpoints) and BOTH (two    *
 /*          circles around each set of ray endpoints).                     *
 /* SYMBOLS  specifies whether the macro generates the SYMBOL statements    *
 /*          used to draw the lines of the plot.  If NO is coded, the user  *
 /*          should provide one SYMBOL statement for each observation.      *
 /*          If YES is coded, The user may modify the color, line type      *
 /*          and plotting symbol of the generated SYMBOL statements with    *
 /*          the SYMBOL_X parameters.  If fewer than NOBS values are        *
 /*          coded in a SYMBOL_X parameter, the last value is repeated      *
 /*          for all remaining SYMBOL statements.                           *
 /* SYMBOL_C specifies the colors. If blank, GPLOt will cycle through the   *
 /*          device color list before using the second generated SYMBOL     *
 /*          statement.                                                     *
 /* SYMBOL_V specifies the plotting symbols.  A value of NONE specifies     *
 /*          that only lines are used.                                      *
 /* SYMBOL_F specifies the font of the plotting symbol (eg. MARKER, SPECIAL)*
 /*          A value of NONE allows for the use of the standard SYMBOL      *
 /*          statement symbols (eg. DOT, PLUS. etc.).                       *
 /* SYMBOL_H specifies the size of the plotting symbol.                     *
 /* SYMBOL_L specifies the line type of the lines joining the star vertices.*
 /*          A value of 0 specifies that no lines be drawn.  If fewer than  *
 /*          NOBS values are coded, the line type values are incremented    *
 /*          by 1.                                                          *
 /* SYMBOL_W specifies the line width of the lines forming the stars.       *
 /* ADJHORIZ is an adjustment of the horizontal plot width (>=1). This      *
 /*          value may need to be increased for long labels                 *
 /* ADJVERT  is an adjustment of the vertical plot width (>=1).             *
 /* ADJLABEL is an adjustment of the closeness of label to ray              *
 /*          endpoints (>=1)                                                *
 /* ADJEBAR  is an adjustment of the closeness of error bar to ray          *
 /*          (in degrees)                                                   *
 /* ADJTICK  is an adjustment of the width of each tick mark (in degrees)   *
 /***************************************************************************
 /* Author:   Kevin Thompson     <KT27032@UAFSYSB.UARK.EDU>                 *
 /* Created:  24AUG1992                                                     *
 /* History:  28May1996 ErrorBar list of numbers with decimal points scanned*
 /*                     correctly. Support for abbreviated varlists on VAR  *
 /*                     option (not recommended).                           *
 /* Requires: BASE SAS  (TRANSPOSE, SORT, SUMMARY, FORMAT)                  *
 /*           SAS/Graph (GPLOT)                                             *
 /* Based on the program STARS.SAS by Michael Friendly.  From               *
 /*   ``SAS System for Statistical Graphics, First Edition''                *
 /*     Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA              *
 /* Reference: Chambers, Cleveland, Kleiner & Tukey, Graphical methods      *
 /*            for data analysis, Wadsworth, 1983. pp158-162.               *
 /***************************************************************************/
 
*Error Checking;
%Local NV NOBS CERROR DLM EB I T C L V H F;
%if %UPCase(&DATA)=_LAST_ %then %let DATA=&SYSLast;
%if %UPCase(&DATA)=_NULL_ %then %do;
    %put ERROR: Data Set &DATA not found.;
    %goto EXIT;
    %end;
%if %scan(&CLASS XXXXXXXXX XXXXXXXXX,2)^=XXXXXXXXX %then %do;
    %put ERROR: Only one CLASS variable is allowed;
    %goto EXIT;
    %end;
%if %quote(&VAR)=   %then %do;
    %put ERROR: No list of VAR variables were provided.;
    %goto EXIT;
    %end;
 
*Use Observation number if CLASS= is omitted;
data s_scaled;
     set &DATA;
     length s_obs 3;
     s_obs + 1;
     label s_obs='Observation';
     keep s_obs &CLASS &VAR;
run;
%if &CLASS= %then %let CLASS=s_obs;
 
 
* Restructure the data;
proc transpose data=s_scaled out=s_scaled(rename=(col1=s_value))
               name=s_name label=s_label;
     by s_obs &CLASS;
     var &var;
quit;
data s_scaled;
     length s_obs s_var 3 s_name $8 s_label $40;
     set s_scaled end=s_end;
     by s_obs &CLASS;
     if s_label=' ' then s_label=s_name;
     if first.s_obs then  s_var=0;
     s_var+1;
     if s_end then do;
        call symput('NV'  , left(put(s_var, 2.)));
        call symput('NOBS', put(s_obs,5.));
        end;
run;
proc sort data=s_scaled;
     by s_var s_obs;
quit;
 
*Rescale the data;
proc summary data=s_scaled;
     by s_var;
     var s_value;
     output out=s_minmax(drop=_type_ _freq_) min=s_min max=s_max;
quit;
data s_scaled;
     merge s_scaled s_minmax;
     by s_var;
     %if %upcase(&MINIMUM)=DATA | &MINIMUM=
        %then %do;
              call symput("MINIMUM", left(put(s_min,best5.)));
              %end;
        %else %do;
              s_min=&MINIMUM;
              %end;
     %if %upcase(&MAXIMUM)=DATA | &MAXIMUM=
        %then %do;
              call symput("MAXIMUM", left(put(s_max,best5.)));
              %end;
        %else %do;
              s_max=&MAXIMUM;
              %end;
 
     select;
        when (         s_value <  s_min) do
             s_scaled = &MINRAY ;
             put "WARNING: Var " s_name "has been reset from " s_value "to " s_min
                 "for &CLASS=" &CLASS;
             end;
        when (s_min <= s_value <= s_max) do;
             s_scaled = &MINRAY + ( 1 - &MINRAY ) * ( s_value - s_min ) /
                                                    ( s_max   - s_min ) ;
             end;
        when (s_max <  s_value         ) do
             s_scaled = 1;
             put "WARNING: Var " s_name "has been reset from " s_value "to " s_max
                 "for &CLASS=" &CLASS;
             end;
        otherwise;
        end;
 
     s_angle = 2 * 3.1415926 * (s_var-1) / &NV;
     s_degree = 180 * s_angle / 3.1415926;
     s_x = s_scaled * cos(s_angle);
     s_y = s_scaled * sin(s_angle);
run;
proc sort data=s_scaled;
     by s_obs s_var;
quit;
 
*Close the star polygon;
data s_scaled;
     set s_scaled s_scaled(where=(s_var=1) in=s_closed);
     by s_obs;
     if s_closed then s_var = -1;
run;
 
*Create the Annotate SAS Data Set;
proc format;
   value posn
               22.5-67.5 = 'C'  /* left, above     */
              292.5-337.5= 'F'  /* left, below     */
                  0-22.5 = '6'  /* left, centered  */
              337.5-360  = '6'  /* left, centered  */
              112.5-157.5= 'A'  /* right, above    */
              202.5-247.5= '7'  /* right, below    */
              157.5-202.5= '4'  /* right, centered */
               67.5-112.5= 'B'  /* centered, above */
              247.5-292.5= 'E'  /* centered, below */
                  other  = '6'
                  ;
run;
data s_anno;
   set s_scaled(where=((s_obs=1) & (s_var>0)));
 
   LENGTH X Y SIZE              LINE      8;
   LENGTH TEXT                         $200;
   LENGTH FUNCTION COLOR STYLE          $ 8;
   LENGTH XSYS YSYS HSYS                $ 1;
   LENGTH WHEN POSITION                 $ 1;
 
   WHEN='B';
   XSYS = "2";  YSYS  = "2" ;
   COLOR="        ";
   LINE=&rayline;                       *Line type of rays;
   HSYS="4";                            *Size of text;
   STYLE    = "        ";               *Font of text;
   ANGLE=0; ROTATE=0;                   *Orientation of text;
   position=put(s_degree, posn1.);
   Text=s_label;                        *Text;
 
   Size=&RAYTHICK;
   %if %substr(%upcase(&CIRCLE),1,1)=Y |
       %substr(%upcase(&CIRCLE),1,1)=B %then %do;
       if _N_=1 then do;
          Function='Move' ;    x=1;       y=0;       output;
          do i = 0 to 2*3.1415926 by 0.01;
             Function='Draw' ; x=cos(i);  y=sin(i);  output;
             end;
          end;
       %end;
   %if %substr(%upcase(&CIRCLE),1,1)=B %then %do;
       if _N_=1 then do;
          Function='Move' ;    x=&MINRAY; y=0;       output;
          do i = 0 to 2*3.1415926 by 0.01;
             Function='Draw' ; x=cos(i);  y=sin(i);  output;
             end;
          end;
       %end;
 
 
   Size=&RAYTHICK;
   Function='Move' ; x=cos(s_angle)*(&MINRAY)  ;
                     y=sin(s_angle)*(&MINRAY)  ;   output;
   Function='Draw' ; x=cos(s_angle)            ;
                     y=sin(s_angle)            ;   output;
   Size=&LABSIZE;
   Function='Label'; x=cos(s_angle)*(&ADJLABEL);
                     y=sin(s_angle)*(&ADJLABEL);   output;
 
   Size=&RAYTHICK;
   s_tickw = 3.1415926 * (&ADJTICK) / 180;
   if &NTICKS > 1 then do s_tickn = (0+(&MINRAY=0)) to (&NTICKS-1);
      s_tick = &MINRAY + s_tickn * (1-&MINRAY)/(&NTICKS-1);
      Function='Move'; x=s_tick*cos(s_angle-s_tickw/(2*s_tick));
                       y=s_tick*sin(s_angle-s_tickw/(2*s_tick)); output;
      Function='Draw'; x=s_tick*cos(s_angle                   );
                       y=s_tick*sin(s_angle                   ); output;
      Function='Draw'; x=s_tick*cos(s_angle+s_tickw/(2*s_tick));
                       y=s_tick*sin(s_angle+s_tickw/(2*s_tick)); output;
 
      end;
 
run;
 
%if &ERRORBAR^= %then %do;
    %let EB = %substr(&ERRORBAR,1,1);
    %if ((0<=&EB) & (&EB<=9)) %then %do;
        data s_errbar;
             set &DATA;
             keep &VAR;
             array _var {*} &VAR;
             %do I = 1 %to &NV;
                 %let EB = %scan(&ERRORBAR,&I,%str( ));
                 if "&EB"=" "
                    then _var{&I} = . ;
                    else _var{&I} = &EB ;
                 %end;
         run;
         %let ERRORBAR=s_errbar;
         %end;
    proc transpose data=&ERRORBAR out=s_errbar(rename=(col1=s_errbar))
                   name=s_name;
         var &VAR;
    quit;
    data s_errbar;
         set s_errbar;
         length s_var 3;
         s_var+1;
         keep s_var s_errbar;
    run;
    data s_errbar;
         merge s_scaled(in=in_sc where=((s_obs = 1) & (s_var > 0)))
               s_errbar(in=in_eb);
         by s_var;
 
         s_diff = s_max - s_min;
         if s_errbar > ( s_max - s_min ) then do;
            put "WARNING: Var " s_name "Errorbar has been reset from "
                s_errbar "to " s_diff;
            s_errbar = s_diff;
            end;
         s_errbar = ( 1 - &MINRAY ) * ( s_errbar ) / ( s_max - s_min );
 
         LENGTH X Y SIZE              LINE      8;
         LENGTH TEXT                         $200;
         LENGTH FUNCTION COLOR STYLE          $ 8;
         LENGTH XSYS YSYS HSYS                $ 1;
         LENGTH WHEN POSITION                 $ 1;
 
         WHEN='B';
         XSYS = "2";  YSYS  = "2" ;
         COLOR="        ";
         LINE=1;                              *Line type of error bars;
         Size=&RAYTHICK;
 
         s_adjeb = 3.1415926 * (&ADJEBAR) / 180;
         s_angle = s_angle - s_adjeb;
 
         Function='Move' ; x=cos(s_angle);    y=sin(s_angle);    output;
         Function='Draw' ; x=(1-s_errbar)*x;  y=(1-s_errbar)*y;  output;
 
         s_tickw = 3.1415926 * (&ADJTICK) / 180;
         do s_tick = (1-s_errbar), 1 ;
            Function='Move'; x=s_tick*cos(s_angle-s_tickw/(2*s_tick));
                             y=s_tick*sin(s_angle-s_tickw/(2*s_tick)); output;
            Function='Draw'; x=s_tick*cos(s_angle                   );
                             y=s_tick*sin(s_angle                   ); output;
            Function='Draw'; x=s_tick*cos(s_angle+s_tickw/(2*s_tick));
                             y=s_tick*sin(s_angle+s_tickw/(2*s_tick)); output;
            end;
    run;
    data s_anno;
         set s_anno s_errbar;
    run;
    %end;
 
%put Note: MSTARS plots for data set &DATA ;
%put       Variables are scaled to range &MINRAY(&MINIMUM) to 1(&MAXIMUM) ;
%put       Number of variables    = &NV ;
%put       Number of observations = &NOBS ;
 
*Create plot;
proc gplot data=s_scaled annotate=s_anno;
     plot s_y*s_x=&CLASS  / haxis=axis1 vaxis=axis2
	  		des="mstars plot of &data by &class";
     axis1 order=(-&ADJHORIZ to &ADJHORIZ by &ADJHORIZ) style=0
           label=none value=none major=none minor=none;
     axis2 order=(-&ADJVERT  to &ADJVERT  by &ADJVERT ) style=0
           label=none value=none major=none minor=none;
 
     %if %substr(%upcase(&SYMBOLS),1,1)=Y %then %do;
         %let dlm=' ';
         %do i = 1 %to &NOBS;
             %let T=%scan(&Symbol_C,&I,&DLM);  %if &T^= %then %let C=&T;
             %let T=%scan(&Symbol_L,&I,&DLM);  %if &T^= %then %let L=&T;
             %let T=%scan(&Symbol_W,&I,&DLM);  %if &T^= %then %let W=&T;
             %let T=%scan(&Symbol_V,&I,&DLM);  %if &T^= %then %let V=&T;
             %let T=%scan(&Symbol_H,&I,&DLM);  %if &T^= %then %let H=&T;
             %let T=%scan(&Symbol_F,&I,&DLM);  %if &T^= %then %let F=&T;
             %if %upcase(&F)=NONE %then %let F=;
             %if &L=0
               %then %do; symbol&I C=&C F=&F H=&H V=&V I=NONE          ; %end;
               %else %do; symbol&I C=&C F=&F H=&H V=&V I=JOIN L=&L W=&W; %end;
             %if &L ^= 0 %then %let L=%eval(&L+1);
         %end;
     %end;
run;
quit;
 
%Exit:
%mend;

