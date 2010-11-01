*include macros(mabbrev);
*include macros(genfmt);
%macro dotplot
       (
        /* proc statement options */
        data=_last_,
        gout=gseg,
 
        /* segregate the plot by this variable */
        subgroup=,
 
        /* vertical axis/variable description */
        ylabvar=,
        ysortby=descending &xvar,
        yaxis=axis1,
        yorder=1 to &nobs by 1,
        ylength=,
        ycolor=black,
        ywidth=1,
        ystyle=1,
        ylabel=(f=simplex c=black),
        yvalue=(f=simplex c=black),
        yminor=none,
        ymajor=none,
        yorigin=,
        yoffset=,
 
        /* horizontal/response axis/variable description */
        xvar=,
        xaxis=axis2,
        xorder=,
        xlogbase=,
        xlogstyl=expand,
        xlength=,
        xcolor=black,
        xwidth=1,
        xstyle=1,
        xlabel=(f=simplex c=red),
        xvalue=(f=simplex c=red),
        xminor=(n=1),
        xmajor=,
        xorigin=,
        xoffset=,
        xref=,
        cxref=black,
        lxref=1,
 
        /* dot description */
        dvalue=J,
        dfont=special,
        dcolor=red,
        dheight=2,
 
        /* line description */
        connect=DOT,
        dlwidth=1,
        dlstyle=2,
        dlcolor=black,
 
        /* aspects of the plot */
        caxis=black,
        ctext=red,
        label=,
 
        /* top and right axis markings */
        topaxis=,
        rgtaxis=,
 
        /* types of plot */
        printer=NO,
        plotter=YES,
 
 
        /* graphics sample library description */
        sampdd=SAMPGRP,
        sampfn=SAMPGRP MACLIB *,
        annomem=ANNOCMS,
 
        /* generate a format? format name? */
        genfmt=YES,
        out=_dot_,
        format=dotfm,
        library=,
        newvar=id,
        newlabel=%upcase(&ylabvar),
 
        /* include a macro in the annotate data step */
        annomac=,
        annoprt=NO
       );
 
%***********************************************************************
%*  DOTPLOT - generate a dot plot (w cleveland, am statistician, 1984).
%*
%*       set the associated file DOTPLOT MEMO for documentatiuon and
%*       examples.
%*
%*                                            Barry W Grau
%*                                            U42054@UICVM.UIC.EDU
%*                                            U42054@UICVM.BITNET
%*                                            June 1988
%***********************************************************************
;
*
options nosource2;
 
%* first, generate the format.;
  %let genfmt=%upcase(&genfmt);
  %if %mabbrev(YES,&genfmt)
       %then %let genfmt=1;
       %else %if %mabbrev(NO,&genfmt)
            %then %let genfmt=0;
            %else %do;
                 %put NOTE: &genfmt IS NOT A VALID VALUE FOR GENFMT.;
                 %put NOTE: "YES" WAS USED.;
                 %let genfmt=1;
                 %end;
 
  %if &genfmt
       %then %do;
            X SET CMSTYPE HT;
            X ERASE &format TEXT *;
            X SET CMSTYPE RT;
            %genfmt
                   (
                    data=&data,
                    out=&out,
                    var=&ylabvar,
                    newvar=&newvar,
                    format=&format,
                    library=&library,
                    sortby=&ysortby
                   );
            %end;
 
* next, draw the connecting lines.;
%if &annomem ^= %str() %then %do;
   x FILEDEF &sampdd DISK &sampfn;
   %inc &sampdd(&annomem) / nosource2 ;
%end;
 
data _annodp_;
     if _n_=1
          then do;
               _point_=1;
               if 0
                    then set &out point=_point_ nobs=_nobs_;
               call symput('NOBS',_nobs_);
               end;
 
     %dclanno;
     set &out;
 
     %if %mabbrev(ZERO,&connect)
         %then %do;
            %system(2,2,4);
         %end;
     %else %do;
            %system(1,2,4);
         %end;
     %move(0,&newvar);
 
     %let connect = %upcase(&connect);
     %if %mabbrev(DOT,&connect)
       | %mabbrev(ZERO,&connect)
          %then %do;
               %system(2,2,4);
               %draw(&xvar,&newvar,&dlcolor,&dlstyle,&dlwidth);
               %end;
          %else %if %mabbrev(NONE,&connect)
               %then %do;
                    &annomac;
                    return;
                    %end;
               %else %do;
                    %if %mabbrev(AXIS,&connect)=0
                    %then
                    %put NOTE: DRAWTO MUST BE AXIS DOT OR NONE. AXIS WAS USED.;
                       %draw(100,&newvar,&dlcolor,&dlstyle,&dlwidth);
                       %end;
     &annomac;
     run;
 
%* should we print the annotate dataset?;
     %let annoprt=%upcase(&annoprt);
     %if %mabbrev(YES,&annoprt)
          %then %let annoprt=1;
          %else %if %mabbrev(NO,&annoprt)
               %then %let annoprt=0;
               %else %do;
                    %put NOTE: ANNOPRT MUST BE YES OR NO. NO WAS USED.;
                    %let annoprt=0;
                    %end;
     %if &annoprt
          %then %do;
               proc print;
               run;
               %end;
 
* finally, draw the dot plot.;
 
*   the printer plot;
%let printer = %upcase(&printer);
%if %mabbrev(YES,&printer)
    %then %let printer=1;
    %else %if %mabbrev(NO,&printer)
         %then %let printer=0;
         %else %do;
              %put NOTE: PRINTER MUST BE YES OR NO. YES WAS USED.;
              %let printer=1;
              %end;
%if &printer
    %then %do;
          proc plot data=&out;
               plot &newvar*(&xvar)='*'/
                    %if &yorder ^= %str()
                         %then vaxis=&yorder;
                    %if &xorder ^= %str()
                         %then haxis=&xorder;
                    %if &xref ^= %str()
                         %then href=&xref;
                    ;
 
          *   variable labels;
             label
             %if &label^=%str()
                 %then &label;
             %if &newlabel^=%str()
                  %then &newvar="&newlabel";
             ;
 
          *   the format;
             %if &format^=%str()
                 %then format &newvar &format..;;
          run;
          %end;
 
 
* the plotter plot;
%let plotter = %upcase(&plotter);
%if %mabbrev(YES,&plotter)
    %then %let plotter=1;
    %else %if %mabbrev(NO,&plotter)
         %then %let plotter=0;
         %else %do;
              %put NOTE: PLOTTER MUST BE YES OR NO. YES WAS USED.;
              %let plotter=1;
              %end;
%if &plotter
    %then %do;
proc gplot data=&out
           gout=&gout;
     plot &newvar*(&xvar)=1/
          anno=_annodp_
          frame
          caxis=&caxis
          ctext=&ctext
          %if &yaxis ^= %str()
               %then vaxis=&yaxis;
          %if &xaxis ^= %str()
               %then haxis=&xaxis;
          %if &xref ^= %str()
               %then %do;
                    href=&xref
                    %if &cxref ^= %str()
                         %then chref=&cxref;
                    %if &lxref ^= %str()
                         %then lhref=&lxref;
                    %end;
         ;
*   the dot;
symbol1 color=&dcolor
        v=&dvalue
        f=&dfont
        h=&dheight;
 
*    the axes;
&yaxis
     %if &yorder ^= %str()
          %then order = &yorder;
     %if &ylength ^= %str()
          %then length = &ylength;
     %if &ycolor ^= %str()
          %then color = &ycolor;
     %if &ywidth ^= %str()
          %then width = &ywidth;
     %if &ystyle ^= %str()
          %then style = &ystyle;
     %if &ylabel ^= %str()
          %then label = &ylabel;
     %if &yvalue ^= %str()
          %then value = &yvalue;
     %if &yminor ^= %str()
          %then minor = &yminor;
     %if &ymajor ^= %str()
          %then major = &ymajor;
     %if &yorigin ^= %str()
          %then origin = &yorigin;
     %if &yoffset ^= %str()
          %then offset = &yoffset;
     ;
&xaxis
     %if &xorder ^= %str()
          %then order = &xorder;
     %if &xlogbase ^= %str()
          %then %do;
               logbase = &xlogbase
               %if &xlogstyl ^= %str()
                    %then %do;
                         %let xlogstyl = %upcase(&xlogstyl);
                         %if &xlogstyl ^= POWER & &xlogstyl ^= EXPAND
                            %then %do;
                             %put NOTE: XLOGSTYL MUST BE "EXPAND" OR "POWER".;
                             %put NOTE: "EXPAND" WAS USED.;
                             %let xlogstyl = EXPAND;
                             %end;
                    logstyle = &xlogstyl
                    %end;
               %end;
     %if &xlength ^= %str()
          %then length = &xlength;
     %if &xcolor ^= %str()
          %then color = &xcolor;
     %if &xwidth ^= %str()
          %then width = &xwidth;
     %if &xstyle ^= %str()
          %then style = &xstyle;
     %if &xlabel ^= %str()
          %then label = &xlabel;
     %if &xvalue ^= %str()
          %then value = &xvalue;
     %if &xminor ^= %str()
          %then minor = &xminor;
     %if &xmajor ^= %str()
          %then major = &xmajor;
     %if &xorigin ^= %str()
          %then origin = &xorigin;
     %if &xoffset ^= %str()
          %then offset = &xoffset;
     ;
 
*   the format;
   %if &format^=%str()
       %then format &newvar &format..;;
 
*   labels;
   label
   %if &label^=%str()
       %then &label;
   %if &newlabel^=%str()
        %then &newvar="&newlabel";
   ;
 
run;
%end;
 
%mend;
