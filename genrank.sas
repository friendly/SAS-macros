/* Produce a generalized rank plot with proc plot. */
%macro genrank(                   /* Required options:                */
               h=,                /* horizontal axis variable         */
               v=,                /* vertical axis variable           */
               label=,            /* label variable                   */
               hpos=,             /* horizontal axis positions        */
               vpos=,             /* vertical axis positions          */
                                  /* Optional options:                */
               data=_last_,       /* SAS data set                     */
               hsep=1,            /* horizontal axis separation param */
               vsep=1,            /* vertical axis separation param   */
               symbol=,           /* symbol character or variable     */
               procopts=,         /* proc statement options           */
               plotopts=,         /* plot statement options           */
               hlabel=,           /* horizontal axis label            */
               vlabel=,           /* vertical axis label              */
               hformat=best7.,    /* horizontal axis tick format      */
               vformat=best7.);   /* vertical axis tick format        */
 
%macro form(coor,fname,var,fmt);  /* format creation macro            */
proc sort data=genwork out=genwork;
	by &coor; run;
 
data genform(keep=start label hlo fmtname);
   set genwork;
   by &coor;
   length start $ 25;
   fmtname = &fname;
   start   = left(&coor);
   label   = put(&var,&fmt);
   hlo     = ' ';
   if first.&coor then output;
   if _n_ = 1 then do;
      hlo   = 'O';
      start = 'OTHER';
      label = ' ';
      output;
      end;
   run;
 
proc format cntlin=genform; run;
%mend;
 
%if %length(&h) = 0 or %length(&v) = 0 or %length(&hpos) = 0 or
    %length(&vpos) = 0 or %length(&label) = 0 %then %do;
    %put ERROR: Required parameter missing.;
    %goto endit;
    %end;
 
options nonotes;
proc sort data=&data(keep=&v &h &label) out=genwork; by &h; run;
 
proc means noprint; /* find minima and maxima */
   var &v &h;
   output out=genstat min=vmin hmin max=vmax hmax;
   run;
 
data genwork; /* find horizontal axis variable ranks */
   set genwork;
   hcoor = _n_;
   run;
 
proc sort data=genwork out=genwork; by &v; run;
 
data genwork(keep=&v &h &label hcoor vcoor);
   set genwork;    /* compute coordinates */
   retain hs vs;
   if _n_ = 1 then do;
      set genstat;
      hs = (&hpos - 1 - &hsep * (_freq_ - 1)) / (hmax - hmin);
      vs = (&vpos - 1 - &vsep * (_freq_ - 1)) / (vmax - vmin);
      if hs < 0 or vs < 0 then
         put / 'ERROR: HPOS= or VPOS= is too small.' /
               '       Macro will not work correctly.' / ;
      end;
   hcoor = round((&h - hmin) * hs + &hsep * (hcoor - 1)) + 1;
   vcoor = round((&v - vmin) * vs + &vsep * (_n_   - 1)) + 1;
   run;
 
%form(hcoor,'hf',&h,&hformat);  /* create format for h axis */
%form(vcoor,'vf',&v,&vformat);  /* create format for v axis */
 
options notes;
proc plot &procopts
   data=genwork(keep=vcoor hcoor &label rename=(vcoor=&v hcoor=&h));
   plot &v * &h $ &label %if %length(&symbol) %then = &symbol;
        / &plotopts hpos=&hpos vpos=&vpos hspace=1 vspace=1;
   format &v vf. &h hf.;
   %if %length(&hlabel) %then label &h = &hlabel%str(;);
   %if %length(&vlabel) %then label &v = &vlabel%str(;);
   run; quit;
 
%endit: options notes;
%mend genrank;
