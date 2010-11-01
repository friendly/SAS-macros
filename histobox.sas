 /*-------------------------------------------------------------------*
  *    Name: HISTOBOX.SAS                                             *
  *   Title: Annotate a histogram with univariate boxplot             *
  *-------------------------------------------------------------------*
  * Author:   Christian Sieder       <sieder@bionorica.neumarkt.de>   *
  *           Bionorica Arzneimittel GmbH                             *
  * Created:  16 Apr 1998                                             *
  * Revised:  not yet                                                 *
  * Version:  1.1                                                     *
  *             *added ftext and htext parameters                     *
  *                                                                   *
  *                                                                   *
  *   Uses some code and ideas of Michael Friendly's BOXAXIS macro    *
  *      Thank you, Michael!                                          *
  *                                                                   *
  *                                                                   *
  *-------------------------------------------------------------------*/

%macro histobox(
                var=,       /*required*/   /*variable to be charted     */
                min=,       /*required*/   /*minimum value to draw      */
                max=,       /*required*/   /*maximum value to draw      */
                nbars=,     /*required*/   /*number of bars to draw     */
                data=_last_,               /*data set                   */
                fal=Absolute frequency,    /*frequency axis label       */
                pal=Relative frequency,    /*percentage axis label      */
                val=,                      /*value axis label           */
                ftext=swissb,              /*text font                  */
                htext=1,                   /*text height                */
                ls=25,                     /*left space                 */
                lls=5,                     /*left label space           */
                ts=20,                     /*title space                */
                tls=10,                    /*top label space            */
                bls=10,                    /*bottom label space         */
                bs=10,                     /*bottom space               */
                rs=20,                     /*right space                */
                fs = 2,                    /*frame space                */
                bw=5,                      /*box width                  */
                bpos=90                    /*box position               */
                );

  %local xo xfactor yo yfactor;
  *let xo=%sysevalf(&ls+&lls);             /*origin x-coordinate        */
  *let xfactor=%sysevalf((100-&ls - &lls - &rs)/100); /*x-factor for chart area    */
  *let yo=%sysevalf(100-&ts - &tls);       /*origin y-coordinate (upper right corner)*/
  *let yfactor=%sysevalf((100-&bs - &bls - &tls - &ts)/100); /*x-factor for chart area*/

  %if ( &var = %str() or &nbars = %str() ) %then %do;
    %put ERROR: analysis variable (VAR) and number of bars (NBARS) must be specified;
    %goto done;
  %end;

  %if ( &min = %str() or &max = %str() ) %then %do;
    %put ERROR: minimum (MIN) and maximum (MAX) for scaling axes must be specified;
       %goto done;
  %end;

  %if &val = %str() %then %let val = &var;

  /*create sorted dataset*/
  data data;
    set &data (keep=&var);
  proc sort;by &var;run;


  /*--------------------------------------*
   | create data set containing bar stats |
   *--------------------------------------*/

  /*data set n contains number of obs*/
  data n;
    set data end=end;
    if end;
    n = _n_;
    keep n;
  run;

  data barstats;
    if _n_=1 then set n;
    retain barno barmin barmax barwidth freq;
    /*initialize vars*/
    barno=1;
	 barmin=&min;
	 barwidth=(&max-&min)/&nbars;
	 barmax=barmin+barwidth;
	 freq=0;pct=0;
 nextobs:
    set data end=end;
    countup:
    if ((&var ge barmin)and(&var lt barmax)) then do;
      freq = freq+1;
      pct=freq/n;
      if end then do;
        if barno le &nbars then output;
        do while (barno lt &nbars);
          barno=barno+1;
			 barmin=barmin+barwidth;
			 barmax=barmax+barwidth;
			 freq=0;pct=0;
          output;
        end;
      end;
    end;
    if &var ge barmax then do;
      if barno le &nbars then output;
      barno=barno+1;
		barmin=barmin+barwidth;
		barmax=barmax+barwidth;
		freq=0;pct=0;
      goto countup;
    end;
    goto nextobs;
    keep barno barmin barmax barwidth freq pct n;
  run;

  /*get largest percentage and freq*/
  proc sql;
  create table barstats as
  select *,  max(pct) as maxpct, max(freq) as maxfreq
                from barstats;

  /*find axis maximum and calculate bar lenght relative to it*/
  data barstats;
    set barstats;
    if mod(maxpct*100,5)=0 then maxpct=maxpct+.05;
    else maxpct =.005*round(maxpct*200,10);
    maxpct=maxpct+.1;                        /*add 10 % space for box-plot*/
    pctofmax=pct/maxpct;
  *proc print;run;

 /*----------------------------------*
  | Find median & quartiles          |
  *----------------------------------*/
  proc univariate data=data noprint;
      var &var;
      output out=quartile
             n=n q1=q1 q3=q3 median=median qrange=iqr mean=mean;
  run;
   /*-----------------------------------------------*
    | Find outside & farout points                  |
    *-----------------------------------------------*/
  data plotdat;
      set data;
      if _n_=1 then set quartile;
      retain q1 q3 iqr;
      keep &var outside;
         if &var >.Z;
      outside=1;
      if &var < (q1-1.5*iqr) or &var > (q3+1.5*iqr)
         then outside=2;
      if &var < (q1-3.0*iqr) or &var > (q3+3.0*iqr)
         then outside=3;
  run;
   /*----------------------------------------------------*
    |  Whiskers go from quartiles to most extreme values |
    |  which are *NOT* outside.                          |
    *----------------------------------------------------*/
  proc univariate data=plotdat noprint;
      where (outside=1);
      var &var;
      output out=whisk min=lo_whisk max=hi_whisk;
  run;
  data boxfile;
      merge quartile whisk;
  *proc print data=boxfile;    *proc print data=plotdat;run;

   /*-----------------------------------------------*
    | Annotate data set to draw boxes & whiskers    |
    *-----------------------------------------------*/
  data boxanno;
    set boxfile;
    retain xsys ysys hsys when position line color;
    length function $8;
    xsys='3'; ysys='3'; hsys='4'; when='B'; position='5'; line=1;color='red';
    yrange=&max-&min;
    xo=(&ls+&lls);             /*origin x-coordinate        */
	 yo=(100-&ts - &tls);       /*origin y-coordinate (upper right corner)*/ 
    xfactor=((100-&ls - &lls - &rs)/100); /*x-factor for chart area    */
    yfactor=((100-&bs - &bls - &tls - &ts)/100); /*x-factor for chart area*/
	 	 
    /*low whisker*/
    if LO_WHISK lt &min then LO_WHISK=&min;
    function = 'MOVE';    x=xo+(&bpos+(.25*&bw))*xfactor;
                          y=yo-(LO_WHISK-&min)/yrange*100*yfactor;
                          output;
    function = 'DRAW';
                          x=xo+(&bpos+(.75*&bw))*xfactor;
                          y=yo-(LO_WHISK-&min)/yrange*100*yfactor;
                          line=1;output;

    function = 'MOVE';    x=xo+(&bpos+(.5*&bw))*xfactor;
                          y=yo-(LO_WHISK-&min)/yrange*100*yfactor;
                          output;
    function = 'DRAW';
                          x=xo+(&bpos+(.5*&bw))*xfactor;
                          y=yo-(Q1-&min)/yrange*100*yfactor;
                          line=1;output;

    /*draw median*/
    function = 'MOVE';    x=xo+&bpos*xfactor;
                          y=yo-(MEDIAN-&min)/yrange*100*yfactor;
                          output;
    function = 'DRAW';
                          x=xo+(&bpos+(1*&bw))*xfactor;
                          y=yo-(MEDIAN-&min)/yrange*100*yfactor;
                          line=1;output;

    /*high whisker*/
    if HI_WHISK gt &max then HI_WHISK=&max;
    function = 'MOVE';    x=xo+(&bpos+(.25*&bw))*xfactor;
                          y=yo-(HI_WHISK-&min)/yrange*100*yfactor;
                          output;
    function = 'DRAW';
                          x=xo+(&bpos+(.75*&bw))*xfactor;
                          y=yo-(HI_WHISK-&min)/yrange*100*yfactor;
                          line=1;output;

    function = 'MOVE';    x=xo+(&bpos+(.5*&bw))*xfactor;
                          y=yo-(HI_WHISK-&min)/yrange*100*yfactor;
                          output;
    function = 'DRAW';
                          x=xo+(&bpos+(.5*&bw))*xfactor;
                          y=yo-(Q3-&min)/yrange*100*yfactor;
                          line=1;output;

    /*draw box*/
    function = 'MOVE';    x=xo+(&bpos+(0*&bw))*xfactor;
                          y=yo-(Q3-&min)/yrange*100*yfactor;
                          output;
    function = 'BAR';     x=xo+(&bpos+(1*&bw))*xfactor;
                          y=yo-(Q1-&min)/yrange*100*yfactor;
                          line =0;output;

    keep XSYS YSYS HSYS WHEN POSITION LINE COLOR FUNCTION X Y ;
  run;



 /*-----------------------------------------------*
  | Annotate data set to draw outside points      |
  *-----------------------------------------------*/
  data outside;
    set plotdat end = end;
    where (outside>1);
    retain xsys ysys hsys style  color noor;
    length function $8;
    xsys='3'; ysys='3'; hsys='4';style=' ';color='red';noor=0;
    yrange=&max-&min;
    xo=(&ls+&lls);             /*origin x-coordinate        */
	 yo=(100-&ts - &tls);       /*origin y-coordinate (upper right corner)*/ 
    xfactor=((100-&ls - &lls - &rs)/100); /*x-factor for chart area    */
    yfactor=((100-&bs - &bls - &tls - &ts)/100); /*x-factor for chart area*/
	 
    /*values out of range*/
    if ((&var lt &min) or (&var gt &max)) then do;
      delete;
      noor=noor+1;
    end;

    /*draw outside values*/
      x=xo+(&bpos+(.5*&bw))*xfactor;
      y=yo-(&var-&min)/yrange*100*yfactor;
      style = '        ';
      function = 'SYMBOL  ';
      if outside = 2
            then text='CIRCLE';*'DOT       ';
            else text=':  ';
    keep XSYS YSYS HSYS COLOR FUNCTION TEXT X Y STYLE    ;
  run;

 /*-----------------------------------------------*
  | Annotate data set to draw and label histogram |
  *-----------------------------------------------*/

  data barcht;
    retain xsys ysys hsys when position line size color;
    length function $8 text $20;
    xsys='3'; ysys='3'; hsys='4'; when='B'; position='5'; line=0; size= &htext;color='black';
    set barstats end = end;
    xo=(&ls+&lls);             /*origin x-coordinate        */
	 yo=(100-&ts - &tls);       /*origin y-coordinate (upper right corner)*/ 
    xfactor=((100-&ls - &lls - &rs)/100); /*x-factor for chart area    */
    yfactor=((100-&bs - &bls - &tls - &ts)/100); /*x-factor for chart area*/
	 
    /*draw bars*/
    position='5';
    function='MOVE';
    x=xo;
    y=yo-barno*(100/&nbars*yfactor);  /*lower left corner of bar*/
    output;
    function = 'BAR';
    x=xo+pctofmax*100*yfactor;
    y=yo-(barno-1)*(100/&nbars*yfactor);  /*upper right corner of bar*/
    output;

    /*label bars*/
    function='LABEL';
    x=xo-1;
    y=yo-(barno-.5)*(100/&nbars*yfactor) +.5;  /*middle left corner of bar*/
    position='4';Text=trim(trim(barmin) ||' - <')|| left(trim(barmax));
    output;

    /*label y-ticks*/
    function='LABEL';
    x=xo + 100*xfactor +1;
    y=yo-(barno-1)*(100/&nbars*yfactor) +.5;  /*lower limit of bar on right axis*/
    position='>';Text=left(trim(barmin));
    output;

    /*draw major y-ticks*/
    function = 'MOVE';
      x=xo+100*xfactor-.5*&fs; y=yo-(barno-1)*(100/&nbars*yfactor);
          output;
    function = 'DRAW'; x=xo+100*xfactor;y=yo-(barno-1)*(100/&nbars*yfactor);line=1;
          output;

    /*draw  4 minor y-ticks*/
    do minor = 1 to 4;
      function = 'MOVE';
        x=xo+100*xfactor-.25*&fs; y=yo-(barno-1+minor*.2)*(100/&nbars*yfactor);
            output;
      function = 'DRAW'; x=xo+100*xfactor;y=yo-(barno-1+minor*.2)*(100/&nbars*yfactor);
            line=1; output;
    end;

    /*complete graph*/
    if end then do;

      /*label max-y- tick*/
      function='LABEL';
      x=xo + 100*xfactor +1;
      y=yo-(barno)*(100/&nbars*yfactor) +.5;  /*upper limit of bar on right axis*/
      position='>';Text=left(trim(barmax));
      output;

      /*draw max-y-tick*/
      function = 'MOVE';
        x=xo+100*xfactor-.5*&fs; y=yo-(barno)*(100/&nbars*yfactor);
          output;
      function = 'DRAW'; x=xo+100*xfactor;y=yo-(barno)*(100/&nbars*yfactor);line=1;
          output;

      /*draw axis frame*/
      position='5';
      function='MOVE';
      x=xo;
      y=yo-100*yfactor -&fs;  /*lower left corner of chart*/
      output;

      function = 'BAR'; line=0;
      x=xo+100*xfactor;
      y=yo + &fs;               /*upper right corner of chart*/
      output;


      /*draw percentage-axis tick marks*/
      do tick = 0 to (maxpct*100-10) by 5;

        function = 'MOVE';    x=xo+tick*xfactor/maxpct;  y=yo-100*yfactor -.5*&fs;
          output;
        function = 'DRAW';    x=xo+tick*xfactor/maxpct;  y=yo-100*yfactor -&fs;line=1;
          output;
        function = 'LABEL';    x=xo+tick*xfactor/maxpct;  y=yo-100*yfactor -&fs - 2;
          position='+';Text=trim(left(tick)) ||'%';
          output;
     end;

      /*draw freq-axis tick marks*/
      if maxfreq lt 10 then freqstep=1;
      else if maxfreq lt 20 then freqstep=2;
      else if maxfreq lt 50 then freqstep=5;
      else if maxfreq lt 100 then freqstep=10;
      else if maxfreq lt 200 then freqstep=20;
      else if maxfreq lt 500 then freqstep=50;
      else if maxfreq lt 1000 then freqstep=100;
      else  freqstep=200;
      freqtick=0;

      do until (freqtick gt maxfreq);
        function = 'MOVE';    x=xo+freqtick/n*100*xfactor/maxpct;  y=yo +.5*&fs;
          output;
        function = 'DRAW';    x=xo+freqtick/n*100*xfactor/maxpct;  y=yo +&fs;line=1;
          output;
        function = 'LABEL';    x=xo+freqtick/n*100*xfactor/maxpct;  y=yo+ &fs + 2;
          position='+';Text=trim(left(freqtick));
          output;

        /*draw minor freq-ticks*/
        if freqstep gt 2 then do;
          do minor = 1 to 4;
            function = 'MOVE';    x=xo+(freqtick+minor*freqstep/5)/n*100*xfactor/maxpct;
                                  y=yo +.75*&fs;
              output;
            function = 'DRAW';    x=xo+(freqtick+minor*freqstep/5)/n*100*xfactor/maxpct;
                                  y=yo +&fs;line=1;
              output;
          end;
        end;

        freqtick=freqtick+freqstep;
      end;

      /*label percent-axis*/
      function = 'LABEL';    x=xo+50*xfactor;  y=yo-100*yfactor -&fs - 5;
           position='+';Text="&pal";
           output;

      /*label freq-axis*/
      function = 'LABEL';    x=xo+50*xfactor;  y=yo+&fs + 5;
           position='+';Text="&fal";
           output;

      /*label value-axis*/
      function = 'LABEL';    x=xo-5;  y=yo+&fs + 5;
           position='4';Text="&val";
           output;

    end; /* of 'if end then do;'*/
    keep XSYS YSYS HSYS WHEN POSITION LINE SIZE COLOR FUNCTION TEXT X Y ;
  run;

 /*-----------------------------------------------------------*
  | concatenate annotate data sets and draw histogram/Boxplot |
  *-----------------------------------------------------------*/

  data histobox;
    set barcht boxanno outside;
  run;


  goptions  ftext=&ftext;

  proc gslide annotate=histobox;run;quit;
  %done:
%mend histobox;

  /*--------------------------------------*
   |     end of macro histobox            |
   *--------------------------------------*/


/*create data to test macro*/
%include goptions;

data testit;
 do patno=1 to 100;
   age=round(normal(0)*9+50,1);
   output;
 end;


/*Macro call*/
%histobox(
	var=age,       /*variable to be charted     */
	min=0,       /*minimum value to draw      */
	max=100,     /*maximum value to draw      */
	nbars=10,    /*number of bars to draw     */
	val=Age of Patients,
	data=testit
	);
