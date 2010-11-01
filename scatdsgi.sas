 /*-------------------------------------------------------------------*
  *    Name: scatdsgi.sas                                             *
  *   Title: Scatterplot matrix using DSGI                            *
  *     Doc: http://www.math.yorku.ca/SCS/sssg/scatmat.html           *
  *   Usage: %scatdsgi(data=, var=, group=);                          *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  4 Oct 1989 11:07:50                                     *
  * Revised:  14 Dec 1992 13:27:24                                    *
  * Version:  1.1                                                     *
  * Usage note:
  *        This macro assumes that output is to a device for which    *
  *   output is controlled by the GOPTIONS NODISPLA/DISPLAY.  For     *
  *   output to a graphics stream file, the statment GOPTION NODISPLAY*
  *   should be changed to GOPTIONS GSFMODE=NONE; and GOPTIONS DISPLAY*
  *   should be changed to GOPTIONS GSFMODE=APPEND;                   *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *-------------------------------------------------------------------*/
 
%macro scatdsgi(
        data =_LAST_,          /* data set to be plotted             */
        var  =,                /* variables to be plotted            */
        group=,                /* grouping variable (plot symbol)    */
        interp=none,           /* plot interpolation method          */
        hsym=,                 /* height of plot symbols             */
        symbols=%str(circle square + : $ = X _ Y),
        colors=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE,
        gout=GSEG);            /* graphic catalog for plot matrix    */
 
 options nonotes;
*proc greplay igout=gseg nofs;
*delete _all_;
*quit;
*-- Parse variables list;
 %let var = %upcase(&var);
 data _null_;
 set &data (obs=1);
    %if %index(&var,-) > 0 or "&var"="_NUMERIC_" %then %do;
 
       * find the number of variables in the list and
         convert shorthand variable list to long form;
     length _vname_ $ 8 _vlist_ $ 200;
     array _xx_ &var;
     _vname_ = ' ';
     do over _xx_;
        call vname(_xx_,_vname_);
        if _vname_ ne "&group" then do;
           nvar + 1;
           if nvar = 1 then startpt = 1;
                       else startpt = length(_vlist_) + 2;
           endpt = length(_vname_);
           substr(_vlist_,startpt,endpt) = _vname_;
        end;
     end;
     call symput( 'VAR', _vlist_ );
   %end;
   %else %do;
     * find the number of variables in the list;
     nvar = n(of &var) + nmiss(of &var);
   %end;
   call symput('NVAR',trim(left(put(nvar,2.))));
     * default symbol height, if hsym not specified;
   ht = scan('1.4 2.3 3 3.7 4.2 4.5 5 5.3 5.4 5.5',nvar,' ');
   call symput('HT',trim(left(put(ht,3.1))));
 RUN;
%if &nvar < 2 or &nvar > 10 %then %do;
   %put Cannot do a scatterplot matrix for &nvar variables ;
   %goto DONE;
   %end;
 
 /*----------------------------------------------------*
  | Determine grouping variable and plotting symbol(s) |
  *----------------------------------------------------*/
%if &group = %str() %then %do;
   %let NGROUPS=1;
   %let plotsym=1;      /* SYMBOL for data panels  */
   %let plotnam=2;      /* for variable name panel */
   %end;
%else %do;
   %let plotsym=&group;
   *-- How many levels of group variable? --;
   proc freq data = &data;
      tables &group / noprint out=_DATA_;
   data _null_;
      set end=eof;
      ngroups+1;
      if eof then do;
         call symput( 'NGROUPS', put(ngroups,3.) );
      end;
    run;
    %let plotnam=%eval(&ngroups+1);
%end;
 
%if &hsym = %str() %then %let h=&ht;
                   %else %let h=&hsym;
%gensym(n=&ngroups, h=&h, i=&interp, symbols=&symbols, colors=&colors);
SYMBOL&plotnam v=none;
%gdispla(OFF);
 
title h=0.01 ' ';
%let plotnum=0;    * number of plots made;
 
%do i = 1 %to &nvar;                   /* rows */
   %let vi = %scan(&var , &i );
   proc means noprint data=&data;
      var &vi;
      output out=minmax min=min max=max;
 
   %do j = 1 %to &nvar;                /* cols */
      %let vj = %scan(&var , &j );
      %let plotnum = %eval(&plotnum+1);
      %if &plotnum < 10 %then %let name=SCAT0&plotnum;
                        %else %let name=SCAT&plotnum;
      %*put plotting &vi vs. &vj (name: &name);
 
      %if &i = &j %then %do;           /* diagonal panel */
         data title;
            length text $8;
            set minmax;
            xsys = '1'; ysys = '1';
            x = 50; y = 50;
            text = "&vi";
            size = 2 * &nvar;
            function = 'LABEL';  output;
 
            x = 6; y = 6; position = '6';
            text = left(put(min, best6.));
            size = &nvar;
            output;
 
            x = 95; y = 95; position = '4';
            text = trim(put(max, best6.));
            size = &nvar;
            output;
 
         proc gplot data = &data;
            plot &vi * &vi = &plotnam
            / frame anno=title vaxis=axis1 haxis=axis1 name="&name";
         axis1 label=none value=none major=none
                            /* was:  major=(h=-%eval(&nvar-1)) */
               minor=none offset=(2);
         run;
      %end;
 
      %else %do;                       /* off-diagonal panel */
         proc gplot data = &data;
            plot &vi * &vj = &plotsym
            / frame nolegend vaxis=axis1 haxis=axis1 name="&name";
         axis1 label=none value=none major=none minor=none offset=(2);
         run;
      %end;
 
   %end; /* cols */
%end;    /* rows */
 
*goptions DISPLAY;         * device dependent;
%gdispla(ON);
 
%macro TDEF(nv);
%* ---------------------------------------------------------------;
%* Format a page of graphs using DSGI.                            ;
%* Start with (1,1) panel in upper left, and copy it across & down;
%* ---------------------------------------------------------------;
 
   data _null_;
      /* make the array of graphs big enough to hold all the graphs */
      array glist{%eval(&nv*&nv)} $;
      /* set the catalog to the one you are working with */
      rc = gset('catalog', 'work', "&gout");
      rc = ginit();
      /* return the list of graphs in the catalog */
      n = %eval(&nv*&nv);
      call gask('graphlist', n, of glist{*}, rc);
      /* open the matrix graph */
      rc = graph('clear', 'matrix', "scatterplot matrix &nv x &nv");

      /* DSGI viewports are in 0-1 coord sys */
      /* now that you are not in MACRO, division works much better */
      /* split up the page according to the number of graphs */
      lx = 1/&nv;
      ly = lx;
      sx = 0;
      sy = &nv * ly;

      k = 1;
      do i = 1 to &nv;
         /* reset the lower left viewport x coord to 0 and decrement */
         /* the starting point of y by the size of the graph in the  */
         /* y direction                                              */
         sx = 0;
         sy = sy-ly;
         /* limitation: there are only 20 transformations you can set */
         /* but since you do error checking for over 10 graphs I think*/
         /* this is OK.                                               */
         do j = 1 to &nv;
            /* define the size of the viewport */
            rc = gset('viewport', j, sx, sy, sx+lx, sy+ly);

            /* 0, 0, 100, 100 is the SAS/GRAPH procedure default coord */
            /* system for the window                                   */
            rc = gset('window', j, 0, 0, 100, 100);
            /* set the transformation */
            rc = gset('transno', j);
            /* play the graph into the active viewport/window */
            rc = graph('insert', glist{k});
            *put 'Doing graph' k 'glist:' glist{k} ;
            /* increment to get the next graph into the array */
            k+1;
            /* update the viewport starting x coord to set up the */
            /* next viewport */
            sx = sx+lx;
            end;
         end;
      rc = graph('update');
      rc = gterm();
      run;
%mend TDEF;
 
%* now you do not need all the checking on NVAR before calling TDEF ;
%TDEF(&nvar);
 
%DONE:
options notes;
%mend;
 
 
 /*----------------------------------------------------*
  |  Macro to generate SYMBOL statement for each GROUP |
  *----------------------------------------------------*/
%macro gensym(n=1, h=1.5, i=none,
              symbols=%str(- + : $ = X _ Y),
              colors=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE);
    %*--  if more than 8 groups, symbols and colors are recycled;
  %local chr col k;
  %do k=1 %to &n ;
     %if %length(%scan(&symbols, &k, %str( ))) = 0 
	      %then %let symbols = &symbols &symbols;
     %if %length(%scan(&colors, &k, %str( ))) = 0 
	      %then %let colors = &colors &colors;
     %let chr =%scan(&symbols, &k,%str( ));
     %let col =%scan(&colors, &k, %str( ));
     symbol&k h=&h v=&chr c=&col i=&i;
  %end;
%mend gensym;
 
 
 
 
