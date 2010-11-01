/*-------------------------------------------------------------------*
 * WINDOW SAS  -  Scale an annotate data set to a square plot        *
 *                environment                                        *
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <friendly@yorku.ca>         *
 * Created:  29 Mar 1990 16:26:36         Copyright (c) 1990         *
 * Revised:  29 Mar 1990 16:26:36                                    *
 * Version:  1.0                                                     *
 *                                                                   *
 *-------------------------------------------------------------------*/
 
%macro window(data=_LAST_,out=);
%*--------------------------------------------;
%* Scale X, Y to window % in annotate data set;
%*  and ensure a square plot environment      ;
%*--------------------------------------------;
%if &out = %str() %then %let out=&data;
   proc means noprint data=&data;
      var X Y;
      output out=range min=minx miny
                       max=maxx maxy;
   data &out;
      set &data end=eof;
      retain min max;
      drop min max minx miny maxx maxy;
      if _n_=1 then do;
         set range ;
         min=min(minx,miny);
         max=max(maxx,maxy);
         end;
      x = 100* (x-min)/(max-min);
      y = 100* (y-min)/(max-min);
      output;
      if eof then do;        * include corners;
         function='POINT';   * for square plot;
         x=0;   y=0;   output;
         x=100; y=0;   output;
         x=100; y=100; output;
         x=100; y=100; output;
         end;
%mend window;
