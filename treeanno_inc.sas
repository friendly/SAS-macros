/* -----------------------------------------------------------------

This macro generates two sas/graph annotate data sets, that can be
used to annotate outlines and labels on a treemap gmap.

Author: Robert Allison (August 2004)

 ----------------------------------------------------------------- */

%macro treelabel(treemap,idvar1,color,framanno,lablanno);

%local treemap idvar1 color framanno lablanno;


 /* Create an annotate dataset, to outline/frame the outer_id areas */
 proc sql;
 create table &framanno as
 select unique 
   &idvar1 as text,
   min(x) as minx,
   min(y) as miny,
   max(x) as maxx,
   max(y) as maxy
   from &treemap
   group by &idvar1;
 quit; run;
 data &framanno (drop = minx miny maxx maxy); set &framanno;
 length function style color $ 12;
 xsys='2';
 ysys='2';
 hsys='3';
 when='a';
 size=.1; /* thickness of annotated grid lines */
 color="&color";
 function='move';
  x=minx; y=miny; output;
 function='draw';
  x=maxx; y=miny; output;
  x=maxx; y=maxy; output;
  x=minx; y=maxy; output;
  x=minx; y=miny; output;
 run;

 /* Create an annotate dataset, with labels for each outer_id area */
 proc sql;
 create table &lablanno as
 select unique
  &idvar1 as text,
  min(x) + ((max(x)-min(x))/2) as x,
  min(y) + ((max(y)-min(y))/2) as y
  from &treemap
  group by &idvar1;
 quit; run;
 data &lablanno; set &lablanno;
 length function style color $ 12;
 xsys='2';
 ysys='2';
 hsys='3';
 when='a';
 position='5';
 size=2.5; 
 color="&color";
 style='"arial/bold"';
 function='label';
 run;

/*
%end;
*/

%mend treelabel;

