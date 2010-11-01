/* -----------------------------------------------------------------

This macro generates a sas/graph annotate data sets, that can be
used to annotate labels on the 'inner' level of a treemap gmap.
This is most useful on a single-level treemap.

This assumes your treemap is *truly* a single-level treemap,
and that each of these id's is unique (which would not necessarily
be true of a 2-level treemap - the 'inner' levels aren't 
guaranteed to be unique).

Author: Robert Allison (August 2004)

 ----------------------------------------------------------------- */

%macro treelabel2(treemap,idvar,color,label);

%local treemap idvar color label;


 /* Create an annotate dataset, with labels for each outer_id area */
 proc sql;
 create table &label as
 select unique
  &idvar as text,
  min(x) as minx, min(y) as miny,
  max(x) as maxx, max(y) as maxy,
  min(x) + ((max(x)-min(x))/2) as x,
  min(y) + ((max(y)-min(y))/2) as y
  from &treemap
  group by &idvar;
 quit; run;

 data &label; set &label;
 length function color $ 12;
 xsys='2';
 ysys='2';
 hsys='3';
 when='a';
 position='5';
 size=2.5; 
 color="&color";
 style='"arial"';
 function='label';
 run;
 
%mend treelabel2;

