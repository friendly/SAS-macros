%macro treemap(
	data=,            /* name of input data set                      */
	id=,              /* names of 1 or 2 ID variables                */
	var=,             /* response variable determining size of tiles */
	cvar=,            /* CHORO variable determining color of tiles   */
	outmap=treemap,   /* output anno data set to draw the treemap    */
	idlabel=,         /* ID variable for labels (usually ID1)        */
	outlab=treelab,   /* output anno data set containing labels      */
	idcolor=black,
	idsize=2,         /* height of text labels */
	idfont=,
	minx=0,
	miny=0,
	maxx=200,
	maxy=200,
	sortby=,
	plot=N,           /* do the GMAP plot internally?                */
	inanno=,          /* input annotate data set                     */
	name=treemap
);

	%local id1 id2;
	%let id1 = %scan(&id, 1, %str( ));
	%let id2 = %scan(&id, 2, %str( ));

	%if %length(&id2)=0 
		%then %mini_tree(&data,&id1,&var,&outmap,&minx,&miny,&maxx,&maxy,&sortby);
		%else %treemac(&data,&id1,&id2,&var,&outmap,&minx,&miny,&maxx,&maxy,&sortby);

	%if %length(&idlabel)>0
		%then %treelabel2(&outmap, &idlabel, &idcolor, &idsize, &idfont, &outlab);

	/*
	This assumes that all the setup has been done first
	  e.g., pattern statements, goptions, etc.
	*/
	%let plot = %upcase(%substr(&plot, 1,1);
	%if %length(&cvar) %then %do;
		%if %length(&id2) 
			%then %let cvar=&id2;
			%else %let cvar=&id1;
		%end;
	%if &plot=Y %then %do;
		proc gmap data=&data map=&outmap anno=&inanno; 
		  id &id1; 
		  choro &cvar /
			 anno=&outlab
			 coutline=grayaa woutline=2 nolegend
			 des="" name="&name"; run;
		%end;
			

%mend;


/*---------------------------------------------------------------------*/



/* Driver macro, for tree map with 2 id 'levels' - it just takes the user's 
   parameters, and then calls the real treemac macro, passing in the dataset
   twice, and the id variables twice, and stuffing in 'outer' as the level.  
   This makes the syntax 'cleaner' for the user calling the macro. */

%macro treemac(mydata,id1,id2,size,treedata,minx,miny,maxx,maxy,sortby);
%local mydata id1 id2 size treedata minx miny maxx maxy sortby;
/*
options mprint;
   proc datasets nolist nowarn; delete &treedata; run;
*/
  %treemac2(&mydata,&mydata,&id1,&id2,&id1,&id2,'outer',&size,&treedata,&minx,&miny,&maxx,&maxy,&sortby);
%mend treemac;



/*---------------------------------------------------------------------*/

/* Driver macro, for 1-level tree map (aka 'mini_tree' :)
   I create an extra/fake variable, so I can just use my regular 'treemac' macro.
*/

%macro mini_tree(mydata,id,size,treedata,minx,miny,maxx,maxy,sortby);
%local mydata id size treedata minx miny maxx maxy sortby;
  data minidata; set &mydata;
     FakeLev1='Lev1';
  run;
 
%treemac2(minidata,minidata,FakeLev1,&id,FakeLev1,&id,'outer',&size,&treedata,&minx,&miny,&maxx,&maxy,&sortby);
%mend mini_tree;




/* -----------------------------------------------------------------

This is a macro to produce a 2-level 'tree map'.

The format in which you call it is ...

 %treemac2(dataset,id1,id2,origid1,origid2,'outer',size,treedata,0,0,100,100);

 dataset = name of your response data set
 origdata = original dataset name, no matter what level of recursion I'm at
 id1 = variable containing unique id of 'outer' level 
          (such as continent, or division, or year)
 id2 = variable containing 'inner' ids (unique within an outer id)
          (such as country, or department, or month)
 origid1 = original id1, no matter what level of recursion I'm at
 origid2 = original id2, no matter what level of recursion I'm at
 'outer' = you'll always start at the 'outer' level - leave this as-is :)
 size = numeric variable used to control the size of the boxes
 treedata = name you want your tree map data set to go into
          (A tree map data set is a sas/graph map data set,
          which you will then draw using 'proc gmap')
 minx,miny,maxx,maxy = controls the size/proportions of the tree map
          (on recursive calls, tells what subset of the original area is 
          left to draw in)

Author: Robert Allison (August 2004)


These tree maps are similar to Joseph Hines 'Rectangular Java Tree' maps ...
http://bip.pc.sas.com/projects/graphs/Java%20Rectangular%20Tree%20Map/html/Main.htm

 ----------------------------------------------------------------- */


%macro treemac2(mydata,origdata,id1,id2,origid1,origid2,level,size,treedata,minx,miny,maxx,maxy,sortby);

/* Declare all the passed-in variables as 'local' - the only thing
   I'm passing back up to the original program is the 'treedata'
   data set itself. */
%local mydata origdata id1 id2 origid1 origid2 level size treedata minx miny maxx maxy sortby;
%*if %length(&sortby)=0 %then %let sortby = &size descending;

 /* Generate a random number, to use in the dataset names.
    This has 2 purposes - #1, to make the names unique so that they
    can co-exist as this process goes deeper and deeper in the 
    levels of recursion, and #2, so that they (hopefully) don't
    duplicate any data set names that the user might have. */
 %local rand;
 data _null_;
  numb = ceil (ranuni(0)*10000000);
  r = '_' || put(numb, Z7.) ;
  call symput ('rand', r);
 run;
 /* I'd prefer to use something like the SCL uniquenum() to get a 
    guaranteed unique number, but not sure of an easy way to do that
    outside of SCL. */


 /* This is really only important the 1st time through, but it's 
    easiest just to go ahead and do it every time through. */
options mprint;
 proc sql;
 create table mydata&rand as
 select unique &id1 length=50, sum(&size) as &size
 %if %length(&sortby) %then , &sortby ;
 from &mydata
 group by &id1;
 quit; run;

/* It's essential to declare these as 'local' since this
   macro recursively calls itself.  If you didn't declare
   them as local variables, then they would default to 
   global variables, and later executions of the macro
   would 'tromp on' values the earlier executions thought
   they had saved. */
%local obscnt halfsum halfcnt;
proc sql noprint; 
select count(*) into :obscnt from mydata&rand; 
select sum(&size)/2 into :halfsum from mydata&rand; 
select int((count(*)/2)+.5) into :halfcnt from mydata&rand; 
create table summed&rand as 
	select &id1 length=50, &size
 	%if %length(&sortby) %then , &sortby ;
	from mydata&rand 
	/*
	order by &size descending;
	*/
	order by 
	%if %length(&sortby) 
		%then &sortby ;
		%else &size descending;	
	;
quit; run;
options nomprint;

data summed&rand; set summed&rand;
foosum+&size;
run;

/* If you're down to just 1 value, then draw a rectangle */
%if (&obscnt eq 1) %then %do;

 proc sql noprint; select &id1 length=50 into :idval from mydata&rand; quit; 
 data temp1;
    length idvar $ 50;
    length &origid1 $ 50;
    length &origid2 $ 50;
    idvar="&idval";
/*
    &origid1=scan(idvar,1,'0d'x);
    &origid2=scan(idvar,2,'0d'x);
*/
    &origid1=scan(idvar,1,'`');
    &origid2=scan(idvar,2,'`');
    x=&minx; y=&miny; output;
    x=&maxx; y=&miny; output;
    x=&maxx; y=&maxy; output;
    x=&minx; y=&maxy; output;
 run;

/* If this is the 'outer' level, do not add anything to your 
   map dataset, but instead re-summarize your original data with a 2-level id,
   and then proceed to the 'inner' level. */
 %if (&level eq 'outer') %then %do;
  proc sql;
  create table treefoo&rand as
  /* To use the same macro to do both the outer & inner rectangles,
     I create this fake/combined 'id' for the inner rectangles,
     and use that as id1 and id2 when I pass it to the macro.
     (When I create the actual tree map data set, I split it
     back apart - of course, I could have problems if any of the
     real map id's have this character in them - I'm using the
     backtick ` character.
  */
  select unique trim(left(&id1))||'`'||trim(left(&id2)) length=50 as id, &size 
 	%if %length(&sortby) %then , &sortby ;
/*
roomforimprovement ... I would like to use a non-keyboard character here,
so it's less likely to be in the user's string, but '0d'x doesn't seem
to work ...
  select unique trim(left(&id1))||'0d'x||trim(left(&id2)) length=50 as id, &size 
*/
  from &origdata where &id1 eq "&idval";
  quit; run;
  %treemac2(treefoo&rand,&origdata,id,id,&origid1,&origid2,'inner',&size,&treedata,&minx,&miny,&maxx,&maxy,&sortby);
 %end; 

/* Else, if this is the inner level, then append to the tree map */

 %else %do;
  /* Append to the tree/block data set */
  %if %sysfunc(exist(&treedata)) %then %do;
   data &treedata; set &treedata temp1; run;
  %end;
  /* Or, if this is the first time through, start the tree/block data set */
  %else %do;
   data &treedata; set temp1; run;
  %end;
 %end;

%end;

/* Otherwise (if > 1 obsn), split the data in two, and run it through again ... */
%else %do; 

/* Split the data into about 1/2 ... */
 data high&rand low&rand; set summed&rand;
  if (_n_ eq 1) then output high&rand;
  else if (foosum le &halfsum) then output high&rand;
  else output low&rand;
 run;

 /* Calculate some stats to use during the next calculations */
 %local allsum lowsum highsum lowpct highpct;
 proc sql noprint; 
 select sum(&size) into :allsum from summed&rand;
 select sum(&size) into :lowsum from low&rand; 
 select sum(&size) into :highsum from high&rand; 
 select sum(&size)/&allsum into :lowpct from low&rand; 
 select sum(&size)/&allsum into :highpct from high&rand; 
 quit; run;

 /* Figure out where to split the grid, to do the recursive drawing */
 %local minx1 minx2 maxx1 maxx2 miny1 miny2 maxy1 maxy2;
 data _null_;

  /* Split it in the x-diretion */
  if (&maxx-&minx) ge (&maxy-&miny) then do;

   /* coordinates to plot the 'high' values in */
   minx1=&minx;
   miny1=&miny;
   maxx1=&minx + &highpct*(&maxx-&minx);
   maxy1=&maxy;

   minx2=&minx + &highpct*(&maxx-&minx);
   miny2=&miny;
   maxx2=&maxx;
   maxy2=&maxy;

  end;


  /* Split it in the y-diretion */
  else do;

   minx1=&minx;
   miny1=&miny;
   maxx1=&maxx;
   maxy1=&miny + &highpct*(&maxy-&miny);

   minx2=&minx;
   miny2=&miny + &highpct*(&maxy-&miny);
   maxx2=&maxx;
   maxy2=&maxy;

  end;

  /* symput these data set variables, so I can use them as macro variables */
  call symput ('minx1', minx1);
  call symput ('miny1', miny1);
  call symput ('maxx1', maxx1);
  call symput ('maxy1', maxy1);

  call symput ('minx2', minx2);
  call symput ('miny2', miny2);
  call symput ('maxx2', maxx2);
  call symput ('maxy2', maxy2);

 run;

 /* Now that you've decided how to split the remaining obsns, call the
    treemac2 macro twice (to do all the obsns in both sides of the split). 
 */
 %treemac2(high&rand,&origdata,&id1,&id2,&origid1,&origid2,&level,&size,&treedata,&minx1,&miny1,&maxx1,&maxy1,&sortby);
 %treemac2(low&rand, &origdata,&id1,&id2,&origid1,&origid2,&level,&size,&treedata,&minx2,&miny2,&maxx2,&maxy2,&sortby);


%end;

%mend treemac2;

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

%macro treelabel2(treemap,idvar,color,size,font,outanno);

%local treemap idvar color label size font;


 /* Create an annotate dataset, with labels for each outer_id area */
 proc sql;
 create table &outanno as
 select unique
  &idvar as text,
  min(x) as minx, min(y) as miny,
  max(x) as maxx, max(y) as maxy,
  min(x) + ((max(x)-min(x))/2) as x,
  min(y) + ((max(y)-min(y))/2) as y
  from &treemap
  group by &idvar;
 quit; run;

 data &outanno; 
   set &outanno;
   length function color $ 12;
   drop minx miny maxx maxy;
   xsys='2';
   ysys='2';
   hsys='3';
   when='a';
   position='5';
   size=&size; 
   color="&color";
   style="&font";;
   function='label';
 run;
 
%mend;


/* -----------------------------------------------------------------

This macro generates a sas/graph annotate data set, that can be
used to annotate outlines  on a treemap gmap.

Author: Robert Allison (August 2004)
Was:  macro treelabel in file treeanno_inc.sas
 ----------------------------------------------------------------- */

%macro treeoutline(treemap,idvar1,color, size, outlanno);

%*local treemap idvar1 color outlanno;


 /* Create an annotate dataset, to outline/frame the outer_id areas */
 proc sql;
 create table &outlanno as
 select unique 
   &idvar1 as text,
   min(x) as minx,
   min(y) as miny,
   max(x) as maxx,
   max(y) as maxy
   from &treemap
   group by &idvar1;
 quit; run;

 data &outlanno (drop = minx miny maxx maxy); 
   set &outlanno;
   length function style color $ 12;
   xsys='2';
   ysys='2';
   hsys='3';
   when='a';
   size=.1; /* thickness of annotated grid lines */
   size=&size; 
   color="&color";
   function='move';
	x=minx; y=miny; output;
   function='draw';
	x=maxx; y=miny; output;
	x=maxx; y=maxy; output;
	x=minx; y=maxy; output;
	x=minx; y=miny; output;
 run;

%mend;


