 /********************************************************************

       name: grid
      title: Replay graphs in a regular grid
    product: graph
     system: all
      procs: greplay gslide
    support: saswss - Warren S. Sarle    update:  11jul98

 DISCLAIMER:

       THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE
 TO ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
 EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
 PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
 CONTAINED HEREIN.

 The %GRID macro lets you easily replay graphs in a regular grid with
 one or more rows and one or more columns. The %GRID macro also
 supports titles and footnotes for the entire replayed graph. For
 example, if you have run GPLOT four times and want to replay these
 graphs in a 2-by-2 grid with the title 'Four Marvelous Graphs', you
 could submit the following statements:

    title 'Four Marvelous Graphs';
    %grid( gplot*4, rows=2, cols=2);

 The %GRID macro allows 10% of the vertical size of the graph for
 titles by default. You can adjust this percentage via the TOP=
 argument in %GRID. Determining the best value for TOP= requires
 trial and error in most cases. To allow space for footnotes, use
 the BOTTOM= argument.

 The graphs to replay must be stored in a graphics catalog with
 library and member names specified by the macro variables &glibrary
 and &gout. By default, SAS/GRAPH stores graphs in WORK.GSEG, which
 is the catalog that the %GRID macro uses by default.  If your
 graphs are in another catalog, you must specify &glibrary and/or
 &gout using %LET statements as shown below.

 Each graph that is stored in a catalog has a name. Each procedure
 assigns default names such as GPLOT, GPLOT1, GPLOT2, etc. Most
 SAS/GRAPH procedures let you specify the name via a NAME= option
 which takes a quoted string that must be a valid SAS name. However,
 if a graph by that name already exists in the catalog, SAS/GRAPH
 appends a number to the name; it does not replace the previous graph
 by the same name unless you specify GOPTIONS GOUTMODE=REPLACE, but
 this option causes _all_ entries in the catalog to be deleted
 everytime you save a new graph, so it is not very useful. If you want
 to replace a single graph in a catalog, sometimes you can use the
 %GDELETE macro to delete the old one and later recreate a graph with
 the same name, but this does not work reliably due to a bug in
 SAS/GRAPH. By default, %GDELETE deletes _everything_ in the catalog;
 this does seem to work reliably.

 When you use BY processing, SAS/GRAPH appends numbers to the graph
 name to designate graphs for each BY group. For example, if you run
 GPLOT with three BY groups and NAME='HENRY', the graphs are named
 HENRY, HENRY1, and HENRY2. The %GRID macro lets you abbreviate this
 list of names as HENRY*3, where the repetition factor following the
 asterisk is the total number of graphs, not the number of the last
 graph.

 *********************************************************************/

%let glibrary=WORK;
%let gout=GSEG;

%let gdisplay=1; /* 1 to display graphs (i.e. goptions display)
                    0 not to display (i.e. goptions nodisplay) */

%macro grid(  /* replay graphs in a rectangular grid */
   list,      /* list of names of graphs, separated by blanks;
                 a name may be followed by an asterisk and a
                 repetition factor with no intervening blanks;
                 for example, ABC*3 is expanded to: ABC ABC1 ABC2 */
   rows=1,    /* number of rows in the grid */
   cols=1,    /* number of columns in the grid */
   title=1,   /* 1=do titles and footnotes; 0=don't and set top=0 */
   top=10,    /* percentage at top to reserve for titles */
   bottom=0); /* percentage at bottom to reserve for footnotes */

   %if &title %then %gtitle;
   %else %do; %let top=0; %let bottom=0; %end;
   %greplay;
   %tdef(rows=&rows,cols=&cols,top=&top,bottom=&bottom)
   %trep(&list,rows=&rows,cols=&cols,title=&title)
   run; quit;
%mend grid;


%macro gdelete(list); /* delete list of graphs from the catalog;
                         default is _ALL_ */

   %if %bquote(&list)= %then %let list=_ALL_;
   proc greplay igout=&glibrary..&gout nofs;
      delete &list;
   run; quit;
%mend gdelete;


%macro gtitle; /* create graph with titles and footnotes only */

   %global titlecnt;
   %if %bquote(&titlecnt)= %then %let titlecnt=1;
                           %else %let titlecnt=%eval(&titlecnt+1);
   %gdelete(title&titlecnt)
   goptions nodisplay;
   proc gslide gout=&glibrary..&gout name="title&titlecnt";
   run;
   %if &gdisplay %then %do;
      goptions display;
   %end;
%mend gtitle;


%macro greplay( /* invoke PROC GREPLAY */
   tc);         /* template catalog; default is JUNK */

   %if %bquote(&tc)= %then %let tc=junk;
   proc greplay nofs tc=&tc;
      igout &glibrary..&gout;
%mend greplay;


%macro tdef(  /* define a template for a rectangular grid */
   rows=1,    /* number of rows in the grid */
   cols=1,    /* number of columns in the grid */
   top=10,    /* percentage at top to reserve for titles */
   bottom=0); /* percentage at bottom to reserve for footnotes */
   %global tdefname; /* returned: name of template */

   %local height width n row col lower upper left right;
   %let height=%eval((100-&top-&bottom)/&rows);
   %let width =%eval(100/&cols);
   %let tdefname=t&rows._&cols;
   tdef &tdefname
      0/ulx=0 uly=100 llx=0 lly=0 urx=100 ury=100 lrx=100 lry=0
   %let n=1;
   %do row=1 %to &rows;
      %let lower=%eval(100-&top-&row*&height);
      %let upper=%eval(&lower+&height);
      %do col=1 %to &cols;
         %let right=%eval(&col*&width);
         %let left =%eval(&right-&width);
         &n/ulx=&left uly=&upper llx=&left lly=&lower
            urx=&right ury=&upper lrx=&right lry=&lower
         %let n=%eval(&n+1);
      %end;
   %end;
   ;
   template &tdefname;
%mend tdef;


%macro trep( /* replay graphs using template defined by %TDEF */
   list,     /* list of names of graphs, separated by blanks;
                a name may be followed by an asterisk and a
                repetition factor with no intervening blanks;
                for example, ABC*3 is expanded to: ABC ABC1 ABC2 */
   rows=,    /* (optional) number of rows in template */
   cols=,    /* (optional) number of columns in template */
             /* rows= and cols= default to values set with %TDEF */
   title=1);  /* 1=do titles and footnotes, 0=don't */

   %global titlecnt;
   %local i l n row col name root suffix nrep;
   %if %bquote(&rows)= %then %let rows=%scan(&tdefname,1,t_);
   %if %bquote(&cols)= %then %let cols=%scan(&tdefname,2,t_);

   treplay
   %if &title %then 0:title&titlecnt ;
   %let nrep=0;
   %let l=0;
   %let n=0;
   %do row=1 %to &rows;
      %do col=1 %to &cols;
         %let n=%eval(&n+1);
         %if &nrep %then %do;
            %let suffix=%eval(&suffix+1);
            %if &suffix>=&nrep %then %do;
               %let nrep=0;
               %goto tryagain;
            %end;
            %let name=&root&suffix;
            %goto doit;
         %end;
%tryagain:
         %let l=%eval(&l+1);
         %let name=%qscan(&list,&l,%str( ));
         %if &name= %then %goto break;
         %let i=%index(&name,*);
         %if &i %then %do;
            %let nrep=%substr(&name,&i+1);
            %if &nrep<=0 %then %goto tryagain;
            %let root=%substr(&name,1,&i-1);
            %let name=&root;
            %let suffix=0;
         %end;
%doit:
         &n:&name
      %end;
   %end;
%break:
   ;
%mend trep;




