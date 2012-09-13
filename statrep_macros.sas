
                         /*---------------------------------------------*/
                         /* The WRITE macro displays one or more pieces */
                         /* of output (tables, notes, graphs, and so    */
                         /* on) that were captured into an ODS document */
                         /* by the OUTPUT macro.                        */
                         /*---------------------------------------------*/
%macro write(fname,      /* is a required keyword that specifies the    */
                         /* first level of the output file name.  This  */
                         /* name will appear at the end of the \Listing */
                         /* or \Graphic tag in the LaTeX file.  The     */
                         /* following examples show where the fname     */
                         /* keyword, in this case 'crsi1a', is          */
                         /* specified in the WRITE macro and in the     */
                         /* \Listing tag:                               */
                         /*                                             */
                         /* %*;%write(crsi1a, objects=inertias)         */
                         /* \Listing[caption={... caption ...}]{crsi1a} */
                         /*                                             */
             width=,     /* specifies the width of graphs.              */
                         /* Example: WIDTH=5.4in.                       */
             height=,    /* specifies the height of graphs.             */
                         /* Example: HEIGHT=4in.                        */
                         /*                                             */
             dpi=,       /* specifies DPI (dots per inch) for ODS       */
                         /* Graphics.  You typically would not use this */
                         /* option except when you would otherwise run  */
                         /* out of memory.  The default is 300 DPI, and */
                         /* a reasonable alternative for ODS graphs     */
                         /* with large file sizes is 100 DPI.  More     */
                         /* details follow.                             */
                         /*                                             */
                         /* This option controls the DPI for ODS        */
                         /* Graphics.  Use the DPI= option in the       */
                         /* OUTPUT macro to control the DPI for GRSEG   */
                         /* graphics.                                   */
                         /*                                             */
                         /* If DPI= is specified, that DPI is used.     */
                         /* If DPI= is not specified, and if a macro    */
                         /* variable DefaultDPI is defined, its value   */
                         /* is used as the DPI.  If DPI= is not         */
                         /* specified and the macro variable DefaultDPI */
                         /* is not defined or if its value is null,     */
                         /* then the default DPI, 300, is used.         */
                         /*                                             */
                         /* In other words, the default DPI is 300.     */
                         /* However, you can set the default DPI to     */
                         /* (say) 100 by putting the following at the   */
                         /* top of your first Sascode block, beginning  */
                         /* in column 1:                                */
                         /*                                             */
                         /* %*;%let defaultdpi=100;                     */
                         /*                                             */
                         /* Since the %LET statement is preceded by a   */
                         /* null macro comment (%*;) beginning in       */
                         /* column 1, it will not show up in the        */
                         /* SAS code listing, but it will affect the    */
                         /* capture program and change the default      */
                         /* DPI for the duration of the running of the  */
                         /* program.  You can change the default DPI at */
                         /* any time with a %LET statement.             */
                         /*                                             */
                         /* With GRSEG graphics, 100 DPI is not         */
                         /* available, so this specification creates    */
                         /* graphs at 96 DPI.                           */
                         /*                                             */
            store=,      /* specifies the name of the ODS document      */
                         /* that is created by the OUTPUT macro.  This  */
                         /* is the ODS document that the WRITE macro    */
                         /* will process.  By default, when null, the   */
                         /* current document from the last OUTPUT macro */
                         /* is used.  If you are processing the last    */
                         /* document created, you can omit specifying   */
                         /* the STORE= option.  To use some other       */
                         /* document, specify STORE=documentlabel.      */
                         /*                                             */
             pattern=,   /* provides an optional additional selection   */
                         /* criterion.  Specify part of a path, for     */
                         /* example, a group name.  Then only objects   */
                         /* whose name includes this group name are     */
                         /* used.  Comparisons are not case sensitive.  */
                         /* The macro looks for an appearance of the    */
                         /* PATTERN=value anywhere in the path.  For    */
                         /* example, PATTERN=fit, will match            */
                         /* "Reg.m2.Fit.Population.ParameterEstimates"  */
                         /* and anything else with "fit" in it          */
                         /* including paths that contain "fitplot",     */
                         /* "fitness", "outfit" and so on.              */
                         /*---------------------------------------------*/
             firstobj=,  /* specifies the first data object.            */
             lastobj=,   /* specifies the last data object.             */
             objects=,   /* specifies the list of data objects.         */
                         /*                                             */
                         /* To specify which objects go into a section  */
                         /* of output, specify one of the following     */
                         /* combinations:                               */
                         /*     * FIRSTOBJ=,                            */
                         /*     * LASTOBJ=,                             */
                         /*     * FIRSTOBJ= and LASTOBJ=,               */
                         /*     * OBJECTS=,                             */
                         /*     * none of the above.                    */
                         /*                                             */
                         /* If FIRSTOBJ= is specified and LASTOBJ= is   */
                         /* not specified, then the last object is the  */
                         /* last object produced by the procedure.  If  */
                         /* LASTOBJ= is specified and FIRSTOBJ= is not  */
                         /* specified, then the default first object is */
                         /* the first object produced by the procedure. */
                         /* If OBJECTS= is specified, then only objects */
                         /* in that list are used.  If none of these    */
                         /* options is specified then, all objects are  */
                         /* used.                                       */
                         /*                                             */
                         /* The term "object" used in this              */
                         /* discussion refers to notes, tables, and     */
                         /* graphs.  Specify the first or last object   */
                         /* or list of objects (tables, notes, graphs). */
                         /* All object name comparisons are case        */
                         /* insensitive.                                */
                         /*                                             */
                         /* All selections are based on paths instead   */
                         /* of label paths.  You can specify the last m */
                         /* levels of the path.  Assuming that you have */
                         /* one model and one ANOVA table, any of the   */
                         /* following specifications for the ANOVA      */
                         /* object are equivalent, along with many      */
                         /* other possibilities:                        */
                         /*                                             */
                         /*   Reg.MODEL1.Fit.Population.ANOVA           */
                         /*   reg.model1.fit.population.anova           */
                         /*   reg#1.model1#1.fit#1.population#1.anova#1 */
                         /*   reg#1.model1.fit.population#1.anova#1     */
                         /*   MODEL1.Fit.Population.ANOVA               */
                         /*   Fit.Population.ANOVA                      */
                         /*   Population.ANOVA                          */
                         /*   ANOVA                                     */
                         /*   ANOVA#1                                   */
                         /*                                             */
                         /* Typically, you need to specify only the     */
                         /* last level.  If an object appears more than */
                         /* once, then you might need to specify more   */
                         /* levels.  The #n notation is allowed, even   */
                         /* in cases where it does not work in an ODS   */
                         /* SELECT statement.  You must specify a name, */
                         /* using the #n notation that matches the name */
                         /* (or the last few levels of the name) as it  */
                         /* appears in the ODS document.  For example,  */
                         /* if there are multiple residual panels, you  */
                         /* can specify OBJECTS=residualplot and        */
                         /* OBJECTS=residualplot#2.  The object list    */
                         /* produced by the macro will show every place */
                         /* that a #2 or #n (n > 1) is required.        */
                         /*                                             */
                         /* If you run a procedure more than once for   */
                         /* this ODS document, then you need #2 on the  */
                         /* procedure name for the second group.  For   */
                         /* example, here are the fully-qualified names */
                         /* from a set of two PROC REG runs:            */
                         /*                                             */
                         /*   Reg.MODEL1.Fit.Fail.NObs                  */
                         /*   Reg.MODEL1.Fit.Fail.ANOVA                 */
                         /*   Reg.MODEL1.Fit.Fail.FitStatistics         */
                         /*   Reg.MODEL1.Fit.Fail.ParameterEstimates    */
                         /*   Reg#2.MODEL1.Fit.Fail.NObs                */
                         /*   Reg#2.MODEL1.Fit.Fail.ANOVA               */
                         /*   Reg#2.MODEL1.Fit.Fail.FitStatistics       */
                         /*   Reg#2.MODEL1.Fit.Fail.ParameterEstimates  */
                         /*                                             */
                         /* The order in which objects come out is      */
                         /* determined by the order in which they are   */
                         /* generated, not the order in which they are  */
                         /* specified in the OBJECTS= option.           */
                         /*                                             */
                         /* If you specify OBJECTS=, then you can also  */
                         /* specify where page breaks can occur.  By    */
                         /* default, breaks might occur between the     */
                         /* bottom of a table and the top of the note   */
                         /* or table that comes after it.  If you want  */
                         /* anything different to occur, use left and   */
                         /* right angled brackets, <>, to start and end */
                         /* a new group and suppress normal page        */
                         /* breaking in between.  After a <, no default */
                         /* page breaking occurs until a > is hit.  A   */
                         /* page break occurs at the > and then in the  */
                         /* default places after that.  You can use     */
                         /* "< list >" in pairs (for example, "<ANOVA   */
                         /* FitStatistics>" to keep certain objects     */
                         /* together.  You can instead use > to cause   */
                         /* page breaks in addition to the default page */
                         /* breaks.  You can also use < to designate    */
                         /* page breaks and suppress all default page   */
                         /* breaking.  A break always occurs before and */
                         /* after a graph regardless of what <>         */
                         /* specification you use.                      */
                         /*---------------------------------------------*/
             pagesize=,  /* specifies the page size.  The default is    */
                         /* the page size currently in effect.  This    */
                         /* specification lasts for the duration of     */
                         /* this step.  The current page size is        */
                         /* restored at the end.                        */
             linesize=,  /* specifies the line size.  The default is    */
                         /* the line size currently in effect.  This    */
                         /* specification lasts for the duration of     */
                         /* this step.  The current line size is        */
                         /* restored at the end.                        */
                         /*                                             */
             options=,   /* specifies one or more binary options:       */
                         /*                                             */
                         /* LIST     - do not run PROC DOCUMENT for     */
                         /*            each group of objects.  Just     */
                         /*            list the contents of the         */
                         /*            document.                        */
                         /*                                             */
                         /* TABLE    - (or TABLES) selects only tables  */
                         /*            and notes (more specifically,    */
                         /*            anything but graphs).            */
                         /*                                             */
                         /* GRAPH    - only selects graphs.             */
                         /*                                             */
                         /* NOPAGE   - no page breaks at the top of     */
                         /*            tables.                          */
                         /*                                             */
                         /* NEWPAGE  - forces a page break on the first */
                         /*            object.                          */
                         /*                                             */
                         /* AUTOPAGE - same page breaks that the        */
                         /*            procedure makes.  There is a     */
                         /*            page break on the first object   */
                         /*            and where the proc forces a new  */
                         /*            page.  AUTOPAGE is the default.  */
                         /*                                             */
                         /* ONEBOX   - is used with the \Listing tag    */
                         /*            and specifies that only one      */
                         /*            block of tables is made.  This   */
                         /*            option strips out all '<>'       */
                         /*            characters from the OBJECTS=     */
                         /*            list and adds a '<' to the       */
                         /*            beginning of the list.  With     */
                         /*            FIRSTOBJ= or LASTOBJ= or no      */
                         /*            OBJECTS= list, the effect is the */
                         /*            same even though the mechanism   */
                         /*            is different.  If anything other */
                         /*            than only tables or a single     */
                         /*            graph is input, an error is      */
                         /*            issued along with the object     */
                         /*            list.                            */
                         /*                                             */
                         /* SKIPFIRST- deletes the first object that    */
                         /*            would otherwise be selected.     */
                         /*            Typical usage is with FIRST=     */
                         /*            to select every object after the */
                         /*            one that was named.              */
                         /*                                             */
                         /* SKIPLAST - deletes the last object that     */
                         /*            would otherwise be selected.     */
                         /*            Typical usage is with LAST= to   */
                         /*            select every object before the   */
                         /*            one that was named.              */
                         /*                                             */
                         /* Example: OPTIONS=TABLE ONEBOX.              */
                         /*                                             */
             style=,     /* specifies the ODS style for ODS Graphics.   */
                         /* Use the STYLE= option in the OUTPUT macro   */
                         /* to control the style for GRSEG graphics.    */
                         /* If STYLE= is specified, that style is used. */
                         /* If STYLE= is not specified, and if a macro  */
                         /* variable DefaultStyle is defined, its value */
                         /* is used as the style.  If STYLE= is not     */
                         /* specified and the macro variable            */
                         /* DefaultStyle is not defined or if its value */
                         /* is null, then the default style (which is   */
                         /* the HTMLBLUE style) is used.                */
                         /*                                             */
                         /* In other words, the default style is the    */
                         /* HTMLBLUE style.  However, you can set the   */
                         /* default style to (say) LISTING by putting   */
                         /* the following line at the top of your first */
                         /* Sascode block, beginning in column 1:       */
                         /*                                             */
                         /* %*;%let defaultstyle=listing;               */
                         /*                                             */
                         /* Since this line begins with a null macro    */
                         /* comment (%*;) that begins in column 1, it   */
                         /* will not show up in the SAS code listing,   */
                         /* but it will affect the capture program and  */
                         /* change the default style for the duration   */
                         /* of the running of the program.              */
                         /*                                             */
             type=);;    /* specifies the type of objects to select.    */
                         /* \Listing specifies TYPE=LISTING, which is   */
                         /* an alias for OPTIONS=TABLE.                 */
                         /* \Graphic specifies TYPE=GRAPH, which is an  */
                         /* alias for OPTIONS=GRAPH.                    */
                         /*---------------------------------------------*/

%local i j k flow error savenote holdlast allobjflag firstflag onlylist
       lastflag rangeflag objflag ngroups h onebox isgraph savesize
       left tyw stw grw abnormal selecttype savetime kl kh jl jh ls
       pagebreak gdevice skipfirst skiplast;

%let abnormal = 0;
%let error    = 0;
%let savenote = %sysfunc(getoption(notes));
%let savetime = %sysfunc(getoption(stimer));
%let savesize = ls=%sysfunc(getoption(ls)) ps=%sysfunc(getoption(ps));
%let holdlast = &syslast;
options nostimer %if &pagesize ne %then ps=&pagesize;
                 %if &linesize ne %then ls=&linesize;;

%if %symexist(adsflow) %then %let flow = %lowcase(&adsflow);

* Set the default style using STYLE= and a DefaultStyle macro variable.;
%if &style eq %then %let style = &defaultstyle;

* Set the default DPI using DPI= and a DefaultDPI macro variable.;
%if &dpi eq %then %let dpi = &defaultDPI;

%let gdevice = %substr(png300, 1, 3 * (1 + (&dpi eq 300)));

* Find out what is in the document.  Store the list in a SAS data set.;
%if %symexist(documentlabel) %then %do;
   %if %nrbquote(&store) eq %then %let store = &documentLabel;
   %end;

%if %nrbquote(&store) eq %then %do;
   %put ERROR: No ODS document available.;
   %let error = 1;
   %goto veryend;
   %end;
%else %put NOTE: Processing document &store..;

%if not %index(&flow, notes) %then %do; options nonotes; %end;

ods document close; run;
ods _all_ close;

proc document name=&store;
   ods output properties=_ads_doclist(where=(type ne "Dir"));
   list / details levels=all;
   run; quit;

%if &syserr > 4 %then %let error = 1; %if &error %then %goto veryend;
ods listing;

data _null_;
   call symputx('ls', getoption('ls'), 'L');
   if 0 then set _ads_doclist nobs=nprts;
   if nprts eq 0 then do;
      put "ERROR: Document &store does not exist.";
      call symputx('error', '1', 'L');
      end;

   length fname $ 40 c $ 1;
   fname = symget('fname');
   c = substr(fname, length(fname), 1);
   if '0' le c le '9' then do;
      put 'ERROR: The name ' fname 'is invalid.  It must not end in a numeral.';
      call symputx('error', '1', 'L');
      end;
   if _error_ then call symputx('error', '1', 'L');
   stop;
   run;

%if &syserr > 4 %then %let error = 1; %if &error %then %goto veryend;

* Basic option setting, checking, and processing.;
data _null_;
   length c $ 1 c2 $ 2 firstobj lastobj options type $ 200 objects $ 32500;

   w = symget('width');
   if w = ' ' then w = '6.4in';
   h = symget('height');
   if h = ' ' then
      call symputx('h', trim(put(0.75 * input(translate(lowcase(w), '    ',
                   'incm'), 12.), 12.)) || substr(w, length(w) - 1, 2), 'L');
   else call symputx('h', trim(h), 'L');

   * Flag which options were specified.;

   type    = lowcase(symget('type'));
   options = lowcase(symget('options'));

   tx = index(type   , 'tables');    if tx then substr(type   , tx, 6) = ' ';
   tt = index(type   , 'table');     if tt then substr(type   , tt, 5) = ' ';
   tl = index(type   , 'listing');   if tl then substr(type   , tl, 7) = ' ';
   t1 = index(type   , 'graphics');  if t1 then substr(type   , t1, 8) = ' ';
   t2 = index(type   , 'graphic');   if t2 then substr(type   , t2, 7) = ' ';
   t3 = index(type   , 'graphs');    if t3 then substr(type   , t3, 6) = ' ';
   t4 = index(type   , 'graph');     if t4 then substr(type   , t4, 5) = ' ';
   no = index(options, 'nopage');    if no then substr(options, no, 6) = ' ';
   nw = index(options, 'newpage');   if nw then substr(options, nw, 7) = ' ';
   ap = index(options, 'autopage');  if ap then substr(options, ap, 8) = ' ';
   li = index(options, 'listing');   if li then substr(options, li, 7) = ' ';
   ls = index(options, 'list');      if ls then substr(options, ls, 4) = ' ';
   ty = index(options, 'tables');    if ty then substr(options, ty, 6) = ' ';
   ta = index(options, 'table');     if ta then substr(options, ta, 5) = ' ';
   ob = index(options, 'onebox');    if ob then substr(options, ob, 6) = ' ';
   g1 = index(options, 'graphics');  if g1 then substr(options, g1, 8) = ' ';
   g2 = index(options, 'graphic');   if g2 then substr(options, g2, 7) = ' ';
   g3 = index(options, 'graphs');    if g3 then substr(options, g3, 6) = ' ';
   g4 = index(options, 'graph');     if g4 then substr(options, g4, 5) = ' ';
   sf = index(options, 'skipfirst'); if sf then substr(options, sf, 9) = ' ';
   sl = index(options, 'skiplast');  if sl then substr(options, sl, 8) = ' ';

   if options ne ' ' then do;
      options = compbl(options);
      put 'WARNING: WRITE macro OPTIONS=' options 'is invalid and is ignored.';
      end;

   if type ne ' ' then do;
      type = compbl(type);
      put 'WARNING: WRITE macro TYPE=' type 'is invalid and is ignored.';
      end;

   table = ta or ty or li or tt or tx or tl;
   graph = g1 or g2 or g3 or g4 or t1 or t2 or t3 or t4;

   if      no then call symputx('pagebreak', 'nopage');
   else if nw then call symputx('pagebreak', 'newpage');
   else            call symputx('pagebreak', 'autopage');

   call symputx('onlylist',   ls ne 0, 'L');
   call symputx('onebox',     ob, 'L');
   call symputx('isgraph',    graph, 'L');
   call symputx('selecttype', ob or table or graph, 'L');
   call symputx('skipfirst',  sf ne 0, 'L');
   call symputx('skiplast',   sl ne 0, 'L');

   if table and graph then do;
      put "ERROR: TYPE=&type and OPTIONS=&options conflict.";
      put "ERROR: Both tables and graphs were selected.";
      call symputx('error', '1', 'L');
      end;

   * Process object names.;
   firstobj = symget('firstobj');
   lastobj  = symget('lastobj');
   objects = lowcase(symget('objects'));
   if ob then objects = '<' || compbl(translate(objects, '  ', '<>'));

   call symputx('ngroups',  '0', 'L');
   call symputx('pattern',  lowcase(symget('pattern')), 'L');
   call symputx('firstobj', lowcase(firstobj), 'L');
   call symputx('lastobj',  lowcase(lastobj),  'L');
   call symputx('objects',  objects,           'L');

   if (firstobj ne ' ' or lastobj ne ' ') and objects ne ' ' then do;
      put 'ERROR: OBJECTS= may not be specified with FIRSTOBJ= and LASTOBJ=.';
      call symputx('error', '1', 'L');
      end;

   * Remove extra spaces and funkiness around < and >.;
   if objects ne ' ' then do;
      objects = ' ' || compbl(objects);

      * Get rid of trailing open brackets.;
      done = 0;
      do i = 1 to 32500 until(done);
         l = length(objects);
         if substr(objects, l, 1) eq '<' then substr(objects, l, 1) = ' ';
         else done = 1;
         end;

      * Get rid of multiple contiguous brackets.;
      c = ' ';
      done = 0;
      do i = 1 to 32500 until(done);
         if i gt length(objects) then done = 1;
         else do;
            c2 = substr(objects, i, 1);
            if (c eq '<' and c2 eq '<') or
               (c eq '>' and c2 eq '>') then substr(objects, i, 1) = ' ';
            c2 = substr(objects, i, 1);
            if not (c2 eq ' ' or c2 eq '<' or c2 eq '>') then c = ' ';
            else if c2 eq '<' or c2 eq '>' then c = c2;
            end;
         end;
      objects = ' ' || compbl(objects);

      * Move brackets next to names.  
        For example, change '< name' to '<name' and 'name >' to 'name>'.;
      done = 0;
      do i = 1 to 32500 until(done);
         if i gt length(objects) then done = 1;
         else do;
            c2 = substr(objects, i, 2);
            if c2 eq '< ' then substr(objects, i, 2) = ' <';
            if c2 eq ' >' then substr(objects, i, 2) = '> ';
            c2 = substr(objects, i, 1);
            if c2 eq '<' then do;
               c = substr(objects, i - 1, 1);
               if c ne ' ' then objects = substr(objects, 1, i - 1) ||
                                          ' <' || substr(objects, i + 1);
               i + 1;
               end;
            else if c2 eq '>' then do;
               c = substr(objects, i + 1, 1);
               if c ne ' ' then objects = substr(objects, 1, i - 1) ||
                                          '> ' || substr(objects, i + 1);
               i + 1;
               end;
            end;
         end;

      * Get rid of null bracket groups.;
      done = 0;
      do i = 1 to 32500 until(done);
         if i gt length(objects) then done = 1;
         else do;
            c2 = substr(objects, i, 2);
            if c2 eq '<>' then substr(objects, i, 2) = ' ';
            end;
         end;
      objects = compbl(left(objects));

      * Get rid of leading close brackets.;
      if objects =: '>' then objects = substr(objects, 2);

      call symputx('objects', left(compbl(objects)), 'L');
      end;

   call symputx('allobjflag', (firstobj   eq ' ' and lastobj eq ' ' and
                               objects eq ' '), 'L');
   call symputx('firstflag', (firstobj eq ' ' and lastobj ne ' '), 'L');
   call symputx('lastflag' , (firstobj ne ' ' and lastobj eq ' '), 'L');
   call symputx('rangeflag', (firstobj ne ' ' and lastobj ne ' '), 'L');
   call symputx('objflag'  , (objects ne ' '), 'L');

   if _error_ then call symputx('error', '1', 'L');
   run;

%if &syserr > 4 %then %let error = 1; %if &error %then %goto veryend;

data _ads_doclist(drop=i p);
   set _ads_doclist;
   ObjNum = _n_;
   * The variable ourpath is used for group subsetting and display in the SAS log.
     It is not used for actual selection (beyond group selection).;
   ourpath = substr(translate(path, '.', '\'), 2);
   do i = 1 to length(ourpath) until(p eq 0);
      p = index(ourpath, '#1.');
      if p then substr(ourpath, p, 3) = '.  ';
      end;
   ourpath = compress(ourpath, ' ');
   p = length(ourpath) - 1;
   if substr(ourpath, p, 2) eq '#1' then substr(ourpath, p, 2) = ' ';

   if _error_ then call symputx('error', '1', 'L');
   run;

%if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;

* Get object names specified in FIRSTOBJ=, LAST=, and OBJECTS= for error checking.;
%if not &allobjflag %then %do;
   data _ads_objects(keep=object);
      length l $ 32500 object $ 200;
      l = symget('firstobj') || ' ' || symget('lastobj')  || ' ' || symget('objects');
      do i = 1 to 32500 until(object eq ' ');
         object = scan(l, i, ' <>');
         if object ne ' ' then output;
         end;

      if _error_ then call symputx('error', '1', 'L');
      run;

   %if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;

   * Make sure every specified name is referenced.;
   data _null_;
      set _ads_objects;
      length object holdobj $ 200;
      holdobj = object;

      * Store object name with pounds and such to match document.;
      %ads_expand_obj(object, holdobj)

      match = 0;
      do i = 1 to nprts until(match);
         set _ads_doclist nobs=nprts point=i;

         * If object has n levels, store in pathpart the last n levels from path.;
         %ads_extr_path(pathpart, path, holdobj)
         match = (pathpart eq object);
         end;
      if not match then do;
         put 'ERROR: Object ' holdobj 'not found.';
         call symputx('error', '1', 'L');
         end;

      if _error_ then call symputx('error', '1', 'L');
      run;

   %if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;
   %end;

* Extract the relevant tables and notes from the list.;
data _ads_select(keep=path type left right object ourpath label pagebreak);
   length objlist $ 32500 fullqualobj object firstlist lastlist $ 200;
   retain object firstlist lastlist objlist ' ' inflag 0 objind 1;
   set _ads_doclist;

   if type eq 'Report' and label =: 'Data Set ' and path =: '\Print'
      then label = substr(label, 10);
   else do;
      label = ' ';
      if type eq 'Report' then type = 'rpt';
      end;

   left = 0; right = 0; fullqualobj = ' ';

   if _n_ eq 1 then do;
      firstlist = symget('firstobj');
      lastlist  = symget('lastobj');
      objlist   = symget('objects');
      end;

   * Perform preliminary subsetting based on group patterns.;
   %if %nrbquote(&pattern) ne %then %do;
      if index(lowcase(ourpath), "&pattern");
      %end;

   righttype = 1;
   if &selecttype then righttype = ((    &isgraph and type eq 'Graph') or
                                    (not &isgraph and type ne 'Graph'));

   %* Are only the last ones selected (FIRSTOBJ= specified)?;
   %if &lastflag and not &allobjflag %then %do;
      %ads_extr_path(pathpart, path, firstlist)
      %ads_expand_obj(fullqualobj, firstlist)
      if pathpart eq fullqualobj then inflag = 1;
      if inflag and righttype then output;
      %end;

   %* Are only the first ones selected (LASTOBJ= specified)?;
   %else %if &firstflag and not &allobjflag %then %do;
      if _n_ eq 1 then inflag = 1;
      %ads_extr_path(pathpart, path, lastlist)
      %ads_expand_obj(fullqualobj, lastlist)
      if inflag and righttype then output;
      if pathpart eq fullqualobj then inflag = 0;
      %end;

   %* Is a range being selected (FIRSTOBJ= and LASTOBJ= specified)?;
   %else %if &rangeflag and not &allobjflag %then %do;
      %ads_extr_path(pathpart, path, firstlist)
      %ads_expand_obj(fullqualobj, firstlist)
      if pathpart eq fullqualobj then inflag = 1;
      %ads_extr_path(pathpart, path, lastlist)
      %ads_expand_obj(fullqualobj, lastlist)
      if inflag and righttype then output;
      if pathpart eq fullqualobj then inflag = 0;
      %end;

   %* Is a list being selected (OBJECTS= specified)?;
   %else %if &objflag and not &allobjflag %then %do;
      do i = 1 to 32500 until(object eq ' ');
         object = scan(objlist, i, ' ');

         if object ne ' ' then do;

            * For an object has n levels, 
              store in pathpart the last n levels from the path.;
            %ads_extr_path(pathpart, path, object)

            left = 0; right = 0;
            if substr(object, 1, 1) eq '<' then do;
               left = 1;
               object = substr(object, 2);
               end;
            if substr(object, length(object), 1) eq '>' then do;
               right = 1;
               object = substr(object, 1, length(object) - 1);
               end;

            * Store the object name with pounds and such to match the document.;
            %ads_expand_obj(fullqualobj, object)

            if fullqualobj eq pathpart then do;
               if righttype then output;
               object = ' ';
               end;
            end;
         end;
      %end;

   %* Is everything selected?;
   %else %do;
      if righttype then output;
      %end;

   if _error_ then call symputx('error', '1', 'L');
   run;

%if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;


* Omit the first or last object from the selection list with SKIPFIRST or SKIPLAST.;
%if &skipfirst or &skiplast %then %do;
   
   data _ads_select;
      set _ads_select end=eof;

      %if &skipfirst %then %do; if _n_ = 1 then delete; %end;
      %if &skiplast  %then %do; if eof     then delete; %end;

      if _error_ then call symputx('error', '1', 'L');
      run;

   %if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;
   %end;

* Set page breaks.;
data _ads_select(keep=path type left right object ourpath group label pagebreak);
   set _ads_select end=eof;
   retain Group 1 oldgroup anyleft 0;
   if type in ('Graph', 'Report', 'rpt') then do;
      left = 1; right = 1;
      if type eq 'Graph' then ngraphs + 1;
      end;
   if left then do;
      group = oldgroup + (not &onebox);
      if &onebox and group eq 0 then group = 1;
      anyleft = 1;
      end;
   oldgroup = group;
   output;
   if right then anyleft = 0;

   other   + (type in ('Note', 'Report', 'rpt'));
   tabtype = (type in ('Table', 'Tree', 'Batch', 'Crosstab', 'Text'));
   if tabtype then ntabs + 1;
   if right or (not anyleft and tabtype) then group = oldgroup + (not &onebox);

   if eof and &onebox and &isgraph and ((ntabs + other) or ngraphs ne 1) then do;
      put 'ERROR: The Graphic tag must select '
          'only one graph and nothing ' "else in document &store..";
      call symputx('onlylist', '1', 'L');
      call symputx('abnormal', '1', 'L');
      end;

   if eof and &onebox and not &isgraph and ((ntabs + other) lt 1 or ngraphs)
      then do;
      put 'ERROR: The Listing tag must select tables and no graphs '
          "in document &store..";
      call symputx('onlylist', '1', 'L');
      call symputx('abnormal', '1', 'L');
      end;

   if not &onlylist and eof and
      not &onebox and (ntabs + other) and ngraphs then do;
      put 'ERROR: You may request either tables or graphs but not both '
          "in document &store..";
      call symputx('onlylist', '1', 'L');
      call symputx('abnormal', '1', 'L');
      end;

   if _error_ then call symputx('error', '1', 'L');
   run;

%if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;

%if %index(&flow, print) %then %do;
   proc print; var path type group object; run;
   proc print; var ourpath type group object; run;
   %end;

* Check for objects that match multiple document pieces.;
%if not &allobjflag %then %do;
   proc sort data=_ads_select out=_ads_objects;
      by object;
      run;

   %if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;

   data _null_;
      set _ads_objects;
      by object;
      if object ne ' ' and not (first.object and last.object) then do;
         put 'ERROR: Multiple matches.  Object ' object
             'matches ' ourpath +(-1)'.';
         call symputx('error', '1', 'L');
         end;
      if _error_ then call symputx('error', '1', 'L');
      run;

   %if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;
   %end;

* Clear out titles, subtitles, and footnotes as necessary.  Build rendering code.;
options nosource;

data _null_;
   length list $ 32500;
   retain list ' ' forcepage &&__New_&store;
   set _ads_select end=end;
   by group;
   if _n_ eq 1 then call execute("proc document name=&store;");

   list = trim(list) || ' or _path_ = "' || trim(path) || '"';

   %if &pagebreak eq autopage %then %do; /* default */
      first = pagebreak;
      if type ne 'Graph' then do;
         first = (first or forcepage);
         forcepage = 0;
         end;
      %end;
   %else %if &pagebreak eq nopage %then %do;
      first = 0;
      %end;
   %else %do; /* new page */
      first = ((_n_ eq 1) or pagebreak);
      %end;

   if first and type ne 'Graph' then put 'Note: New page for ' path;

   if first    then call execute("obfootn " || path || ';');
   else if end then call execute('obtitle ' || path || '; obstitle ' ||
                                 path || '; run; quit;');
   else call execute('obfootn ' || path || '; obstitle ' || path ||
                   '; obtitle ' || path || ';');

   if last.group then do;
      list = substr(list, 5);
      call symputx('_list' || left(put(group, 5.)), trim(list), 'L');
      call symputx('_gtype' || left(put(group, 5.)), put(type, $1.), 'L');
      call symputx('ngroups', group, 'L');
      if type eq 'Report' then
         call symputx('_data' || left(put(group, 5.)), trim(label), 'L');
      list = ' ';
      end;

   if _error_ then call symputx('error', '1', 'L');
   run;

%if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;

options source &savenote;

%let __new_&store = 0;

%if &onebox and &ngroups ne 1 and not &abnormal %then %do;
   %if &isgraph %then %let i = Graphic;
   %else  %let i = Listing;
   %put ERROR: There must be exactly one group of objects with &i..;
   %let onlylist = 1;
   %let abnormal = 1;
   %end;

* Render each group of data objects.;
%if not &onlylist %then %do;
   ods _all_ close;
   ods graphics / reset=index imagename="";

   %let kl = 0; %let kh = 0;
   %do i = 1 %to &ngroups;

      %if %symexist(_list&i) %then %do;
         %if &kl eq 0 %then %let jl = ; %else %let jl = &kl;
         %if &kh eq 0 %then %let jh = ; %else %let jh = &kh;
         %let kl = %eval(&kl + 1);
         %let kh = %eval(&kh + 1);

         %if &&_gtype&i eq G %then %do; /* if graph replay */
            ods listing style=&style image_dpi=&dpi gpath="&graphicdir";
            ods graphics / reset=index imagename="&fname&jl"
                %if %nrbquote(&width)  ne %then width=&width;
                %if %nrbquote(&height) ne %then height=&height;;
            goptions dev=&gdevice fileonly gsfname=gsasfile gsfmode=replace
                     hsize=%if %nrbquote(&width) ne %then &width;
                     %else 6.4in;
                     vsize=&h border;
            filename gsasfile "&graphicdir/&fname&jl..png";
            %put NOTE: Writing Graph file: &graphicdir/&fname&jl..png;
            %end;
         %else %do;
            ods listing file="&listingdir/_tmp";
          %end;
   
         %if not %index(&flow, notes) %then %do; options nonotes; %end;

         proc document name=&store;
            replay ^(where=(&&_list&i)) / levels=all;
            run; quit;

         %if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;
         options &savenote;

         %if &&_gtype&i eq G %then %do; /* if graph replay */
            filename gsasfile "&graphicdir/_tmp.png";
            %end;

         ods _all_ close;
         ods graphics / reset=index imagename="" reset=width;

         %if &&_gtype&i ne G %then %do; /* if not graph */
            /* break up big listing files */
            options nonotes;

            data _null_;
               retain kl &jl;
               length fn $ 40;
               infile "&listingdir/_tmp" lrecl=%eval(&ls+1) pad;
               input line $char%eval(&ls+1).;
               page = substr(line, 1, 1) eq '0C'x;
               if _n_ eq 1 or page then do;
                  fn = "&listingdir/&fname";
                  if page then line = substr(line, 2);
                  if kl then fn = compress(fn || put(kl, 5.) || '.lst');
                        else fn = compress(fn || '.lst');
                  kl + 1;
                  call symputx('kl', kl, 'L');
                  put 'Note: Writing Listing file  : ' fn;
                  end;
               file x filevar=fn nopad;
               l = length(line);
               put line $varying. l;
               if _error_ then call symputx('error', '1', 'L');
               run;

            %if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;
            options &savenote;
            %end;
         %end;
      %end;
   ods listing;
   %end;

%if &ngroups eq 0 %then %do;
   %put ERROR: No objects selected.;
   %let error = 1;
   %goto endit;
   %end;

%if not %index(&flow, notes) %then %do; options nonotes; %end;

proc sort data=_ads_doclist; by path; run;

%if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;

proc sort data=_ads_select; by path; run;

%if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;

data _ads_doclist;
   merge _ads_doclist(in=dl) _ads_select(keep=path group in=s);
   by path;
   if s then Status = 'Selected';
   if _error_ then call symputx('error', '1', 'L');
   run;

%if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;

proc sort data=_ads_doclist; by objnum; run;

%if &syserr > 4 %then %let error = 1; %if &error %then %goto endit;

%endit:;

* Get some formatting information for rendering the document contents list.;
data _null_;
   set _ads_doclist end=eof;
   left = max(left, length(ourpath));
   tyw  = max(tyw,  length(type));
   if eof then do;
      left + 1;   tyw + 1;    stw = 9;    grw = 5;
      atright = min(&ls - tyw - stw - grw - 1, left + 3);
      call symputx('atright', atright,  'L');
      call symputx('tyw',     tyw,      'L');
      call symputx('stw',     stw,      'L');
      call symputx('grw',     grw,      'L');
      end;
   if _error_ then call symputx('error', '1', 'L');
   run;

%if &syserr > 4 %then %let error = 1;

* Render the document contents list, wrapping the path if necessary.;
data _null_;
   length p $ 500;
   status = '        '; group = .;
   file log;
   set _ads_doclist;
   ghead = 'Group';   thead = 'Type';   shead = 'Status';
   if _n_ eq 1 then put / 'Objects'
       @&atright                  thead $&tyw..
       @%eval(&atright+&tyw)      shead $&stw..
       @%eval(&atright+&tyw+&stw) ghead $&grw.. /;
   do j = 1 to 100 while(ourpath ne ' ');
      do l = &atright - 2 to &atright / 2 by -1
         while(not (substr(ourpath, l, 1) in ('.', ' ')));
         end;
      p = substr(ourpath, 1, l);
      if j eq 1 then
           put p $%eval(&atright-2). +1 type $&tyw.. status $&stw.. group 3.;
      else put p $%eval(&atright-2).;
      ourpath = substr(ourpath, l + 1);
      end;
   if _error_ then call symputx('error', '1', 'L');
   run;

%if &syserr > 4 %then %let error = 1;

%veryend:;

* Clean up.;
%if not %index(&flow, notes) %then %do; options nonotes; %end;
proc datasets nolist; delete _ads_select _ads_doclist _ads_objects; run; quit;

%if &syserr > 4 %then %let error = 1;

%let syslast = &holdlast;
%if &error or &abnormal %then %put ERROR: Macro write ended abnormally.;
run;

filename __f1 "&listingdir/_tmp";
filename __f2 "&graphicdir/_tmp.png";
data _null_; rc = fdelete('__f1'); rc = fdelete('__f2'); run;

options obs=max nosyntaxcheck &savenote &savetime &savesize;
%mend;

*------------------------------------------------------------------------;

                         /*---------------------------------------------*/
                         /* The OUTPUT macro opens an ODS document and  */
                         /* captures output until it encounters an      */
                         /* ENDOUTPUT macro or a WRITE macro.           */
                         /*---------------------------------------------*/
%macro output(label,     /* is a required keyword that specifies the    */
                         /* ODS document name.  This is used in the     */
                         /* STORE= option in the WRITE macro and in     */
                         /* \Listing and \Graphic tags.                 */
                         /*                                             */
             style=,     /* specifies the ODS style for GRSEG graphics. */
                         /* Use the STYLE= option in the WRITE macro to */
                         /* control the style for ODS Graphics.  If     */
                         /* STYLE= is specified, that style is used.    */
                         /* If STYLE= is not specified, and if a macro  */
                         /* variable DefaultStyle is defined, its value */
                         /* is used as the style.  If STYLE= is not     */
                         /* specified and the macro variable            */
                         /* DefaultStyle is not defined or if its value */
                         /* is null, then the default style (which is   */
                         /* the HTMLBLUE style) is used.                */
                         /*                                             */
                         /* In other words, the default style is the    */
                         /* HTMLBLUE style.  However, you can set the   */
                         /* default style to (say) LISTING by putting   */
                         /* the following line at the top of your first */
                         /* Sascode block, beginning in column 1:       */
                         /*                                             */
                         /* %*;%let defaultstyle=listing;               */
                         /*                                             */
                         /* Since this line begins with a null macro    */
                         /* comment (%*;) in column 1, it will not show */
                         /* up in the SAS code listing, but it will     */
                         /* affect the capture program and change the   */
                         /* default style for the duration of the       */
                         /* running of the program.                     */
                         /*                                             */
             dpi=);;     /* specifies DPI (dots per inch) for GRSEG     */
                         /* graphics.  You typically would not use this */
                         /* except when you would otherwise run out of  */
                         /* memory.  The default is 300 DPI, and a      */
                         /* reasonable alternative for GRSEG graphs     */
                         /* with a large file size is 96 DPI.  More     */
                         /* details follow.                             */
                         /*                                             */
                         /* With GRSEG graphics, you do not have        */
                         /* continuous control over DPI as you do with  */
                         /* ODS Graphics.  With the macro default DPI   */
                         /* of 300, you get 300 DPI.  Otherwise, you    */
                         /* get the device default of 96 DPI.           */
                         /*                                             */
                         /* This option controls the DPI for GRSEG      */
                         /* graphics.  Use the DPI= option in the WRITE */
                         /* macro to control the style for ODS          */
                         /* Graphics.  If DPI= is specified, that DPI   */
                         /* is used.  If DPI= is not specified, and if  */
                         /* a macro variable DefaultDPI is defined, its */
                         /* value is used as the DPI.  If DPI= is not   */
                         /* specified and the macro variable DefaultDPI */
                         /* is not defined or if its value is null,     */
                         /* then the default DPI (300) is used.         */
                         /*                                             */
                         /* In other words, the default DPI is 300.     */
                         /* However, you can set the default DPI for    */
                         /* GRSEG graphics to 96 by putting the         */
                         /* following line at the top of your first     */
                         /* Sascode block, beginning in column 1:       */
                         /*                                             */
                         /* %*;%let defaultdpi=100;                     */
                         /*                                             */
                         /* This is not a typo.  If you set the default */
                         /* DPI to anything other than 300, you get 96  */
                         /* DPI with GRSEG graphics and the specified   */
                         /* value with ODS Graphics.  With ODS          */
                         /* Graphics, the values of 300 and 100 are     */
                         /* most commonly used.                         */
                         /*                                             */
                         /* Since the %LET statement is preceded by a   */
                         /* null macro comment (%*;) in column 1, it    */
                         /* will not show up in the SAS code listing,   */
                         /* but it will affect the capture program and  */
                         /* change the default DPI for the duration of  */
                         /* the running of the program.  You can change */
                         /* DPI at any time with one of these           */
                         /* statements.                                 */
                         /*---------------------------------------------*/

%local log lst savenote gdevice;

* Set the default style using STYLE= and a DefaultStyle macro variable;
%if &style eq %then %let style = &defaultstyle;

* Set the default DPI using DPI= and a DefaultDPI macro variable;
%if &dpi eq %then %let dpi = &defaultDPI;

%let gdevice = %substr(png300, 1, 3 * (1 + (&dpi eq 300)));
%if not (&dpi eq 96 or &dpi eq 100 or &dpi eq 300)
   %then %put NOTE: DPI=96 will be used for GRSEG graphics.;

%let savenote = %sysfunc(getoption(notes));
%if %sysfunc(symexist(_ads_debug)) %then %do;
   %let log = 0;
   %let lst = 0;
   %let _ads_debug = %lowcase(&_ads_debug);
   %if %index(&_ads_debug, flowall) %then %do;
      %let log = 1;
      %let lst = 1;
      %end;
   %else %if %index(&_ads_debug, flowlog) %then %let log = 1;
   %else %if %index(&_ads_debug, flowlst) %then %let lst = 1;
   %if &log or &lst %then %do;
      ods listing;
      data _null_;
         %if &log %then %do; put "Begin Sascode block ""&label""."; %end;
         %if &lst %then %do;
            file print;
            put _page_;
            put "Begin Sascode block ""&label"".";
            %end;
         run;
      %end;
   %end;

run; quit;

options nonotes;
libname mydir '.';
options nodate nonumber obs=max nosyntaxcheck;
%global documentlabel;

ods _all_ close;
ods document close;

data _null_; call symputx("__New_&label", '1', 'G'); run;

proc datasets nolist nowarn;
   delete gseg(memtype=catalog);
   run; quit;

proc datasets nolist nowarn library=sasuser;
   delete __&label(memtype=catalog);
   run; quit;

* Select the style for GRSEG graphics.;
ods listing style=&style file="&listingdir/_tmp";
ods listing select none;
filename __f "&listingdir/_tmp";
data _null_; rc = fdelete('__f'); run;

* Set the global label, and open the ODS Document.;
%let documentLabel = &label;
ods document name  = &label(write) cat=sasuser.__&label;

* Specify 300 DPI for GRSEG graphics (gdevice = png or png300).;
goptions dev=&gdevice fileonly gsfname=gsasfile gsfmode=replace border;
filename gsasfile "&graphicdir/_tmp.png";
options &savenote;
%mend;

*------------------------------------------------------------------------;

                         /*---------------------------------------------*/
                         /* The ENDOUTPUT macro ends output capture.    */
                         /*---------------------------------------------*/
%macro endoutput(label);;/* specifies the ODS document name.  This is   */
                         /* used only to display program flow           */
                         /* information in the SAS log when the macro   */
                         /* variable _ads_debug is defined.             */
                         /*---------------------------------------------*/

run; quit; 
options obs=max nosyntaxcheck;
ods document close;

%local log lst;

%if %sysfunc(symexist(_ads_debug)) %then %do;
   %let log = 0;
   %let lst = 0;
   %let _ads_debug = %lowcase(&_ads_debug);
   %if %index(&_ads_debug, flowall) %then %do;
      %let log = 1;
      %let lst = 1;
      %end;
   %else %if %index(&_ads_debug, flowlog) %then %let log = 1;
   %else %if %index(&_ads_debug, flowlst) %then %let lst = 1;
   %if &log or &lst %then %do;
      ods listing;
      data _null_;
         %if &log %then %do; put "End Sascode block ""&label""."; %end;
         %if &lst %then %do;
            file print;
            put "End Sascode block ""&label"".";
            put _page_;
            %end;
         run;
      %end;
   %end;
ods _all_ close;
%mend;

*------------------------------------------------------------------------;

                         /*---------------------------------------------*/
                         /* The STARTLIST macro starts the capture of   */
                         /* listing output.  Use this macro when you    */
                         /* want to program your own output capture     */
                         /* (for example, when you want to show only    */
                         /* part of a table).                           */
                         /*---------------------------------------------*/
%macro startlist(fname,  /* is a required keyword that specifies the    */
                         /* first level of the output file name.  This  */
                         /* name will appear at the end of the \Listing */
                         /* tag.                                        */
                         /*                                             */
             pagesize=,  /* specifies page size.  The default is the    */
                         /* page size currently in effect.  This        */
                         /* specification lasts for the duration of     */
                         /* this step.  The current page size is        */
                         /* restored at the end.                        */
             linesize=);;/* specifies line size.  The default is the    */
                         /* line size currently in effect.  This        */
                         /* specification lasts for the duration of     */
                         /* this step.  The current line size is        */
                         /* restored at the end.                        */
                         /*---------------------------------------------*/

%global _ads_printfname _ads_savesize;
%let _ads_printfname = &fname;
%let _ads_savesize = ls=%sysfunc(getoption(ls)) ps=%sysfunc(getoption(ps));
quit;
ods _all_ close;
ods document close;
options nodate nonumber %if &pagesize ne %then ps=&pagesize;
                        %if &linesize ne %then ls=&linesize;;
ods listing file="&listingdir/_tmp";
%mend;

                         /*---------------------------------------------*/
%macro endlist;;         /* The ENDLIST macro ends the capture of       */
                         /* listing output.                             */
                         /*---------------------------------------------*/
%local ls savenote;
run;
ods _all_ close;
%let ls = %sysfunc(getoption(ls));
%let savenote = %sysfunc(getoption(notes));

options nonotes;
data _null_;
   retain kl 0;
   length fn $ 40;
   infile "&listingdir/_tmp" lrecl=%eval(&ls+1) pad;
   input line $char%eval(&ls+1).;
   page = substr(line, 1, 1) eq '0C'x;
   if _n_ eq 1 or page then do;
      ext = 'lst';;
      fn = ext || "/&_ads_printfname";
      if page then line = substr(line, 2);
      if kl then fn = compress(fn || put(kl, 5.) || '.' || ext);
            else fn = compress(fn || '.' || ext);
      kl + 1;
      put 'Note: Writing Print file: ' fn;
      end;
   file x filevar=fn nopad;
   l = length(line);
   put line $varying. l;
   run;

options &savenote &_ads_savesize;

filename __f "&listingdir/_tmp";
data _null_; rc = fdelete('__f'); run;
%mend;

*------------------------------------------------------------------------;

                         /*---------------------------------------------*/
                         /* The STARTLOG macro starts the capture of    */
                         /* the SAS log.                                */
                         /*---------------------------------------------*/
%macro startlog(fname);; /* is a required keyword that specifies the    */
                         /* first level of the output file name.  This  */
                         /* name will appear at the end of the \Listing */
                         /* tag.                                        */
                         /*---------------------------------------------*/
%global __fname __ls;
%let __fname=&fname;
%let __ls = %sysfunc(getoption(ls));

options nostimer nosource2 nosource;
proc printto log="&listingdir/__tmp" new; run;
%mend;


                         /*---------------------------------------------*/ 
                         /* The ENDLOG macro ends the capture of the    */
                         /* SAS log.                                    */
                         /*---------------------------------------------*/ 
%macro endlog(store=1,   /* set STORE=0 to capture messages only and    */
                         /* not code.                                   */
              range=1);; /* Specify a Boolean expression to select only */
                         /* certain observations.  Examples:            */
                         /* RANGE=_n_ < 5,                              */
                         /* RANGE=not index(line, 'ERROR').             */
                         /*---------------------------------------------*/

%local savenote;
%let savenote = %sysfunc(getoption(stimer)) %sysfunc(getoption(source2))
                %sysfunc(getoption(source));

proc printto log=log; run;

options stimer source2 source;

data _null_;
   infile "&listingdir/__tmp" lrecl=%eval(&__ls+4) pad;
   input line $char&__ls..;
   if anycntrl(line) = 0;
   %if &store %then %do;
      if index(line, '%endlog') = 0;
      %end;
   %else %do;
      if not (n(input(scan(line, 1, ' +'), ?? 12.)) and 
              compress(line, ' 0123456789') =: '+');
      %end;
   if &range;
   file "&listingdir/&__fname..lst" lrecl=%eval(&__ls+4);
   put line $char&__ls..;
   run;

filename __f "&listingdir/__tmp";
data _null_; rc = fdelete('__f'); run;

options &savenote;
%mend;

*------------------------------------------------------------------------;

* This utility macro stores in the out macro variable the 
  object name with pounds and such to match the name in the document.;
%macro ads_expand_obj(out, in);
   length _w $ 200;
   &out = ' ';
   do _j = 1 to 32500 until(_w = ' ');
      _w = scan(&in, _j, '.');
      if _w ne ' ' then do;
         if not index(_w, '#') then _w = trim(_w) || '#1';
         &out = trim(&out) || '\' || _w;
         end;
      end;
   &out = substr(&out, 3);
   %mend;

* This utility macro stores in the out macro variable the last n levels 
  (for an object with n levels) from the path.;
%macro ads_extr_path(out, path, object);
   _levs = 1 + length(&object) - length(compress(&object, '.'));
   call scan(&path, -_levs, _p, _len, '\');
   &out = lowcase(substr(&path, max(_p, 1)));
   if substr(&out, 1, 1) eq '\' then &out = substr(&out, 2);
   %mend;

*------------------------------------------------------------------------;
