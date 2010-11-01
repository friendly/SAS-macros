 /****************************************************************/
 /*          S A S   S A M P L E   L I B R A R Y                 */
 /*                                                              */
 /*    NAME: PAINT                                               */
 /*   TITLE: Set interpolated colors for PROC INSIGHT            */
 /* PRODUCT: STAT                                                */
 /*  SYSTEM: ALL                                                 */
 /*    KEYS: GRAPH                                               */
 /*   PROCS: SQL CONTENTS PRINT SUMMARY FORMAT CATALOG MEANS     */
 /*          DATASETS PRINCOMP                                   */
 /*    DATA:                                                     */
 /*                                                              */
 /* SUPPORT: WFK                                                 */
 /*     REF:                                                     */
 /*    MISC:                                                     */
 /****************************************************************/

 /*-------------------------------------------------------------------

 DISCLAIMER:

       THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE
 TO ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
 EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
 PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
 CONTAINED HEREIN.

 -------------------------------------------------------------------*/

%macro paint(             /*-----------------------------------------*/
                          /* This macro reads an input DATA= data    */
                          /* set and creates an OUT= data set with a */
                          /* new variable _OBSTAT_ that contains     */
                          /* observation symbols and colors          */
                          /* interpolated from the COLORS= list      */
                          /* based on the values VAR= variable.      */
                          /* Three interpolation methods are         */
                          /* available with the LEVEL= option.       */
                          /* The resulting data set is suitable for  */
                          /* display with PROC INSIGHT.              */
                          /*                                         */
                          /* This macro creates work data sets       */
                          /* named TEMPDATA and (possibly) TEMPDAT2  */
                          /* and by default creates an OUT= data set */
                          /* named COLORS.                           */
                          /*-----------------------------------------*/
                          /* THIS NEXT OPTION MUST BE SPECIFIED:     */
                          /*                                         */
var=,                     /* Variable to use to interpolate between  */
                          /* colors.                                 */
                          /*-----------------------------------------*/
                          /*                                         */
                          /* All of these next options may           */
                          /* optionally be specified:                */
                          /*                                         */
data=_last_,              /* Input SAS data set.                     */
                          /*                                         */
out=colors,               /* Output SAS data set.                    */
                          /*                                         */
colors=,                  /* COLORS=optional-color-list              */
                          /* optional-data-value-list.               */
                          /* The colors must be selected from: red,  */
                          /* green, blue, yellow, magenta, cyan,     */
                          /* black, white, orange, brown, gray,      */
                          /* olive, pink, purple, violet.  For other */
                          /* colors, specify the RGB color name      */
                          /* (XCrrggbb where rr is the red value, gg */
                          /* is the green, and bb is blue, all three */
                          /* specified in hex).  When no value is    */
                          /* specified, the default is               */
                          /* COLORS=blue magenta red.                */
                          /* COLORS=red green 1 10, interpolates     */
                          /* between red and green, based on the     */
                          /* values of the VAR= variable, where      */
                          /* values of 1 or less map to red, values  */
                          /* of 10 or more map to green, and values  */
                          /* in between map to colors in between.    */
                          /* COLORS=red yellow green 1 5 10,         */
                          /* interpolates between red at 1, yellow   */
                          /* at 5, and green at 10.  If the data     */
                          /* value list is omitted it is computed    */
                          /* from the data.                          */
                          /*                                         */
level=interval,           /* interval - interpolate on the VAR=      */
                          /* variable.                               */
                          /* ordinal  - interpolate on ranks of the  */
                          /* VAR= variable.  This can even work with */
                          /* character variables; the ranks are just */
                          /* the category numbers.                   */
                          /* nominal  - one color or symbol per      */
                          /* category.                               */
                          /* Only the first three characters are     */
                          /* checked.                                */
                          /*                                         */
symbols=circle,           /* List of insight symbols from page 323   */
                          /* of insight manual.  Specify integers    */
                          /* from 1 to 8 or select from: square plus */
                          /* circle diamond x up down star.  Note    */
                          /* that 'triangle' is not specified with   */
                          /* 'up' and 'down'.  Equivalent examples:  */
                          /* SYMBOLS=1 2 3 4 5 6,                    */
                          /* SYMBOLS=square plus circle diamond x up,*/
                          /* SYMBOLS=1 plus circle 4 5 up            */
                          /* Note that when LEVEL=interval, only the */
                          /* first symbol is used.  With             */
                          /* LEVEL=nominal and LEVEL=ordinal, the    */
                          /* first symbol is used for the first      */
                          /* category, the second symbol is used for */
                          /* the second category, ..., and if there  */
                          /* are more categories than symbols, the   */
                          /* last symbol is reused for all           */
                          /* subsequent categories.  Extra symbols   */
                          /* are ignored.  The default first symbol  */
                          /* is circle, and the last symbol is       */
                          /* substituted for invalid symbols.        */
                          /*                                         */
format=,                  /* Paint variable format used with         */
                          /* LEVEL=nominal and LEVEL=ordinal         */
                          /* variables.                              */
                          /*                                         */
order=internal,           /* PROC SUMMARY ORDER= option for          */
                          /* level=ordinal and level=nominal.        */
                          /*                                         */
                          /* These next four options specify the     */
                          /* first four columns of _OBSTAT_ variable */
                          /* from page 323 of the insight manual.    */
                          /* Column 5, the symbol, is specified with */
                          /* SYMBOLS=.                               */
select=0,                 /* select state:       0 for not selected. */
show=1,                   /* show/hide state:       0 for hide.      */
include=1,                /* include/exclude state: 0 for exclude    */
label=0,                  /* label/unlabel state:   0 for unlabel.   */
                          /* The values must be numeric expressions  */
                          /* including constants, variables, and     */
                          /* arithmetic expressions.  All nonzero    */
                          /* expression results are converted to 1.  */
                          /*                                         */
rgbround=-99 1 1 1,       /* Rounding factors used for the COLORS=   */
                          /* variable and RGB values.  The first     */
                          /* value is used to round the COLORS=var   */
                          /* variable.  Specify a positive value to  */
                          /* have the variable rounded to multiples  */
                          /* of that value.  Specify a negative      */
                          /* value n to have a maximum of abs(n)     */
                          /* colors.  For the other three values,    */
                          /* specify positive values.  The last      */
                          /* three are rounding factors for the red, */
                          /* green, and blue component of the color. */
                          /* By default, when a value is missing,    */
                          /* there is no rounding.                   */
                          /*                                         */
missing=,                 /* specifies how missing values in the     */
                          /* VAR= variable are handled.  By default  */
                          /* when MISSING= is null, the observation  */
                          /* is not shown (SHOW=0 is set).  The      */
                          /* specified value is a color followed by  */
                          /* a symbol.  When only one value is       */
                          /* specified in the SYMBOLS= list, the     */
                          /* symbol is optional, and the SYMBOLS=    */
                          /* symbol is used.                         */
                          /*                                         */
debug=,                   /* vars   - print macro options and macro  */
                          /*          variables for debugging.       */
                          /* dprint - print intermediate data sets.  */
                          /* notes  - do not specify OPTIONS NONOTES */
                          /*          during most of the macro.      */
                          /* time   - prints total macro run time.   */
                          /* mprint - run with OPTIONS MPRINT.       */
                          /* Concatenate names for more than one     */
                          /* type of debugging.  Example:            */
                          /* DEBUG=vars dprint notes time mprint.    */
);                        /*-----------------------------------------*/

%if not %index(%nrbquote(&debug), notes) %then %str(options nonotes;);

*------store starting time, initialize a few variables-------;
data _null_;
   time = datetime();
   call symput('start', compress(put(time, best15.)));
   length debug $ 40;

   *------debugging initializations------;
   debug = symget('debug');
   call symput('dbyes'   , compress(put(index(debug, 'vars')  , 3.)));
   call symput('dbprint' , compress(put(index(debug, 'dprint'), 3.)));
   call symput('dbtime'  , compress(put(index(debug, 'time')  , 3.)));
   call symput('dbmprint', compress(put(index(debug, 'mprint'), 3.)));

   *------make sure these are not null------;
   if symget('select' ) eq ' ' then call symput('select' , '1');
   if symget('show'   ) eq ' ' then call symput('show'   , '0');
   if symget('include') eq ' ' then call symput('include', '0');
   if symget('label'  ) eq ' ' then call symput('label'  , '1');
   if symget('symbols') eq ' ' then call symput('symbols', '3');

   *------other initializations------;
   call symput('ndigits', '8');
   if upcase(symget('data')) eq '_LAST_'
      then call symput('data', symget('syslast'));
   call symput('abort', put(_error_ ne 0, 1.));
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;
%if &dbmprint %then %str(options mprint;);

%if &dbyes %then
   %dump(var data out colors symbols level format order
         select show include label rgbround debug missing
         dbyes dbprint dbtime dbmprint abort);

*------store page size for file log------;
proc sql noprint;
   select setting into :logps from dictionary.options
          where optname = 'PAGESIZE';
   quit;

%if &logps < 500 %then %let logps = %eval(&logps + 1);
%else %let logps = 500;
%if &logps < 20 %then %let logps = 20;

*------output data set names, types------;
proc contents data=&data noprint out=tempdata;
   %if &format ne %then %str(format &var &format;);
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

%if &dbprint %then %do;
   proc print data=tempdata;
      title3 'tempdata - temporary data set, proc contents results';
      run;
   title3;
   %end;

data _null_;
   file log ps=&logps;
   length format lname $ 12 paintvar $ 8;
   set tempdata end=eof;
   retain paintvar ' ' pntfound 0;

   *------get the name of the paint variable, check LEVEL=------;
   if _n_ eq 1 then do;
      paintvar = upcase(left(symget('var')));
      if paintvar eq ' ' then do;
         put 'ERROR: VAR= must be specified.';
         call symput('abort', '1');
         end;
      lname = lowcase(symget('level'));
      if      lname =: 'nom' then lname = 'nominal';
      else if lname =: 'ord' then lname = 'ordinal';
      else if lname =: 'int' then lname = 'interval';
      if not (lname in ('nominal', 'ordinal', 'interval')) then do;
         put 'ERROR: Invalid LEVEL=' lname +(-1) '.';
         call symput('abort', '1');
         end;
      call symput('level', trim(lname));
      end;

   *------check for existence of paint variable------;
   if upcase(name) eq paintvar then do;
      pntfound = 1;

      *------check type, generate type appropriate where clause------;
      if type eq 2 then do;
         call symput('wherenm', trim(paintvar) || " ne ' '");
         if symget('level') eq 'interval' then do;
            call symput('level', 'ordinal');
            put 'WARNING: LEVEL=ordinal is assumed with '
                'character ' 'variables and ' 'LEVEL=interval.';
            end;
         end;
      else call symput('wherenm', 'n(' || trim(paintvar) || ')');

      *------set variable format------;
      if format eq ' ' then do;
         if type eq 2 then format = '$';
                      else format = 'best';
         formatl = 8;
         end;
      else if formatl <= 0 then formatl = 8;
      format = compress(format || put(formatl, 3.));
      if index(format, '.') eq 0 then format = compress(format || '.');
      call symput('format' , trim(format));
      call symput('formatl', compress(put(formatl, 3.)));
      end;

   *------paint variable found?------;
   if eof and not pntfound and paintvar ne ' ' then do;
      put 'ERROR: The paint variable VAR=' paintvar 'was not found.';
      call symput('abort', '1');
      end;

   if _error_ then call symput('abort', '1');
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

*------substitute category numbers for nominal, ordinal------;
%if &level ne interval %then %do;

   *------get list of formatted categories------;
   proc summary data=&data order=&order noprint;
      class &var;
      output out=tempdata;
      format &var &format;
      run;

   %if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

   %if &dbprint %then %do;
      proc print data=tempdata;
         title3 'tempdata - temporary data set, proc summary results';
         run;
      title3;
      %end;

   *------create CNTLIN= data set for PROC FORMAT------;
   data tempdat2;
      set tempdata(keep=&var where=(&wherenm)) end=eof;
      length label fmtname $ 8;

      *------format data values into integers------;
      fmtname = '$painfmt';
      start   = put(&var, &format);
      label   = compress(put(_n_, best8.));

      if eof and _n_ < 1000 then
         call symput('ndigits', put(1 + (_n_ > 9) + (_n_ > 99) +
                     2 * (symget('level') eq 'ordinal'), 1.));

      keep start label fmtname;
      if _error_ then call symput('abort', '1');
      run;

   %if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

   %if &dbprint %then %do;
      proc print data=tempdat2;
         title3 'tempdat2 - temporary data set, proc format input';
         run;
      title3;
      %end;

   *------format for mapping data values into category numbers------;
   proc format cntlin=tempdat2;
      run;

   %if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

   *------create CNTLIN= data set for PROC FORMAT------;
   data tempdat2;
      set tempdata(keep=&var where=(&wherenm));
      length label $ &formatl start fmtname $ 8;

      *------format category numbers back into data values------;
      fmtname = '$backfmt';
      if _n_ eq 1 then do;
         hlo = 'O'; output;
         end;
      hlo   = ' ';
      start = left(put(_n_, best8.));
      label = put(&var, &format);
      output;

      keep start label fmtname hlo;
      if _error_ then call symput('abort', '1');
      run;

   %if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

   %if &dbprint %then %do;
      proc print data=tempdat2;
         title3 'tempdat2 - temporary data set, proc format input';
         run;
      title3;
      %end;

   *------format mapping category numbers back into data values------;
   proc format cntlin=tempdat2;
      run;

   %if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

   *------store category numbers as numerics------;
   data &out;
      set &data;
      if &wherenm
         then __catnum = input(put(put(&var, &format), $painfmt.), 32.);
         else __catnum = .;
      if _error_ then call symput('abort', '1');
      run;

   %if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

   %if &dbprint %then %do;
      proc print data=&out;
         title3 "&out - temporary data set, category numbers";
         run;
      title3;
      %end;

   proc catalog c=work.formats;
      delete painfmt / et=formatc;
      run; quit;

   %if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

   *------updated input data set, paint variable------;
   %let data = &out;
   %let var  = __catnum;
   %end;

*------find minimum, maximum of paint variable------;
proc means data=&data noprint;
   var &var; /* at this point &var is guaranteed to be numeric */
   output out=tempdata max=paintmax min=paintmin;
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

%if &dbprint %then %do;
   proc print data=tempdata;
      title3 'tempdata - temporary data set, proc means output';
      run;
   title3;
   %end;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

*------initialization------;
data _null_;
   file log ps=&logps;
   length colors rgbs endpts rounds holdlist $ 200 rank $ &ndigits
          name1 name2 color endpt level $ 8 misscol $ 15;
   ok = 1; level = symget('level'); rank = ' ';

   set tempdata(drop=_type_ _freq_);   /* min, max of paint variable */

   *------table of recognized colors and their hex rgb------;
   allcols = 'BLUE----MAGENTA-RED-----YELLOW--GREEN---CYAN----' ||
             'ORANGE--PINK----BROWN---OLIVE---PURPLE--VIOLET--' ||
             'BLACK---WHITE---GRAY----';
   hexcols = '0000ff--ff00ff--ff0000--ffff00--00ff00--00ffff--' ||
             'ff8000--ff0080--a05000--2a8307--703070--b090d0--' ||
             '000000--ffffff--808080--';

   *------anything specified for the COLORS= option?------;
   colors = left(symget('colors'));
   if colors eq ' ' then colors = 'blue magenta red';

   *------count colors list elements------;
   do i = 1 to 2 until(ncolors > 1);

      holdlist = ' ';
      do ncolors = 1 to 200 until(name2 = ' ');
         name2 = scan(colors, ncolors, ' ');
         if n(input(name2, ?? 32.)) then name2 = ' ';
         if level eq 'nominal'
            then holdlist = trim(holdlist) || ' ' || name2;
         end;
      ncolors = ncolors - 1;

      if ncolors eq 1 then do;
         colors = scan(colors, 1, ' ');
         colors = trim(colors) || ' ' || colors;
         end;
      end;

   *------nominal, one color per category------;
   if level eq 'nominal' and ncolors ne paintmax then do;

      *------store the first part of the list------;
      colors = ' ';
      do i = 1 to min(ncolors, paintmax);
         colors = trim(colors) || ' ' || scan(holdlist, i, ' ');
         end;
      colors = upcase(colors);

      *------augment the list if necessary------;
      do i = ncolors + 1 to paintmax;
         do k = i - ncolors to 15;
            color = scan(allcols, k, '-');
            if index(colors, trim(color)) eq 0 then do;
               colors = trim(colors) || ' ' || color;
               k = 20;
               end;
            end;
         if k < 20 then do;
            put 'ERROR: Too many distinct data '
                'values. ' ' Try LEVEL=ordinal.';
            ok = 0; goto endit;
            end;
         end;

      ncolors = paintmax;
      end;

   *------construct list of (decimal) RGB values------;
   rgbs = ' '; /* ncolors reds, ncolors greens, then ncolors blues */
   do j = 1 to 3;
      do i = 1 to ncolors;

         color = upcase(scan(colors, i, ' '));
         link getrgb;

         if length(rgbs) + length(name1) + 1 > 200 then do;
            put 'ERROR: COLORS= list is too long.';
            ok = 0; goto endit;
            end;

         *------build list------;
         rgbs = trim(rgbs) || ' ' || trim(name1);
         end;
      end;

   *------store line segment end points in list if specified------;
   endpts = ' ';
   name1  = scan(colors, ncolors + 1, ' ');
   if name1 ne ' ' then do;
      do i = 1 to ncolors;
         name1 = scan(colors, ncolors + i, ' ');
         if nmiss(input(name1, ?? 32.)) then do;
            put 'ERROR: COLORS= data value of ' name1 'is not valid.';
            ok = 0; goto endit;
            end;
         if length(endpts) + length(name1) + 1 > 200 then do;
            put 'ERROR: COLORS= list is too long.';
            ok = 0; goto endit;
            end;
         endpts = trim(endpts) || ' ' || name1;
         end;
      name1 = scan(colors, 2 * ncolors + 1, ' ');
      if name1 ne ' ' then do;
         put 'ERROR: COLORS= data value list is too long.';
         ok = 0; goto endit;
         end;
      end;

   *------check for sufficient range------;
   if n(paintmin, paintmax) ne 2 then do;
      put "ERROR: All missing VAR= variable &var..";
      ok = 0; goto endit;
      end;

   if paintmax - paintmin < 1e-8 then do;
      put 'ERROR: Insufficient range in VAR= - minimum ('
          paintmin +(-1) ') and maximum (' paintmax +(-1) ').';

      %if &level ne interval %then %do;
         rank = compress(put(paintmin, best8.));
         put @8 'Minimum rank: ' paintmin +(-1)
             '. ' ' Formatted value: ' rank $backfmt&formatl..;
         rank = compress(put(paintmax, best8.));
         put @8 'Maximum rank: ' paintmax +(-1)
             '. ' ' Formatted value: ' rank $backfmt&formatl..;
         %end;

      ok = 0; goto endit;
      end;

   *------parse RGBROUND= option------;
   rounds = symget('rgbround'); holdlist = ' ';
   do i = 1 to 4;
      name = scan(rounds, i, ' ');
      num  = input(name, ?? 32.);
      if nmiss(num) then num = .;
      if i > 1 and num <= 0 then num = 1;
      holdlist = trim(holdlist) || ' ' || compress(put(num, best8.));
      end;

   rounds = holdlist;
   n = input(scan(rounds, 1, ' '), ?? 32.);
   if n(n) and n < 0 then n = -(paintmax - paintmin) / (n + 1);
   rounds = trim(rounds) || ' ' || compress(put(n, best8.));

   *------create end points list if not specified------;
   if endpts eq ' ' then do;
      inc = (paintmax - paintmin) / (ncolors - 1);
      do i = paintmin to paintmax by inc;
         endpt = compress(put(i, best8.));
         if length(endpts) + length(endpt) + 1 > 200 then do;
            put 'ERROR: COLORS= list is too long.';
            ok = 0; goto endit;
            end;
         endpts = trim(endpts) || ' ' || compress(endpt);
         end;
      end;

   *------print legend------;
   colors = lowcase(colors);
   put / 'Legend:';
   do i = 1 to ncolors;
      color = scan(colors, i, ' ');
      name2 = scan(endpts, i, ' ');
      %if &level ne interval %then %do;
         rank = left(name2);
         rank = right(rank);
         put name2 $backfmt&formatl..
             ' (' rank $char&ndigits.. ') = ' color;
         %end;
      %else %do;
         endpt = right(name2);
         put endpt $char8. ' = ' color;
         %end;
      end;
   put ' ';

   *------set missing value color------;
   color   = upcase(scan(symget('missing'), 1, ' '));
   misscol = ' ';
   if color ne ' ' then do;
      do j = 1 to 3;
         link getrgb;
         substr(misscol, 1 + (j - 1) * 5, 5) =
                         put(min(65535, num * 257.117647), 5.);
         end;
      color = lowcase(color);
      put 'Missing values = ' color +(-1) '.' / ' ';
      end;

   *------output results------;
   call symput('ncolors' , compress(put(ncolors, 3.)));
   call symput('misscol' , misscol);
   call symput('rgbs'    , trim(left(rgbs  )));
   call symput('rgbround', trim(left(rounds)));
   call symput('endpts'  , trim(left(endpts)));

endit: if _error_ or not ok then call symput('abort', '1');
   stop;

getrgb: *------get the jth component of RGB for a color------;
   k = index(allcols, trim(color));

   if color eq ' ' then do;
      put 'ERROR: COLORS= problem. ' ' The list may be too long.';
      ok = 0; goto endit;
      end;

   *------grab hex code for name, or parse CXrrggbb------;
   if k then name1 = substr(hexcols, k + (j - 1) * 2, 2);
   else      name1 = substr(color  , (j - 1) * 2 + 3, 2);

   *------check for validity------;
   num = input(name1, ?? hex2.);

   if nmiss(num) then do;
      put 'ERROR: The color ' color 'is not valid.';
      ok = 0; goto endit;
      end;

   name1 = compress(put(num, 3.));
   return;
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

%if &level ne interval %then %do;
   proc catalog c=work.formats;
      delete backfmt / et=formatc;
      run; quit;

   %if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;
   %end;

proc datasets nolist nowarn;
   delete tempdata tempdat2;
   run; quit;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

*------process the input data set------;
options notes;
data &out;

   file log ps=&logps;
   length _obstat_ $ 20 __asym $ 8 __allsym $ 64 __symlis $ 200;
   retain _obstat_ __allsym ' ' __sym1 __symn 3
          __ncolor &ncolors __symmis __symlis __nomord __minov
          __maxov __roupai __roured __rougre __roublu;
   array __cols[3] __col1-__col3;
   array __paint[%eval(4 * &ncolors)] _temporary_ (&rgbs &endpts);
   array __rou[5] __dummy __roured __rougre __roublu __roupai
                  (&rgbround .);

   *------initial setup------;
   if _n_ eq 1 then do;

      *------nominal or ordinal------;
      __nomord = (symget('level') ne 'interval');

      *------symbol names------;
      __allsym = 'SQUARE--PLUS----CIRCLE--DIAMOND-' ||
                 'X-------UP------DOWN----STAR----';
      __symlis = symget('symbols');
      __asym   = scan(__symlis, 1, ' ');
      link getsym;
      if n(__i) then __sym1 = __i;

      *------find last symbol------;
      do __i = 1 to 200 until(__asym = ' ');
         __asym = scan(__symlis, __i, ' ');
         end;
      __asym = scan(__symlis, __i - 1, ' ');
      link getsym;
      if n(__i) then __symn = __i; else __symn = __sym1;

      *------symbols for missing values------;
      if symget('missing') ne ' ' then do;
         __asym = scan(symget('missing'), 2, ' ');
         if __asym eq ' ' then do;
            if scan(__symlis, 2, ' ') eq ' ' then __asym = __symlis;
            else do;
               put 'ERROR: A symbol must be specified with '
                   'MISSING= when ' 'there is ' 'more than '
                   'one symbol ' 'in SYMBOLS=.';
               call symput('abort', '1');
               stop;
               end;
            end;

         link getsym;
         if n(__i) then __symmis = __i; else __symmis = __sym1;
         end;

      *------computed colors------;
      __minov = __paint[3 * __ncolor + 1];
      __maxov = __paint[4 * __ncolor];
      do __i = 3 * __ncolor + 2 to 4 * __ncolor;
         if __paint[__i] - __paint[__i - 1] < 1e-8 then do;
            put 'ERROR: Invalid COLORS= data values. '
                ' Numbers ' 'must increase.';
            call symput('abort', '1');
            stop;
            end;
         end;

      end;

   set &data end=eof;
   __catnum = &var; _obstat_ = ' ';

   *------assign colors------;
   do __i = 2 to __ncolor;
      __minx = __paint[3 * __ncolor + __i - 1];
      __maxx = __paint[3 * __ncolor + __i];
      if n(__catnum) then do;
         __hold = __catnum;
         if n(__roupai) then __hold = round(__hold, __roupai);
         __hold = max(min(__hold, __maxov), __minov);
         if __minx <= __hold <= __maxx then do;
            do __j = 1 to 3;
               __minc = __paint[(__j - 1) * __ncolor + __i - 1];
               __maxc = __paint[(__j - 1) * __ncolor + __i];
               __cols[__j] = (__hold - __minx) * (__maxc - __minc) /
                             (__maxx - __minx) + __minc;
               __cols[__j] = min(65535, round(__cols[__j] * 257.117647,
                                              __rou[__j + 1]));
               end;
            _obstat_ = repeat(' ', 4)  || put(__col1, 5.) ||
                       put(__col2, 5.) || put(__col3, 5.);
            end;
         end;
      end;

   *------fill in first part of _OBSTAT_ variable------;
   substr(_obstat_, 1, 1) = put((&select ) ne 0, 1.);
   substr(_obstat_, 2, 1) = put((&show   ) ne 0, 1.);
   substr(_obstat_, 3, 1) = put((&include) ne 0, 1.);
   substr(_obstat_, 4, 1) = put((&label  ) ne 0, 1.);
   substr(_obstat_, 5, 1) = put(__sym1         , 1.);

   *------assign symbols, nominal and ordinal------;
   if __nomord and n(__catnum) then do;
      __asym = scan(__symlis, __catnum, ' ');
      link getsym;
      if nmiss(__i) then __i = __symn;
      substr(_obstat_, 5, 1) = put(__i, 1.);
      end;

   *------handle missing data------;
   if nmiss(__catnum) then do;

      *------by default, hide the observation------;
      if symget('missing') eq ' ' then do;
         substr(_obstat_,  2,  1) = '0';
         substr(_obstat_,  6, 15) = ' ';
         substr(_obstat_, 10,  1) = '0';
         substr(_obstat_, 15,  1) = '0';
         substr(_obstat_, 20,  1) = '0';
         end;

      *------otherwise set the color and symbol------;
      else do;
         substr(_obstat_,  6, 15) = symget('misscol');
         substr(_obstat_,  5,  1) = put(__symmis, 1.);
         end;
      end;

   *------total macro time------;
   if eof and &dbtime then do;
      __time = put(datetime() - &start, 6.1);
      put 'NOTE: The PAINT macro used ' __time
          'seconds to ' "create OUT=&out..";
      end;

   if _error_ then call symput('abort', '1');

   drop __dummy __asym __catnum __allsym __sym1 __symn __symlis
        __symmis __nomord __ncolor __col1-__col3 __minx __maxx
        __minc __maxc __j __i __hold __minov __maxov __roupai
        __roured __rougre __roublu __time;

   return;

getsym: *------get the numeric value for __asym------;

   __i = input(__asym, ?? 32.);
   if nmiss(__i) then do;
      __i = index(__allsym, trim(upcase(__asym)));
      __i = floor((__i - 1) / 8) + 1;
      end;
   if __i > 8 or __i < 1 then __i = .;
   return;

   run;

%if &syserr > 4 %then %let abort = 1;

%if &dbprint %then %do;
   proc print data=&out;
      title3 "&out - final output data set";
      run;
   title3;
   %end;

%endit:

%if &dbyes %then
   %dump(abort colors data dbmprint dbprint dbtime dbyes
         debug misscol endpts format formatl level missing ncolors
         order out select show include label rgbround
         rgbs start symbols var logps wherenm ndigits

options notes;
%if &dbmprint %then %str(options nomprint;);

%if &abort %then %put ERROR: The PAINT macro ended abnormally.;

%mend paint;

*------debugging routine to dump out macro variables------;
%macro dump(stuff);
%let i = 1;
%let word = %scan(&stuff, 1);
%do %while(&word ne);
   %put &word=&&&word...;
   %let i = %eval(&i + 1);
   %let word = %scan(&stuff, &i);
   %end;
%mend dump;

