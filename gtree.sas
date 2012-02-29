/*-------------------------------------------------------------------*
 *    Name: gtree.sas                                                *
 *   Title: Draw a tree dendrogram from PROC CLUSTER/VARLCUS         *
       Doc: http://www.datavis.ca/sasmac/gtree.html            
 *                                                                   *
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <friendly@yorku.ca>         *
 *  Source:  Based on Buckner & Lotz, SAS SUGI, 1988, 1363-1368      *
 * Created:  27 Sep 1989 13:32:13                                    *
 * Revised:  09 May 2006 13:15:18                                    *
 * Version:  1.5                                                     *
 * - Added %sysfunc functions to delete temp file                    *
 * - Added PRINT= and NAME= params; changed FONT=DUPLEX to FONT= to  *
 *   use default GOPTIONS font; added HLABEL= to control height of   *
 *   item labels                                                     *
 *                                                                   *
 *-------------------------------------------------------------------*/
/* GTREE macro allows item labels up to 16 characters in length.
   The first 8 characters of the item labels MUST be unique after
   removing blanks and '-', and must constitute a valid SAS name.
 
   Provisions have been made for:
    - plotting trees for similarities (reversing the scale)
    - labelling items in color given by variable in input data set
    - clipping the sim/dissimilarity scale
    - output data set containing node order in the tree

   Note: In SAS V7+, PROC TREE in SAS/STAT now does what the GTREE macro
	was designed for-- to give a high-resolution graphic version of a
	clustering solution, but the available options differ.

*/
%macro gtree(
       tree=_LAST_,  /* OUTTREE= data set from PROC CLUSTER or VARCLUS*/
       out=out,      /* name of output data set                       */
       height=,      /* Name of variable indicating height in tree    */
       metric=DIS,   /* SIMilarity or DISsimilarity                   */
       label=HEIGHT, /* label for similarity/dissimilarity axis       */
       font=,        /* font for item labels (default: goptions)      */
		 hlabel=,      /* height for text labels                        */
       orient=V,     /* orientation: H (horizontal) or V (vertical)   */
       ctree='BLACK',/* color for tree                                */
       citem='BLACK',/* color for item labels: quoted color or variabl*/
       trimlo=,      /* ignore values of height less than this        */
       trimhi=,      /* ignore height values greater than this        */
       sym=none,     /* plotting symbol for cluster joins             */
		 print=NO,     /* NO|YES=proc output|ALL=+ tree info            */
		 name=gtree
       );

%global ls;
%let ls=80;
%let metric=%upcase(&metric);
%let print=%upcase(&print);
%if %upcase(&tree)=_LAST_ %then %let tree = &syslast;
%let abort=0;

options nonotes;
%*-- Create temporary file for proc printto output;
%let fn=gtree;
%tempfile(&fn);

/*-----------------------------------------------------------*
 | Capture PROC TREE output. LIST option gives the height    |
 | of each node in the tree and the relative order of the    |
 | leaves in the dendrogram.                                 |
 *-----------------------------------------------------------*/
proc printto new print=&fn;

proc tree data=&tree &metric list
	%if &print=NO %then %do; noprint %end; ;
   %if %length(&height)>0 %then %do;
      height &height;
      %end;
	run;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

proc printto print=print;
 
%if &print=ALL %then %do;
data _null_;
   file print notitle noprint;
   infile &fn;
   input;
   put _infile_;
%end;
 
/*-------------------------------------------------------*
 | Read nodes from tree procedure LIST output to extract |
 | node height and calculate node order                  |
 *-------------------------------------------------------*/
data _step1_;
   infile &fn missover end=eof;
   retain maxl 0 icol 1;               * maximum label length;
   drop maxl icol;
   length _name_  $16;                 * print name of item;
	%if &sysscp = CMS 
		%then %do; 
			input name $ 2-9 @2 _name_ $& @;
			icol = 2;
		%end;
		%else %str(input name $ 1-8 @1 _name_ $& @;);
		
   if upcase(name) = 'TREE PRO' |
      upcase(name) = 'NAME OF ' |
              name = ''  then return;
 
   if name = 'CL1' | name = 'CLUS1' then do;
      input @icol name $& height;
      link trimit;
      node = 'ROOT';
      end;
   else do;
      input @icol name $& height parent $& child $;
      link trimit;
      if child ^= ''
         then node = 'BRAN';
         else node = 'LEAF';
      end;
   maxl = max(maxl,length(_name_));
   name = compress(name,' -');              * allow for multiword labels;
   parent=compress(parent,' -');
   call symput(name,put(height,best.));
   if node^='ROOT' then do;
      ph = input(symget(parent),best12.);
      parnum = input(substr(parent,verify(parent,'CLUS')),best12.);
      end;
   if node='LEAF' then order+1;
   output;
   if eof then do;
      maxl = min(maxl,16);
      call symput('LABLEN',trim(put(maxl,2.)));
      call symput('ITEMS', trim(put(order,3.)));
      put 'GTREE: Maximum label length: ' maxl;
      put 'GTREE: Number of items:      ' order;
      end;
      return;
trimit:
      %if %length(&trimlo) > 0 %then %do;
         height = max(height, &trimlo);
         %end;
      %if %length(&trimhi) > 0 %then %do;
         height = min(height, &trimhi);
         %end;
     return;
run;

%if &print=ALL %then %do;
proc print data=_step1_;
%end;
 
data _null_;
   set _step1_ end=eof;
   retain hroot hleaf;
   where node in ('ROOT','LEAF');
   if node='ROOT' then hroot= height;
                  else hleaf= height;
   if eof then do;
      *-- Find nicely scaled min/max and increment for height scale;
      *   (Necessary only to deal with similarity values properly);
      hinc= abs(hroot - hleaf)/5;
      pow = 10**floor( log10(hinc) );
      nice=1000;
      do inc = 1, 2, 2.5, 4, 5;
         ut = inc * pow;
         if abs(hinc-ut) < nice then do;
            nice = abs(hinc-ut);
            best = ut;
         end;
      end;
      hinc=best;
      min = min(hroot,hleaf);
      max = max(hroot,hleaf);
      min = hinc * floor(min/hinc);
      max = hinc * ceil (max/hinc);
      hinc = sign(hroot-hleaf)*best;
      if hleaf < hroot then do; hleaf=min; hroot=max; end;
                       else do; hroot=min; hleaf=max; end;
      call symput('HROOT', put(hroot,best.));
      call symput('HLEAF', put(hleaf,best.));
      call symput('HINC' , put(hinc ,best.));
      put 'GTREE: Height of leaf:       ' hleaf;
      put 'GTREE: Height of root:       ' hroot;
      put 'GTREE: Height increment:     ' hinc;
      end;
proc sort data=_step1_;
   by descending parnum;
 
data &out;
   set _step1_;
   by descending parnum;
   retain start finish;
   drop name start finish;
   if node='ROOT' |
      node='BRAN' then order = input(symget(name),best12.);
   if first.parnum then start = order;
   if last.parnum  then finish= order;
   mid = (finish + start)/2;
   if node^='ROOT' then call symput(parent,put(mid,best10.));
%if &print=ALL %then %do;
proc print;
%end;
 
%if &orient=V %then %do;
    goptions nocells hpos=57 vpos=75;
    %let tx = x;
    %let ty = y;
    %let pos= 4;
    %end;
%else %do;
    goptions nocells hpos=75 vpos=57;
    %let tx = y;
    %let ty = x;
    %let pos=4;
    %end;
 
/*---------------------------------------*
 | draw tree from node order and heights |
 *---------------------------------------*/
data _anno1_;
   set &out(keep=order height ph mid node);
   length color function $8 ;
   xsys = '2'; ysys = '2';
   color = &ctree;
 
   *-- Draw ascending lines;
   function = 'MOVE';  &tx = order;  &ty = height;  output;
   function = 'DRAW';                &ty = ph;      output;
   if node='ROOT' then do;
      &ty.sys='1';                   &ty = 100;     output;
      end;

data _anno2_;
   set &out(keep=order height ph parnum);
   by descending parnum;
   length color function $8 ;
   xsys = '2'; ysys = '2';
   color = &ctree;
   *-- Draw connecting lines;
   if first.parnum
      then function = 'MOVE';
      else function = 'DRAW';
   &tx = order;  &ty = ph; output;
 
*-- custom leaf labels ;
data _anno3_;
   length text $ %eval(&LABLEN+1);
   set &out(keep=order height _name_ node) ;
   length color function $8 ;
   drop _name_ height node offset;
   retain offset 0;
   xsys = '2'; ysys = '2';
   &tx = order;
   &ty = height;
 
   if node='LEAF';
   color = &citem;
	%if %length(&hlabel) %then %do;
		size=&hlabel;
		%end;
	%else %do;
         if &ITEMS le 25  then size = 1.5;
    else if &ITEMS le 50  then size = 1.0;
    else if &ITEMS le 100 then size = 1.0 - .25 * (&ITEMS/100);
    else                       size = .75 - .25 * (&ITEMS/150);
      if _n_=1 then put 'GTREE: Using Hlabel = ' size;
	 	%end;
	%if %length(&font) %then %do;
		style="&font";
	%end;
	position="&pos";
   %if &orient=V %then %do;
      angle=90; rotate=0;
      %end;
   function = 'LABEL';
   *-- Append a non-printing character to name to align right;
   text =trim(right(substr(_name_,1,&LABLEN )))||'00'x;
   if offset = 0 then do;
      offset = max(&lablen+1, size*(&lablen+1));
      put 'GTREE: Offset = ' offset;
      call symput('OFFSET',put(offset,best10.));
      end;
data anno;
   set _anno1_ _anno2_ _anno3_;
 
/*-------------------------------------------*
 | Draw the tree, horizontally or vertically |
 *-------------------------------------------*/
proc gplot data=&out anno=anno;
   %if &orient=V %then %do;       %* vertical tree ;
   plot  height * order / haxis=axis1 vaxis=axis2 vminor=1
		name="&name"
		des="GTREE of &tree";
   plot2 height * order / haxis=axis1 vaxis=axis2 vminor=1;
   %end;
   %else %do;                     %* horizontal tree;
   plot  order * height / haxis=axis2 vaxis=axis1 hminor=1
		name="&name"
		des="GTREE of &tree";
   %end;
   symbol v=&sym i=none  c=&ctree r=2;
   axis1  label=none value=none style=0     /* item axis        */
          major=none minor=none
          offset=(3 pct);
   axis2  label=(h=1.3 "&label")    /* tree height axis */
          value=(h=1.3)
          offset=(&offset,2)
          %if &metric = SIM %then %do;
          order=(&hleaf to &hroot by &hinc)
          %end;
          ;
run; quit;
 /*------------------------------------*
  | Clean up datasets no longer needed |
  *------------------------------------*/
proc datasets nofs nolist library=work memtype=(data);
    delete _step1_ _anno1_ _anno2_ _anno3_;
    run;
%tempdel(&fn);
goptions hpos= vpos=;
%done:
options notes;

%mend gtree;

%macro tempfile(fileref);
	%global tempfn;
	%if &sysscp = CMS
		%then %let tempfn=&fileref output a;
	%else %if &sysscp = WIN
		%then %let tempfn=c:\&fileref..out;
	%else  /* assume UNIX */
		%let tempfn=/tmp/&fileref..out;
	filename &fileref "&tempfn" lrecl=&ls;
%mend;

%macro tempdel(fileref);
	%global tempfn;
    *-- Avoid annoying flash with X commands;
    %if %sysevalf(&sysver  > 6.10) %then %do;
        %let rc=%sysfunc(fdelete(&fileref));
        %let rc=%sysfunc(filename(&fileref,''));
    %end;

    %else %do;
	%if &sysscp = CMS
		%then cms erase &tempfn;
	%else %if &sysscp = WIN
		%then %do;
			options noxsync noxwait; run;
			%sysexec(erase &tempfn);   run;
			options   xsync   xwait; run;
		%end;
	%else /* assume flavor of UNIX */
		 %sysexec(rm &tempfn);
    %end;
%mend;

