 /*--------------------------------------------------------------*
  *    Name: webods.sas                                          *
  *   Title: Execute a SAS file producing HTML output            *
  *     Doc: http://datavis.ca/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  10-04-1997                                         *
  * Revised:  27 Sep 2000 09:24:09                               *
  * Version: 1.2                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
 Description:

 Usage:

 =*/

%global sasfile device devtyp indir;
%macro webods(
    infile=,
    window=output,
    opts=nodate number,
    outdir= /* n:\sasuser\psy6140\examples\regress\output\, */
          c:\winnt\temp\webods\,
    outfile=,
	 toc=Y,
	 frame=Y,
	 source2=N,
    graphout=0,
    dev=gif);  /* or gif260, gif373, ... */

%let devtyp=ODSHTML;
%if %length(&indir)=0
	%then %let indir=n:\psy3030\examples\regress\;
	
%let abort=0;
%let window=%upcase(&window);



%* -- Make sure indir ends with / or \;
%if &sysscp = WIN %then %let sep=\;
    %else %let sep=/;

%if %length(&indir) > 0 %then %do;
   %if "%substr(&indir,%length(&indir),1)" ^= "&sep"
       %then %let indir=&indir.&sep;
%end;

%if %index(&infile,.)=0 %then %let infile=&infile..sas;
%let sasfile=%scan(&infile,1);

%* -- Does input file exist?;
%if %sysfunc(fileexist(&indir.&infile))=0 %then %do;
   %put ERROR: (webods) Input file &indir.&infile does not exist;
    %let abort=1; %goto done;
%end;

%* -- Make sure outdir ends with / or \;
%if %length(&outdir) > 0 %then %do;
   %if "%substr(&outdir,%length(&outdir),1)" ^= "&sep"
       %then %let outdir=&outdir.&sep;
	%let outdir=&outdir.&sasfile.&sep;
%end;

%if %sysfunc(fileexist(&outdir)) eq 0
	%then %do;
	options noxwait;
	%put NOTE: Creating &outdir;
	%sysexec(md &outdir);
	%put NOTE: SYSRC = &sysrc;
	%end;

%if %length(&outfile)=0
   %then %let outfile=&sasfile..htm;
%let outtoc = toc.htm;
%let outfrm = index.htm;


OPTIONS FORMCHAR='|----|+|---+=|-/\<>*';
title;
footnote;


ods listing close;

%let driver=&dev;
goptions dev=&dev transparency noborder;

%if %sysfunc(cexist(work.gseg)) %then %do;
  proc catalog cat=work.gseg et=grseg kill;
%end;

*-- Reset symbol statements, and restore default
    graphic options;

goptions reset=symbol;
%include goptions;

%let title=Output from &infile;
title "&title";

%setstyle(&title, &infile);

options ps=55 ls=80 center pageno=1 &opts;

%*--------------------------------------------------;
%*  Set webout filename for output                  ;
%*--------------------------------------------------;
%put NOTE: WebODS output to &outdir.&outfile;
/*
filename webout "&outdir.&outfile";
filename webtoc "&outdir.&outtoc";
filename webfrm "&outdir.&outfrm";
*/

%*---------------------------------------------------;
 * Begin capturing the output for conversion to HTML ;
%*---------------------------------------------------;
ods html 
	body="&outfile" (title="Output from &infile")
	%if &toc=Y %then contents = "&outtoc";
	%if &frame=Y %then frame = "&outfrm";
	metatext='name="Author" content="Michael Friendly"'
	style=XmpDefault
	path="&outdir" (url=none)
   gpath="&outdir" (url=none)
   ;


%*---------------------------------------------------;
 * Include and run the target infile                 ;
%*---------------------------------------------------;
%include "&indir.&infile" / source2;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;



/* Close the HTML destination, open LISTING again */
ods html close;
ods listing;

goptions reset=all;
/*
filename webout clear;
%if &toc=Y %then %do;    filename webtoc clear; %end;
%if &frame=Y %then %do;  filename webfrm clear; %end;
*/

%done:
%let devtyp=SCREEN;

%if &abort %then %put ERROR: The WEBODS macro ended abnormally.;
%mend;

%macro setstyle(title,infile);

%*let sasicon=http://www.yorku.ca/dept/psych/lab/images/sasicon.gif;
%let sasicon=http://www.math.yorku.ca/SCS/icons/saslstat.gif;

%*-- Modify template for smaller html, custom prehtml;
proc template;
   define style XmpDefault;
   parent = styles.default;  /* inherit from the default style */
	replace fonts /          /* make all fonts somewhat smaller */
		'TitleFont2' = ("Arial, Helvetica, Helv", 2, Bold Italic)
		'TitleFont' =  ("Arial, Helvetica, Helv", 3, Bold Italic)
		'StrongFont' = ("Arial, Helvetica, Helv", 2, Bold Italic)
		'EmpahsisFont' = ("Arial, Helvetica, Helv", 2, Italic)
		'FixedEmpahsisFont' = ("Courier", 1, Italic)
		'FixedStrongFont' = ("Courier", 1, Bold)
		'FixedHeadingFont' = ("Courier", 1)
		'BatchFixedFont' = ("SAS Monospace, Courier", 1)
		'FixedFont' = ("Courier", 1)
		'headingEmphasisFont' =  ("Arial, Helvetica, Helv", 2, Bold Italic)
		'headingFont' =  ("Arial, Helvetica, Helv", 2, Bold)
		'docFont' =  ("Arial, Helvetica, Helv", 1)
		;
	replace body from document /
		prehtml =
			"<table width=""100%"" bgcolor=""#A0FFFF"" cellspacing=0 cellpadding=0>
<tr align=center><td width=""15%"" align=center>
<a href=""http://www.sas.com"">
<img src=""&sasicon"" width=92 height=32></a></td>
<th align=center><h2><font face=""Arial"">&title</font></h2></th>
<td width=""15%"" align=center><a href=""../&infile"">
<font face=""Arial"">Source</font></a></td></tr></table>"
		background = cxFFFFFF;
   end;
run;
%mend;
