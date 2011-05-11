 /*--------------------------------------------------------------*
  *    Name: webout.sas                                          *
  *   Title: Execute a SAS file producing HTML output            *
  *     Doc: http://datavis.ca/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  10-04-1997                                         *
  * Revised:  10-09-1997 08:43                                   *
  * Version: 1.1                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
 Description:

 Usage:

 =*/

%global sasfile device;
%macro webout(
   indir= /* d:\sasuser\teaching\psy3030\hints\, */
          n:\psy6140\examples\regress\,
    infile=resline3.sas,
    window=output,
    opts=nodate number,
    outmeth=,
    ftpdir=~friendly/www/lab/,
    outdir=g:\sasuser\psy6140\examples\regress\output\,
    outfile=,
    proploc=,
    graphout=0,
    dev=gif373);

%let abort=0;
%let window=%upcase(&window);

%if %index(&infile,.)=0 %then %let infile=&infile..sas;


%* -- Make sure indir ends with / or \;
%if &sysscp = WIN %then %let sep=\;
    %else %let sep=/;

%if %length(&indir) > 0 %then %do;
   %if "%substr(&indir,%length(&indir),1)" ^= "&sep"
       %then %let indir=&indir.&sep;
%end;

%* -- Does input file exist?;
%if %sysfunc(fileexist(&indir.&infile))=0 %then %do;
   %put ERROR: (webout) Input file &indir.&infile does not exist;
    %let abort=1; %goto done;
%end;

%* -- Make sure outdir ends with / or \;
%if %length(&outdir) > 0 %then %do;
/*   %if %index(&outdir,:\/)=0 %then %let outdir = &indir.&outdir;   */
   %if "%substr(&outdir,%length(&outdir),1)" ^= "&sep"
       %then %let outdir=&outdir.&sep;
%end;

%let sasfile=%scan(&infile,1);
%if %length(&outfile)=0
   %then %let outfile=&sasfile..htm;

%local linesize pagesize center first thiswin;
%let linesize=%sysfunc(getoption(linesize));
%let pagesize=%sysfunc(getoption(pagesize));
%let center  =%sysfunc(getoption(center));
%*put ls=&linesize ps=&pagesize center=&center;

OPTIONS FORMCHAR='|----|+|---+=|-/\<>*';
%* dm "&window clear";
title;
FOOTNOTE;

%*--------------------------------------------------;
%*  Set webout filename for output (FTP or local)   ;
%*--------------------------------------------------;
%if &outmeth=FTP %then %do;
%put NOTE: Webout output via FTP to &outdir.&outfile;
filename webout FTP "&outdir.&outfile"
   host='euclid.psych.yorku.ca' user='friendly' pass='friendly'
   rcmd='ascii';
%end;
%else %do;
%put NOTE: Webout output to &outdir.&outfile;
filename webout "&outdir.&outfile" ;
%end;

filename lstout "&outdir.&sasfile..lst";

%if &graphout>0 %then %do;
   %let driver=&dev;
   %gif;
   %if %sysfunc(cexist(work.gseg)) %then %do;
      proc catalog cat=work.gseg et=grseg kill;
   %end;
   goptions reset=symbol;
%end;

%*---------------------------------------------------;
 * Begin capturing the output for header             ;
%*---------------------------------------------------;

%*let sasicon=http://www.yorku.ca/dept/psych/lab/images/sasicon.gif;
%let sasicon=http://www.math.yorku.ca/SCS/icons/saslstat.gif;
%let title=Output from &infile;

%out2htm(capture=on);
options nonumber nodate ls=132;
data _null_;
   file print;
   saspower = '<a href="http://www.sas.com">' ||
              "<img src=""&sasicon"" width=92 height=32></a>";
   put "<table width=""100%"" bgcolor=""#A0FFFF"" cellspacing=0 cellpadding=0>" /
       " <tr align=center>" /
       "  <td width=""15%"" align=center>" / saspower "</td>"
       "  <th align=center><h2><font face=""Arial"">&title</font></h2></th>" /
       "  <td width=""15%"" align=center><a href=""../&infile""><font face=""Arial"">Source</font></a>";
   %if &graphout > 0 %then %do;
       %if &graphout>1 %then %let gr=Graphs;
           %else %let gr=Graph;
   put "    <br><a href=""#graphs""><font face=""Arial"">&graphout &gr</font></a>";
   %end;
   %else %do;
   put "    <br><font face=""Arial"">0 Graphs</font>";
   %end;
   put "    </td></tr>" / "</table>";

%out2htm(htmlfref=webout,
   brtitle=&title, capture=off, encode=n, dtag=NO FORMATTING,
    window=output,  openmode=replace);

%*---------------------------------------------------;
 * Begin capturing the output for conversion to HTML ;
%*---------------------------------------------------;
options ps=55 ls=78 center pageno=1 &opts;
dm "autopop=off" ;

%if %index(&window,LOG)>0
   %then %out2htm(capture=on,window=LOG,runmode=b);

%if %index(&window,OUTPUT)>0
   %then %out2htm(capture=on,window=OUTPUT,runmode=b);

%*---------------------------------------------------;
 * Include and run the target infile                 ;
%*---------------------------------------------------;
%include "&indir.&infile" / source2;
dm "file lstout ATTR REPACE";
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;


%*---------------------------------------------------;
 *  Stop capturing and format the SAS output as HTML ;
%*---------------------------------------------------;

%if %index(&window,OUTPUT)>0 %then %do;
    %out2htm(htmlfref=webout,
   brtitle=&title, capture=off,
         window=OUTPUT,
         openmode=append, runmode=b,
    %if %length(&proploc)>0 %then %do;
         proploc=&proploc,
         %end;
         tcolor=red, hcolor=blue, fcolor=green, tface=Arial);
    %end;

%if %index(&window,LOG)>0 %then %do;
    %out2htm(htmlfref=webout,
   brtitle=&title, capture=off,
         window=LOG,
         openmode=append, runmode=b,
    %if %length(&proploc)>0 %then %do;
         proploc=&proploc,
         %end;
         ecolor=red, ncolor=blue, wcolor=green, tface=Arial);
    %end;


%if &graphout>0 %then %do;
/*   %if &outmeth ^= FTP %then %do;
  options noxwait; %sysexec("cd &outdir"); %end; */
  goptions horigin=0in vorigin=0in hpos=70 vpos=50 lfactor=1;
   %let first=%eval(&graphout-1);
   %graphout(
       device=&dev,
       name=&sasfile,
       ext=gif,
       outdir=&outdir,
       first=-&first);

%*---------------------------------------------------;
 * Begin capturing the output for graphics           ;
%*---------------------------------------------------;
   %out2htm(capture=on);
   title;
   data _null_;
      set _cont_ end=last;
      file print;
      if _n_=1 then
         put "<h3><font color=red><a name=""graphs"">"
             "Graphic output from &infile</a></font></h3>" /
             "<table border=0>";
      put '<tr><td width="25%"><b>' desc '</b><br>'
               '<font size=-1>[' name date ']</font></td>'
              '<td><img src="' +0 file +0 '"></tr>';
      if last then
         put '</table>';

    run;
    %out2htm(htmlfref=webout, capture=off, encode=n, dtag=NO FORMATTING,
       window=output,  openmode=append);

%end;
options ls=&linesize ps=&pagesize &center;

%done:
%if &abort %then %put ERROR: The WEBOUT macro ended abnormally.;
%mend;

%macro header(title);

%mend;
