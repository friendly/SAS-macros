 /*----------------------------------------------------------------------*
  *    Name: graphout.sas                                                *
  *   Title: Output graphs from a catalog to separate file(s)            *
  *     Doc: http://www.yorku.ca/dept/psych/lab/sas/graphout.htm         *
  *----------------------------------------------------------------------*
  *  Author:  Andrew Smith, Computer Services, Univ. Reading             *
  * Modified: Michael Friendly                                           *
  * Revised: 17 Apr 2003 11:28:29                                        *
  * Version: 1.2                                                         *
  *  1.2 Default driver changed to cgmof97l                              *
  *                                                                      *
  *----------------------------------------------------------------------*/
 /*=
=Description:

 The graphout macro uses the GREPLAY procedure to replay any or all of the
 graphs previously generated in your SAS session, saving each one to an
 individual file, for use with word processing programs, etc.  (With SAS V7+,
 you can produce graphs for use in other applications more easily using
 ODS.)

=Usage:

 The graphout macro is called with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example:

 %graphout(device=jpeg, name=myfig, ext=jpg);

==Parameters:

* DEVICE=cgmof97L
     Graphics output device driver to be used (the default is MS Office 97, landscape).
     Some other possibilities:
     CGMWP61P- WordPerfect 6.1, portrait
     cgmwp61L- WordPerfect 6.1, landscape;
     cgmwpwa - WordPerfect, Windows;
     cgmhg3a - Harvard Graphics;
     cgmmw6c - Microsoft Word 6.0 (color)
     bmp     - BMP file format - 256 colors
     pslepsfc- Encapsulated postscript, color

* NAME=SAS
     Base name for output files. The files are named sequentially, prefixed with the
     value of the name parameter, e.g., sas1.cgm, sas2.cgm, ...
* EXT=
     File name extension for output files.  The appropriate extension is used
     by default if the DEVICE driver contains 'CGM', 'EPS', 'PS', 'GIF', 'WMF'
     'BMP' or 'JPEG'.  Otherwise, the default extension is 'GRF'.
* OUTDIR=
     Output directory for output files.  [Default: current directory]
* CATALOG=GSEG
     Catalog containing the graphs (WORK.GSEG is the SAS default)
* FIRST=1
     The sequence umber of first graph to plot in the graphic catalog to be plotted.
     >0 means absolute number of first graph,
     <1 means number of first graph relative to last
     (i.e. 0 means last graph only, -1 means first is one before last, etc.)
* LAST=0
     Number of the last graph to plot.
     >0 means absolute number of last graph,
     <1 means number of last graph relative to number of graphs in the catalog (i.e. 0 means
     last graph in the catalog, -1 means one before last, etc.)


 =*/

%macro graphout(
       device=cgmof97L,
       name=sas,
       ext=,
       catalog=gseg,
       outdir=,
       first=1,
       last=0);
 /*************************************************************************/
 /* Output files are named name1.ext ... name(npics).ext                  */
 /* Parameters:                                                           */
 /*   device=cgmmpwa  Graphics device to be used (WordPerfect, Windows)   */
 /*                   Some other possibilities:                           */
 /*                    cgmhg3a - Harvard Graphics                         */
 /*                    cgmmw6c - Microsoft Word 6.0                       */
 /*                    imggif  - GIF, 256 colors                          */
 /*                    imgjpeg - JPEG                                     */
 /*                    wmf     - Windows metafile                         */
 /*   name=sas        Base name for output files (serial number is added) */
 /*   ext=cgm         File name extension                                 */
 /*   catalog=gseg    Catalog containing the graphs (gseg is SAS default) */
 /*   first=1         Number of first graph to plot                       */
 /*                     >0 means absolute number of first graph,          */
 /*                     <1 means number of first graph relative to last   */
 /*                     (i.e. 0 means last graph only, -1 means first is  */
 /*                     one before last, etc.)                            */
 /*   last=0          Number of last graph to plot                        */
 /*                     >0 means absolute number of last graph,           */
 /*                     <1 means number of last graph relative to number  */
 /*                     of graphs in the catalog (i.e. 0 means last graph */
 /*                     in the catalog, -1 means one before last, etc.)   */
 /*                                                                       */
 /* Andrew Smith, Computer Services, University of Reading, 1994, 1995.   */
 /*************************************************************************/

    %let device=%upcase(&device);
    goptions device=&device gsfmode=replace gsflen=80;

  /* determine how many graphs are in the catalog */
  proc catalog catalog=&catalog;
    contents out=_cont_;
    run;
  %let npics=0;

  data _null_;
   set _cont_ end=last;
    if (last);
    call symput('npics',_n_);
    run;

  %if &npics=0 %then %do;
     %put WARNING: (graphout) There are no graphics in &catalog.;
     %goto done;
  %end;

  %if (&last<1) %then %do;
    %if (%eval(&npics+&last)>=1) %then %let last = %eval(&npics + &last);
    %else %do;
      %put NOTE (graphout): last=&last too low - setting last=1.;
      %let last = 1;
    %end;
  %end;
  %else %if (&last>&npics) %then %do;
    %put NOTE (graphout): last=&last too high - setting last=&npics.;
    %let last = &npics;
  %end;
  %if (&first>&last)
  %then %put NOTE (graphout): first=&first too high - no plots done.;
  %else %do;
    %if (&first<1) %then %do;
      %if (%eval(&last+&first)>=1) %then %let first = %eval(&last + &first);
      %else %do;
        %put NOTE (graphout): first=&first too low - setting first=1.;
        %let first = 1;
      %end;
    %end;

    %if %length(&outdir) > 0 %then %do;
        %let outdir = %trim(&outdir);
        %if &sysscp=WIN
            %then %let dirsep=\;
            %else %let dirsep=/;
        %if "%substr(&outdir, %length(&outdir), 1)" ^= "&dirsep"
            %then %let outdir = &outdir.&&dirsep;
        %end;

    %if %length(&ext) = 0 %then %do;
              %if %index(&device,EPS) > 0  %then %let ext=eps;
        %else %if %index(&device,PS) > 0   %then %let ext=ps;
        %else %if %index(&device,GIF) > 0  %then %let ext=gif;
        %else %if %index(&device,JPEG) > 0 %then %let ext=jpg;
        %else %if %index(&device,CGM) > 0  %then %let ext=cgm;
        %else %if %index(&device,WMF) > 0  %then %let ext=wmf;
        %else %if %index(&device,BMP) > 0  %then %let ext=bmp;
        %else %let ext=grf;
        %end:

    %put NOTE (graphout): Plotting &name&first..&ext to &name&last..&ext in &outdir;

    proc greplay igout=&catalog nofs;
    %let j=1;
    %do i = &first %to &last;
      filename gsf&j "&outdir.&name&j..&ext";
      goptions gsfname=gsf&i;
      replay &i;
      %let j=%eval(&j+1);
    %end;
    quit;

	*-- Return _cont_ data set with file info;
	data _cont_;
		set _cont_;
		if _n_ >= &first and _n_<= &last then output;
	data _cont_;
		set _cont_;
		length file $16;
		retain _j_ 0;
		drop _j_ libname memname;
		_j_+1;
		file = trim("&name")||trim(left(put(_j_,3.)))||".&ext";
  %end;

%done:
%mend;
