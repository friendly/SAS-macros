 /*--------------------------------------------------------------*
  *    Name: odsgraphics.sas                                     *
  *   Title: Simplify output with ODS graphics                   *
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 03 May 2005 10:05:15                                *
  * Revised: 17 Feb 2012 09:29:58                                *
  * Version: 1.0-1                                               *
  *  - added code to set file=, better support for HTML          *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The ODSGRAPHICS macro designed to make it easier to produce SAS/Graph
 output using ODS GRAPHICS, particularly when SAS is run in batch or
 from a script, and when the SAS program may produce multiple graphs,
 which should be output to separate files.  
 
 The current implementation was designed to allow the basename of
 graphic output files (FILE=) to be specified either in the macro call or
 by a global input macro variable (&GSASFILE); similarly for the
 GPATH=.
 

=Usage:

 The ODSGRAPHICS macro is defined with one positional parameter and
 optional keyword parameters.  The arguments may be listed within
 parentheses in any order, separated by commas. For example:
 
	%odsgraphics(ON);
	proc reg plots;
		model y=x;
		run;
	%odsgraphics(OFF)
 
==Parameters:

* SWITCH       ON or OFF

* TYPE=       Type of ODS output: HTML or LATEX.  If not specified,
              the global macro variable &DEVTYP is consulted.

* STYLE=      Output style [Default: STYLE=JOURNAL]

* GPATH=      Graphics path (override GSASDIR)

* FILE=       Base name for graphics files (override GSASFILE)
                

 =*/

%global devtyp gsasdir gsasfile sasfile;  /* input globals */
%global odstype;                           /* output global */

%macro odsgraphics(
	switch,        /* ON or OFF */
	type=,         /* type of ODS output: HTML or LATEX */
	style=journal, /* output style */
	gpath=,        /* graphics path (override gsasdir)  */
	file=          /* base name for graphics files */
	);

%if %upcase(&switch)=ON %then %do;
*		ods trace on / listing;
	
	%if %length(&file)=0 %then %do;
		%let file=%scan(&gsasfile,1,.);
		%if %length(&file)=0 %then %do;
			%let file=%scan(&sasfile,1,.);
			%end;
		%end;

	%if %length(&type)=0 %then %do;
		%if %upcase(&devtyp)=PS %then %do;
		   %let type=latex;
		   %let ssheet=sas.sty;
		   %end;
		%else %if %upcase(&devtyp)=GIF %then %do;
		   %let type=html;
		   %let ssheet=sas.css;
		   %end;
		%end;
	%if %upcase(&type)=LATEX %then %do;
		%let ssheet=sas.css;
		%let ext=tex;
		%end;
	%else %do;
		%let ssheet=sas.css;
		%let ext=html;
		%end;
	
	%let odstype=&type;
	
	%if %length(&gpath)=0 %then %do;
		%let gpath=&gsasdir;
		%end;

	ods &type 
		style=&style
		gpath="&gpath" (url="&gpath")
		file="&file..&ext"
    	stylesheet="&ssheet"(url="sas");

    ods graphics on / reset  imagename="&file";
	%end;

%else %do;
	ods &odstype close;
*		ods trace off;
	%end;

%mend;

	
