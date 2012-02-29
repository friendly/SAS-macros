 /*-------------------------------------------------------------------*
  *    Name: gskip.sas                                                *
  *   Title: Device-independent macro for multiple plots              *
        Doc: http://www.datavis.ca/sasmac/gskip.html               
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created: 12 Jul 1996 16:43:00                                     *
  * Revised: 05 Apr 2006 08:57:12                                     *
  * Version: 1.2                                                      *
  *  - Removed DEVTYP=FOILS                                           *
  * 1.2  Inlined defined macro                                        *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The GSKIP macro is designed to provide device-independence in producing
 multiple plots in one SAS job, where some graphic formats require
 producing separately named files, while others (PS, PDF) can produce
 all graphs to a single output file.  For multifile devices, using
 e.g., the EPS, GIF, CGM and WMF drivers,
 it assigns a new output filename for the next plot.  Otherwise, it has
 no effect.

=Usage:

 The GSKIP macro has one optional positional parameter.  It relies on global
 macro parameters, DISPLAY, DEVTYP, FIG, GSASFILE, and GSASDIR.
 These parameters are normally initialized either in the F<AUTOEXEC.SAS>
 file, or in device-specific macros.  For example, for normal graphic
 output to the Graph Window, assign DISPLAY and DEVTYP as
 
   %let devtyp=SCREEN;
   %let displa=ON;

 For EPS file output,
 
   %let devtyp=EPS;
	%let fig=1;
	%let gsasfile=myfig;
 
 GSKIP is normally used after each graphic procedure or macro to advance
 the FIG counter and open a new graphic output file.  For example,
 
   proc gplot;
		plot y * x;
		run;
	%gskip();

 Normally, you should use a RUN statement at the end of each graphic step
 (before calling %gskip).

==Parameters:

* INC       The value by which the FIG counter is incremented, normally
            1 (the default).  Use the INC parameter after a plot with
			a BY statement.
 
==Global Parameters:

* DISPLAY	String value, ON or OFF, usually set by the GDISPLA macro.
            The GISKP macro takes no action if DISPLAY=OFF.

* DEVTYP    String value, the type of graphic device driver.  The values 
            EPS, GIF, CGM, JPG and WMF cause FIG= to be incremented
			and a new output filename assigned.   All others are ignored.

* FIG       A numeric value, the number of the current figure.

* GSASFILE  String value, the basename of the graphic output file(s).
            The output files are named according to the macro expression
				
               %scan(&gsasfile,1,.)&fig..%lowcase(&devtyp)

            e.g.,  myfile1.eps, myfile2.eps, ....
				
* GSASDIR   String value, the output directory in which the graphic
            files are written.  If not specified, output goes to the
			current directory.
 
 =*/

%global fig gsasfile gsasdir display devtyp;
%macro gskip(inc);
/*  quit; run; */
	%if &DISPLAY = OFF %then %goto done;    %*-- Only if we are displaying;
	%*-- List of device types requiring separate file outputs;
	%local filedevs thisdev;
	%*-- Avoid confusing PS with EPS;
	%let filedevs = %str( ) EPS GIF CGM WMF JPG PNG %str( );
	%let thisdev = %str( )%upcase(&devtyp)%str( );
	%*put GSKIP: filedevs = |&filedevs| thisdev=|&thisdev|;
	%if %index(&filedevs, &thisdev) > 0 %then %do;

	/*
	%if %upcase(&devtyp)=EPS or %upcase(&devtyp)=GIF
	 or %upcase(&devtyp)=CGM or %upcase(&devtyp)=WMF 
	 or %upcase(&devtyp)=JPG or %upcase(&devtyp)=PNG 
		%then %do;
	*/
	%if %length(&inc)=0 %then %let inc=1;
   %if %defined(gsasdir)=0 %then %let gsasdir=;
		%let fig = %eval(&fig + &inc);
		%let gsas = %scan(&gsasfile,1,.)&fig..%lowcase(&devtyp);
		%put GSKIP: gsasfile now: "&gsasdir.&gsas";
		filename gsas&fig  "&gsasdir.&gsas";
		goptions gsfname=gsas&fig;
	%end;
	
/* Skip the blank page in Zeta foils -- legacy code removed
   %if &devtyp=FOILS %then %do;
   Proc Gslide;
      note j=c 'Page skip for FOILS';
   run;
   %end;
*/

%done:;
%mend gskip;

%macro defined (mvar);
	%if %sysevalf(&sysver  >= 9) %then %do;
  		%symglobl(&mvar);
		%end;
	%else %do;

 		%local dsid rc scope;
		  /** Open the vmacro view which contains info about macor vars **/
		%let dsid=%sysfunc(open(sashelp.vmacro (where=(name="%upcase(&mvar)"))));
		  /** Fetch a record into the pdv if it exists **/
		%let rc=%sysfunc(fetch(&dsid));
		  /** Return varnum 1, the scope **/
		%let scope = %sysfunc(getvarc(&dsid,1));
		  /** Close the view **/
		%let rc=%sysfunc(close(&dsid));
		%if &scope = GLOBAL %then 1; %else 0;
  		%end;

%mend;

