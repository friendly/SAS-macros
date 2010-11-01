 /*-------------------------------------------------------------------*
  *    Name: gdispla.sas                                              *
  *   Title: Device-independent DISPLAY/NODISPLAY control             *
        Doc: http://www.datavis.ca/sasmac/gdispla.html             
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created: 14 Feb 1991 11:19                                        *
  * Revised: 17 Apr 2003 11:43:49                                     *
  * Version: 1.0                                                      *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The GDISPLA macro is used to switch graphics display off or on in
 a device-independent way.  It allows for the fact that for direct
 output to the display device, the required GOPTIONS are NODISPLAY
 or DISPLAY, whereas for output to a GSF, GIF, or EPS file, the options
 are GSFMODE=NONE or GSFMODE=APPEND.  
 
 It is typically used with the PANELS macro or the SCATMAT macro, or 
 other programs which produce multiple plots and then join those plots 
 in a template using PROC GREPLAY.

=Usage:

 The GDISPLA macro is called with positional parameters.  The first
 (SWITCH) parameter must be specified.
 
	%let devtype=SCREEN;
	%gdispla(OFF);
	proc gplot;
		plot y * x;
		by group;
	%gdispla(ON);
	%panels(rows=1, cols=3);
 
==Parameters:

* SWITCH        A string value, OFF or ON.

* IML           Specify any non-blank value to use the GDISPLA macro
                within SAS/IML.

==Global parameters:

 The macro uses one global macro parameter, DEVTYP, to determine the
 appropriate action. This parameter is normally initialized either in 
 the AUTOEXEC.SAS file, or in device-specific macros

* DEVTYP    String value, the type of graphic device driver.  The
            values EPS, GIF, CGM and WMF cause the macro to use
				the GSMODE option; the value DEVTYP=SCREEN causes the
				macro to use the DISPLAY or NODISPLAY option.
				All other values are ignored.

 =*/

%macro gdispla(
          switch,
          iml
			 );

%global DISPLAY DEVTYP;

   %let switch=%upcase(&switch);
   %let devtyp=%upcase(&devtyp);
   %if &switch=ON %then %do;
		%let DISPLAY=ON;
       %if &devtyp=SCREEN /*and &sysver=5.18 */
           %then %let cmd= %str(goptions display;);
           %else
			  %if &devtyp=EPS or &devtyp=GIF
	         or &devtyp=CGM or &devtyp=WMF 
					%then %let cmd= %str(goptions display gsfmode=replace;);
					%else %let cmd= %str(goptions display gsfmode=append;);
       %end;
   %else %if &switch=OFF %then %do;
		%let DISPLAY=OFF;
       %if &devtyp=SCREEN 
           %then %let cmd= %str(goptions nodisplay;);
           %else %let cmd= %str(goptions nodisplay gsfmode=none;);
       %end;
   %else %put ERROR in GDISPLA: first parameter (SWITCH) must be ON or OFF;

	%if &iml = %str() %then %do;
		run;
		&cmd;
		%*put GDISPLA: &cmd;
		%end;
	%else %do;		/* Called from IML */
		start command(cmd);
			call execute(cmd);
		finish;
		run command("&cmd");
		%end;
	
%mend gdispla;
