 /*--------------------------------------------------------------*
  *    Name: gask.sas                                            *
  *   Title: Retrieve graphics options to global macro variables *
        Doc: http://www.datavis.ca/sasmac/gask.html        
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 12 Nov 1996 10:12:23                                *
  * Revised: 24 Jan 2000 15:34:30                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The GASK macro queries the current graphics size options and assigns
 their values to global macro variables.   The macro retrieves the
 following graphic options: HPOS VPOS HSIZE VSIZE XMAX YMAX

=Usage:

 The GASK macro is defined with 1 keyword parameter.
 
	%gask();
	%gask(unit=cm);
 
==Parameters:

* UNIT=     Specifies the unit in which graphics size values are returned.
            [Default: GUNIT=IN]

 =*/

%macro gask(unit=in);

%let unit=%upcase(&unit);
%global hpos vpos hsize vsize xmax ymax;
   %*-- Determine vpos, hpos from DGSI;
data _null_;
   rc=ginit();
/*
   %let parms = vpos hpos vsize hsize;
   %let wd = 1;
   %let parm = %nrbquote(%scan(&parms, &wd));
   %do %until(&parm = %str());
      call gask("&parm", &parm, rc);

      put %quote(&parm) &&parm;
		call symput("&parm", compress(put(&parm, best6.)));
      %let wd = %eval( &wd + 1);
      %let parm = %nrbquote(%scan(&parms, &wd));
   %end;
*/
*	call gask('device', device, rc);
*	call symput('device', device);
   call gask('vpos',  vpos, rc);
   if rc=0 then call symput('vpos',  compress(put(vpos,3.)));
   call gask('hpos',  hpos, rc);
   if rc=0 then call symput('hpos',  compress(put(hpos,3.)));
   call gask('vsize', vsize, rc);
   if rc=0 then call symput('vsize', compress(put(vsize,best6.2)));
   call gask('hsize', hsize, rc);
   if rc=0 then call symput('hsize', compress(put(hsize,best6.2)));
   
	call gask('maxdisp',units,xmax,ymax,xpix,ypix,rc2);
   rc3 = gterm();

	*-- Convert to cm;
	xmax = xmax * 100;
	ymax = ymax * 100;

	%if &unit = IN %then %do;
		xmax = xmax / 2.54;
		ymax = ymax / 2.54;
		%end;
	call symput('xmax', compress(put(xmax,6.2)));
	call symput('ymax', compress(put(ymax,6.2)));
   run;
%*put NOTE: device=&device;
%put NOTE: hsize=&hsize vsize=&vsize hpos=&hpos vpos=&vpos xmax=&xmax ymax=&ymax unit=&unit;
%mend;

/*
*goptions device=pscolor;
*goptions pslepsfc;
%include goptions;
%gask();
*/
