
/* Include file to control global options for all SAS/GRAPH programs */

/* general graphics options, can be overridden by specific device macros */
%macro goptions(device);
goptions ftext=swiss;
goptions vsize=8in;
goptions htext=1.5 htitle=2;
goptions hpos=70 vpos=65;                /* match pscolor device */
%global gstarted;
%let gstarted=AUTO;

%gstart(&device);
%gdispla(ON);
%global GOUT;
%let GOUT=gout=WORK.GSEG;
*libname gfont0 '~/sasuser/gfont';
%mend;

