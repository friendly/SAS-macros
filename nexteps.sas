%global driver fig gsasfile devtyp;

%macro nexteps;
	%let gsasfn =%SYSGET(GSASFILE);
	%let driver =%SYSGET(DRIVER);
	%if &gsasfn=%str() %then %let gsasfn=grfout.eps;
   %if &driver=%str() %then %let driver = PSLEPSFC;  *-- PSLEPSF for b/w;
	%let dev=&driver;
	%let devtyp = EPS;
	%let fig=1;
	%let gsasfile=&gsasfn;
	%put gsasfile is: "&gsasfile";
	filename gsasfile  "&gsasfile";

	goptions horigin=.5in vorigin=.5in;   *-- override, for BBfix;
	goptions device=&dev gaccess=gsasfile
       hpos=80   vpos=75                /* match pscolor device */
		 hsize=8in vsize=8.5in
		 gend='0A'x  gepilog='showpage' '0A'x   /* only for 6.07 */
		 gsflen=80 gsfmode=replace;
	*goptions colors=(black);
%mend;
