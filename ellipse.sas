 *-------------------------------------------------------------;
 *  Draw an ellipse, given center, axis lengths, and tilt      ;
 *-------------------------------------------------------------;
%macro ELLIPSE(
    CX,             /* X-coordinate of center          */
    CY,             /* Y-coordinate of center          */
    RX,             /* length of horizontal axis       */
    RY,             /* length of vertical axis         */
    TILT,           /* Slant of major axis             */
	steps=36,
    COLOR=black,    /* Color of line                   */
    line=1,         /* Line pattern (1..32)            */
    WIDTH=1,        /* Width of line  (1..5)           */
	xsys=2,
	ysys=2,
	out=,
	in=
	);

%if %length(&out) %then %do;
data &out;
	%end;

	drop ang rot a xp yp tilt;
   %let ANG=0;
   %let ROT=360;
*  radians = (2 * 3.1415927) / 360 = 2*arcos(-1)/360;
   tilt= &TILT * arcos(-1)/180;         /* convert deg. to radians */
   ang = &ANG  * arcos(-1)/180;
   rot = ang + &ROT * arcos(-1)/180;
   color = "&color";
   line  = &line;
   size  = &width;
 
   Do A = ang to rot by 2*arcos(-1)/&steps;
      id+1;
      x = &rx     * cos(a);
      y = &ry     * sin(a);
      xp= (x*cos(tilt)) - (y*sin(tilt));
      yp= (x*sin(tilt)) + (y*cos(tilt));
      x = xp+ &cx;
      y = yp+ &cy;
      if a=ang then function = 'move    ';
               else function = 'draw    ';
      output;
   end;
	retain xsys "&xsys" ysys "&ysys";	
%mend;
