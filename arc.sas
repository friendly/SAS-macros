 *-------------------------------------------------------------;
 *  Draw a circular arc, given center, radius, angle, rotation ;
 *-------------------------------------------------------------;
%macro arc( cx, 		/* X-coordinate of center		   */
        	cy, 		/* Y-coordinate of center		   */
        	ang,		/* Starting angle (degrees) of arc */
        	rot,		/* Degrees of rotation of arc	   */
        	radius, 	/* Radius of arc (>0)			   */
        	color,  	/* Color of line				   */
        	pattern,	/* Line pattern (1..32) 		   */
        	width	);  /* Width of line  (1..5)		   */
   drop radians a ang rot;
   radians = (2 * 3.1415927) / 360;
   ang = &ANG * radians;
   rot = ang + &ROT * radians;
 
   color = "&color";
   line  = &pattern;
   size  = &width;
   do a = ang to rot by 0.05;
      x = &cx + (&radius * cos(a));
      y = &cy + (&radius * sin(a));
      if a=ang then function = 'move    ';
               else function = 'draw    ';
*      function = 'draw    ';
      output;
   end;
%mend;
 
