%macro ell(
    cx,
    cy,
    sx,
    sy,
    r,              /* corrrelation */
    level=0.68,
    npoints=36,
    color=black,    /* Color of line                   */
    line=1,         /* Line pattern (1..32)            */
    width=1,        /* Width of line  (1..5)           */
    xsys=2,
    ysys=2,
    out=,
    in=
    );

%if %length(&out) %then %do;
data &out;
    %end;

    retain xsys "&xsys" ysys "&ysys";
	drop d t;   
   color = "&color";
   line  = &line;
   size  = &width;
   d = arcos(r);
   t = sqrt(cinv(&level, 2));
  
   do a = 0 to (2*3.1415926) by (2*3.1415926)/&npoints;
        x = t * &sx * cos(a + d/2) + &cx;
        y = t * &sy * cos(a - d/2) + &cy;
      if a=0 then function = 'move    ';
             else function = 'draw    ';
      output;
   end;
%mend;
    
