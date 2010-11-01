%macro arrow(
	x1, y1,            /* start point                                 */
	x2, y2,            /* end point, with arrow head                  */
	vechead=2.0 0.5,   /* head length and perpendicular offset        */
	color=,
	vtoh=1             /* plot aspect ratio in XSYS,YSYS coordinates  */
	);

%let singular = 1e-8;                            /* approximate zero */
*------set up vector heads------;
%let headr = %scan(%bquote(&vechead),1,%str( ));
%let headw = %scan(%bquote(&vechead),2,%str( ));
%if %length(%bquote(&headw)) = 0 %then %let headr = ;
%if %length(%bquote(&vechead)) = 0 or %length(%bquote(&headw))
   %then %let vechead = %str( );

	drop vecslope headx heady xonvec yonvec;
   comment  = 'vector ';
   %if %length(&color) %then 
    %str(color="&color");
	%line(&x1, &y1, &x2, &y2);

      %if %length(&headw) %then %do;
         comment  = 'vechead';
         *------compute slope of vector------;
         vecslope = &x2 - &x1;
         if abs(vecslope) > &singular then
            vecslope = (&y2 - &y1) / (vecslope / (&vtoh));
         else vecslope = .;
 
         *------find point on vector headr distance from end------;
         %*linept(xtick,&nlines - ytick,headx,heady,vecslope,-&headr);
         %linept(&x2,&y2,headx,heady,vecslope,-&headr);
 
         *------slope of line perpendicular to vector------;
         if vecslope = . then vecslope = 0;
         else if abs(vecslope) > &singular
            then vecslope = -1.0 / vecslope;
         else vecslope = .;
 
         *------draw vector head------;
         %linept(headx,heady,xonvec,yonvec,vecslope,-&headw);
			line=1;
         x = xonvec; y = yonvec; function = 'DRAW '; output;
         %linept(headx,heady,xonvec,yonvec,vecslope,&headw);
         %line(&x2,&y2,xonvec,yonvec);
         %end;
 
%mend;

*------draw a line------;
%macro line(x1,y1,x2,y2);
 
   x = &x1; y = &y1; function = 'MOVE '; output;
   x = &x2; y = &y2; function = 'DRAW '; output;
 
   %mend line;
 
*------find a point (px,py) r distance from (x,y) on a  ------;
*------line with slope m, negative r means toward origin------;
%macro linept(x,y,px,py,m,r);
 
	drop vecdir vecm2;
   vecdir = -sign(&r);
 
   if &m ne . then do;
      vecm2 = sqrt((&r) * (&r) / (1.0 + (&m) * (&m)));
      if (&x) > (&x1) then vecm2 = -vecm2;
      &px = vecdir * vecm2 + (&x);
      &py = vecdir * (&m) * vecm2 / (&vtoh) + (&y);
      end;
 
   else do;
      &px = (&x);
      &py = abs(&r);
      if (&y) > (&y1) then vecdir = -vecdir;
      &py = vecdir * (&py) / (&vtoh) + (&y);
      end;
 
   %mend linept;
 

