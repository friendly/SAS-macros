/* Demo multiple regression surfaces */
 
%include goptions ;
goptions vsize=7 in hsize=6 in  htext=1.3
         colors=(black, red, green, blue, brown,violet);
 
/*----------------------------------------------------------------*
 |  Macro to annotate a 3D plot with a grid representing a plane. |
 *----------------------------------------------------------------*/
%macro plane(model,
	out=_anno_,
	color=greenn,
	X1Lo=0,X1Hi=10,X1By=2,
	X2Lo=0,X2Hi=10,X2By=2);

data &out;
*  When = 'BEFORE';
   Color= "&color";
   XSYS='2'; YSYS='2'; ZSYS='2';
   Do X1 = &X1Lo to &X1Hi by &X1By;      /* Draw lines parallel to X2 */
      Do X2 = &X2Lo, &X2Hi;
         X  = X1;
         Y  = X2;
         Z  = &model;
         If X2=&X2Lo then FUNCTION='MOVE';
                     else FUNCTION='DRAW';
         output;
         End;
   End;
   Do X2 = &X2Lo to &X2Hi by &X2By;      /* Draw lines parallel to X1 */
      Do X1 = &X1Lo, &X1Hi;
         X  = X1;
         Y  = X2;
         Z  = &model;
         If X1=&X1Lo then FUNCTION='MOVE';
                     else FUNCTION='DRAW';
         output;
         End;
   End;
%mend plane;
 
%macro surface(model,
            X1Lo=0,X1Hi=10,X1By=1,
            X2Lo=0,X2Hi=10,X2By=1,
            ANNO=,
            GOPT=grid,
            Title=);
 
Data _mreg_;
   Do X1=&X1Lo to &X1Hi by &X1By;
   Do X2=&X2Lo to &X2Hi by &X1By;
      Y = &model ;
      output;
      end; end;
 
Proc G3D data=_mreg_;
     Plot X2 * X1 = Y /
	   CTOP=BLUE &anno &gopt;
     Title  H=1.8 "Multiple Regression Surface &title";
     Title2 H=1.5 font=duplex "Y = &model";
run;
*gskip;
%mend;

data anno1;
	retain xsys ysys zsys '2'; length function $8;
	x = 0; y = 0; z=-10;  function='MOVE'; output; 
	x = 0; y = 0; z=15;   function='DRAW'; output;
	x = 2; y = 6; z=10+x-2*y;  
			function = 'MOVE'; output;
 		z=z+9; line=3;
 			function = 'DRAW'; output;
		text='DOT'; size=2;
			function = 'SYMBOL'; output;

%surface(10 + X1 - 2*X2, x1by=2, x2by=2, anno=anno=anno1,
		gopt=grid xticknum=6 yticknum=6,
		title=(no error));
%plane(10+ X1 - 2*X2, X1By=5, X2By=5);

*surface(10 + X1 - 2*X2 + normal(0),anno=ANNOTATE=_anno_,
     title=(with error));
%surface(10 + X1 - 2*X2 - .2*X1*X2 + .1*x2**2, anno=ANNOTATE=_anno_,
     title=(with X1*X2 interaction));
/* 
Data _mreg_;
   Do X1=1 to 10;
      X2=10 - X1;
      Y = 10 + X1 - 2*X2;
      output;
      end;
 
Proc G3D data=_mreg_;
     Scatter X2 * X1 = Y / ZMIN=-10 COLOR='BLUE'
                           ANNOTATE=_anno_ grid;
     Title  H=1.7 "Multiple Regression Surface (X1, X2 dependent)";
     Title2 H=1.3 font=triplex "Y = 10 + X1 - 2*X2";
 
run;
*/
