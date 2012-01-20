/*-------------------------------------------------------------------*
 *    Name: faces.sas                                                *
 *   Title: Draw asymmetric faces to represent multivariate data     *
 *     Doc: http://datavis.ca/sasmac/faces.html           *
 *                                                                   *
 * Usage:  See comments in macro parameter lists                     *
 *                                                                   *
 * Notes:  The program generates a very large data set to draw the   *
 *    faces, approximately 800 annotate observations for each face.  *
 *    Disk usage depends on the number of faces plotted per page     *
 *    the product of the parameters                                  *
 *        blks * rows * cols                                         *
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <friendly@yorku.ca>         *
 * Created:  23 Jul 1991 11:38:23                                    *
 * Revised:  28 Sep 2011 15:04:32                                    *
 * Version:  1.6-0                                                   *
 * Requires: gskip
 * - Added %gskip if multiple pages                                  *
 * - Added ecolor= to color eyes
 * - added std= to standardize data
 * - added min= and max= to enforce range
 * Original source:  ASYMFACE.SAS by M. Schupbach (1989)             *
 *-------------------------------------------------------------------*/
%macro FACES(
   data=_last_,    /* Name of input data set               */
   out=asym,       /* Name of output anno set              */
   id=,            /* Character ID variable                */
   idnum=,         /* Numeric ID variable                  */
   std=,           /* how to standardize variables         */
   min=.,
   max=.,
   blks=1,         /* Blocks per page                      */
   rows=4,         /* Rows per block                       */
   cols=4,         /* Columns per block                    */
   res=3,          /* resolution: 1=high/3=low             */
   frame=Y,        /* frame around each face?              */
   color='BLACK',  /* color of each face: variable         */
   hcolor='BLACK', /* hair color: name or string in quotes */
   ecolor='BLACK', /* eye color                            */
   row=,           /* use to assign particular             */
                   /* locations to faces                   */
   col=,           /* column variable                      */
   blk=,           /* block variable                       */

   /* Variables can be assigned to features either by      */
   /* listing 18 variable names for LEFT and RIGHT or      */
   /* by assigning individually to L and R parameters.     */
   /* Variable names can appear more than once.            */
   /* Use . in LEFT= or RIGHT= to skip a parameter (leave  */
   /* unassigned).                                         */
   left=,
   right=,
   /* Names of variables assigned to features              */
   r1=, r2=, r3=, r4=, r5=, r6=, r7=, r8=, r9=,
   r10=,r11=,r12=,r13=,r14=,r15=,r16=,r17=,r18=,
   l1=, l2=, l3=, l4=, l5=, l6=, l7=, l8=, l9=,
   l10=,l11=,l12=,l13=,l14=,l15=,l16=,l17=,l18=,
   gout=GSEG,      /* name of graphics catalog             */
   name=FACES      /* name for graphic catalog entry       */
   );
 
  *-- if left= or right= is specified, assign to corresponding
      L or R parameters if not specified individually;
  %local i wd;
  %if %length(&left)>0 %then
     %do i=1 %to 18;
         %let wd = %scan(&left,&i,%str( ));
         %if %length(&&l&i) = 0 and &wd^=.
            %then %let l&i = &wd;
     %end;
  %if %length(&right)>0 %then
     %do i=1 %to 18;
         %let wd = %scan(&right,&i,%str( ));
         %if %length(&&r&i) = 0 and &wd^=.
            %then %let r&i = &wd;
     %end;
 
  *-- Check whether all parameters have been assigned --;
  %do i=1 %to 18;
    %if %length(&&r&i) = 0 or &&r&i=. %then
       %do;
         %put FACES: Parameter R&i has not assigned. Using .5;
         %let r&i = .5;
       %end;
    %if %length(&&l&i) = 0 %then
       %do;
         %put FACES: Parameter L&i has not assigned. Using .5;
         %let l&i = .5;
       %end;
    %end;

%if %length(&std) %then %do;

	proc stdize data=&data out=_scaled_ method=&std;
	run;
	%if %length(&id.&idnum) %then %do;
		data _scaled_;
			merge &data(keep=&id &idnum) _scaled_ ;
			run;
		%end;

	*scale(data=&data, out=_scaled_, id=&id, copy=&idnum);
	%let data=_scaled_;
	
%end;

 
  *-- Determine number of observations and number of pages --;
data _null_;
   point=1;
   set &data point=point nobs=nobs;
   pgs =1 + int( nobs / (&blks * &rows * &cols) - .001);
   call symput('NOBS',put(nobs,3.));
   call symput('PGS' ,put(pgs, 2.));
   file print;
   put @30 'Asymmetric Faces'///;
   put @5  "Data set:                &data " /
       @5  'Number of observations:   ' nobs   //;
   put @28 'Variable assignments'//
    @5 'Parameter      Facial Feature               Left' @60 'Right'/
    @5 '---------   ----------------------------    ---- Variable ----'//
    @5 " 1 (EYSI)   Eye size                        &l1 " @60 "&r1"  /
    @5 " 2 (PUSI)   Pupil size                      &l2 " @60 "&r2"  /
    @5 " 3 (POPU)   Position of pupil               &l3 " @60 "&r3"  /
    @5 " 4 (EYSL)   Eye slant                       &l4 " @60 "&r4"  /
    @5 " 5 (HPEY)   Horizontal position of eye      &l5 " @60 "&r5"  /
    @5 " 6 (VPEY)   Vertical position of eye        &l6 " @60 "&r6"  /
    @5 " 7 (CUEB)   Curvature of eyebrow            &l7 " @60 "&r7"  /
    @5 " 8 (DEEB)   Density of eyebrow              &l8 " @60 "&r8"  /
    @5 " 9 (HPEB)   Horizontal position of eyebrow  &l9 " @60 "&r9"  /
    @5 "10 (VPEB)   Vertical position of eyebrow    &l10" @60 "&r10" /
    @5 "11 (UPHA)   Upper hair line                 &l11" @60 "&r11" /
    @5 "12 (LOHA)   Lower hair line                 &l12" @60 "&r12" /
    @5 "13 (FALI)   Face line                       &l13" @60 "&r13" /
    @5 "14 (DAHA)   Darkness of hair                &l14" @60 "&r14" /
    @5 "15 (HSSL)   Hair shading slant              &l15" @60 "&r15" /
    @5 "16 (NOSE)   Nose line                       &l16" @60 "&r16" /
    @5 "17 (SIMO)   Size of mouth                   &l17" @60 "&r17" /
    @5 "18 (CUMO)   Curvature of mouth              &l18" @60 "&r18" ///
    @5 "            Face color                      &color" @60 "&color"/
    @5 "            Hair color                      &hcolor" @60 "&hcolor";
   stop;
run;
 
%do pg = 1 %to &pgs;
   %let firstob = %eval( (&pg-1) * &blks * &rows * &cols + 1);
   %let lastob  = %eval( (&pg) * &blks * &rows * &cols);
   %if &lastob > &nobs %then %let lastob = &nobs;
   %let obs = %eval(&lastob - &firstob +1);
   %put FACES: Page &pg - observations &firstob to &lastob ( &obs );
 
 
  *-- Construct annotate data set for this page --;
data &out;
  set &data(firstobs=&firstob obs=&lastob) end=eof;
  keep function xsys ysys x y position color text row col size;
  length xsys ysys position $1 function color $8 text $16;
  retain xsys '5' ysys '5' miss 0;
   %*********************************************;
   %*  blk = number of block for face 0..&blks-1*;
   %*  row = number of row of face in block     *;
   %*  col = number of column of face in block  *;
   %*  no  = number of observation              *;
   %*********************************************;
    no = _n_;
    %if %length(&row)=0 and %length(&col)=0 %then
        %do;
          blk = int((no-1)/(&rows*&cols) + .001);
          row = int((no-1 - blk*(&rows*&cols))/&cols + .001);
          col = int(no-1 - blk*(&rows*&cols) - row*&cols);
        %end;
    %else %do;
         blk = 0;
         row = &row -1;
         col = &col -1;
        %end;
    blk = mod(blk,&blks);
    fnum= no + &firstob-1;
    %if %length(&color) = 0 %then %let COLOR = 'BLACK';
    %if %length(&hcolor)= 0 %then %let HCOLOR = 'BLACK';
    %if %length(&idnum) = 0 %then %let idnum = fnum;
    %FACE(&r1 ,&r2 ,&r3 ,&r4 ,&r5 ,&r6 ,&r7 ,&r8 ,&r9 ,&r10,&r11,&r12,
          &r13,&r14,&r15,&r16,&r17,&r18,&l1 ,&l2 ,&l3 ,&l4 ,&l5 ,&l6 ,
          &l7 ,&l8 ,&l9 ,&l10,&l11,&l12,&l13,&l14,&l15,&l16,&l17,&l18,
          blk,row,col,&idnum, &id,&blks,&rows,&cols,&res,&frame,&color,
          &hcolor,&ecolor, &min, &max);
/*  if fnum = &lastob then */
       if miss>0 then
       put 'FACES: ' miss 'variables contained missing values';

%*-- Setting HSIZE and VSIZE should be done outside of the FACES macro;
%*-- The comments below describe how this is done for one particular
     device;

%*******************************************************************;
%*  24 faces yield 2 blocks with 16 faces, each block is 88 units  *;
%*  large, blocks are separated by 2 units. Thus two blocks need   *;
%*  88+2+88=178 units. HSIZE is calculated by the formula          *;
%*                                                                 *;
%*         HSIZE = (blk*88+(blk-1)*2)/(4*2.57)      (inches)       *;
%*                                                                 *;
%*  in our case this yields 178/(4*2.57)=17.315                    *;
%*  VSIZE is set constant to VSIZE=12.288                          *;
%*  This will yield blocks of approximately 20.5x27.1 centimeters  *;
%*******************************************************************;


   proc gslide anno=&out name="&name" gout=&gout;
   run;
   %if &pgs>1 %then %gskip;
%end;
%mend FACES;
 
*************************************************************;
*                      A S Y M F A C E                      *;
*                     =================                     *;
*                VERSION FEBRUARY 24TH, 1989                *;
*                                                           *;
*                 Adapted by M. Schuepbach                  *;
*           Department of Mathematical Statistics           *;
*                    University of Berne                    *;
*       Sidlerstrasse 5, CH - 3012 Berne, Switzerland       *;
*               E-MAIL: U116@CBEBDA3T.BITNET                *;
*                                                           *;
*  meaning of parameters R1-R18 and L1-L18                  *;
*  ----------------------------------------------           *;
*    R = right face side  L = left face side                *;
*                                                           *;
*  parameter no.   name   meaning                           *;
*  ---------------------------------------------------------*;
*       1          EYSI   eye size                          *;
*       2          PUSI   pupil size                        *;
*       3          POPU   position of pupil                 *;
*       4          EYSL   eye slant                         *;
*       5          HPEY   horizontal position of eye        *;
*       6          VPEY   vertical position of eye          *;
*       7          CUEB   curvature of eyebrow              *;
*       8          DEEB   density of eyebrow                *;
*       9          HPEB   horizontal position of eyebrow    *;
*      10          VPEB   vertical position of eyebrow      *;
*      11          UPHA   upper hair line                   *;
*      12          LOHA   lower hair line                   *;
*      13          FALI   face line                         *;
*      14          DAHA   darkness of hair                  *;
*      15          HSSL   hair shading slant                *;
*      16          NOSE   nose line                         *;
*      17          SIMO   size of mouth                     *;
*      18          CUMO   curvature of mouth                *;
*                                                           *;
*************************************************************;

%macro face(RIG1,RIG2,RIG3,RIG4,RIG5,RIG6,RIG7,RIG8,RIG9,RIG10,
           RIG11,RIG12,RIG13,RIG14,RIG15,RIG16,RIG17,RIG18,
           LEF1,LEF2,LEF3,LEF4,LEF5,LEF6,LEF7,LEF8,LEF9,LEF10,
           LEF11,LEF12,LEF13,LEF14,LEF15,LEF16,LEF17,LEF18,
           blk,row,col,NO,ID,blks,rows,cols,res,frame,color,hcolor,ecolor,
		   min, max);

**************************************************************;
*     framing and labeling the faces                         *;
**************************************************************;
   color='BLACK';
   %if %substr(&frame,1,1) = Y %then %do;
   function='MOVE';X=-11;Y=-15; link OUTPUT;
   function='DRAW';X=-11;Y=15;  link OUTPUT;
                   X= 11;Y=15;  link OUTPUT;
                   X= 11;Y=-15; link OUTPUT;
                   X=-11;Y=-15; link OUTPUT;
   %end;
   size = 1 + 1 / (&cols);
   %if &NO ^= %str() %then %do;
   function='LABEL';X=10.5;Y=14.5;
     position='D'; TEXT=&NO; link OUTPUT;
   %end;
   %if &ID ^= %str() %then %do;
   function='LABEL';X=10.5;Y=-14.5;
     position='A'; TEXT=&ID; link OUTPUT;
   %end;
   size = .;
    array p{18} p1-p18;
    ARRAY XUP{121}  XXUP1-XXUP121;
    ARRAY XLOW{121} XXLO1-XXLO121;
    ARRAY YUP{121}  YYUP1-YYUP121; 
    ARRAY YLOW{121} YYLO1-YYLO121;
    ARRAY XFACE{97} XXFA1-XXFA97;
    ARRAY YFACE{97} YYFA1-YYFA97;
    ARRAY XXU{121}  XXXU1-XXXU121;
    ARRAY XXL{121}  XXXL1-XXXL121;
    ARRAY YYU{121}  YYYU1-YYYU121;
    ARRAY YYL{121}  YYYL1-YYYL121;
    ARRAY XT1{3}    XXT1-XXT3;
    ARRAY XT2{3}    XXTT1-XXTT3;
    ARRAY YT1{3}    YYT1-YYT3;
    ARRAY YT2{3}    YYTT1-YYTT3;
    XUMAX1= 8.1147; XUMAX2=  2.7487; XUMAX3=-7.3495;
    XUMAX4= 4.2360; XUMAX5=  2.8299; XUMAX6=-3.5240;
    YUMAX1= 6.7029; YUMAX2=-10.3740; YUMAX3=-3.6243;
    YUMAX4= 5.8058; YUMAX5=  0.5964; YUMAX6=-1.5585;
    XUMIN1= 5.5221; XUMIN2=  3.7880; XUMIN3=-3.0211;
    XUMIN4=-0.2974; XUMIN5=  0.9965; XUMIN6= 0.0660;
    YUMIN1= 6.1704; YUMIN2= -5.6920; YUMIN3=-0.5460;
    YUMIN4= 0.9206; YUMIN5= -0.6389; YUMIN6=-0.2504;
    XLMIN1= 2.3096; XLMIN2=  2.7696; XLMIN3=-0.2053;
    XLMIN4=-0.2040; XLMIN5=  0.3026; XLMIN6=-0.1693;
    YLMIN1= 8.1185; YLMIN2=  0.3246; YLMIN3=-1.5201;
    YLMIN4= 0.3933; YLMIN5=  0.1948; YLMIN6=-0.4255;
    XLMAX1= 3.5608; XLMAX2=  4.0885; XLMAX3= 0.2812;
    XLMAX4=-0.5919; XLMAX5= -0.3595; XLMAX6= 0.0412;
    YLMAX1= 3.9792; YLMAX2= -1.9186; YLMAX3=-0.8270;
    YLMAX4=-0.9849; YLMAX5=  0.1044; YLMAX6=-0.3504;
    XFMAX1= 6.3767; XFMAX2= -2.1462; XFMAX3=-4.1037;
    XFMAX4=-2.9179; XFMAX5=  1.2404; XFMAX6= 1.5972;
    YFMAX1=-6.5371; YFMAX2= -8.7286; YFMAX3= 1.2045;
    YFMAX4= 7.5676; YFMAX5=  0.3221; YFMAX6=-3.8549;
    XFMIN1= 4.6951; XFMIN2= -2.6606; XFMIN3= 0.1939;
    XFMIN4=-1.3368; XFMIN5= -1.4519; XFMIN6= 0.5025;
    YFMIN1=-4.7097; YFMIN2= -5.4093; YFMIN3=-2.2439;
    YFMIN4= 0.2125; YFMIN5=  1.9345; YFMIN6= 0.2350;

	dmin = &min;
	dmax = &max;	
**************************************************************;
*        begin drawing the RIGHT - face - side               *;
**************************************************************;
    dir = 1;       /* X-direction for this half of face */
    %do i = 1 %to 18;
        p{&i} = &&RIG&i;      /* assign RIGht to array */
        %end;
    link chk_miss;

	*link chk_range;
 
   color = &color;  link do_face;
   color = &color;  link do_nose;  link do_mouth; link do_brow;  
   color = &hcolor; link do_hair;
   color = &ecolor; link do_eye;
 
*************************************************************;
*           begin drawing the LEFT - face - side            *;
*************************************************************;
    dir =-1;       /* X-direction for this half of face */
    %do i = 1 %to 18;
        p{&i} = &&LEF&i;      /* assign LEFt to array */
        %end;
    link chk_miss;
 
   color = &color;  link do_face;
   color = &color;  link do_nose;  link do_mouth; link do_brow;  
   color = &hcolor; link do_hair;
   color = &ecolor; link do_eye;
   return;   /* to next observation */
***********************************************************;
*            end of computing one face                    *;
***********************************************************;
 
output:
  **************************************************************;
  * Rescale the x and y values to range between 0 and 100%     *;
  * The x values must be scaled by a factor of                 *;
  *           100/(&blks*88+(&blks-1)*2).                      *;
  * With &blks=2, the scaling factor is 100/178.               *;
  * The y-scaling factor is  100/(30*rows).                    *;
  **************************************************************;
 *   X = (X + 11 + &blk * 90 + &col * 22) * 100/178;
     X = (X + 11 + &blk * ((22*&cols)+2) + &col * 22)
         * 100/(&blks*&cols*22 + (&blks-1)*2);
     Y = (Y + 15 + (30*&rows) - (&row+1) * 30) * 100/(30*&rows);
     output;
  return;
 
chk_miss:
    do i = 1 to 18;          /* check for missing data */
       if p{i} = . then do;
          miss+1;
          p{i} = .5;
          end;
       end;
    return;


chk_range:                   * restrict range to (&min, &max);
	do i = 1 to 18;
		if dmin ^= . then p{i} = max( p{i}, dmin);
		if dmax ^= . then p{i} = min( p{i}, dmax);
		end;
	return;
 
  ***************************************************************;
  * Routines for drawing parts of the face using RIG or LEF parms;
  ***************************************************************;
 
do_face:
  ***************************************************************;
  * drawing the upper (p{11}) and lower (p{12}) hair lines and  *;
  * face line (p{13}) using hair shading (p{14}) and hair slant *;
  * (p{15}).                                                    *;
  ***************************************************************;
    XUP{1}=0.0;
    YUP{1}=9.9;
    XLOW{1}=0.0;
    YLOW{1}=6.5;
    XFACE{1}=7.0;
    YFACE{1}=0.0;
   *--  face line (p{13}) --*;
    do J4=2 to 96;
        H=-1.+(J4-1)/48.;
        smin=%poly5(h,xfmin1,xfmin2,xfmin3,xfmin4,xfmin5,xfmin6);
        smax=%poly5(h,xfmax1,xfmax2,xfmax3,xfmax4,xfmax5,xfmax6);
      XFACE{J4}=SMIN+ p{13}*(SMAX-SMIN);
        smin=%poly5(h,yfmin1,yfmin2,yfmin3,yfmin4,yfmin5,yfmin6);
        smax=%poly5(h,yfmax1,yfmax2,yfmax3,yfmax4,yfmax5,yfmax6);
      YFACE{J4}=SMIN+ p{13}*(SMAX-SMIN);
    end;
    XFACE{97}=0.0;
    YFACE{97}=-10.0;
   *--  upper (p{11}) and lower (p{12}) hair line  --*;
    do J1=2 to 44;
        H1=-1.+(J1-1)/60.;
        H2=-1.+(J1-1)/51.;
        H3=-1.+(J1-1)/22.;
        smin=%poly5(h2,xumin1,xumin2,xumin3,xumin4,xumin5,xumin6);
        smax=%poly5(h1,xumax1,xumax2,xumax3,xumax4,xumax5,xumax6);
      XUP{J1}=SMIN+ p{11}*(SMAX-SMIN);
        smin=%poly5(h2,yumin1,yumin2,yumin3,yumin4,yumin5,yumin6);
        smax=%poly5(h1,yumax1,yumax2,yumax3,yumax4,yumax5,yumax6);
      YUP{J1}=SMIN+ p{11}*(SMAX-SMIN);
        smin=%poly5(h3,xlmin1,xlmin2,xlmin3,xlmin4,xlmin5,xlmin6);
        smax=%poly5(h2,xlmax1,xlmax2,xlmax3,xlmax4,xlmax5,xlmax6);
      XLOW{J1}=SMIN+ p{12}*(SMAX-SMIN);
        smin=%poly5(h3,ylmin1,ylmin2,ylmin3,ylmin4,ylmin5,ylmin6);
        smax=%poly5(h2,ylmax1,ylmax2,ylmax3,ylmax4,ylmax5,ylmax6);
      YLOW{J1}=SMIN+ p{12}*(SMAX-SMIN);
    end;
    do J2=45 to 101;
        H1=-1.+(J2-1)/60.;
        H2=-1.+(J2-1)/51.;
        smin=%poly5(h2,xumin1,xumin2,xumin3,xumin4,xumin5,xumin6);
        smax=%poly5(h1,xumax1,xumax2,xumax3,xumax4,xumax5,xumax6);
      XUP{J2}=SMIN+ p{11}*(SMAX-SMIN);
        smax=%poly5(h2,xlmax1,xlmax2,xlmax3,xlmax4,xlmax5,xlmax6);
      XLOW{J2}=SMIN+ p{12}*(SMAX-SMIN);
        smin=%poly5(h2,yumin1,yumin2,yumin3,yumin4,yumin5,yumin6);
        smax=%poly5(h1,yumax1,yumax2,yumax3,yumax4,yumax5,yumax6);
      YUP{J2}=SMIN+ p{11}*(SMAX-SMIN);
        smax=%poly5(h2,ylmax1,ylmax2,ylmax3,ylmax4,ylmax5,ylmax6);
      YLOW{J2}=SMIN+ p{12}*(SMAX-SMIN);
    end;
    do J3=102 to 120;
        H1=-1.+(J3-1)/60.;
        XLOW{J3}=XFACE{J3-101};
        YLOW{J3}=YFACE{J3-101};
        smax=%poly5(h1,xumax1,xumax2,xumax3,xumax4,xumax5,xumax6);
      XUP{J3}=XLOW{J3}+ p{11}*(SMAX-XLOW{J3});
        smax=%poly5(h1,yumax1,yumax2,yumax3,yumax4,yumax5,yumax6);
      YUP{J3}=YLOW{J3}+ p{11}*(SMAX-YLOW{J3});
    end;
    XLOW{121}=XFACE{20};
    YLOW{121}=YFACE{20};
    XUP{121}=XFACE{20};
    YUP{121}=YFACE{20};
    function='MOVE'; X=dir*XUP{1};Y=YUP{1};   link OUTPUT;
    do I=1 to 121 by &res;
      function='DRAW'; X=dir*XUP{I};Y=YUP{I};  link OUTPUT;
    end;
    function='MOVE'; X=dir*XLOW{1};Y=YLOW{1};  link OUTPUT;
    do I=1 to 121 by &res;
      function='DRAW'; X=dir*XLOW{I} ;Y=YLOW{I};  link OUTPUT;
    end;
    function='MOVE'; X=dir*XFACE{1};Y=YFACE{1};  link OUTPUT;
    do I=1 to 97 by &res;
      function='DRAW'; X=dir*XFACE{I} ;Y=YFACE{I};  link OUTPUT;
    end;
    return;
 
do_hair:
   ********************************************************;
   *  shading the area between upper and lower hair line  *;
   *  using darkness (p{14}) and slant (p{15}) of hair.   *;
   ********************************************************;
    DD=3.0*(1.0-( p{14}*.9));
    if DD<0.1 then DD=0.1;
    ANGLE=45.- p{15}*90.;
    T1=ANGLE*3.14149/180.;
    CO=COS(T1); SI=SIN(T1);
    XMIN=10000; XMAX=-XMIN;
    do I=2 to 121;
      %ROT(XUP{I-1},YUP{I-1},CO,SI,XXU{I});
      if XXU{I}>XMAX then XMAX=XXU{I};
      if XXU{I}<XMIN then XMIN=XXU{I};
      %ROT(XUP{I-1},YUP{I-1},-SI,CO,YYU{I});
      %ROT(XLOW{I-1},YLOW{I-1},CO,SI,XXL{I});
      if XXL{I}>XMAX then XMAX=XXL{I};
      if XXL{I}<XMIN then XMIN=XXL{I};
      %ROT(XLOW{I-1},YLOW{I-1},-SI,CO,YYL{I});
    end;
    XXL{1}=XXL{2}; YYL{1}=YYL{2};
    XXU{1}=XXL{2}; YYU{1}=YYL{2};
    if (XXU{2}<XXL{2} AND XUP{2}>0.0) OR
      (XXU{2}>XXL{2} AND XUP{2}<0.0) then do;
      XXL{1}=XXU{2}; YYL{1}=YYU{2};
      XXU{1}=XXU{2}; YYU{1}=YYU{2};
    end;
    if XUP{2}>0.0 then X0=DD+XMIN;
    else X0=XMAX-DD;
    DONE=0;
    do UNTIL(DONE=1);
      NL=0; NU=0;
      do I=1 to 120;
        if (XXU{I}<X0 AND XXU{I+1}>X0) OR
          (XXU{I}>X0 AND XXU{I+1}<X0) then do;
          NU=NU+1;
          XT1{NU}=X0;
          YT1{NU}=((YYU{I}-YYU{I+1})*X0
                    -(YYU{I}*XXU{I+1}-YYU{I+1}*XXU{I}))
                      / (XXU{I}-XXU{I+1});
        end;
        if (XXL{I}<X0 AND XXL{I+1}>X0) OR
          (XXL{I}>X0 AND XXL{I+1}<X0) then do;
          NL=NL+1;
          XT2{NL}=X0;
          YT2{NL}=((YYL{I}-YYL{I+1})*X0
                    -(YYL{I}*XXL{I+1}-YYL{I+1}*XXL{I}))
                      / (XXL{I}-XXL{I+1});
        end;
      end;
      if NL=0 AND NU=2 then do;
        %ROT(XT1{1},YT1{1},CO,-SI,XX);
        %ROT(XT1{1},YT1{1},SI,CO,YY);
        function='MOVE'; X=dir*XX;Y=YY;    link OUTPUT;
        %ROT(XT1{2},YT1{2},CO,-SI,XX);
        %ROT(XT1{2},YT1{2},SI,CO,YY);
        function='DRAW'; X=dir*XX ;Y=YY;   link OUTPUT;
      end;
      else if NL=2 AND NU=0 then do;
        %ROT(XT2{1},YT2{1},CO,-SI,XX);
        %ROT(XT2{1},YT2{1},SI,CO,YY);
        function='MOVE'; X=dir*XX;Y=YY;    link OUTPUT;
        %ROT(XT2{2},YT2{2},CO,-SI,XX);
        %ROT(XT2{2},YT2{2},SI,CO,YY);
        function='DRAW'; X=dir*XX ;Y=YY;   link OUTPUT;
      end;
      else if (NL GE 1 AND NU=1) OR (NL=1 AND NU GE 1) then do;
        %ROT(XT1{1},YT1{1},CO,-SI,XX);
        %ROT(XT1{1},YT1{1},SI,CO,YY);
        function='MOVE'; X=dir*XX;Y=YY;    link OUTPUT;
        %ROT(XT2{1},YT2{1},CO,-SI,XX);
        %ROT(XT2{1},YT2{1},SI,CO,YY);
        function='DRAW'; X=dir*XX ;Y=YY;   link OUTPUT;
      end;
      else if NL=2 AND NU=2 then do;
        if (YT1{1}<YT1{2} AND YT2{1}<YT2{2}) OR
           (YT1{1}>YT1{2} AND YT2{1}>YT2{2}) then do;
          %ROT(XT1{1},YT1{1},CO,-SI,XX);
          %ROT(XT1{1},YT1{1},SI,CO,YY);
          function='MOVE'; X=dir*XX;Y=YY;   link OUTPUT;
          %ROT(XT2{1},YT2{1},CO,-SI,XX);
          %ROT(XT2{1},YT2{1},SI,CO,YY);
          function='DRAW'; X=dir*XX ;Y=YY;  link OUTPUT;
          %ROT(XT1{2},YT1{2},CO,-SI,XX);
          %ROT(XT1{2},YT1{2},SI,CO,YY);
          function='MOVE'; X=dir*XX;Y=YY;   link OUTPUT;
          %ROT(XT2{2},YT2{2},CO,-SI,XX);
          %ROT(XT2{2},YT2{2},SI,CO,YY);
          function='DRAW'; X=dir*XX ;Y=YY;  link OUTPUT;
        end;
        else do;
          %ROT(XT1{1},YT1{1},CO,-SI,XX);
          %ROT(XT1{1},YT1{1},SI,CO,YY);
          function='MOVE'; X=dir*XX;Y=YY;   link OUTPUT;
          %ROT(XT2{2},YT2{2},CO,-SI,XX);
          %ROT(XT2{2},YT2{2},SI,CO,YY);
          function='DRAW'; X=dir*XX ;Y=YY;  link OUTPUT;
          %ROT(XT1{2},YT1{2},CO,-SI,XX);
          %ROT(XT1{2},YT1{2},SI,CO,YY);
          function='MOVE'; X=dir*XX;Y=YY;   link OUTPUT;
          %ROT(XT2{1},YT2{1},CO,-SI,XX);
          %ROT(XT2{1},YT2{1},SI,CO,YY);
          function='DRAW'; X=dir*XX ;Y=YY;  link OUTPUT;
        end;
      end;
      if XUP{2}>0.0 then do;
        X0=X0+DD;
        if X0 GE XMAX then DONE=1;
      end;
      else do;
        X0=X0-DD;
        if X0 LE XMIN then DONE=1;
      end;
    end;
    return;
 
do_nose:
  *******************************************************;
  *      begin computing the nose-line using p{16}      *;
  *******************************************************;
    TI=2.282;
    X1T=%poly5(TI,1.2245,-0.4339,0.1431,-0.0135,-0.1396,0.0537);
    XX=0.3+( p{16}*(X1T-0.3));
    function='MOVE'; X=dir*XX ;Y=0.0;   link OUTPUT;
    do I=2 to 81 by &res;
      YY=-((I-1)/20);
      TI=YY+2.282;
      X1T=%poly5(TI,1.2245,-0.4339,0.1431,-0.0135,-0.1396,0.0537);
      XX=0.3+( p{16}*(X1T-0.3));
      function='DRAW'; X=dir*XX ;Y=YY;  link OUTPUT;
    end;
    return;
 
do_mouth:
  ************************************************************;
  *  compute the size (p{17}) and curvature (p{18}) of mouth *;
  ************************************************************;
    YY1=-6.0; D=( p{18} - .5)/3.;
    XX=0.0;
    YY=YY1;
    function='MOVE'; X=dir*XX ;Y=YY;
      link OUTPUT;
    do I=2 to 30 by &res;
      XX=(I-1)/10 ;
      TI=XX-1.5  ;
      X0T=%poly5(TI,-6.1531,-0.1583,-0.0347,-0.0418,-0.0038,0.0174);
      X1T=%poly5(TI,-5.7326,-0.3889,-0.1487,0.0233,-0.0366,0.0347);
      YY1=X0T+( p{17}*(X1T-X0T));
      A=D*XX*XX ;
      YY=YY1+A;
      function='DRAW'; X=dir*XX ;Y=YY;  link OUTPUT;
    end;
    XX=3; YY1=-6.5;
    YY=YY1+9*D;
    function='DRAW'; X=dir*XX ;Y=YY;  link OUTPUT;
    YY1=-7.0;
    XX=0.0;
    YY=YY1;
    function='MOVE'; X=dir*XX ;Y=YY;  link OUTPUT;
    do I=2 to 30 by &res;
      XX=(I-1)/10;
      TI=XX-1.5  ;
      X0T=%poly5(TI,-6.6522,0.1503,-0.0513,0.0402,0.0040,-0.0148);
      X1T=%poly5(TI,-6.9965,0.0482,0.0609,0.0191,0.0201,0.0144);
      YY1=X0T+( p{17}*(X1T-X0T));
      A=D*XX*XX ;
      YY=YY1+A;
      function='DRAW'; X=dir*XX ;Y=YY;  link OUTPUT;
    end;
    XX=3; YY1=-6.5;
    YY=YY1+9*D;
    function='DRAW'; X=dir*XX ;Y=YY;   link OUTPUT;
    return;
 
do_brow:
  ****************************************************************;
  *  compute the curvature (p{7}) and density (p{8}) of eyebrows *;
  *  and horizontal (p{9}) and vertical (p{10}) position of eye  *;
  ****************************************************************;
    I=1;
    XB=2.0+ p{9}+  p{9};
    YB=1.0+ p{10}+  p{10};
    DENS= p{8}/2.; H=2.; XL=XB-H; DMIN=-1.; DMAX=.5;
    D=DMIN +  p{7}* 1.5; C=D/(H*H); XX=XL;
    YY=YB+C*(XX-XB)*(XX-XB)-DENS;
    function='MOVE'; X=dir*XX ;Y=YY;    link OUTPUT;
    do I=2 to 25;
      XX=XL+(I-1)*H/12.;
      YY=YB+C*(XX-XB)*(XX-XB)+((-1)**I)*DENS;
      function='DRAW'; X=dir*XX ;Y=YY;  link OUTPUT;
    end;
    return;
 
do_eye:
  ****************************************************************;
  * size (p{1}), horizontal (p{5}) and vertical (p{6}) position, *;
  * slant (p{4}) of eye and size (p{2}) and position (p{3}) of   *;
  * pupil                                                        *;
  ****************************************************************;
    H=1.5; DMIN=.2; DMAX=1.2; RPMIN=.2; RPMAX=1.2; PSHIFT=1.;
   *--  horizontal position of eye (p{5})  --*;
    XM=2.0 + 2* p{5} ;
    YM=2* p{6} - 1.;
    XL=XM - H; XR=XM + H; PIHALF=1.570796327;
   *--  eye slant (p{4})  --*;
    PSI=( p{4} - .5)*PIHALF;
    COSPSI=COS(PSI); SINPSI=SIN(PSI);
   *****************************************************;
   *  size of eye (p{1}), restriction if out of range  *;
   *****************************************************;
    D=DMIN+ p{1} ;
    if D GT H then D=H;
    if D LE .05 then GOTO EYE0R;
   *--  parameters for circle to compute eye line  --*;
    Y0=YM-(H+D)*(H-D)/(2*D); RAD2=(YM-Y0+D)**2;
   *--  upper eye line  --*;
    XX=XL; YY=Y0+SQRT(MAX(0.,RAD2-(XX-XM)**2.));
    %ROTATE(XX,YY,COSPSI,SINPSI,XM,YM);
    function='MOVE';   X=dir*XX; Y=YY;    link OUTPUT;
    do I=2 to 26;
      XX=XL+(I-1)*H/25.; YY=Y0+SQRT(MAX(0.,RAD2-(XX-XM)**2.));
      %ROTATE(XX,YY,COSPSI,SINPSI,XM,YM);
      function='DRAW';   X=dir*XX; Y=YY;  link OUTPUT;
    end;
    do I=27 to 51; II=52-I;
      X1=XL+(II-1)*H/25.; XX=2*XM-X1;
      YY=Y0+SQRT(MAX(0.,RAD2-(X1-XM)**2.));
      %ROTATE(XX,YY,COSPSI,SINPSI,XM,YM);
      function='DRAW';   X=dir*XX; Y=YY;  link OUTPUT;
    end;
   *--  lower eye line  --*;
    XX=2*XM-XL; YY=2*YM-Y0-SQRT(MAX(0.,RAD2-(XL-XM)**2.));
    %ROTATE(XX,YY,COSPSI,SINPSI,XM,YM);
    function='MOVE';   X=dir*XX; Y=YY;    link OUTPUT;
    do I=2 to 26;
      X1=XL+(I-1)*H/25.; XX=2*XM-X1;
      Y1=Y0+SQRT(MAX(0.,RAD2-(X1-XM)**2.)); YY=2*YM-Y1;
      %ROTATE(XX,YY,COSPSI,SINPSI,XM,YM);
      function='DRAW';   X=dir*XX; Y=YY;  link OUTPUT;
    end;
    do I=27 to 51; II=52-I;
      XX=XL+(II-1)*H/25.;
      Y1=Y0+SQRT(MAX(0.,RAD2-(XX-XM)**2.)); YY=2*YM-Y1;
      %ROTATE(XX,YY,COSPSI,SINPSI,XM,YM);
      function='DRAW';   X=dir*XX; Y=YY;  link OUTPUT;
    end;
   *--  size of pupil (p{2}) --*;
    RPUP=RPMIN+ p{2} ;
    if RPUP LT 0. then RPUP=0. ;
   *--  position of pupil (p{3})  --*;
    XPUP=XM+( p{3} - .5)*PSHIFT;
   *--  upper pupil line  --*;
    XX=XPUP-RPUP;
    if XX LT XL then XX=XL;
    if XX GT XR then XX=XR;
    YY=YM; HP=Y0+SQRT(MAX(0.,RAD2-(XX-XM)**2.)); YY=MIN(YY,HP);
    %ROTATE(XX,YY,COSPSI,SINPSI,XM,YM);
    function='MOVE';   X=dir*XX; Y=YY;   link OUTPUT;
    do I=2 to 51;
      FI=PIHALF*(I-1)/25.; XX=XPUP-RPUP*COS(FI);
      if XX LT XL then XX=XL;
      if XX GT XR then XX=XR;
      YY=YM+RPUP*SIN(FI); HP=Y0+SQRT(MAX(0.,RAD2-(XX-XM)**2.));
      YY=MIN(YY,HP);
      %ROTATE(XX,YY,COSPSI,SINPSI,XM,YM);
      function='DRAW';   X=dir*XX; Y=YY;  link OUTPUT;
    end;
   *--  lower pupil line  --*;
    FI=PIHALF*2.; XX=XPUP-RPUP*COS(FI);
    if XX LT XL then XX=XL;
    if XX GT XR then XX=XR;
    YY=YM+RPUP*SIN(FI); HP=Y0+SQRT(MAX(0.,RAD2-(XX-XM)**2.));
    YY=MIN(YY,HP); YY=2*YM-YY;
    %ROTATE(XX,YY,COSPSI,SINPSI,XM,YM);
    function='MOVE';   X=dir*XX; Y=YY;    link OUTPUT;
    do I=2 to 51; II=52-I;
      FI=PIHALF*(II-1)/25.; XX=XPUP-RPUP*COS(FI);
      if XX LT XL then XX=XL;
      if XX GT XR then XX=XR;
      YY=YM+RPUP*SIN(FI); HP=Y0+SQRT(MAX(0.,RAD2-(XX-XM)**2.));
      YY=MIN(YY,HP); YY=2*YM-YY;
      %ROTATE(XX,YY,COSPSI,SINPSI,XM,YM);
      function='DRAW';   X=dir*XX; Y=YY;  link OUTPUT;
    end;
    GOTO EYE1R;
  EYE0R:
   *************************************;
   *  construction if eye size is 0.0  *;
   *************************************;
    XX=XL; YY=YM;
    %ROTATE(XX,YY,COSPSI,SINPSI,XM,YM);
    function='MOVE';   X=dir*XX; Y=YY;    link OUTPUT;
    XX=XR; YY=YM;
    %ROTATE(XX,YY,COSPSI,SINPSI,XM,YM);
    function='DRAW';   X=dir*XX; Y=YY;    link OUTPUT;
  EYE1R:
    return;
**************************************************************;
* END OF COMPUTING ASYMMETRICAL FACES                        *;
**************************************************************;
%MEND FACE;
 
%* Evaluate a 5-th degree polynomial by Horners method;
%macro poly5(x,c0,c1,c2,c3,c4,c5);
   (((( &c5*&x + &c4)*&x + &c3)*&x +&c2)*&x + &c1)*&x + &c0
%mend;
 
%macro rotate(rdx,rdy,rc,rs,rzx,rzy);
   rdx1=&rdx-&rzx; rdy1=&rdy-&rzy;
   &rdx=&rc*rdx1-&rs*rdy1+&rzx;
   &rdy=&rs*rdx1+&rc*rdy1+&rzy;
%mend rotate;
 
%macro rot(x,y,al,be,rrot);
   &rrot=&x*&al+&y*&be;
%mend rot;
