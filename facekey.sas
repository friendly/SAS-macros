/*-------------------------------------------------------------------*
 *    Name: FACEKEY.SAS                                              *
 *   Title: Create a variable-feature key for faces display          *
 *          with PROC GSLIDE                                         *
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <friendly@yorku.ca>         *
 * Created:  12 Oct 1991 12:41:23                                    *
 * Revised:  13 Sep 2007 12:44:18                                    *
 * Version:  1.0-1                                                   *
 *  - Begin to handle variable assignments in same way as faces      *
 *                                                                   *
 *-------------------------------------------------------------------*/
%macro facekey(
     data=_last_,  /* data set containing variables for faces  */
     title1=,      /* main title string for slide              */
     title2=,      /* additional titles, if desired            */
     title3=,
     footnote=,    /* footnote string, if desired              */
     left=,        /* list of 18 variables - left side of face */
     right=,       /* list of 18 variables - right side of face*/
   /* Names of variables assigned to features              */
   r1=, r2=, r3=, r4=, r5=, r6=, r7=, r8=, r9=,
   r10=,r11=,r12=,r13=,r14=,r15=,r16=,r17=,r18=,
   l1=, l2=, l3=, l4=, l5=, l6=, l7=, l8=, l9=,
   l10=,l11=,l12=,l13=,l14=,l15=,l16=,l17=,l18=,
     label=Y,      /* Use variable labels? N-> use names       */
     cols=1,       /* Columns in display - not implemented     */
     hpos=80,
     vpos=25,
	 font=,
	 htext=1.5,
     name=facekey, /* name for graphic catalog entry           */
     gcat=gseg     /* name of graphic catalog                  */
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
 
   *-- Collect parameter variables in a data set;
data _parm_;
   length parm $3 /*name $8 */;
   keep num parm name;
   %do i= 1 %to 18;
       num = &i;
       parm = "L&i";
       name = scan("&left" ,&i,' ');
       output;
       %end;
   %do i= 1 %to 18;
       num = &i;
       parm = "R&i";
       name = scan("&right" ,&i,' ');
       output;
       %end;
*proc print;
proc sort;
   by name;
 
proc contents data=&data mtype=data out=_info_ noprint;
data _info_;
   set _info_;
   keep name length label type;
   if label=' ' then label=name;
*proc print;
proc sort;
   by name;
data _parm_;
   merge _parm_(in=wanted)
         _info_(drop=type length);
   by name;
   if name='.' then name=' ';
   if wanted;
proc sort data=_parm_;
   by num parm;
*-- Join the left and right variable info into one record;
data _parm_;
   set _parm_ end=eof;
   length nam left right $40;
   retain left right ll lr;
   retain ml mr 0;
   keep num pname left right;
   by num;
   %if &label=Y
       %then %do; nam = label;   %end;
       %else %do; nam = name ;   %end;
   if first.num then do;
      left = nam; ll=length(nam); ml=max(ll,ml);
      end;
   if last.num then do;
      right= nam; lr=length(nam); mr=max(lr,mr);
      link pname;
      output;
      end;
   if eof then do;
      call symput('MAXL',put(ml,2.));
      call symput('MAXR',put(mr,2.));
      end;
   return;
pname:         /* names of face parameters */
   select( num );
      when( 1 ) pname='Eye size          ';
      when( 2 ) pname='Pupil size        ';
      when( 3 ) pname='Pupil position    ';
      when( 4 ) pname='Eye slant         ';
      when( 5 ) pname='Eye X position    ';
      when( 6 ) pname='Eye Y position    ';
      when( 7 ) pname='Eyebrow curvature ';
      when( 8 ) pname='Density of eyebrow';
      when( 9 ) pname='Eyebrow X position';
      when(10 ) pname='Eyebrow Y position';
      when(11 ) pname='Upper hair line   ';
      when(12 ) pname='Lower hair line   ';
      when(13 ) pname='Face line         ';
      when(14 ) pname='Hair darkness     ';
      when(15 ) pname='Hair shading slant';
      when(16 ) pname='Nose line         ';
      when(17 ) pname='Mouth size        ';
      when(18 ) pname='Mouth curvature   ';
   end;
   return;
proc print;
   id num;
data facekey;
   set _parm_;
   length text $25;
   drop c1-c3 pname left right;
   xf = 100/&hpos;
   c1= xf*5;
   c2= xf*(c1+20);
   c3= xf*(c2+max(&maxl,16)+2);
/*   c1= xf*c1; c2=xf*c2;  */
   xsys='5'; ysys='5'; hsys='4';
   function='LABEL'; 
   style="&font"; 
   position='6';
   if _n_=1 then do;  put 'FACEKEY:' c1= c2= c3=;
      size = 1.2*&htext;
      y= 97;
      x=c1; text='Parameter';            output;
      x=c2; text='Left Side Variable' ;  output;
      x=c3; text='Right Side Variable';  output;
   *  position='F'; y=y-1;
      x=c1; text=repeat('_',13     );    output;
      x=c2; text=repeat('_',17     );    output;
      x=c3; text=repeat('_',18     );    output;
      end;
   size = &htext; position='6';
   y= 94 - 5*num;
   x=c1; text=pname;  output;
   x=c2; text=left ;  output;
   x=c3; text=right;  output;
*proc print;
proc gslide anno=facekey name="&name" gout=&gcat;
   title1 &title1;
   title2 &title2;
   title3 &title3;
   footnote &footnote;
   %if %length(&footnote)>0 %then %do;
   footnote2 h=0.2 ' ';
   %end;
run;
title;
%mend;
