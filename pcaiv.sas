 /*---------------------------------------------------------------------
   *** SPECIES.SAS ***

   This macro performs a P.C.A.I.V. (Principal Component Analysis with
   respect to Instrumental Variables) with 1, 2 or 3 qualitative
   variables from a text file (at last, the 3 qualitative columns).
   graphical outputs:  1. correlation circles
                       2. fitted values from principal components
   for more explanations, see at the end of the program.
   ---------------------------------------------------------------------*/


/* 1-5 PCAIV circles
   6-10 Fitting values
   11-14 Submit macro and options
   1 - reading data
   2 - centring and scaling data
   3 - analysis of variance (regression)
   4 - PCAs
   5 - submitting the macros
   6 - choosing effect: fit window 
   7 - select in the data : YmeanG; means
   8 - select in the data : axis Xi; means
   9 - regression (YmeanG, Xi)
   10 - graph of the fitting model : 2 curves
   11 - ascii file
   12 - another pca macro
   13 - submit the program: Graphiq
        with fromprin window 
   14 - proc greplay
   15 - summary of background
  written by Philippe Vey/HEA/ORSTOM Montpellier
   ORSTOM,H.E.A.centre de Montpellier,911, av. Agropolis
   34032 Montpellier Cedex1,
   tel: (0)4.67.63.69.79
  Philippe.Vey@mpl.orstom.fr
  home: 15 rue d aquitaine .34170 castenau le lez. fr.
  (0)4.67.72.82.34 LOOK FOR A JOB
  version2: november  1997 */





   /* set the graphics environment */
title;footnote;
goptions reset=global gunit=pct
         ftext=swiss  htitle=4.8 htext=2.8
         cback=white   colors=(black);




               /* *** I - PCAIVs  *** */


         /* *** 1 - READING DATA                                      1 *** */

/* ** data window ** */
/* ***************************************************************** */
%window donnees color=white
#3  @ 12  "         Welcome to "
/   @ 12  "           SPECIES.SAS       " attr=rev_video
/   @ 12  "Copyright 1997 P. Vey/ORSTOM "
/   @ 12  " "
/   @ 12  "file (with acces) "   @43 Xfile 44
/   @ 12  "tab or space separator  (t/e)       "    @58 separ 1 attr=underline
/   @ 12  "n° of the line of the first observation  "    @58 fobs 3  attr=underline
/   @ 12  "number of quantitative variables to study  "    @58 p 3  attr=underline
/   @ 12  "operating system SUN or Windows (s/p)      "    @58 osystem 1 attr=underline
/   @ 12  "name of factor1 (qualitative variable)      "  @58 fact1 8  attr=underline
/   @ 12  "     of factor2 (qualitative variable)      "  @58 fact2 8  attr=underline
/   @ 12  "     of factor3 (qualitative variable)      "  @58 fact3 8  attr=underline
/   @ 12  "number of modalities for factor1        "    @58 modal1 2  attr=underline
/   @ 12  "       of modalities for factor2        "    @58 modal2 2  attr=underline
/   @ 12  "       of modalities for factor3        "    @58 modal3 2  attr=underline
/   @ 12  "type of data             "               @48 type 34 attr=underline
/   @ 12  "statistical source of data  "            @48 source 34 attr=underline
/   @ 12  " "
/   @ 12  " ";
 %display donnees;
/*****************************************************************************/


    /* ** read file and name the p variables (ESP1-ESP&p)
           and 3 qualitative variables
           create POISSONS table ** */
%MACRO readi ;
data  poissons;
infile "&Xfile" dlm='09'X lrecl=6000 firstobs=&fobs;
input esp1-esp&p &fact1 &fact2 &fact3 ;
run;
%mend readi;

%MACRO readis ;
data  poissons;
infile "&Xfile" lrecl=6000 firstobs=&fobs;
input esp1-esp&p &fact1 &fact2 &fact3 ;
run;
%mend readis;



/* separator tabulator or space */
%macro lecture;
%if &separ=t %then %readi;
%if &separ=e %then %readis;
%if &separ=$ %then %graphiq;
%mend lecture;
%lecture;




         /* *** 2 - CENTRING AND SCALING THE DATA
                     in columns by triplet      

 proc standard mean=0 std=1 data= poissons  out= ycentre ;
 var esp1-esp&p;
 run;




          /* *** 3 - ANALYSIS OF VARIANCE (REGRESSION)                   3 *** */

  /* **  variance analysis of ycentre (esp1-esp25)
       'predicted value'= means by modality
                        = fitted values of y = y observed
        create variables Y&MEAN and YMMOD table  */

  /* ** an. variance from centred and scaled data  y=a (then b,c)
 %MACRO AV1;
 proc glm data= ycentre noprint;
 class &classv;
 model esp1-esp&p =&modelv /p;
 output out= ymmod&av  p=&ymean r=yw1-yw&p;
 run;
%mend AV1;


 /* ** variance an. from residual data of y=a b c
       as Y=Ya+Yb+Yc+Yab+Ybc+Yac+Yabc
       after Y=a b c it remains Y=Yab+Ybc+Yac+Yabc
       thus with  Y=a b a*b it remains only Yab observed -a b effects are not
       here anymore ** */
 %MACRO AV2;
 proc glm data= ymmodw noprint;
 class &classv;
 model yw1-yw&p =&modelv /p;
 output out= ymmod&av  p=&ymean r=res;
 run;
 %mend AV2;




          /* *** 4 - PCAs                                               4 ***/

%let laisse=1 ;%let neige=2;
%MACRO PCALINE;

 /* set the graphics environment */
%if &osystem=s %then %let dim=4.8;
%if &osystem=s %then %str(goptions
                                   rotate=landscape;);
%if &osystem=p %then %str(goptions targetdevice=winprtc
                                    rotate=portrait
                                     htitle=3.8;  );
%if &osystem=p %then %let dim=6;


    /* ** PCA from covariance matrix
           create CPRIN table : principal components (prin1-prin&p)
           = psy matrix
           = coord. of observ. on circle correlation axis
           = principal components scores : variables Xi
           create NPOIJ : scoring coefficients
           (valeurs propres et vecteurs propres) **/

proc princomp cov data= ymmod&av outstat= npoij&av
                out= cprin&av prefix=x&av noprint;
var &ymean;
run;

   /* extract all eigenvalues LAMBDA (valeurs propres)  */
data  npoij&av;
set  npoij&av;
if _TYPE_='EIGENVAL' then _name_='VP';
if _TYPE_='MEAN' then _name_='MEAN';
if _TYPE_='N' then _name_='N';
if _TYPE_='SUMWGT' then _name_='SUMWGT';
run;
proc transpose data= npoij&av out= t;
run;

  /* sum of the lambda */
proc means data= t noprint;
var VP;
output out=total sum=sumvp;
run;

  /* lambda sum in a macro variable called total 
 options nomlogic nosymbolgen nomprint;
data  t2;
set   t total;
drop MEAN N ;
call symput ('total',SUMVP);
run;

   /* inertia rate for every axis */
data  t3;
set  t2;
inerti=int((VP/&total)*100);
run;


  /* call Symput : put each eigenvalue in a macro variable INERTI(1-n)
     left : stick(on the left) inerti to the precedent word in the title
     put : eliminate in the log, the message numeric-to-conversion performed   */
data  t4;
set  t3;
call symput ('inerti'|| left(put(_n_,4.)),left(INERTI));
run;

   /* put the inertie rate in the log */
%put &modelv;
%macro hensd;
%do w=1 %to &p;
%put v.p.: &&inerti&w;
run;
%end;
%mend hensd;
%hensd;


    /* *** scaling the principal components
            thus variables inside a 1 radius circle *** */
proc standard data= cprin&av std=1 out= cprd&av;
var x&av&1-x&av&p ;
run;

    /* *** covariances and correlations of var. p1-p&p  *** */
proc corr data= cprd&av cov outp=covcorr nosimple ;
var x&av&1-x&av&p ;
with &ymean;
run ;
/* ** drop the 3 lines means, n and std ** */
data drop3l;
set covcorr;
if _type_='COV' or _type_='CORR';
run;


   /* ** graph correlation circles  ** */
   /* change names of variables from p1-p&p to 1-&p */ 
data _poids_;
do i=1 to 2;
do j=1 to &p;
output;
end;
end;
run;
data text;
merge drop3l _poids_;
run;
    /* table of coord. of points rij (coeff de corr)
                        and points cij (covariance)  */
data coord1;
set text;
if _TYPE_ = 'CORR' ;r1=x&&av&&laisse;r2=x&&av&&neige;output;
run;
data coord2;
set text;
if _TYPE_ = 'COV' ; c1=x&&av&&laisse;c2=x&&av&&neige;output;
run;
data coord3;
merge coord2 coord1; by j;
run;

      /* table for annotate functions : text, move and draw */ 
data annot;
set coord3;
if _TYPE_ = 'COV'or _TYPE_ = 'CORR';
function='label' ; xsys='2';ysys='2'; toto=put(j,z2.);text=toto;
                   X=x&&av&&laisse; Y=x&&av&&neige; output;
function='move';x=c1;y=c2;output;
function='draw';x=r1;y=r2;line=1;size=0.3; output;
run ;
  /* construct the table for the circle : CERCLE  */
data cercle;
do i=1 to 100;
x=cos(arcos(-1)*i/10);
z=sin(arcos(-1)*i/10);
output;
end;
run;



   /* define footnote, titles, symbols and axis  */
footnote ;
symbol1 i=none v=none;
symbol2 i=spline v=none;
 axis1  label=("axis &laisse") length=&dim in;
 axis2  label=(angle=90 "axis &neige") length=&dim in;

     /*  *** graph of variables on the correlation circle  *** */
data graphiq;
set annot cercle ;
run;
* filename gsasfile 'pubnesc';
title1 "PCAIV with respect to &modelv" ;
title2 "&titre2";
footnote1 j=right "axis &laisse: (in %) &&inerti&laisse " j=left "&type" ;
footnote2 j=right "axis &neige: (in %) &&inerti&neige " j=left "source: &source";

proc gplot data=graphiq;
      plot y*x=1 y*x=1 z*x=2
       / overlay  annotate=annot
             frame  vref=0 lvref=33 href=0 lhref=33
             vaxis=axis2 haxis=axis1 ;
run;
%mend PCALINE;




        /* *** 5 - DEFINE AND SUBMIT THE MACROS av and pca             5 *** */

/* ** define the macro for main effects a, b, c
                       and for main effects + 2-way interaction a|b b|c a|c ** */
%MACRO EFFPRCPX;
%AV1
%PCALINE
%mend EFFPRCPX;

/* ** define the macro for 2-way interaction ab bc ac  **/
%MACRO INTERACT;
%AV2
%let titre2=2-way interaction only;
%PCALINE
%mend INTERACT;

/* define macro variables
   and submit the macros for the 10 matrixes (effects)
   av=effect
   modelv and classv= model and class for the analysis of variance
   ymean= observed variables from the analysis of variance to
           project on the circle **/

%let eff=&modal3;
%let include=&fact1 &fact2;
%let classv=&fact3;
%let modelv=&fact3;
%let ymean=yc1-yc&p;
%let av=c;
%let titre2=main factor;
%EFFPRCPX;

%let classv= &fact2;
%let modelv= &fact2;
%let ymean=yb1-yb&p;
%let av=b;
%EFFPRCPX;

%let classv=&fact1;
%let modelv=&fact1;
%let ymean=ya1-ya&p;
%let av=a;
%EFFPRCPX;

%let classv= &fact1 &fact2;
%let modelv= &fact1|&fact2;
%let ymean=yd1-yd&p;
%let av=d;
%let titre2=main factors and 2-way interaction;
%EFFPRCPX;
%let classv= &fact1 &fact3;
%let modelv= &fact1|&fact3;
%let ymean=ye1-ye&p;
%let av=e;
%EFFPRCPX;
%let classv= &fact2 &fact3;
%let modelv= &fact2|&fact3;
%let ymean=yf1-yf&p;
%let av=f;
%EFFPRCPX;

%let classv=&fact1 &fact2 &fact3;
%let modelv=&fact1 &fact2 &fact3;
%let ymean=nnf;
%let av=w;
 proc glm data= ycentre noprint;
 class &classv;
 model esp1-esp&p =&modelv /p;
 output out= ymmod&av  p=&ymean r=yw1-yw&p;
 run;

%let classv=&fact1 &fact2;
%let modelv=&fact1*&fact2;
%let ymean=yh1-yh&p;
%let av=ab;
%INTERACT;
%let classv=&fact1 &fact3;
%let modelv=&fact1*&fact3;
%let ymean=yi1-yi&p;
%let av=ac;
%INTERACT;
%let classv=&fact2 &fact3;
%let modelv=&fact2*&fact3;
%let ymean=yj1-yj&p;
%let av=bc;
%INTERACT;


  /* ** take the centred and scaled data and just change the name of the
        table and the name of the variables for YMMODG and ESPi  ** */

data  ymmodg;
set  ycentre;
run;
data  ymmodg;
set  ymmodg;
array yg{&p} yg1-yg&p;
array esp{&p} esp1-esp&p;
do i=1 to &p;
yg{i}=esp{i};
end;
run;
%let modelv=centred and scaled raw data;
%let ymean=esp1-esp&p;
%let av=g;
%let titre2==pca ;
%PCALINE;


quit;
footnote;title;

/* We have 10 correlation circles                                             */
/******************************************************************************/





             /* *** II - FITTING VALUES *** */

  /* set the graphics environment */
title;footnote;
goptions reset=global gunit=pct
          ftext=swiss  htitle=3.8 htext=2.8
         cback=white   colors=(black)
         ;
goptions rotate=landscape;



            /* *** 6 - CHOICE OF THE USER : FIT WINDOW
                        and windows to sort the 2-way interactions             6 *** */

%let av= ;
/***************************************************************/
%WINDOW fit irow=4  icolumn=2  color=white columns=90
  #2 @2  " To fit values:" attr=rev_video
  /  @2 " Choice of a VARIABLE according to a SOURCE of VARIATION."
  /  @2  " (for the 2-ways interactions, main factors are included )"
  /  @2  " "
  /  @2  " Sources of variation:"  attr=underline
  /  @2  " &fact1    "  @15 "==> a"
  /  @2  " &fact2    "  @15 "==> b"
  /  @2  " &fact3    "  @15 "==> c"
  /  @2  " &fact1 &fact2 "      @23  "==> d"
  /  @2  " &fact1 &fact3 "      @23  "==> e"
  /  @2  " &fact2 &fact3 "      @23  "==> f "
  /  @2  " and scaled and centred raw data ==> g"
  / @2 "   "
  / @2 "   "
  / @2 "  Source of variation (a b c d e f or g)    :"  @48 av 1 attr=underline
  / @2 "     "
  / @2 "  Variable (from 1 to &p)                  : " @48 vrec 2 attr=underline ;


%MACRO ck;
 /* *** to create the macro variables &cat, &inte, &last, &mgrp and modal0
        outside the macros : D E F or G  *** */
%let cat=O; %let inter=O; %let last=1;
%let mgrp=0;
%let modal0=0;


  /* *** windows to sort the 2-ways interaction  *** */
  /* * macros E D F and G to sort and windows  ** */
  /* ex: to select between 2*24 or 24*2  */
  
%MACRO E;
%window E  irow=10 rows=18 icolumn=2  columns=90 color=white
#1 @2  "enter an x on the line of the sereadisd graphic,"
/  @2  "then press enter untill the end. "
/  @2  " "
/  @2  "Quel profil souhaitez-vous?: "
/  @2  "  -  &fact1 (&modal1 series de &modal3 &fact3)"   @45 cross 1  attr=underline
/  @2  "  -  &fact3 (&modal3 series de &modal1 &fact1)"    @45 crosse 1  attr=underline
/  @2  " "
/  @2  " " ;
%display E;
%if &cross=x %then %do;%let cat=1; %let last=3;
                       %let mgrp=&fact1 &fact3; %end;
%if &crosse=x %then %do;%let cat=3; %let last=1;
                        %let mgrp=&fact3 &fact1; %end;
%if &cross ne x and &crosse ne x %then %e;
%let mbalanc=%eval(&modal1 * &modal3);
%mend E;

%MACRO D;
%window D  irow=10 rows=18 icolumn=2  columns=90 color=white
#1 @2  "enter an x on the line of the sereadisd graphic,"
/  @2  "then press enter until the end. "
/  @2  " "
/  @2  "Quel profil souhaitez-vous?: "
/  @2  "  -  &fact1 (&modal1 series de &modal2 &fact2)"   @45 cross 1  attr=underline
/  @2  "  -  &fact2 (&modal2 series de &modal1 &fact1)"    @45 crosse 1  attr=underline
/  @2  " "
/  @2  " " ;
%display D;
%if &cross=x %then %do;
%let cat=1; %let last=2;%let mgrp=&fact1 &fact2; %end;
%if &crosse=x %then %do;
%let cat=2;%let last=1;%let mgrp=&fact2 &fact1; %end;
%if &cross ne x and &crosse ne x %then %d;
%let mbalanc=%eval(&modal1 * &modal2);
%mend D;

%MACRO F;
%window F  irow=10 rows=18 icolumn=2  columns=90 color=white
#1 @2  "enter an x on the line of the sereadisd graphic,"
/  @2  "then press enter untill the end. "
/  @2  " "
/  @2  "Quel profil souhaitez-vous?: "
/  @2  "  -  &fact2 (&modal2 series de &modal3 &fact3)"   @45 cross 1  attr=underline
/  @2  "  -  &fact3 (&modal3 series de &modal2 &fact2)"    @45 crosse 1  attr=underline
/  @2  " "
/  @2  " " ;
%display F;
%if &cross=x %then %do;
%let cat=2; %let last=3; %let mgrp=&fact2 &fact3; %end;
%if &crosse=x %then %do;
%let cat=3;  %let last=2;%let mgrp=&fact3 &fact2; %end;
%if &cross ne x and &crosse ne x %then %f;
  %let mbalanc=%eval(&modal2 * &modal3);
%mend F;

%MACRO G;
%window G  irow=10 rows=18 icolumn=2  columns=90 color=white
#1 @2  "enter an x on the line of the sereadisd graphic,"
/  @2  "then press enter untill the end. "
/  @2  " "
/  @2  "Quel profil souhaitez-vous?: "
/  @3  " -  &fact1-&fact2 (&modal1*&modal2 series de &modal3 &fact3)" @62 cross1 1   attr=underline
/  @3  " -  &fact2-&fact1 (&modal2*&modal1 series de &modal3 &fact3)" @62 cross2 1  attr=underline
/  @3  " -  &fact1-&fact3 (&modal1*&modal3 series de &modal2 &fact2)" @62 cross3 1  attr=underline
/  @3  " -  &fact3-&fact1 (&modal3*&modal1 series de &modal2 &fact2)" @62 cross4 1  attr=underline
/  @3  " -  &fact2-&fact3 (&modal2*&modal3 series de &modal1 &fact1)" @62 cross5 1  attr=underline
/  @3  " -  &fact3-&fact2 (&modal3*&modal2 series de &modal1 &fact1)" @62 cross6 1  attr=underline ;
%display G;
%if &cross1=x %then %do;
%let cat=1; %let inter=2;%let last=3; %let mgrp=&fact1 &fact2 &fact3 ;%end;
%if &cross2=x %then %do;
%let cat=2; %let inter=1; %let last=3;%let mgrp=&fact2 &fact1 &fact3 ; %end;
%if &cross3=x %then %do;
%let cat=1;
%let inter=3;
%let last=2;%let mgrp=&fact1 &fact3 &fact2 ; %end;
%if &cross4=x %then %do;
%let cat=3;
%let inter=1;
%let last=2; %let mgrp=&fact3 &fact1 &fact2 ; %end;
%if &cross5=x %then %do;
%let cat=2;
%let inter=3;
%let last=1; %let mgrp=&fact2 &fact3 &fact1 ; %end;
%if &cross6=x %then %do;
%let cat=3;
%let inter=2;
%let last=1;%let mgrp=&fact3 &fact2 &fact1 ; %end;
%if &cross1 ne x and &cross2 ne x   and &cross3 ne x
     and &cross4 ne x  and &cross5 ne x  and &cross6 ne x %then %g;
%mend G;


   /** according to the selected effect &av,
       define values of cat, mgrp or submit a macro     **/

%if %upcase(&av)=A %then %let cat=1 ;
%if %upcase(&av)=A %then %let mgrp=&fact1 ;
%if %upcase(&av)=B %then %let cat=2;
%if %upcase(&av)=B %then %let mgrp=&fact2;
%if %upcase(&av)=C %then %let cat=3;
%if %upcase(&av)=C %then %let mgrp=&fact3;
%if %upcase(&av)=D %then %D;
%if %upcase(&av)=E %then %E;
%if %upcase(&av)=F %then %F;
%if %upcase(&av)=G %then %G;


%put your choice has been recorded;
%put please wait from 15 sec. to 3 minutes;




        /* *** 7 - SELECT DATA OF THE CHOSEN EFFECT : ymean&av
                   IN THE FIT WINDOW
                   then MEANS BY &MGRP
                   and calculate xabc and xreal                          7 *** */

   /* ** xabc = abcissa on the graph, for 2-ways interaction it
                is different from the total x; ex: [1,3[ for (1,48)
         xreal= for missing data, calculate the total vector to create
                 a row for not mentionned missing data,
                 so calculate the total x [1,48] deduced from the 2 i.v.
                 of the 2-ways interaction                                  ** */

%MACRO PORT;
x=&fact1;
xreal=x;
xabc=x;
y=ya&vrec;
%mend PORT;

%MACRO YEAR;
x=&fact2;
xreal=x;
xabc=x;
y=yb&vrec;
%mend YEAR;

%MACRO FORTN;
x=&fact3;
xreal=x;
xabc=x;
y=yc&vrec;
%mend FORTN;


%MACRO AB ;
xreal=(&&modal&last * (&&fact&cat - 1)) + &&fact&last;
xabc= &&fact&cat + ((&&fact&last -1)/&&modal&last);
y=yd&vrec;
run;
%mend AB  ;
%MACRO AC  ;
 xreal=(&&modal&last * (&&fact&cat - 1)) + &&fact&last;
xabc= &&fact&cat + ((&&fact&last -1)/&&modal&last);
y=ye&vrec;
run;
%mend AC  ;
%MACRO BC  ;
xreal=(&&modal&last * (&&fact&cat - 1)) + &&fact&last;
xabc= &&fact&cat + ((&&fact&last -1)/&&modal&last);
y=yf&vrec;
run;
%mend BC  ;

%MACRO ABC;
xreal=((&&modal&last*&&modal&inter) * (&&fact&cat - 1))+(&&modal&last * (&&fact&inter - 1))
        + &&fact&last;
xabc=&&fact&inter + ((&&fact&cat - 1)*&&modal&inter) + ((&&fact&last - 1)/&&modal&last);

y=yg&vrec;
run;
%mend ABC;



  /* *** select the ymeans of the selected effect and _n_ total  *** */
%MACRO SELECTI;
data  ymmod2;
set  ymmod&av;
drop esp1-esp&p;
%if %upcase(&av)=A %then %PORT;
%if %upcase(&av)=B %then %YEAR;
 %if %upcase(&av)=C %then %FORTN;
%if %upcase(&av)=D %then %AB   ;
%if %upcase(&av)=E %then %AC   ;
%if %upcase(&av)=F %then %BC   ;
%if %upcase(&av)=G %then %ABC   ;
%mend SELECTI;
%SELECTI;


  /*** sort and means ***/
%MACRO MEANS2;
proc sort data= ymmod2;
by &mgrp;
run;

proc means data= ymmod2;
by &mgrp;
var xreal xabc y y&av&vrec;
output out= mymmod mean=xreal xabc y y&av&vrec;
run;


%mend MEANS2;
%MEANS2;



         /* *** 8 - SELECT ALL CHOSEN AXIS (COUNTING, SCANNING)
                    IN THE FROMPRIN WINDOW 
                    then MEANS BY MGRP                                 8 *** */


 /*** count &axisnbrs, scan &list1 and &list2,
      generates the 2 list of names sel&i et axis&i,

%let axisnbrs=1;
%let word=%qscan(&list1, &axisnbrs,+);
%do %while (&word ne);
%let axisnbrs=%eval(&axisnbrs+1);
%let word=%qscan(&list1, &axisnbrs, +);
%end;
%let axisnbrs=%eval(&axisnbrs-1);

%do i=1 %to &axisnbrs;
%let sel&i=%scan(&list1,&i,+);
%put &&sel&i;
%end;
%do i=1 %to &axisnbrs;
%let axis&i=%scan(&list2,&i,+);
%put &&axis&i;
%end;


/* generates the 2 list of names sel&i and axis&i */
%let semicln=%str(;);
%MACRO nam(name1,name2,number);
%do i=1 %to &number;
%let sel&i=%scan(&list1,&i,+);
&name1 &name2&&sel&i &semicln
%end;
%mend nam;

%MACRO na(name1,name2,number);
%do i=1 %to &number;
%let sel&i=%scan(&list1,&i,+);
%let axis&i=%scan(&list2,&i,+);
&name1&&sel&i&&&name2&i
%end;
%mend na;


/* count &axisnbrs */
%MACRO axisnbr;
%let axisnbrs=1;
%let word=%qscan(&list1, &axisnbrs,+);
%do %while (&word ne);
%let axisnbrs=%eval(&axisnbrs+1);
%let word=%qscan(&list1, &axisnbrs, +);
%end;
%let axisnbrs=%eval(&axisnbrs-1);
%put &axisnbrs;
%mend axisnbr;
%axisnbr;


%MACRO REGR;


   /* *** select the matrix psy  *** */

%MACRO MEANS;

%let semicln=%str(;);
data  comp2;
set  cprin&av;
%nam(set, cprin,&axisnbrs);
   %if %upcase(&av)=G %then %str(drop esp1-esp&p;);
   %else %str(
drop esp1-esp&p yw1-yw&p;);
run;

/** sort and means **/
proc sort data= comp2;
by &mgrp;
run;
proc means data= comp2;
by &mgrp;
var y&av&vrec %na(x,axis,&axisnbrs);
output out= mcp mean=y&av&vrec %na(x,axis,&axisnbrs) ;
run;
%mend MEANS;
%MEANS;




           /* *** 9 - REGRESSION YmeanG ,Xi
                       and create the balanced table : DBALAN       

/* ** weight _freq_ very important b/c the original size
      of the table has been reduced  **/
proc glm data= mcp;
model y&av&vrec= %na(x,axis,&axisnbrs)/p;
output out= regmycp p=yfitt r=ecart;
weight _freq_;
run;


 /* ** union of the 2 tables : mymmod (ymeang) and regmycp (xci)
       if x=. for missing data to have values for y=0
       when there is a hole in the data table */
data  merge;
merge  regmycp  mymmod;
by &&fact&cat;
drop _type_;
x=xabc;
if x=. then y&av&vrec=0;
if x=. then yfitt=0;
%if %upcase(&av)=A
 or %upcase(&av)=B
  or %upcase(&av)=C %then  %let mbalanc=&&modal&cat;

%if %upcase(&av)=D
 or %upcase(&av)=E
 or %upcase(&av)=F %then %str (if x=. then x=(xreal+&&modal&last-1)/&&modal&last;);

%if %upcase(&av)=D
 or %upcase(&av)=E
 or %upcase(&av)=F %then  %let mbalanc=%eval(&&modal&cat * &&modal&last);

%if %upcase(&av)=G %then  %let mbalanc=%eval(&&modal&cat * &&modal&inter * &&modal&last);

run;

/* create column of xreal for table with missing values */
data dbalan;
do xreal=1 to &mbalanc;
output;
end;
run;
data   dbalan;
merge dbalan  merge;
by xreal;
run;


%if %upcase(&av)=D
 or %upcase(&av)=E
 or %upcase(&av)=F %then  %str(
data dcat;
do newf1=1 to &&modal&cat;
do  newf2=1 to &&modal&last;
output;
end;
end;
run;
data  dbalan;
merge dcat  dbalan;
&&fact&cat=newf1;
&&fact&last=newf2;
drop newf1 newf2;
run; );

%mend REGR;
%REGR;



                /* *** 10 - GRAPH OF THE FITTING MODEL : 2 CURVES
                             effect and fitted values,
                             create &balanced for missing values, double variable,
                             graduation of the axis, title, symbol, plot y*x      10 ** */

%MACRO DXGRAPH;

 /* define &balanced for g when missing data
     ex : balanced = column of 2*17*24   */
%let balanced=&modal1 * &modal2 * &modal3;
%if %upcase(&av)=G %then %let modal8=%eval((&balanced/&&modal&last)+1);
%if %upcase(&av)=G and &balanced<1200 %then %str(goptions htitle=3.4 htext=1.8;
                              axis5 order=(1 to &modal8 by &&modal&inter)
                              minor=none
                              label=(angle=0 "&&fact&cat*&&fact&inter" )
                              value=(tick=&modal8 ' '););


   /* create variable y to put the 2 curves on the same graph  */
data  yfitt2;
	set  dbalan;
	var&vrec="&av effect";y=y&av&vrec;output;
	var&vrec='fitted';y=yfitt;output;
	run;

   /* calculate minimum and maximum */
proc means data= yfitt2 max min;
	var y;
	output out=outmean  max=ymax min=ymin;
	run;
data _null_;
	set outmean;
	call symput ('max',ceil(ymax));
	call symput ('min',floor(ymin));
	run;




     /* macro for plot y*x */
%MACRO PLOT;
%if %upcase(&av)=A %then %str(
                    symbol2 font=none i=join l=2 v=none;
                    proc gplot data= yfitt2;
                    plot y*x=var&vrec/skipmiss frame haxis=axis1;);
%if %upcase(&av)=B %then %str(symbol2 font=none i=join l=2 v=none;
                      proc gplot data= yfitt2;
                       plot y*x=var&vrec/skipmiss frame haxis=axis11;);
%if %upcase(&av)=C %then %str(symbol2 font=none i=join l=2 v=none;
                       proc gplot data= yfitt2;
                        plot y*x=var&vrec/skipmiss  frame haxis=axis111;);
%if %upcase(&av)=D or %upcase(&av)=E or %upcase(&av)=F  %then
                   %str(proc gplot data= yfitt2 uniform;
                   plot y&av&vrec*x=&&fact&cat  /frame skipmiss nolegend haxis=axis2
                                                                 vaxis=axis4 ;
                   plot2 yfitt*x=&&fact&cat  /frame skipmiss nolegend haxis=axis2
                                                                vaxis=axis3; );
%if %upcase(&av)=G %then %str(proc gplot data= yfitt2;
                   plot yg&vrec*x=&&fact&cat
                   /skipmiss frame vaxis=axis4 haxis=axis5 nolegend;
                   plot2 yfitt*x=&&fact&cat /frame vaxis=axis3
                                haxis=axis5 nolegend;);
 %mend PLOT;

  /* ** macro for symbols, curve in continuous line for effect ALPHA
                           and dotted line for fitted values  ** */
%let modal5=%eval(&&modal&cat *2);
%let modal6=%eval(&&modal&cat +1);
%MACRO SYMBOL;
%do i=1 %to &&modal&cat;
symbol&i font=none i=join  l=1 value=none;
%end;
%do j=&modal6 %to &modal5;
symbol&j font=none i=join  l=2  value=none;
%end;
%mend SYMBOL;
%SYMBOL;


   /* *** graduate axis ** */
  /* definit les axiss:pour les 3 effets simples C h=axis1 */
%if &modal1<25 %then %str(
axis1 label=("&fact1" ) order=(1 to &modal1) minor=none; );
%if &modal2<25 %then %str(
axis11 label=("&fact2" ) order=(1 to &modal2) minor=none; );
%if &modal3<25 %then %str(
axis111 label=("&fact3" ) order=(1 to &modal3) minor=none; );

  /* define axis : for the 2-way interaction hor. =axis2
                                             vert.= axis3 or 4  */
%let modal7=%eval((&&modal&cat)+1);
%if &&modal&cat<25 %then %str(
axis2 order=(1 to &modal7)
      minor=none
      label=(angle=0 "&&fact&cat" )
      value=(tick=&modal7 ' ') ;        );
axis3 order=(&min &max)
      label=("fitted values" j=right "in dotted line");
axis4 order=(&min &max)
      label=("variable &vrec") ;


   /* define titles  */
%MACRO TITRE;
%if %upcase(&av)=A %then %str(title1  "Fitting model of the variation &fact1 ";);
%if %upcase(&av)=B %then %str(title1  "Fitting model of the variation &fact2 ";);
%if %upcase(&av)=C %then %str(title1  "Fitting model of the variation &fact3 ";);
%if %upcase(&av)=D %then %str(title1  "Fitting model of the variation &fact1-&fact2 ";);
%if %upcase(&av)=E %then %str(title1  "Fitting model of the variation &fact1-&fact3 ";);
%if %upcase(&av)=F %then %str(title1  "Fitting model of the variation &fact2-&fact3 ";);
%if %upcase(&av)=G %then %str(title1  "Fitting model of the variation totale ";);
%mend TITRE;
%TITRE;
title2 j=center "of variable &vrec";
footnote1 j=left "fitted values from prin. comp.:" j=right "&type";
footnote2 j=left " %na(x,axis,&axisnbrs)" j=right "source: &source.";


*filename gsasfile 'graphic1';
%PLOT;
run;

quit;
title; footnote;
%mend DXGRAPH;
%DXGRAPH;

%mend ck;





 /* *** III - SUBMIT THE PROGRAM *** */
options nomlogic nosymbolgen nomprint;


         /* *** 11 - FILE ASCII                                  11 *** */

%macro FILEPUT;
%window FILE irow=8 icolumn=8 rows=14 columns=90  color=white
 #1 @4 "ASCII FILE" attr=rev_video
 /  @4 "Columns values would be in order: y, fitted y, frequence of y, &mgrp and  "
 /  @4 "the principal component(s) %na(x,axis,&axisnbrs)."
 /  @4 " "
 /  @6 "name of the created file with acces : "  @47  Yfile 34 attr=underline
 /  @6 " ";
%display FILE;
data bidon;
set  dbalan;
file "&Yfile";
put y yfitt _freq_ &mgrp %na(x,axis,&axisnbrs);
run;
%boucle;

%mend FILEPUT;





      /* *** 12 - ANOTHER PCA                                        12 *** */

  /* set the graphics environment */
title;footnote;
goptions reset=global gunit=pct
         ftext=swiss  htitle=4.8 htext=2.8
         cback=white   colors=(black)
         ;
goptions rotate=landscape;

%MACRO PCA;
title1;
%let titre2= ;
%window APCA irow=8 icolumn=2 color=white
  #1 @2  " PCAIV:"  attr=rev_video
  /  @2  " Choice of the SOURCE of VARIATION (effect)"
  /  @2  " and PRINCIPAL COMPONENTS."
  /  @2  " "
  /  @2  " Sources of variation:"  attr=underline
  /  @2  " &fact1    "  @15 "==> a"
  /  @2  " &fact2    "  @15 "==> b"
  /  @2  " &fact3    "  @15 "==> c"
  /  @2  " &fact1 &fact2 "      @23  "==> d"
  /  @2  " &fact1 &fact3 "      @23  "==> e"
  /  @2  " &fact2 &fact3 "      @23  "==> f "
  /  @2  " and centred and scaled raw data ==> g"
  / @2 "   "
  / @2 "  Source of variation (a b c d e f ou g)          "  @60 av 1 attr=underline
  / @2 "  Number of the prin. component for the horizontal axis" @60 laisse 2 attr=underline
  / @2 "  Number of the prin. component for the vertical axis" @60 neige 2 attr=underline ;
%DISPLAY APCA;
%let modelv=&av;
%let ymean=y&av&1-y&av&p;
%pcaline;
%boucle;
%mend PCA;




/*
                        and fromprin window incorporated               13 *** */


/* ****** window boucle et macro ********* */
%window QUEST irow=8 icolumn=8 rows=16 columns=85  color=white
#1 @4 "OPTIONS" attr=rev_video
/  @4 " "
/  @4 "- To perform another fitting model, enter " "o"
/  @4 "- To save values of the last fitting model graphic in a text file, enter: " "f"
/  @4 "- To perform another PCAIV with circle correlation graphical output, enter: " "a"
/  @4 "- Otherwise, to quit, enter: " "n"
/  @6 " "
/  @6 "Choice: "   @14 getdata 1 attr=underline required=yes;

%MACRO BOUCLE;
%display QUEST;
%if %upcase(&getdata)=O %then %graphiq;
%if %upcase(&getdata)=F %then %fileput;
%if %upcase(&getdata)=A %then %pca;

%mend BOUCLE;



/* ***  MACRO GRAPHIQ and fromprin window incorporated                 *** */

%MACRO GRAPHIQ;
%DISPLAY fit;
%if (&av)=$ %then %goto out;
%if (&vrec)=$ %then %goto out;

%if %UPCASE(&av) ne A
and %UPCASE(&av) ne B
and %UPCASE(&av) ne C
and %UPCASE(&av) ne D
and %UPCASE(&av) ne E
and %UPCASE(&av) ne F
and %UPCASE(&av) ne G
and %UPCASE(&av) ne $ %then %do;
                     %let sysmsg=Attention : YOUR CHOICE IS NOT VALID.;
                     %graphiq;
                     %goto out;
                     %end;

%window fromprin color=white irow=5
  #2 @2  " Choice of principal components for the fitting model." attr=rev_video
  /  @2  " (separate each term of the 2 lists by a +)"
  /  @2  " "
  /  @2  " PCA:"  attr=underline
  /  @2  " &fact1    "  @15 "==> a"
  /  @2  " &fact2    "  @15 "==> b"
  /  @2  " &fact3    "  @15 "==> c"
  /  @2  " &fact1 &fact2 (2-way interaction only)"      @41  "==> ab"
  /  @2  " &fact1 &fact3 (2-way interaction only)"      @41  "==> ac"
  /  @2  " &fact2 &fact3 (2-way interaction only)"      @41  "==> bc"
  /  @2  "   "
  /  @2  "   "
  /  @2  "  Number of a or several prin. components : "    @51 list2 50 attr=underline
  /  @2  "  Corresponding PCA  (among a b c ab ac bc) :"    @51 list1 50 attr=underline
  /  @2  "   "
  /  @2  "  exemple:"
  /  @2  "  To fit the values of variation &fact1 &fact3 from"
  /  @2  "  the first axis of PCA &fact1, from the 2 first of PCA &fact3 and from"
  /  @2  "  the third of PCA &fact1 &fact3, enter:  1+1+2+3 "
  /  @2  "               then on the line below :  a+c+c+ac "
  /  @2  "      ";
%DISPLAY fromprin;
%let cat=O;
%let inter=O;
%let last=O;
%let mgrp=0;
%let axisnbrs=0;
%ck
%BOUCLE;
%out:
%mend GRAPHIQ;

%GRAPHIQ;

quit;


          /* 14 - PROC GREPLAY                                        14 *** */

 /* ** pls graphiques sur une meme page ** */
* proc greplay igout=GSEG gout=GSEG tc=sasuser.mytemplt template=l2r2s;
* run;


          /* *** 15 - SUMMARY OF THE PROGRAM                          15 *** */

 /*--------------------------------------------------------------------------
A PCAIV is a statistical method which explains quantitative variables
from variables nammed instrumental, here 1, 2 or 3 qualitative
variables. PCAIV is a statistical method which combines both
PCA and multivariate regression analysis.
(see Rao 64, Sabatier 89, Pech and Laloe 97)
Matrix is represented in terms of an analysis of variance model:
Y = Ya +Yb +Yc +Yab +Yac +Ybc +Yabc.
PCAIV consists in doing several PCA, not from the Y centred and scaled
matrix but, from the 7 matrixes after decomposition.
In a PCAIV, every variable is represented, not by a single point like in
PCA but, by 2 points connected by a line segment. Graphical position
of these 2 points allow us to measure the influence of the effect ALPHA,
- i.e. the part of variation of yq explained by ALPHA -.
Indeed, it equals qraphically to the square of the ratio of the
distances interior point-centre of the circle on exterior point-centre
of the circle.
Principal Components are linear combination of the starting variables.
Fitting a variable from the principal components consists in doing the
regression of the variable yqALPHA of PCA. The curve in
continuous line represents the fitted values ; curve in dotted line
represents the fitted values yqALPHA.

A 20 pages with snapshots user's guide of this macro
s.p.e.c.i.e.s.s.a.s is available, contact :
Philippe Vey    (0)4. 67. 63. 69. 79    vey@melusine.mpl.orstom.fr
Francis Laloe   (0)4. 67. 63. 69. 64    laloe@mpl.orstom.fr
Nicolas Pech    (0)4. 67. 63. 69. 63    pech@mpl.orstom.fr
ORSTOM MONTPELLIER, service H.E.A.
911, Avenue Agropolis
BP 5045
34032  Montpellier Cedex 1 - france
Standard    Tel : (0)4 67 41 61 00
Fax : 04 67 54 78 00
   --------------------------------------------------------------------------*/
/* Copyright 1997 Philippe Vey/ORSTOM.*/

