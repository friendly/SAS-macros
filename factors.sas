%macro Factors(a,lib=work,FactorDSN=generate);
data generate1;
   do R=1 to &a;
   do C=1 to &a;
   ***************************************
   create vectors for
   S symmetry
   L vertical mobility
   T Triangles
   Q Quasi-Independencefactor variable
   DA Diagonal Absolute
   D Diagonal factor variable
   F Fixed distance
   OS1 Odds symmetry I
   OS2 Odds symmetry II
   ****************************************;
	  if R lt C then
		 do;
		 k=abs(R-C);
		 S=k+1-(R+1)*((0.5*R)+1)+(&a+3)*(R+1)-3-2*&a;
		 L=1;
		 T=1;
		 Q=&a+1;
		 DA=abs(R-C);
		 D=abs(R-C);
		 F=abs(R-C)+1;
		 OS1=R;
		 OS2=2*&a-C;
		 end;

	  else if R gt C then
		 do;
		 k=abs(R-C);
		 S=(k+1)-(C+1)*(.5*C+1)+(&a+3)*(C+1)-3-2*&a;
		 L=1;
		 T=2;
		 Q=&a+1;
		 DA=abs(R-C);
		 D=abs(R-C)+(&a-1);
		 F=1;
		 OS1=(&a-1)+C;
		 OS2=(&a+1)-R;
		 end;

	  else /*if R eq C then */
		 do;
		 S=1-(C+1)*(.5*C+1)+(&a+3)*(C+1)-3-2*&a;
		 L=2;
		 T=3;
		 Q=R;
		 DA=&a;
		 D=2*&a-1;
		 F=1;
		 OS1=2*&a-1;
		 OS2=2*&a-1;
		 end;

	  U=R*C;
	  output;
	  drop k;
	  end; end;
   run;

* generate FF,SS, H, and DD vectors;
* use ID variable to help in merging with the V vectors;
data generate2;
   set generate1;
   ID=_N_;
   ARRAY FF(&a);
   ARRAY SS(&a);
   ARRAY H(&a);
   ARRAY DD(&a);
   do i=1 TO &a;
	  if R=i then FF(i)=1;
		 else FF(i)=0;
	  if C=i then SS(i)=1;
		 else SS(i)=0;
	  H(i)=FF(i)+SS(i);
	  DD(i)=FF(i)-SS(i);
	  end;
   drop i;
   run;

*** create the V vectors;
%global h;
%do h=1 %to %eval(&a-1);
   data vdata&h;
   ID=0;
   %do i=1 %to &a;
   %do j=1 %to &a;
	  ID=ID+1;
	  %If &i<=%eval(&h) and &j <=%eval(&h) %then %do;
	  v&h=2; %end;
	  %else %if &i >=%eval(&h+1) and &j>=%eval(&h+1)
	  %then %do; v&h=2; %end;
	  %else %do; v&h=1; %end;
	  output;
	  %end;
	  %end;
   proc sort data=vdata&h; by ID;
   %end;
run;

*** combine v vectors with other factors ---;
data &lib..&FactorDSN(drop=ID);
   merge generate2
   %do k=1 %to %eval(&a-1);
   vdata&k
   %end;
   ;
   by ID;
   run;
%mend factors;


*********** STEP 2 Macro ************************;
%macro analysis(a,depvar,CombinedDSN,lib=work,FactorDSN=generate);
data &lib..&CombinedDSN;
merge &DataLib..&DataDSN &lib..&FactorDSN;
*** Fit asymetry models O, F, T, D, OS1, OS2, 2RPA, LDPS respectively ***;
%let b = %eval(&a-1);
proc genmod data=&lib..&CombinedDSN;
class s;
model &depvar= s /dist=poi;
title 'Asymetry Model';
title2 'O Model';

proc genmod data=&lib..&CombinedDSN;
class s;
model &depvar= s f/dist=poi;
title2 'LDPS Model';

proc genmod data=&lib..&CombinedDSN;
class s t;
model &depvar= s t /dist=poi;
title2 'T Model';

proc genmod data=&lib..&CombinedDSN;
class s d;
model &depvar= s d /dist=poi;
title2 'D Model';

proc genmod data=&lib..&CombinedDSN;
class s os1;
model &depvar= s os1 /dist=poi;
title2 'OS1 Model';

proc genmod data=&lib..&CombinedDSN;
class s os2;
model &depvar= s os2 /dist=poi;
title2 'O Model';

proc genmod data=&lib..&CombinedDSN;
class s t;
model &depvar= s t f /dist=poi;
title2 '2RPA Model';
run;

*** Fit skew-symmetry models O, T, D, QOS respectively ***;
proc genmod data=&lib..&CombinedDSN;
class r c s;
model &depvar= r c s /dist=poi;
title 'Skew-symmetry Model';
title2 'O Model';

proc genmod data=&lib..&CombinedDSN;
class r c s t;
model &depvar= r c s t /dist=poi;
title2 'T Model';

proc genmod data=&lib..&CombinedDSN;
class r c s d;
model &depvar= r c s d /dist=poi;
title2 'D Model';

proc genmod data=&lib..&CombinedDSN;
class r c s os1;
model &depvar= r c s os1 /dist=poi;
title2 'QOS Model';
run;

*** Fit non-independence models as they appear respectively in Table 2 ***;
proc genmod data=&lib..&CombinedDSN;
class r c;
model &depvar= r c /dist=poi;
title 'Non-Independence Model';
title2 'O Model';

proc genmod data=&lib..&CombinedDSN;
class r c;
model &depvar= r c f/dist=poi;
title2 'F Model';

proc genmod data=&lib..&CombinedDSN;
class r c;
model &depvar= r c u/dist=poi;
title2 'U Model';

proc genmod data=&lib..&CombinedDSN;
class r c v1-v&b;
model &depvar= r c v1-v&b /dist=poi;
title2 'V Model';

proc genmod data=&lib..&CombinedDSN;
class r c;
model &depvar= r c L /dist=poi;
title2 'L Model';

proc genmod data=&lib..&CombinedDSN;
class r c t;
model &depvar= r c t /dist=poi;
title2 'T Model';

proc genmod data=&lib..&CombinedDSN;
class r c q;
model &depvar= r c q /dist=poi;
title2 'Q Model';

proc genmod data=&lib..&CombinedDSN;
class r c d;
model &depvar= r c d /dist=poi;
title2 'D Model';

proc genmod data=&lib..&CombinedDSN;
class r c da;
model &depvar= r c da /dist=poi;
title2 'DA Model';

proc genmod data=&lib..&CombinedDSN;
class r c da t;
model &depvar= r c da t /dist=poi;
title2 'DAT Model';

proc genmod data=&lib..&CombinedDSN;
class r c;
model &depvar= r c u f /dist=poi;
title2 'UF Model';

*** Fit Non-symmetry + independence models ***;
*** O, F, U, T, D, DA, and DAT respectively ***;
proc genmod data=&lib..&CombinedDSN;
model &depvar= h1-h&b /dist=poi;
title 'Non-symmetry and Independence Model';
title2 'O Model';

proc genmod data=&lib..&CombinedDSN;
model &depvar= h1-h&b f /dist=poi;
title2 'F Model';

proc genmod data=&lib..&CombinedDSN;
model &depvar= h1-h&b u /dist=poi;
title2 'U Model';

proc genmod data=&lib..&CombinedDSN;
class t;
model &depvar= h1-h&b t /dist=poi;
title2 'T Model';

proc genmod data=&lib..&CombinedDSN;
class d;
model &depvar= h1-h&b d /dist=poi;
title2 'D Model';

proc genmod data=&lib..&CombinedDSN;
class da;
model &depvar= h1-h&b da /dist=poi;
title2 'DA Model';

proc genmod data=&lib..&CombinedDSN;
class da t;
model &depvar= h1-h&b da t /dist=poi;
title2 'DAT Model';
run;
%mend analysis;
