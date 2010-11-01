   ***************************************
   create vectors for
   S symmetry
   L vertical mobility
   T Triangles
   Q Quasi-Independence factor variable
   DA Diagonal Absolute
   D Diagonal factor variable
   F Fixed distance
   OS1 Odds symmetry I
   OS2 Odds symmetry II
   ****************************************;
%macro square(
	size,
	row = R,
	col = C,
	sym = S,
	data=,
	out=generate);


options nonotes;
data generate1;
   do R=1 to &size;
   do C=1 to &size;
	  if R lt C then
		 do;
		 k=abs(R-C);
		 S=k+1-(R+1)*((0.5*R)+1)+(&size+3)*(R+1)-3-2*&size;
		 L=1;
		 T=1;
		 Q=&size+1;
		 DA=abs(R-C);
		 D=abs(R-C);
		 F=abs(R-C)+1;
		 OS1=R;
		 OS2=2*&size-C;
		 end;

	  else if R gt C then
		 do;
		 k=abs(R-C);
		 S=(k+1)-(C+1)*(.5*C+1)+(&size+3)*(C+1)-3-2*&size;
		 L=1;
		 T=2;
		 Q=&size+1;
		 DA=abs(R-C);
		 D=abs(R-C)+(&size-1);
		 F=1;
		 OS1=(&size-1)+C;
		 OS2=(&size+1)-R;
		 end;

	  else /*if R eq C then */
		 do;
		 S=1-(C+1)*(.5*C+1)+(&size+3)*(C+1)-3-2*&size;
		 L=2;
		 T=3;
		 Q=R;
		 DA=&size;
		 D=2*&size-1;
		 F=1;
		 OS1=2*&size-1;
		 OS2=2*&size-1;
		 end;

	  U=R*C;
	  output;
	  drop k;
	  end; end;
	  label 
		R = 'Row'
	  	C = 'Col'
		S = 'Symmetry'
		T = 'Triangle Asymmetry'
		Q = 'Quasi Independence'
		DA = 'Diagonal Absolute'
		D = 'Diagonals Asymmetry'
		L = 'Uniform loyalty'
		;
   run;

* generate FF,SS, H, and DD vectors;
* use ID variable to help in merging with the V vectors;
data generate2;
   set generate1;
   ID=_N_;
   ARRAY FF(&size);
   ARRAY SS(&size);
   ARRAY H(&size);
   ARRAY DD(&size);
   do i=1 TO &size;
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
%do h=1 %to %eval(&size-1);
   data vdata&h;
   ID=0;
   %do i=1 %to &size;
   %do j=1 %to &size;
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
data &out(drop=ID);
   merge generate2
   %do k=1 %to %eval(&size-1);
	  vdata&k
	  %end;
   ;
   by ID;
   run;

options notes;
%if %length(&data) %then %do;
data &out;
	merge &data &out;
	%end;

proc datasets nolist nowarn;
	delete generate1 generate2
   %do k=1 %to %eval(&size-1);
	  vdata&k
	  %end;
	;
%mend;
/*
%square(4);
proc print;
*/
