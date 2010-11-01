 /*=
=Description:

 The GL macro ("generate levels") is used in a DATA step to create
 factor variables when only the data values are read in.  It is used
 in a DATA step as a function,

   factor = %gl(k, n);

 to create a numeric classification variable with values 1 to k, in
 blocks of n. It mimicks the %gl() function in the GLIM software package.


=Example:

 For data in a 3 x 4 table, where the first factor (A) has 3 levels and
 varies most slowly, and the second factor (B) has 4 levels:

    data mydata;
       input y @@;
       A = %gl(3, 4);
       B = %gl(4, 1);
    cards;
      12 32 56 64
      18 28 46 59
       9 21 40 23
 =*/

%macro gl(k,n);
  %* use in the DATA step;
  1+mod(int((_N_-1)/&n),&k);
%mend;
