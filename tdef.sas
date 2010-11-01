%macro tdef(nv, size, shift );
%* ---------------------------------------------------------------;
%* title: Generate a TDEF statement for a scatterplot matrix             ;
%* Start with (1,1) panel in upper left, and copy it across & down;
%* ---------------------------------------------------------------;
%local i j panl panl1 lx ly;
 
   TDEF scat&nv DES="scatterplot matrix &nv x &nv"
   %let panl=0;
   %let lx = &size;
   %let ly = %eval(100-&size);
   %do i = 1 %to &nv;
   %do j = 1 %to &nv;
       %let panl  = %eval(&panl + 1);
       %if &j=1 %then
          %do;
             %if &i=1 %then %do;      %* (1,1) panel;
               &panl/
                ULX=0  ULY=100   URX=&lx URY=100
                LLX=0  LLY=&ly   LRX=&lx LRY=&ly
                %end;
             %else
                %do;                  %* (i,1) panel;
                   %let panl1 = %eval(&panl - &nv );
               &panl/ copy= &panl1 xlatey= -&shift
                %end;
          %end;
       %else
          %do;
               %let panl1 = %eval(&panl - 1);
               &panl/ copy= &panl1 xlatex= &shift
          %end;
   %end;
   %end;
     %str(;);      %* end the TDEF statement;
%mend;
