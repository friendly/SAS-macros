 /****************************************************************/
 /*          S A S   S A M P L E   L I B R A R Y                 */
 /*                                                              */
 /*    NAME: JOHNSYS4                                            */
 /*   TITLE: Johnson System of Distributions (Macro)             */
 /* PRODUCT: QC                                                  */
 /*  SYSTEM: ALL                                                 */
 /*    KEYS: SQC                                                 */
 /*   PROCS: IML CAPABILITY(w/ ANNOTATE DATASET) MACRO           */
 /*    DATA:                                                     */
 /*                                                              */
 /*   NOTES:                                                     */
 /*    MISC: Macro JOHNSYS takes as input the data set (stored   */
 /*          as macro variable DATA). The proper Johnson system  */
 /*          is selected using the ratio given in Slifker and    */
 /*          Shenton (1980).  The parameter estimates for the    */
 /*          appropriate system are then computed and displayed  */
 /*          along with the johnson curve on a histogram.        */
 /*                                                              */
 /*     REF: Slifker, J. F. and Shapiro, S. S. (1980), "The      */
 /*          Johnson System: Selection and Parameter Estimation",*/
 /*          Technometrics, Vol. 22, No. 2, pg. 239-246.         */
 /*                                                              */
 /*          Bowman, K. O. and Shenton, L. R. (1983), "Johnson's */
 /*          System of Distributions", Encyclopedia of           */
 /*          Statistical Sciences, Vol. 4, pg. 303-314.          */
 /*          John Wiley & Sons, Inc.                             */
 /*                                                              */
 /****************************************************************/


 /*------------------------------*/
 /*                              */
 /* The JOHNSYS macro definition */
 /*                              */
 /*------------------------------*/

%macro johnsys(data=,var=, freq=);

%*-- Expand dataset in frequency form;
%if "&freq" ^= "" %then %do;
data _new_;
	set &data;
	do i=1 to &freq;
		output;
		end;
	drop i;
%let data=_new_;
%end;

proc iml;

sort &data out=sorted by &var;
use sorted;

read all var {&var} into x;

nobs=nrow(x);

 /*---  Choose z-value for percentile fit ---*/

zval  = .524;
reset print log;
p3    = probnorm(3*zval);
p1    = probnorm(zval);
pm1   = probnorm(-1*zval);
pm3   = probnorm(-3*zval);

i4    = p3*nobs  + .5;
i3    = p1*nobs  + .5;
i2    = pm1*nobs + .5;
i1    = pm3*nobs + .5;

x3z   = x[int(i4)] + mod(i4,1)*(x[int(i4)+1] - x[int(i4)]);
x1z   = x[int(i3)] + mod(i3,1)*(x[int(i3)+1] - x[int(i3)]);
xm1z  = x[int(i2)] + mod(i2,1)*(x[int(i2)+1] - x[int(i2)]);
xm3z  = x[int(i1)] + mod(i1,1)*(x[int(i1)+1] - x[int(i1)]);

m     = x3z  - x1z;
n     = xm1z - xm3z;
p     = x1z  - xm1z;

 /*--- Ratio used to choose proper Johnson system ---*/

ratio = m*n/p**2;

tol   = .05;

 /*---Select appropriate Johnson Family & Estimate Parameters---*/

if (ratio > 1.0 + tol) then do;

 /*--- Johnson Su Parameter Estimates ---*/

   temp    = .5*(m/p + n/p);
   eta     = 2*zval/(log(temp + sqrt(temp*temp -1 )));

   temp    = (n/p - m/p) / ( 2*(sqrt(m*n/(p*p) - 1)) );
   gamma   = eta*log(temp + sqrt(temp*temp + 1));

   lambda  = 2*p*sqrt(m*n/(p*p)-1)/((m/p+n/p- 2)*sqrt(m/p+n/p+2));
   epsilon = (x1z + xm1z)/2 + p*(n/p - m/p) / ( 2*(m/p+n/p-2));

   create parms var{eta gamma lambda epsilon};
   append;
   close parms;
   type = '1';
   end;

else if (ratio < 1.0 - tol ) then do;

 /*--- Johnson Sb Parameter Estimates ---*/

   temp    = .5* sqrt( (1 + p/m)*(1 + p/n) );
   eta     = zval / log( temp + sqrt(temp*temp - 1) );

   temp    = (p/n-p/m)*sqrt((1+p/m)*(1+p/n)- 4)/(2*(p*p/(m*n)-1));
   gamma   = eta*log(temp + sqrt(temp*temp + 1) );

   lambda  = p*sqrt(( (1 + p/m)*(1+p/n)-2)**2 - 4)/(p*p/(m*n)-1);
   epsilon = (x1z + xm1z)/2-lambda/2+p*(p/n-p/m)/(2*(p*p/(m*n)-1));

   create parms var{eta gamma lambda epsilon};
   append;
   close parms;
   type = '2';

   end;

else do;

   /* Johnson Sl Parameter Estimates */

   eta     = 2*zval / log(m/p);
   gamma   = eta*log( (m/p - 1) / (p*sqrt(m/p)) );
   epsilon = (x1z + xm1z)/2 - (p/2)* (m/p + 1) / (m/p - 1);

   /* create dataset containing parameters */
   create parms var{gamma eta epsilon};
   append;
   close parms;
   type = '3';

   end;

call symput('type',type);

quit;

 /*---------------------------------------*/
 /*                                       */
 /* Use PROC CAPABILITY to save width of  */
 /* histogram bars in OUTFIT= dataset.    */
 /*                                       */
 /*---------------------------------------*/

proc capability data=&data noprint;
   hist &var / outfit = out1 normal(noprint) noplot;
   run;

 /*------------------------------------------------------------*/
 /*                                                            */
 /* Create annotate dataset that will superimpose the johnson  */
 /* curve and a table containing the parameter estimates onto  */
 /* a histogram.                                               */
 /*                                                            */
 /*------------------------------------------------------------*/

data anno;
   merge parms out1;
   length function style color cepsilon clambda cgamma ceta $ 8
          text $ 20;
   function = 'point';
   color    = 'yellow';
   size     = 2;
   xsys     = '2';
   ysys     = '2';
   when     = 'a';
   pi       = 3.14159;

   /*--- Determine constant based on Johnson family ---*/

   %if (&type = 1) %then %do;
      constant = (100*_width_/lambda)*(eta/sqrt(2*pi));
      %end;

   %else %if (&type = 2) %then %do;
      constant = (100*_width_/lambda)*(eta/sqrt(2*pi));
      %end;

   %else %do;
      constant = (100*_width_*eta/sqrt(2*pi));
      %end;

   start    = _midpt1_ - .5*_width_;
   finish   = _midptn_ + .5*_width_;

   /*--- Draw Johnson Curve ---*/

   do x=start to finish by .01;

      %if (&type = 1) %then %do;
         a        = (x-epsilon)/lambda;
         invsinh  = log(a + sqrt(a**2 + 1));
         y        = constant*
                    (1/sqrt(1+a**2))*
                    exp(-.5*(gamma+eta*invsinh)**2);
         %end;

      %else %if (&type = 2) %then %do;
         if (x > epsilon) then do;
            a     = (x-epsilon)/lambda;
            fy    = log(a / (1 - a ));
            y     = constant*
                    (1/(a*(1-a)))*
                    exp(-.5*(gamma + eta*fy)**2);
            end;
         else y = 0;
         %end;

      %else %do;
         if (x > epsilon) then do;
            a     = (x-epsilon);
            y     = constant*
                    (1/a)*
                    exp(-.5*(gamma + eta*log(a))**2);
            end;
         else y = 0;
         %end;

      output;
      function ='draw';
      end;

   %if (&type= 3) %then %do;
      lambda = 0;
      %end;

   /*--- Format Parameters with 3 Decimal Places ---*/

   cepsilon = put(epsilon,6.3);
   cgamma   = put(gamma,  6.3); ceta = put(eta,   6.3);

   %if (&type = 1) or (&type = 2) %then %do;
      clambda = put(lambda,6.3);
      %end;

 /*--- Find Length of Text String (Use Length Drawing Table) ---*/

   x0 = 0;
   y0 = 0;
   rc = ginit();
   rc = gset('texheight', 3);
   rc = gset('texfont','swiss');
   %if (&type = 1) %then %do;
      call gask('texextent', x0, y0, 'Johnson (Su)',xend,yend,
                            x1,x2,x3,x4,y1,y2,y3,y4,rc);
      %end;
   %else %if (&type = 2) %then %do;
      call gask('texextent', x0, y0, 'Johnson (Sb)',xend,yend,
                            x1,x2,x3,x4,y1,y2,y3,y4,rc);
      %end;

   %else %do;
      call gask('texextent', x0, y0, 'Johnson (Sl)',xend,yend,
                            x1,x2,x3,x4,y1,y2,y3,y4,rc);
      %end;

   rc  = gterm();
   len = xend - x0;

   /*--- Draw Table containing parameter estimates ---*/

  function = 'move';      xsys = '1';
                          ysys = '1';
                          x = 99; y = 99;          output;
  function = 'move';      xsys = '9';
                          ysys = '9';
                          x = -1; y = -5;          output;
  function = 'cntl2txt';                           output;
  function = 'push';                               output;
  function = 'label';     xsys     = '3';
                          ysys     = '3';
                          hsys     = '3';
                          size     =  3;
                          position = 'a';
                          style    = 'swiss';
                          color    = 'white';
                          x = .; y = .;
   %if (&type= 1) %then %do;
                          text = 'Johnson (Su)'; output; %end;
   %else %if (&type= 2) %then %do;
                          text = 'Johnson (Sb)'; output; %end;
   %else %do;
                          text = 'Johnson (Sl)'; output; %end;

   function = 'move';     xsys = '9';
                          ysys = '9';
                          x = -len; y = 0;         output;
   function = 'move';     x = -1;   y = 1.5;       output;
   function = 'draw';     color = 'yellow';
                          hsys  = '4';
                          size  =  2;
                          x = -5; y = 0;           output;

   function = 'pop';                               output;
   function = 'move';     x =.;      y =.;         output;
   function = 'push';                              output;
   function = 'move';     x =-len-6; y =-1;        output;
   function = 'cntl2txt';                          output;
   function = 'label';    xsys     = '3';
                          hsys     = '3';
                          size     =  3;
                          color    = 'white';
                          position = 'c';
               x = .;  y = -5;  text = 'Epsilon';  output;
   function = 'cntl2txt';                          output;
   function = 'label';
                       y = -10; text = 'Gamma';    output;
   function = 'cntl2txt';                          output;
   function = 'label';
                       y = -15; text = 'Eta';      output;

   %if (&type = 1) or (&type = 2) %then %do;
   function = 'cntl2txt';                          output;
   function = 'label';
                       y = -20; text = 'Lambda';   output;
   %end;

   function = 'pop';                               output;
   function = 'move'; x=.; y=.;                    output;
   function = 'push';                              output;
   function = 'move'; y=-1;                        output;
   function = 'cntl2txt';                          output;
   function = 'label'; position = 'a';  x = .;
                       y = -5; text = cepsilon;    output;
                       y = -5; text = cgamma;      output;
                       y = -5; text = ceta;        output;
   %if (&type = 1) or (&type = 2) %then %do;
                       y = -5; text = clambda;     output;
   %end;

   %if (&type = 3) %then %do;
      %let y = 23;
      %end;
   %else %do;
      %let y = 28;
      %end;

   function = 'move';     xsys = '1';
                          ysys = '1';
                          x = 99; y = 99;          output;
   function = 'poly';     xsys  = '1';
                          ysys  = '1';
                          style = 'solid';
                          color = 'blue';
                          x = 99;  y = 99;         output;
   function = 'polycont'; xsys = '9';
                          ysys = '9';
                          x = -8-len;  y = -999;   output;
                          x = -8-len;  y = -&y;    output;
                          x = -999;    y = -&y;    output;

   function = 'move';     x = .;       y = -6.5;   output;
   function = 'draw';     size  = 1;
                          hsys  = '4';
                          color = 'white';
                          x = -8-len; y = 0;       output;

   keep function xsys  ysys     hsys  x     y
        text     size  position style color when;

   run;


 /*--------------------------------------------------------------*/
 /*                                                              */
 /* Display histogram with Johnson curve and parameter estimates */
 /*                                                              */
 /*--------------------------------------------------------------*/

pattern1 c=red v=s;

proc capability data=&data graphics annotate=anno noprint;
   hist &var / cframe = gray;
   run;

%mend;
