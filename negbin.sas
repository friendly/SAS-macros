 /***************************************************************/
 /*                           SAS SAMPLE LIBRARY                */
 /*                                                             */
 /*                                                             */
 /* NAME   : negbin.sas                                       */
 /* TITLE  : Estimation of Heterogeneity Parameter in GENMOD    */
 /* PRODUCT: SAS                                                */
 /* SYSTEM : ALL, VERSIONS 6.08 AND ABOVE                       */
 /* KEYS   : generalized linear models,                         */
 /* PROCS  : GENMOD                                             */
 /* AUTHOR : Joseph Hilbe, Arizona State University, Tempe, AZ  */
 /*          Internet: atjmh@asuvm.inre.asu.edu                 */
 /*                                                             */
 /* REF    : Hilbe, Joseph (1994), Log Negative Binomial        */
 /*             Regression Using the GENMOD Procedure SAS/STAT  */
 /*             Software, Proceedings of SUGI 19, SAS Institute.*/
 /*          Hilbe, Joseph (1994), Log Negative Binomial        */
 /*             Regressionas a Generalized Linear Model,        */
 /*             Technical Report26, Graduate College Committee  */
 /*             on Statistics,Arizona State University, Tempe,  */
 /*             AZ 85287.                                       */
 /*                                                             */
 /* MISC:                                                       */
 /*  Modified by M. Friendly                                    */
 /***************************************************************/

 /***************************************************************
 Fits an obsrdispersed generalized linear model, where the mean and
 variance functions are:

  E(Y) = mu
  V(Y) = mu + alpha * mu**2 = mu + mu**2/theta
  
 hence, equivalent to a Poisson distribution with extra-Poisson
 variation acording to a gamma distribution with shape parameter theta.
 
 
    USAGE:                                                       
    1. Load data to be modeled into memory                       
    2. Load this macro into memory: negbin.sas                 
    3. Call macro with desired options                           
                                                                 
      %negbin(data=<data name>, y=<response>,               
      xvars=<predictors>, class=<factors>, offset=<offset>,    
      ithist=<default=0, set 1 to show iteration history>, 
		expected=<default=0, use observed information matrix; 1 = expected
		 Fisher information matrix.                                        
                                                                 
    EXAMPLE:                                                     
    Data set named lnbex with response xnb, two predictors x1    
    and x2,                                                      
    a factor f1, and offset of1.                                 
                                                                 
    %negbin(data=lnbex, y=xnb, xvars=x1 x2 f1, class=f1,   
    offset=of1);                                                 
                                                                 
 ***************************************************************/


%macro negbin(data=,
	y=,  resp=,
	xvars=, model=,
	class=,
	offset=,
	ithist=0,
	expected= 0,
	maxiter=50,
	obstats=obstats
	);

*title 'Log Negative Binomial Regression';  *-- use existing title(s);

* Turn off printing;
%global _print_;
%let _print_ = OFF;

%*let maxiter = 50;      %*-- Now an input parameter;
%let iter = 1;
%let conv = 0;
%let abort = 0;

%if %length(&offset) > 0 
	%then %let offstmt = OFFSET=&offset;
	%else %let offstmt= ;

%if %length(&class) > 0 
	%then %let clsstmt = %str(CLASS &class;);
	%else %let clsstmt= ;

%if(&expected=1) 
	%then %let expstmt = EXPECTED ;
	%else %let expstmt= ;

%*-- Handle parameter synonyms;
%if %length(&y)=0 %then %let y=&resp;
%if %length(&y)=0 %then %do;
	%put ERROR:  You must supply a Y= or RESP= response variable.;
	%let abort=1;
	%goto done;
	%end;
	
%if %length(&xvars)=0 %then %let xvars=&model;
%if %length(&xvars)=0 %then %do;
	%put ERROR:  You must supply a XVARS= or MODEL= list.;
	%let abort=1;
	%goto done;
	%end;
	
* Data is first modeled using Poisson regression;
proc genmod data=&data;
   &clsstmt
   make 'modfit' out=modfit;
   model &y = &xvars / dist   = poisson
                          &offstmt;
   run;

data _NULL_;
   %if( &ithist = 1 ) %then %str(file print;);
   set modfit;
   if _N_ = 3 then
      do;
         call symput( 'disp', put( valuedf, best10.6 ) );
         %if( &ithist = 1 ) %then
            %do;
               temp2 = 1/valuedf;
               put 'Iteration number: ' "&iter";
               put 'Pearson Chi2/DF:  ' valuedf;
               put 'modfit alpha:     ' temp2;
            %end;

      end;
   run;

%let alpha = 1 / &disp;

*  Iterate on NB model;
*  Iterate until dispersion stops changing;
%do %while( &conv = 0 ) ;
	options nonotes;
*  Data is now modeled using a log-linked negative binomial;
proc genmod data=&data;
   &clsstmt
   make 'modfit' out = modfit;

   _K = &alpha;
   _A = _MEAN_;
   _Y = _RESP_;
   variance _VAR = _A+_K*_A*_A;
   if (_Y>0) then
      _D = 2 * (_Y*log(_Y/_A)-
                           (1+_K*_Y)/_K * log((1+_K*_Y)/(1+_K*_A)));
   else if (_Y=0) then _D = 2 * log(1+_K*_A)/_K;
   deviance _DEV = _D;
   model &y = &xvars  / &offstmt link = log itprint;
   run;

%let iter = %eval( &iter + 1 );

data _NULL_;
   %if( &ithist = 1 ) %then %str(file print;);
   set modfit;
   if criterio='Deviance' then      /* _N_ = 1 */
      do;
         call symput( 'deviance', put( value, 10.4 ) );
         call symput( 'devdf', put( valuedf, 10.4 ) );
      end;

   if criterio='Pearson Chi-Square' then     /* _N_ = 3 */
      do;
         call symput( 'PX2', put( value, 10.4 ) );
         call symput( 'PX2df', put( valuedf, 10.4 ) );

         temp3 = &alpha;
         if ( ABS( valuedf - &disp ) <= 1.e-3 OR &iter > &maxiter )
            then  call symput( 'conv', '1' ) ;
         else
            do;
               temp3 = valuedf * temp3;
               call symput( 'disp', put( valuedf, best10.6 ) );
               call symput( 'alpha', put( temp3, best20.10 ) );
            end;
         %if( &ithist = 1 ) %then
            %do;
               put 'Iteration number: ' "&iter";
               put 'Pearson Chi2/DF:  ' valuedf;
               put 'Alpha:            ' temp3;
            %end;
      end;
   run;
* Print final model;
%if ( &conv = 1 )  %then
   %do;
	   %let _print_ = ON;
		options notes;
		%put NOTE: NGEBINOM converged at iteration &iter, alpha=&alpha;

      proc genmod data=&data;
         &clsstmt
         make 'modfit'  out = modfit;
         make 'parmest' out = parmest;
			%if %length(&obstats) %then %do;
         make 'obstats' out = &obstats noprint;
			%end;
         _K = &alpha;
         _A = _MEAN_;
         _Y = _RESP_;
         variance _VAR = _A + _K*_A*_A;

         _D = -2 * (_Y*log(_K*_A)-(_Y+1/_K)*log(1+_K*_A)+
                 lgamma(_Y+1/_K)-lgamma(_Y+1)-lgamma(1/_K));

         deviance _DEV = _D;
         model &y = &xvars  / 
				&offstmt &expstmt link = log 
				%if %length(&obstats) %then %do;
				obstats residuals
				%end;
				;
         run;

data &obstats;
	merge &data 
		&obstats;
run;
/*
$calc j=%cu(1)-1$
$calc s1=%cu(%log(th+j)) : s2=-%cu(1/(th+j)**2) : lfac=%cu(%log(j+1)) $
$calc inf=-%cu(%pw*(s2(%yv)+(th+%yv)/(th+%fv)**2-2/(th+%fv)+1/th))$
$calc llik=2*%cu(%pw*(s1(%yv)+%yv*%log(%fv)+th*%log(th)
            -(th+%yv)*%log(%fv+th)))$
*/

*-- Find std error of theta;
proc iml;
	use &obstats;
	read all var{&y}   into Y;
	read all var{pred} into mu;

	wt=1;	
	n = nrow(y);
	theta = 1/&alpha;
	j = cusum( j(n,1) )-1;
	s1 = cusum( log(theta+j) );
	s2 = cusum( 1/(theta+j)##2 );
	inf = sum(wt# (s2[y]+(theta+Y)/(th+mu)**2-2/(th+mu)+1/th) );
	se = -1/inf;
	print s1 s2 inf se;
	
data _NULL_;
   file print;
   set modfit end=eof;
   if eof then do;
      iter = &iter;
      alpha = &alpha;
		theta = 1/&alpha;
      put 'Number of iterations: ' iter  10.0 ;
      put 'Alpha:                ' alpha 10.4 '    '
		    'Theta                 ' theta 10.4;
      put 'Deviance:             ' "&deviance    "
          'Deviance/DF:          ' "&devdf";
      put 'Pearson Chi2:         ' "&PX2    "
          'Pearson Chi2/DF:      ' "&PX2df";
      put 'LogLikelihood:        ' value 10.4;
      end;

   run;

   %end;

* End DO loop;
%end;
proc print data=parmest label;
	id parm;
   run;


%done:
%if &abort %then %put ERROR: The NEGBIN macro ended abnormally.;
%let _print_ = ON;
%mend ;
