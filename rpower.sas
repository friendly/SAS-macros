/*-------------------------------------------------------------------*
 *    Name: rpower.sas                                               *
 *   Title: retrospective power analysis for univariate GLMs.        *
 *                                                                   *
 | Macro for retrospective power analysis for a PROC GLM analysis.   |
 | The program reads the OUTSTAT= data set constructed in a PROC GLM |
 | step.  For each effect tested, the program calculates the nominal |
 | power of the test, if the sample means were population values.    |
 | For specified alpha and power values, the required sample size    |
 | to achieve that power is also determined.                         |
 |                                                                   |
 | Usage:                                                            |
 |    %include rpower;                                               |
 |    proc glm data=  outstat=STATS;                                 |
 |       class  classvars;                                           |
 |       model  depvars = independents / options;  * use SSn option; |
 |       contrast 'name' effect {coefficients};                      |
 | Examples:                                                         |
 |    %rpower(data=STATS, alpha=.01, power=.90)                      |
 |    %rpower(data=STATS, alpha=.01, power=.80 to .90 by .05)        |
 |    %rpower(data=STATS, alpha=%str(.01, .05))                      |
 |                                                                   |
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <friendly@yorku.ca>         *
 * Created:  3 Apr 1991 12:34:23                                     *
 * Revised:  2 Nov 2000 07:28:54                                     *
 * Version:  1.1                                                     *
 *  1.1 Fixed V8 lc varname bug, but N calc gives errors             *
 *-------------------------------------------------------------------*/
%macro rpower(
       data=_last_,     /* outstat= data set from GLM     */
       alpha = .05,     /* alpha level of tests           */
       power = .90,     /* required power                 */
       sigma = 1,       /* alternate estimate of root MSE */
       out=_power_      /* name of output dataset         */
		 );

	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options /*nonotes*/ validvarname=upcase;
		%end;
	%else %do;
	   options nonotes;
		%end;

data &out;
   set &data(rename=(_source_=source));
   retain SSE DFE MSE;
   drop ss sse dfe mse fcrit fc prob ;
   drop t n0 b b0 it ncx;
   format prob power powera 5.3 f nc nca 6.2 n totn 5.1;
   label power = 'Nominal power'
         powera= 'Adj. power'
         beta  = "Req'd power"
         nc    = 'Non- Centrality'
         nca   = 'Adj_Non- Centrality'
         f     = 'F Value'
         n     = "Req'd N"
			totn  = 'Total N';
 
   if upcase(source)='ERROR' then do;
      SSE = SS; DFE = DF;         /* save SSE & DF values */
      MSE = SSE/DFE;
      if MSE = . then MSE = &sigma **2;
      return;
      end;
   else do;
      do alpha = &alpha ;
      do beta  = &power ;
         *-- Calculate F non-centrality (OBrien, 1988, p.1098);
      *  nc = df * F;
         nc = SS / MSE;
         *-- Adjusted F non-centrality, correcting for bias;
         nca= max( 0, ( nc * (dfe-2)/dfe )-df );
         *-- Find power from non-centrality;
         fcrit = finv(1-alpha, df, dfe);
         power = 1 - probf(fcrit, df, dfe, nc );
         powera= 1 - probf(fcrit, df, dfe, nca);
         *-- Find sample size;
         link findn;
			totn = n * (df+1) * (n-1);
         output;
      end; /* beta  */
      end; /* alpha */
   end;    /* not ERROR */
 
findn:
      *-- Find sample size required to give power=beta;
          * uses simple secant method;
      if alpha >= beta then do; n=0; return; end;
      if beta > .999 then do; n=. ; beta=.; return; end;
      n = 1.1;
      fc = finv(1-alpha, df, (df+1)*(n-1));
      b0=1-probf(fc,df,(df+1)*(n-1),n*nc);
 
      if b0 > beta then do;
         n=1;
         return;
      end;
 
      n0=n;
      n=n+1;
      it=0;
      ncx = nca;
      if ncx=0 then ncx = nc;
      fc = finv(1-alpha, df, (df+1)*(n-1));
      b=1-probf(fc,df,(df+1)*(n-1),n*ncx);
      do while( abs(beta - b) > .00001 );
         it=it+1;
         if it>10 then goto done;
         t=n;
         n=n0+(beta-b0)*(n-n0)/(b-b0);
         if ( n <= 1.1 ) then n=max(t/2,1.1);
*     put it= beta= b= b0= n= nc= nca=;
         n0=t; b0=b;
         fc = finv(1-alpha, df, (df+1)*(n-1));
         b=1-probf(fc,df,(df+1)*(n-1),n*ncx);
      end;
done: if b=. then n=.;
      return;
proc print data=&out split=' ';
   id source  _name_;

%exit:
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
%mend;
