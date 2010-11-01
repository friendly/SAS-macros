/*-------------------------------------------------------------------*
  *    Name: MPOWER.SAS                                              *
  *   Title: retrospective power analysis for multivariate GLMs.     *
 |                                                                   |
 | The program reads the OUTSTAT= data set constructed in a PROC GLM |
 | step.  For each effect tested, the program calculates the nominal |
 | power of the test, if the sample means were population values.    |
 |                                                                   |
 | Usage:                                                            |
 |    %include mpower;                                               |
 |    proc glm data=  outstat=STATS;                                 |
 |       class  classvars;                                           |
 |       model  depvars = independents / options;  * use SSn option; |
 |       contrast 'name' effect {coefficients};                      |
 | Examples:                                                         |
 |    %mpower(data=STATS, yvar=depvars)                              |
 |    %mpower(data=STATS, yvar=depvars, alpha=.01, tests=WILKS ROY)  |
 |                                                                   |
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <friendly@yorku.ca>         *
 * Created:  11 Aug 1991 14:12:31                                    *
 * Revised:  17 Nov 2003 13:19:35                                    *
 * Version:  1.0                                                     *
 *                                                                   *
 *-------------------------------------------------------------------*/
%macro mpower(
        yvar=,               /* list of dependent varriables          */
        data=_last_,         /* outstat= data set from GLM            */
        out=_data_,          /* name of output data set with results  */
        alpha=.05,           /* error rate for each test              */
        tests=WILKS PILLAI LAWLEY ROY   /* tests to compute power for */
		  );
 
%if %length(&yvar)=0 %then %do;
    %put MPOWER: YVAR= must specify list of dependent variables;
    %goto exit;
    %end;
proc iml;
start mstats(h, e, dfh, dfe, stats, df, alpha, tests, power)
      global(verbose);
   call geneig(roots, vectors, h, e);
   theta = roots / (1+roots);
   p = nrow(h);
   s = min(p,dfh);                   * non-zero roots;
   m = .5#(abs(p-dfh)-1) ;
   n = (dfe - p - 1)/2;
   reset name fuzz;
   if verbose>0 then print roots theta s m n;
   free stats df pow;
 
      *-- Wilks lambda --;
   if any(tests=1) then do;
      lambda  = (1/(1+roots[1:s]))[#];
      pdf = p##2 + dfh##2 -5;
      if pdf <=0
         then t = 1;
         else t = sqrt((p##2 # dfh##2 - 4) / (p##2 + dfh##2 - 5));
      a = dfe +dfh - (p+dfh+1)/2;
      df1= p#dfh;
      df2= (t#a)- (df1/2) + 1;
      fw = ((1 - lambda##(1/t)) / df1)
         / ((    lambda##(1/t)) / df2);
      eta= 1 - lambda##(1/s);
      run mpower(fw, dfh, df1, df2, alpha, nc, power);
      stats = (lambda || fw);
      df = df1 || df2;
      pow = eta|| nc || power;
      end;
 
      *-- Pillai trace --;
   if any(tests=2) then do;
      pillai = sum(theta[1:s]);
      df1 = s # (2#m + s + 1);
      df2 = s # (2#n + s + 1);
      fp  = (pillai/(s-pillai)) # (df2/df1);
      eta = pillai/s;
      run mpower(fp, dfh, df1, df2, alpha, nc, power);
      stats = stats // (pillai || fp);
      df = df // (df1 || df2);
      pow = pow // (eta || nc || power);
      end;
 
      *-- Lawley-Hotelling trace --;
   if any(tests=3) then do;
      lawley = sum(roots[1:s]);
      df2 = s#(dfe-p-1) + 2;
      fl = (lawley # df2) / (s#df1);
      eta= lawley / sum( 1 / (1 - (theta[1:s])) );
      run mpower(fl, dfh, df1, df2, alpha, nc, power);
      stats = stats // (lawley || fl);
      df = df // (df1 || df2);
      pow = pow // (eta || nc || power);
      end;
 
      *-- Roy maximum root --;
   if any(tests=4) then do;
      roy = roots[1];
      df1 = max(p,dfh);
      df2 = dfe - df1 + dfh;
      fr  = roy # (df2/df1);
      eta = theta[1];
      run mpower(fr, dfh, df1, df2, alpha, nc, power);
      stats = stats // (roy || fr); df = df // (df1 || df2);
      pow = pow // (eta || nc || power);
      end;
 
      if verbose>0 then do;
      sname = {"Wilks' Lambda" "Pillai's Trace" "Lawley Trace"
               "Roy's max. Root"}[union(tests)];
      reset noname;
      print stats[r=sname c={"Value" "F"} format=8.3]
            df [c={"df1" "df2"} format=5.0]
            pow[c={"Eta##2" "Non-Cent." "Power"} format=best6.];
      end;
      power = pow;
finish;
start mpower(f, dfh, df1, df2, alpha, nc, power);
     nc    = f # dfh;
     fcrit = finv(1-alpha, df1, df2);
     if nc > 100 then power=1;
                 else power = 1 - probf(fcrit, df1, df2, nc );
finish;
 
   use &data;
   read all var{&yvar}   where(_type_='ERROR') into e[r=_name_ c=var];
   read all var{df}      where(_type_='ERROR')  into dfe[r=_name_];
   dfe = dfe[1];
 
   read all var{&yvar}   where(_type_^='ERROR') into ssh[r=_name_ c=var];
   read all var{df}      where(_type_^='ERROR') into dfs[r=_source_];
   read all var{f }      where(_type_^='ERROR') into uf[r=_source_];
   read all var{_source_ _type_}
        where (_type_^='ERROR') into id;
   close &data;
   slabl={WILKS PILLAI LAWLEY ROY};
   t = {&tests};
   do i = 1 to 4;
      if ncol(union(slabl,t[i])) = ncol(slabl) then do;
         tests=tests || i;
         sl = sl // slabl[i];
         end;
      end;
   p = nrow(e);
   effects = nrow(ssh) / p;
   verbose = 1;
   do i = 1 to effects;
      r1 = 1 + p#(i-1);
      r2 =     p#i;
      dfh= dfs[r1];
      f  = uf [r1];
      h = ssh[r1:r2,];
      effect = trim(id[r1,]);
      reset name;
      rlabl = repeat(effect,nrow(sl),1) || sl;
      alpha = &alpha;
         print 'Power analysis for' effect alpha;
         run mstats(h, e, dfh, dfe, stats, df, alpha, tests, power);
         out = out // (repeat(alpha,nrow(sl),1)||power);
         rlab= rlab// rlabl;
      *  end;
      end;
   create _out_ from out[c={ALPHA ETA2 NC POWER}];
   append from out;
   create _lab_ from rlabl[c={_SOURCE_ _TYPE_ STAT}];
   append from rlab;
quit;
data &out;
   merge _lab_ _out_;
   label nc='Non Centrality'
         eta2='Eta Squared'
         stat='Statistic';
%exit:
%mend;
