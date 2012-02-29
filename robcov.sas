 /*--------------------------------------------------------------*
  *    Name: robcov.sas                                          *
  *   Title: Calculate robust covariance matrix via MCD or MVE   *
        Doc: http://www.datavis.ca/sasmac/robcov.html      
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 11 Feb 2002 10:23:14                                *
  * Revised: 20 Mar 2007 15:56:37                                *
  * Version: 1.3-2                                               *
  *  1.1  Added plot of robust vs. classical distances           *
  *  1.2  Added consistency, small-sample corrections            *
  *       Added WHERE= to select observations                    *
  *       Added stop; to data _null_; %label -> %labels          *
  *  1.3  Fixed problem relating to the OUT= dataset             *
  *       Partial fix to problem with numeric ID: ID= var must be*
  *       character if specified.                                *
  *       Fixed above by forcing ID var to be character.         *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The ROBCOV macro calculates robust estimates of the mean vector,
 variance-covariance matrix and/or correlation matrix for a multivariate
 sample.  It also calculates classical estimates of multivariate
 (Mahalanobis) distance, and robust estimates of multivariate distance
 which ignore potential outliers.  Optionally, it produces a plot of
 robust vs. classical distances.
 
 These results may be used both to detect multivariate outliers and
 leverage points, and to provide robust versions of any multivariate
 technique which depends on the covariance or correlation matrix,
 simply by substituting the robust result for the classical ones,
 or by using the resulting WEIGHT variable to exclude potential outliers.
 Typical applications include regression, canonical correlation,
 factor analysis, and structural equation models, when the input
 may be supplied as a covariance (correlation) matrix, *or* when the
 procedure ignores observations with WEIGHT=0 to be excluded.

 Two types of highly-robust estimators are provided: the minimum covariance
 determinant (MCD) estimator, based on the fast-MCD algorithm of
 Rousseeuw and Van Driessen (1999), and the minimum volume ellispoid (MVE)
 estimator based on earlier work by Rousseeuw (1984).  MCD is defined
 as minimizing the determinant of the covariance matrix computed on
 h points, while MVE is defined as minimizing the volume of the 
 p-dimensional ellipsoid containing h points.  
 
==Method:

 The macro is essentially an easily-used interface to the SAS/IML
 MCD and MVE call routines.  It creates a copy of the input variables
 augmented by the observation distances, robust distances, and weights.
 It creates robust covariance and correlation data sets in the form
 which may be used as input to other SAS procedures.

=Usage:

 The ROBCOV macro is defined with keyword parameters.  There are no
 required parameters, but you should include an ID= variable for more
 interpretable results.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
   %robcov(data=hawkins, var=x1-x4 y, id=case);
 
==Parameters:

* DATA=       Name of the input data set [Default: DATA=_LAST_]

* VAR=        List of input variable names.  You may use any of the
              standard SAS abbreviations for variable lists, e.g.,
              VAR=X1-X20 Y1-Y3, VAR=AGE--STORAGE, or VAR=_NUMERIC_.
              [Default: VAR=_NUMERIC_]

* ID=         Name of an observation ID variable.  This variable is
              simply copied to the output OUT= data set.

* WHERE=      A WHERE clause to select observations to be read, e.g.,
              WHERE=Sex='Male'.

* METHOD=     Robust estimation method: MVE or MCD [Default: METHOD=MCD]

* OPT=        Options for the MVE or MCD call, a list of 5 numbers controlling
              the details MVE and MCD methods and printed output.  See the SAS
              System Help (e.g., 'MCD call') for details on opt[].  The
              default, OPT=, is usually sufficient; two of these options
              may be specified more easily with the QUANTILE= and PRINT=
              options.

* QUANTILE=   The quantile value of the h parameter, specified as a
              fraction of (n+p+1).  This value may also be specified in
              terms of number of observations as the value of opt[4].
              If neither QUANTILE= or opt[4] are specified, the default
              works out to QUANTILE=.5 or h=(n+p+1)/2.  This default has
              a high breakdown-bound (50%) but higher values, up to about
              QUANTILE=.75 are more efficient under milder deviations
              from multivariate normality.
              
* CORRECT=    Apply small sample and consistency corrections to the robust
              covariance matrix and robust distances? [Default: CORRECT=Y]

* PRINT=      This parameter controls the printed output from the
              macro, and should be one of the following: NONE,
              SOME, ALL.  Unless PRINT=NONE, opt[1] is modified.
              [Default: PRINT=SOME]

* OUT=        Output data set corresponding to the input DATA= data set,
              with classical Mahalanobis distances (DISTANCE), robust 
              distances (RDIST), and observation weights (WEIGHT).
              [Default: OUT=OUTDATA]

* OUTR=       Name for the output data set containing the robust 
              correlation matrix, a TYPE=CORR data set.  This data set
              is only produced if the OUTR= name is specified;
              if so, the macro fiddles with opt[3].
              [Default: OUTR=]

* OUTC=       Name for the output data set containing the robust 
              covariance matrix. [Default: OUTC=OUTCOV]                

* PLOT=       If non-blank, a plot RDIST * DISTANCE is produced,
              with reference lines and case labels identifying
              outliers. [Default: PLOT=RDIST]

* SUBSET=     A logical expression to determine which observations
              are labeled in the plot. [Default: (WEIGHT=0)]

* SYMBOL=     Value for point symbols in the plot. [Default: SYMBOL=DOT]

==Notes:

 The SAS/IML MCD estimator underestimates the scale of the covariance
 matrix, so the robust distances are somewhat too large, and
 too many observations tend to be nominated as outliers.  
 A scale-correction (Pison et al., 2002) has been implemented, and
 seems to work well enough to make CORRECT=Y the default.

 For best results with the graphic plot, you should specify appropriate
 graphic options, such as HTEXT=, FTEXT=, HTITLE=, FTITLE= in a
 GOPTIONS statement.

=References:

 Pison, G., Van Aelst, S. & Willelms, G. (2002) "Small sample corrections
 for LTS and MCD,"  Metrika, 55, 111-123.
 
 Rousseeuw, P. J. (1984). "Least median of squares regression," JASA,
 79, 871-880.

 Rousseeuw, P. J. & Van Driessen, K. (1990) "A Fast Algorithm for the
 Minimum Covariance Determinant Estimator,"  Technometrics, 41, 212-223.
 
 =*/

%macro robcov(
    data=_last_,     /* name of input data set                     */
    var=_numeric_,   /* list of input variable names               */
    id=,             /* name of an observation ID variable         */
    where=,
    method=mcd,      /* robust estimation method: MVE or MCD       */
    opt=,            /* options for MVE or MCD                     */
    quantile=,       /* quantile, h, as a fraction (sets opt[4])   */
    correct=Y,       /* apply small-sample, consistency correction?*/
    print=some,      /* control printed output: None, Some, All, Out */
    out=outdata,     /* output data set, with robust distances     */
    outr=,           /* output robust correlation matrix           */
    outc=outcov,     /* output robust covariance matrix            */
    plot=rdist,      /* Plot RDIST * DISTANCE?                     */
    subset=(weight=0),/* Subset of points to label in the plot     */
    symbol=dot
    );

%local abort me;
%let me=&sysmacroname;
%let abort=0;
%let method = %upcase(&method);

%let print  = %substr(%upcase(&print),1,1);
%let correct= %substr(%upcase(&correct),1,1);

%if ^(&method = MCD or &method = MVE) %then %do;
   %put WARNING: (&me) METHOD must be MCD or MVE, not &method.;
   %put METHOD has been reset to METHOD=MVE;
   %let method=MVE;
   %end;

%if %sysevalf(&sysver  < 6.12) %then %do;
   %put ERROR: &me requires at least SAS 6.12;
   %let abort=1;
   %goto DONE;
   %end;
%if %sysevalf(&sysver  < 7) %then %do;
   %if &method = MCD %then %do;
      %put WARNING: The MCD method requires SAS 7 or later.;
      %put METHOD has been reset to METHOD=MVE;
      %let method=MVE;
      %end;
   %end;

%*-- If there is no ID variable, create one;
%if %length(&id)=0 %then %do;
	data _new_;
		set &data;
		_id_ = put(_n_, best8.);
*		_id_ = _n_;
		run;
	%let id = _id_;
	%let data = _new_;
	%end;
%else %do;  %*-- force a character ID variable;
	data _new_;
		length _id_ $8;
		set &data;
		_id_ = &id;
	%let id = _id_;
	%let data = _new_;
	%end;

%*-- Parse the variables list if it contains special lists (IML restriction);
%let ovar = &var;
%if %index(&var,-) > 0 or %index(&var,:) > 0 or "&var"="_NUMERIC_" %then %do;
 data _null_;
 set &data (obs=1);
        %*  convert shorthand variable list to long form;
     length _vname_ $ 8 _vlist_ $ 200;
     array _xx_ &var;
     _vname_ = ' ';
     do over _xx_;
        call vname(_xx_,_vname_);
        _vlist_ = trim(_vlist_)|| ' ' || trim(_vname_);
     end;
     call symput( 'var', trim(_vlist_) );
     put "NOTE: VAR= &ovar list translated to: VAR=" _vlist_;
 RUN;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
%end;

options nonotes;
proc iml;

start nomiss(X, obsnames);
   *-- Remove rows with any missing data from matrix and obsnames;
   *   (pass ' ' for obsnames if no row labels are present);
   *   Version for single matrix;
   matrix = X;
   miss = loc(matrix <=.Z);
   if type(miss)='U'
      then return;           /* no missing data */
      else do;
         nr = nrow(matrix);
         nc = ncol(matrix);
         rows= 1+floor((miss-1)/nc);
         rows= unique(rows);
         keep=remove(1:nrow(matrix),rows);
         deleted = nr - ncol(keep);
         matrix = matrix[keep,];
         reset noname;
         print 'Warning:' deleted 'row(s) with missing data have been removed.';
         reset name;
         if obsnames ^={' '}
            then do;
               obs = obsnames[rows];
               obs = shape(obs,1,nrow(obs)#ncol(obs));
               if type(obs)='N' then obs=char(obs,3,0);
               obsnames=obsnames[keep];
            end;
         end;
         X = matrix;
   finish;

/*-- Calculate small sample correction factor for standard MCD
    (Croux and Haesbroeck; Pison van Aelst & Williams)
*/

start cfactor(p, n, quantile);

    if(p > 2) then do;
        coef500 = t({
           -1.42764571687802 1.26263336932151 2,
           -1.06141115981725 1.28907991440387 3});
        coef875 = t({
           -0.455179464070565 1.11192541278794 2,
           -0.294241208320834 1.09649329149811 3});

        y500 = 1 + coef500[1,] / (p##coef500[2,]);
        y500 = log( 1 - y500 );
        A500 = j(2,1) || t(log(1/(coef500[3,] * p##2)));

        y875 = 1 + coef875[1,] / (p##coef875[2,]);
        y875 = log( 1 - y875 );
        A875 = j(2,1) || t(log(1/(coef875[3,] * p##2)));

        c500 = solve(A500, t(y500));
        c875 = solve(A875, t(y875));
        fp500n = 1 - exp(c500[1])/(n##c500[2]);
        fp875n = 1 - exp(c875[1])/(n##c875[2]);
        end;
    else do;
        if(p = 2) then do;
            fp500n = 1 - (exp(0.673292623522027))/
              n##{0.691365864961895};
            fp875n = 1 - (exp(0.446537815635445))/
              n##{1.06690782995919};
            end;
        if(p = 1) then do;
            fp500n = 1 - (exp(0.262024211897096))/
              n##{0.604756680630497};
            fp875n = 1 - (exp(-0.351584646688712))/
              n##{1.01646567502486};
            end;
        end;

    *-- Do inverse interpolation for quantile;
    if((0.5 <= quantile) & (quantile <= 0.875)) then
        fpan = fp500n + (fp875n - fp500n)/0.375 * (quantile - 0.5);

    if((0.875 < quantile) & (quantile <= 1)) then
        fpan = fp875n + (1 - fp875n)/0.125 * (quantile - 0.875);

    return(1/fpan);
    finish;

*-- Mahalanobis distances;
start distance(x, mu, sigma);
    x = x - shape(mu, nrow(x), ncol(x));
    s = inv(sigma);
*   dist = vecdiag(x * s * t(x));
    dist = ((x * s) # x)[,+];
    return(dist);
   finish;

*-- Main routine;

    *-- Read the data and the observation ID;
     use &data;
     read all var{&var} into  x[ colname=name ]
        %if %length(&where)>0 %then where(&where);
      ;
     %if &id ^= %str() %then %do;
        read all var{&id} into  obs
        %if %length(&where)>0 %then where(&where);
        ;
          if type(obs) = 'N' then obs = trim(left(char(obs,4,0)));
     %end;
     %else %do;
        obs = char(1:nrow(x),3,0);
     %end;
    *-- Remove cases with any missings;
      run nomiss(X, obs);
      n = nrow(X);
      p = ncol(X);

    *-- Set options for MCD/MVE;
    %if %length(&opt)=0 %then %do;
        opt = j(5,1,0);
        %end;
    %else %do;
        opt = { &opt };
        %end;

    *-- If asked for the robust correlation matrix, make sure it is returned;
    %if %length(&outr) %then %do;
        if opt[3] < 2 then opt[3] = 2;
        %end;
   *-- reset some options;
    prin = index('NSA', "&print");  %*-- No, Some, All -> 1:3;
    if prin>0 then opt[1]=prin-1;
    
    %if %length(&quantile) %then %do;
        quantile = &quantile;
       h = round(&quantile # (n+p+1));
      opt[4] = h;
       %end;
    %else %do;
        if opt[4]=0 then quantile=0.5;
            else quantile = opt[4] / (n+p+1); 
        %end;
       

    /* Compute robust scatter matrix */
    call &method(sc, coef, dist, opt, X);
    
    *print "&method Coefficient matrix", coef[colname=name];
    
    *-- Unpack the results;
    mu  = coef[1,];
    eig = coef[2,];
    cov = coef[3:(2+p),];
    if nrow(coef) > 2+p
        then corr= coef[(3+p):(2+2*p),];

    mdist  = t(dist[1,]);
    rdist  = t(dist[2,]);
    weight = t(dist[3,]);

    %if &correct=Y %then %do;
    *-- Correct for consistency factor and small samples;
    h = sc[1];
    qalpha = cinv(quantile, p);
    calpha = 1/(probgam(qalpha/2, p/2 +1) / quantile);
    correct= cfactor(p, n, quantile);
    cov = calpha # correct # cov;

    *-- Re-compute weights if cov is non-singular;
    if (abs(det(cov)) > 1e-7) then do;
        rdist = sqrt(distance(X, mu, cov));
        cut = sqrt(cinv(.975, p));
        weight = choose( rdist < cut, 1, 0);
        end;
    else do;
        print "Covariance matrix is singular.";
        end;
    %end;

/*    
    *-- Output the data, with appended variables;
    %if %length(&out) %then %do;
        out = X || mdist || rdist || weight;
        clabel = name || {distance rdist weight};
        create &out from out[rowname=obs colname=clabel];
        append from out[rowname=obs];		
        %end;
*/

    *-- Output only 
	the appended variables;
    %if %length(&out) %then %do;
        out = mdist || rdist || weight;
        clabel = {distance rdist weight};
        create &out from out[rowname=obs colname=clabel];
        append from out[rowname=obs];		
        %end;

    *-- Output the covariance matrix, in _type_=cov style ;
    %if %length(&outc) %then %do;
        out = cov // mu // j(1, p, n);
        _type_ = repeat('COV', p, 1) // {'MEAN', 'N'};
        _name_ = shape(name, p, 1) // {' ', ' '};
        mattrib out colname=name rowname=_name_;
        *print 'Robust Covariance matrix', _type_ _name_ out[format=8.4];
        create &outc from out[rowname=_name_ colname=_name_];
        append from out[rowname=_name_];
        %end;

    *-- Output the covariance matrix, in _type_=corr style ;
    %if %length(&outr) %then %do;
        std = t(sqrt(vecdiag(cov)));
        out = corr // mu // std // j(1, p, n);
        _type_ = repeat('CORR', p, 1) // {'MEAN', 'STD', 'N'};
        _name_ = shape(name, p, 1) // {' ', ' ', ' '};
        mattrib out colname=name rowname=_name_;
        *print 'Robust Correlation matrix', _type_ _name_ out[format=8.4];;
        create &outr from out[rowname=_name_ colname=_name_];
        append from out[rowname=_name_];
        %end;
done:
quit;

/* -- obsolete code, replaced below
%if %length(&out) %then %do;
    *-- Add variable labels, rename &ID in the output data set;
    data &out;
        set &out;
        %if %length(&id) %then %do;
            rename obs = &id;
            %end;
        label
            distance = 'Mahalanobis Distance'
        %if &correct=Y
            %then rdist = "Scaled Robust &method Distance";
            %else rdist = "Robust &method Distance";
            weight = 'Observation weight';
        run;
*/
*proc contents data=&out;

%if %length(&out) %then %do;
    *-- Add variable labels, rename &ID in the output data set;
    data &out;
        set &out;
            rename obs = &id;
        label
            distance = 'Mahalanobis Distance'
        %if &correct=Y
            %then rdist = "Scaled Robust &method Distance";
            %else rdist = "Robust &method Distance";
            weight = 'Observation weight';
        run;
	proc sort data=&out;
		by &id;
	proc sort data=&data out=_tempdata_;
		by &id;
	%*-- now merge with the original input data;
*proc contents data=&out;
*proc contents data=_tempdata_;
options notes;
	data &out;
		merge _tempdata_ &out;
		by &id;

    %if %index(AO, &print) %then %do;
        proc print data=&out;
            id &id;
			var &var distance rdist weight;
			%if &print=O %then %do;
				where weight=0;
				%end; 
            run;
        %end;
    %end;

%if %length(&outc) %then %do;
    *-- Make it a proper type='COV' data set;
    data &outc(type='COV');
        length _type_ $8;
        set &outc;
        if _name_ ^= ' ' then _type_='COV ';
            else _type_='MEAN';
        if lag1(_type_) = 'MEAN' then _type_='N';
    %if &print ^= N %then %do;
        proc print;
            id _type_ _name_; 
            run;
        %end;
    %end;

%if %length(&outr) %then %do;
    *-- Make it a proper type='CORR' data set;
    data &outr(type='CORR');
        length _type_ $8;
        set &outr;
        if _name_ ^= ' ' then _type_='CORR ';
            else _type_='MEAN';
        if lag1(_type_) = 'MEAN' then _type_='STD';
        if lag1(_type_) = 'STD'  then _type_='N';
    %if &print ^= N %then %do;
        proc print; 
            id _type_ _name_; 
            run;
        %end;
    %end;

%if %length(&plot) %then %do;
    %*-- Plot robust distance vs. mahalanobis distance;
    %*-- Find reference values;
    data _null_;
        if 0=1 then set &out;
        array vars{*} &var;
        p = dim(vars);
        ref = sqrt( cinv(0.975, p) );
        call symput('ref', left(put(ref, 8.3)));
		stop;
    run;
    
    %labels(data=&out, x=distance, y=rdist, text=&id, subset=&subset,
        pos=4, xoff=-0.1, out=labels);

    proc gplot data=&out;
        plot rdist * distance = 1
            distance * distance = 2 / overlay
            href=&ref vref=&ref lhref=3 lvref=3
            vaxis = axis1 vm=0  hm=0
            anno=labels
            frame
            ;
        axis1 label=(a=90 r=0);
        symbol1 v=&symbol c=black i=none;
        symbol2 v=none    c=gray  i=join;
    run; quit;
    %end;
%done:
  options notes;
%mend;
    
