 /*--------------------------------------------------------------*
  *    Name: missrc.sas                                          *
  *   Title: MLEs for incomplete n-way contingency tables        *
        Doc: http://www.datavis.ca/sasmac/missrc.html      
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 08 Oct 1998  8:26                                   *
  * Revised:  6 Nov 2001 10:00:58                                *
  * Version: 1.4                                                 *
  *  1.2  Fixed type^='N' bugglet (thx: Steve Gregorich)         *
  *  1.3  Fixed validvarname and GENMOD bugs for V7+             *
  *  1.4  Fixed naming bug introduced in V8 [What part of        *
  *       "upward compatible" don't you understand?]             *
  *       Added NWAY= option                                     *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:

 The missrc macro estimates cell probabilities in an n-way table with
 ignorable missing data (missing completely at random, or missing at
 random) on the table variables.  Lipsitz etal (1998) show
 that in this case the cell probabilities may be estimated as a
 specially structured Poisson generalized linear model with a structured
 design matrix and an offset containing various marginal totals.
 Friendly (2000) generalizes this construction to n-way tables.

 The macro constructs this design matrix and offset variable, estimates
 the cell probabilities using PROC GENMOD, and returns a table with
 the estimates, their standard errors, and fitted cell frequencies.

=Usage:

 The missrc macro is called with keyword parameters.  Only the VAR= 
 parameters is required.  The arguments may be listed within
 parentheses in any order, separated by commas. For example:

 %missc(var=R C, count=count);

==Parameters:

* DATA=     Specifies the name of the input data set to be analyzed.  If
            omitted, the most recently created data set is used.

* VAR=      Specifies the names of the table variables.  In this version,
            all VAR= variables must be numeric, with non-negative
            integer levels.  The missing level must have the SAS
            missing value, .

* COUNT=    Specifies the name of the variable holding the cell frequencies.
            If not specified, COUNT=COUNT is assumed.

* OUT=      Specifies the name of the output data set containing estimated
            cell probabilities, standard errors, etc.  The default is
            OUT=CELLS.

* DESIGN=   Specifies the name of the output design matrix data set.  The
            default is DESIGN=DESIGN.

* NWAY=     If non-blank, the output dataset contains only the cells in
            the full n-way table.

= Example
 Little & Rubin (1987, p. 183) gave the following table:

                     C1     C2   Missing
            R1      100     50     30
            R2       75     75     60
            Missing  28     60

 Create a SAS data set:
   data little;
        input R C count @@;
   cards;
   1  1  100    1  2  50    1  .  30
   2  1   75    2  2  75    2  .  60
   .  1   28    .  2  60
   ;
   %missrc(data=little, var=R C);

 The following output data set is produced.  The variable P is the observed
 cell probability, ESTIMATE is the MLE, and FITTED is the estimated cell
 frequency.

  R    C    COUNT       P       PARM    ESTIMATE     STDERR      FITTED

  .    1      28      .                   .          .             .
  .    2      60      .                   .          .             .
  1    .      30      .                   .          .             .
  1    1     100     0.33333    P11      0.27947    0.022310    133.589
  1    2      50     0.16667    P12      0.17402    0.020978     83.184
  2    .      60      .                   .          .             .
  2    1      75     0.25000    P21      0.23872    0.022660    114.108
  2    2      75     0.25000    P22*     0.30778    0.025298    147.120

 If the option NWAY=1 (or any non-blank) is used, rows with P=. are excluded
 from the OUT= data set.
 
= References
 Lipsitz, S. R., Parzen, M. and Molenberghs, G. (1998) ``Obtaining the maximum
 likelihood estimates in incomplete R x C Contingency tables using a
 Poisson generalized linear model''.  J. Comp. and Graphical Statistics,
 7, 356--376.
 
 Friendly, M. (2000) ``Note on "Obtaining the Maximum Likelihood Estimates
 in Incomplete R x C  Contingency Tables..."''. J. Comp. and 
 Graphical Statistics, 9(1), 158-166.

=*/

%macro missrc(
   data=&syslast,
   var=,
   count=count,
   design=design,
   out=cells,
	nway=,
	verbose=0);

	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=V6;
		%end;
	%else %do;
	   options nonotes;
		%end;

%let abort=0;
%if &var=%str()
   %then %do;
      %put ERROR: The VAR= variables must be specified;
      %let abort=1;
      %goto DONE;
   %end;

proc freq data=&data;
   weight &count;
   tables %cross(&var) / norow nocol noprint out=&out;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

proc sort data=&out;
   by &var;

data &out;
   set &out end=last;
   p = percent/100;
   label p='Cell proportion';
   drop total percent;
   total+count;
   if last then do;
      call symput('total', left(put(total,best9.)));
      if total=0 then call symput('abort', '1');
      end;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
proc print;
	id &var;
	sum count;
	
%put MISSRC:  Constructing the design matrix %upcase(&design).;
proc iml;
start symput(name, val);
   if type(val) ='N'
      then value = trim(char(val));
      else value = val;
   call execute('%let ', name, '=', value, ';');
   finish;

	verbose={&verbose};
   reset fw=5;
   use &data;
   read all var{&var} into var;
   read all var{&count} into f;
	do;
	if type(var) ^= 'N' then do;
		*-- translate character to numeric;
		do j=1 to ncol(var);
			uniq = unique(var[,j]);
			d = design(var[,j]) * t((1:ncol(uniq))-1);
			d = choose(d=0, ., d);
			v = v || d;
			end;
		var = v;  free v;
	/*
		file log;
		put "ERROR:  The VAR= variables (&var) must be numeric";
		call symput('abort', 1);
		goto done;
	*/
		end;
   
   *-- Change missing to -1 (miss);
   miss = -1;
   do i=1 to ncol(var);
      if any(var[,i]=.) then var[loc(var[,i]=.),i] = miss;
      end;

   *-- Find miss/non-miss types;
   nv = ncol(var);
*   type = (var=miss) * 2##(t(nv:1)-1);
   type = (var^=miss) * 2##(t(1:nv)-1);
   types = unique(type);
   nt = ncol(types);
	if any(verbose) then print 'Missing/Non-missing types' types, type var f;
	
   *-- Find number of levels of each variable;
   do j = 1 to nv;
      lej = unique(var[,j]);   *-- distinct levels of this var;
      lev = lev || lej[,loc(lej^=miss)];
      nl = nl || ncol(unique(var[,j]))-any(var[,j]=miss);
      end;
   nc = nl[#] - 1;             *-- # cols of design matrix;
   * print nl nc, lev;
      
   *-- Construct design matrix;
   do i=nt to 1 by -1;         *-- Process types;
      ti = types[i];           *-- Type of this block;
      tr = loc(type = ti);     *-- Rows of this type;
      fi = f[tr];              *-- counts;

      xi=1;
      do j=1 to nv;
         if any( var[tr,j] = miss )
            then xi = xi @ J(1,nl[j],1);
            else xi = xi @ (I(nl[j]));
         end;
      xi = xi[1:nrow(xi)-1,1:nc];
      xi = sum(fi) # (xi // {-1} # (xi[+,] > 0));
      ofi = sum(fi) # (J(nrow(xi)-1,1,0) // 1);
*     print xi ofi;
      x = x // xi;
      offset = offset // ofi;
      count = count // fi;
      end;

*   print count x offset;

   *-- Generate cell parameter names;
   do i=1 to nv;
      vi = 1;  li=1;
      do j=1 to nv;
         if i=j
            then vi = vi @ lev[,li:(li+nl[i]-1)];   * was: (1:nl[j]);
            else vi = vi @ J(1,nl[j]);
         end;
      v = v // vi;
		li = li+nl[i];
      end;
		* print v;
   lab = 'P' + rowcatc(trim(char(t(v),2)));
   lab = lab[1:nrow(lab)-1];

   lvec = rowcat(t(lab + ' '));

   out = type ||count || x || offset;
   lab = {'_type_', 'count'} // lab // 'offset';
   create &design from out[c=lab];
   append from out;
   call symput('parms', lvec);
   do i=1 to ncol(nl);
      call symput('nl'+char(i,1), nl[i]);
      end;
   call symput('nf', ncol(nl));
   call symput('last', rowcatc(char(t(v[,ncol(v)]),2)));
 done:
 end;
quit;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
%if &verbose %then %put MISSRC: parms=&parms nf=&nf nl1=&nl1 nl2=&nl2 last=&last;
proc print;
	id _type_;

%if %sysevalf(&sysver  < 6.10) %then %do;
   %put MISSRC:  Cant run GENMOD in SAS Version &sysver.  Sorry.;
   %let abort=1;
   %goto done;
   %end;

%put MISSRC:  Running GENMOD with data=&design.;
%let parm=parm;
proc genmod data=&design;
   model count = &parms / covb
         dist=poisson link=id offset=offset noint;
	%if %sysevalf(&sysver  < 7) %then %do;
		make 'parmest' out=parm;
		make 'cov'     out=cov  noprint;
		%end;
	%else %do;
		ods output ParameterEstimates=parm;
		ods output CovB=cov;
		%if %sysevalf(&sysver  > 8) %then %let parm=paramete;
		%end;
   run;

*-- Find remaining cell prob and its std error;
proc iml;
   use parm;
   read all var{estimate} into p  where(df>0);
   read all var{stderr}   into se where(df>0);
   read all var{&parm}    into parm where(df>0);

   use cov;
   read all into cov;

   unit = J(nrow(p),1);
   se22 = sqrt(t(unit) * cov * unit);

   estimate = p // (1-sum(p));
   stderr = se // se22;
   last = 'P' + trim(left("&last")) + '*';
   parm = parm // last;

   create parms var{parm estimate stderr};
   append;
   quit;

%put MISSRC:  Creating output data set %upcase(&out).;
data parms;
   set parms end=last;
   fitted = estimate * &total;
   label fitted = 'Fitted cell frequency';
   %do i=1 %to &nf;
      %scan(&var,&i,%str( )) = input(substr(parm,&i+1,1), 1.);
      %end;

%if &verbose %then %do;   
proc print;
%end;

data &out;
   merge &out parms;
   by &var;
	%if %length(&nway) %then %do;
		where (p is not missing);
		%end;

proc print;
	id &var;
run;

%done:
%if &abort %then %put ERROR: The MISSRC macro ended abnormally.;
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;

%mend;

%macro cross(list);
%*-----------------------------------------------------;
%* Return string of v1 * v2 * v3 ... from list of words;
%*-----------------------------------------------------;
   %local i word;
   %let i=1;
   %let word = %scan(&list,&i,%str( ));
   %do %while(&word^= );
      %if &i=1
         %then %let result = &word;
         %else %let result = &result * &word;
       %let i = %eval(&i+1);
       %let word = %scan(&list,&i,%str( ));
   %end;
   &result
%mend cross;

