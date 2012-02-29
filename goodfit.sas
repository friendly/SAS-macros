 /*-------------------------------------------------------------------*
  *    Name: goodfit.sas                                              *
  *   Title: Goodness of fit tests for discrete distributions         *
        Doc: http://www.datavis.ca/sasmac/goodfit.html             
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created: 09 Dec 1997 13:29                                        *
  * Revised: 24 Apr 2003 11:25:16                                     *
  * Version: 1.4                                                      *
  * 1.2  Corrected error in DF calculation with SUMAT= given          *
  * 1.3  Fixed validvarname for V7+.  Inlined %words macro            *
  * 1.4  Fixed problem with missing frequencies in SAS V7+            *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:

 The GOODFIT macro carries out Chi-square goodness-of-fit tests for
 discrete distributions.  These include the uniform, binomial,
 Poisson, negative binomial, geometric, and logarithmic series
 distributions, as well as any discrete (multinomial) distribution
 whose probabilities you can specify.  Both the Pearson chi-square
 and likelihood-ratio chi-square are computed.

 The data may consist either of individual observations on a single
 variable, or a grouped frequency distribution.  

 The parameter(s) of the distribution may be specified as constants
 or may be estimated from the data.

=Usage:
 
 The GOODFIT macro is called with keyword parameters.
 The arguments may be listed within parentheses in any order, separated by commas. For example: 
 
   %goodfit(var=k, freq=freq, dist=binomial);

 You must specify a VAR= analysis variable and the keyword for the
 distribution to be fit with the DIST= parameter.  All other parameters
 are optional.

==Parameters:

* DATA=	Specifies the name of the input data set to be analyzed.

* VAR=	Specifies the name of the variable to be analyzed, the basic
			count variable.

* FREQ=	Specifies the name of a frequency variable for a grouped data
			set.  If no FREQ= variable is specified, the program assumes
			the data set is ungrouped, and calculates frequencies using
			PROC FREQ.  In this case you can specify a SAS format with the
			FORMAT= parameter to control the way the observations are
			grouped.

* DIST=  Specifies the name of the discrete distribution to be fit.
			The allowable values are:
			UNIFORM, DISCRETE, BINOMIAL, POISSON, NEGBIN, GEOMETRIC,
			LOGSERIES.

* PARM=	Specifies the value of parameter(s) for the distribution being
			fit.  If PARM= is not specified, the parameter(s) are estimated
			using maximum likelihood or method of moment estimators.

* SUMAT=	For a distribution where frequencies for values of the VAR=
			variable >= k have been lumped into a single category, specify
			SUMAT=k causes the macro to sum the probabilities and fitted
			frequencies for all values >=k.

* FORMAT= The name of a SAS format used when no FREQ= variable has been
			specified.

* OUT=	Name of the output data set containing the grouped frequency
			distribution, estimated fitted frequencies (EXP) and the values
			of the Pearson (CHI) and deviance (DEV) residuals.

* OUTSTAT= Name of the output data set containing goodness-of-fit
			statistics.

=Bugs:

=See also:

* ROOTGRAM

 =*/
 

%macro goodfit(
	data=_last_,       /* name of the input data set             */
	var=,              /* analysis variable (basic count)        */
	freq=,             /* frequency variable                     */
	dist=,             /* name of distribution to be fit         */
	parm=,             /* required distribution parameters?      */
	sumat=100000,      /* sum probs. and fitted values here      */
	format=,           /* format for ungrouped analysis variable */
	out=fit,           /* output fit data set                    */
	outstat=stats);    /* output statistics data set             */


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

%let usedata=&data;
%let dist=%upcase(&dist);
%let abort=0;

%if &var=%str() | &dist=%str()
   %then %do;
      %put ERROR: The VAR= and DIST= parameters must be specified.;
      %let abort=1;
      %goto DONE;
   %end;

%if %length(%scan(&var,2))
   %then %do;
      %put ERROR: Only one VAR= variable is allowed.;
      %let abort=1;
      %goto DONE;
   %end;

%if %index(UNIFORM DISCRETE BINOMIAL POISSON NEGBIN GEOMETRIC LOGSERIES,&dist)=0
   %then %do;
      %put ERROR: The DIST=&DIST is not a recognized distribution.;
      %let abort=1;
      %goto DONE;
   %end;

%*-- Assume individual observations if no freq was given;
%if %length(&freq)=0 %then %do;
	proc freq data=&data;
		tables &var / noprint out=_counts_;
		%if %length(&format) %then %do;
			%if %index(&format,.)=0 %then %let format = &format..;
			format &var &format;
			%end;
		run;
	
	%let usedata=_counts_;
	%let freq=count;
%end;

%*-- Fix problem with missing frequencies in SAS V7+;
%if %length(&sumat) %then %do;
data &usedata;
	set &usedata;
	if &freq =. then &freq = 0;
%end;
	
%*-- Find total frequency, number of cells, mean, var;
proc summary data=&usedata vardef=weight;
	var &var;
	weight &freq;
	output out=_total_ sum=_total_ sumwgt=n mean=_mean_ max=_max_ var=_var_;
*proc print;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

%*-- Find number of cells summed, if any;
data _total_;
	set &usedata end=eof nobs=_cells_;
	if _n_=1 then set _total_(drop=_type_);
	keep _total_ n _mean_ _max_ _var_ _sumd_;
	retain _sumd_ 0;
	
	if _cells_ > &sumat then do;
		if &var > &sumat then _sumd_+1;
		end;
	if eof then output;
	
*proc print;

%*-- Determine if any parameters were passed;
%if %length(&parm) > 0
	%then; %let nparm = %words(&parm);
%*put nparm= &nparm;
%let pname=;
%let eparm=;

data &out;
	set &usedata end=eof nobs=_cells_;
	if _n_=1 then set _total_;
	drop _max_ _total_ n _mean_ _sumd_;

	%if &nparm>0 %then %do;
		%*-- Store parameters in an array;
		array _xp_  _xp1-_xp&nparm ( &parm );
		drop _xp1-_xp&nparm;
	%end;
	
	%if &dist=UNIFORM %then %do;
		df = (_cells_ - _sumd_) - 1;
		if &var < &sumat
			then phat = 1 / _cells_;
			else phat = (_cells_ - &sumat + 1)/_cells_;
		%end;		

	%else %if &dist=DISCRETE %then %do;
		%let pname=cell proportions;
		%if &nparm=0 %then %do;
            phat = &freq * &var /n;
			%end;
        %else %do;
		%*-- Parameters are proportional to cell probabilities; 
		if _n_=1 then do;
			drop _tot_;
			_tot_ = sum(of _xp1-_xp&nparm);
			do _i_=1 to &nparm;
				_xp_[_i_] = _xp_[_i_] / _tot_;
				end;
			end;
			
		phat = _xp_[_n_];
        %end;
		df = (_cells_ - _sumd_) - 1;
		%end;

	%else %if &dist=POISSON %then %do;
		%let pname=lambda;
		%if &nparm=0 %then %do;
			df = (_cells_ - _sumd_) - 2;
			parm = _mean_;
			call symput('eparm', left(put(parm, 7.4)));
			%end;
		%else %do;
			parm = _xp_[1];
			df = (_cells_ - _sumd_) - 1;
			%end;
		if &var < &sumat
         then phat = exp(-parm) * parm**&var / gamma(&var+1);
			else phat = 1 - poisson(parm, &var-1);
		%end; 

	%else %if &dist=BINOMIAL %then %do;
		%let pname=p;
		%if &nparm=0 %then %do;
			p = _mean_ / _max_;
			call symput('eparm', left(put(p, 7.4)));
			df = (_cells_ - _sumd_) - 2;
			%end;
		%else  /* %if &nparm>0 %then */ %do;
			p = _xp_[1];
			df = (_cells_ - _sumd_) - 1;
			%end;
		if &var=0
			then phat = probbnml(p, _max_, &var);
			else if &var < &sumat
                 then phat = probbnml(p, _max_, &var) -
 		                     probbnml(p, _max_, &var-1);
            else phat =  1 - probbnml(p, _max_, &var-1);
		%end;

	%else %if &dist=NEGBIN %then %do;
		%let pname=n, p;
  		%if &nparm=0 %then %do;
			df = (_cells_ - _sumd_) - 3;
        	p = _mean_ / _var_;
        	parm = _mean_**2 / (_var_ - _mean_);
			call symput('eparm', trim(left(put(parm, 7.4))) || ', ' || left(put(p, 7.4)));
        	%end;
        %else %do;		*-- parameters are: n, p;
			parm = _xp_[1];
			p =    _xp_[2];
			df = (_cells_ - _sumd_) - 1;
           %end;
		if &var < &sumat then
			phat = (gamma(parm+&var)/(gamma(&var+1)*gamma(parm))) *
               (p**parm)*(1-p)**&var;
		else do v=&var by 1 until (term < .00001); 
			term = (gamma(parm+v)/(gamma(v+1)*gamma(parm))) *
               (p**parm)*(1-p)**v;
			phat = sum(phat, term);
			end;
		drop term v;
		%end;

	%else %if &dist=GEOMETRIC %then %do;	**-- INCOMPLETE --;
		%let pname=p;
  		%if &nparm=0 %then %do;
			df = (_cells_ - _sumd_) - 2;
         parm = 1/(_mean_);
			call symput('eparm', left(put(parm, 7.4)));
			%end;
		%else %do;
			df = (_cells_ - _sumd_) - 1;
			parm = _xp_[1];
			%end;
*		phat = ((_mean_-1)**(&var-1)) /(_mean_**&var);
		if &var < &sumat then
			phat = parm * (1-parm)**(&var-1);
		else do v=&var by 1 until (term < .00001); 
			term =  parm * (1-parm)**(v-1);
			phat = sum(phat, term);
			end;
		drop term v;
		%end;

	%else %if &dist=LOGSERIES %then %do;
		%let pname=theta;
  		%if &nparm=0 %then %do;
			df = (_cells_ - _sumd_) - 2;
			*Birch estimator;
			parm = 1 - (1 / (1 + ((5/3)- log(_mean_)/16)*(_mean_-1)+2)*log(_mean_));
			call symput('eparm', left(put(parm, 7.4)));
			%end;
		%else %do;
			parm = _xp_[1];
			df = (_cells_ - _sumd_) - 1;
			%end;
		if &var < &sumat then
			phat = parm**&var/(-&var*log(1-parm));
		else do v=&var by 1 until (term < .00001); 
			term =  parm**v/(-v*log(1-parm));
			phat = sum(phat, term);
			end;
		drop term v;
		%end;

	exp  = n * phat;	
	chi = (&freq - exp) / sqrt(exp);
	
	if &freq = 0 
		then dev = 0;
		else dev = 2* &freq * log(&freq/(exp + (exp=0)));
	dev = sign(&freq - exp) * sqrt(abs(dev));
	if &var <= &sumat 
		then output;

	label exp='Fitted frequency'
        phat= 'Fitted probability'
	 	chi = 'Pearson residual'
		dev = 'Deviance residual';

proc print;		
	id &var;
	var &freq phat exp chi dev;
	sum &freq phat exp;
	format chi dev 7.3;

data &outstat;
	keep dist stat value df prob;
	set &out end=eof;
	chisq + (chi**2);
	g2 + sign(dev)*(dev**2);

	*-- Output statistics to dataset;
	if eof then do;
		pchisq = 1-probchi(chisq,df);
		pg2 = 1-probchi(g2,df);

		dist = "&dist";
		stat = 'Pearson Chi-square  ';
		value= chisq;
		prob = pchisq;
		output;

		stat = 'Likelihood ratio G2';
		value= g2;
		prob = pg2;
		output;

	*-- Prepare printed output summary;
		file print;
		length label $40;
		call label(&var, label);
		if upcase("&var")=label then label='';
		put /  @10 "Goodness-of-fit test for data set %upcase(&data)"
		    // @10 "Analysis variable:       %upcase(&var) " label
		    /  @10 "Distribution:            &dist";
		%if &nparm>0 %then %do;
		put    @10 "Specified Parameters:    &pname = &parm";
		%end;
		%if %length(&eparm)>0 %then %do;
		put    @10 "Estimated Parameters:    &pname = &eparm";
		%end;
				
		put /  @10 'Pearson chi-square    = ' chisq 
		    /  @10 'Prob > chi-square     = ' pchisq
		    // @10 'Likelihood ratio G2   = ' g2 
		    /  @10 'Prob > chi-square     = ' pg2
			 // @10 'Degrees of freedom    = ' df;
		end;
   run;
*proc print;

*-- Clean up datasets no longer needed;
proc datasets lib=work memtype=data nolist nowarn;
   delete _total_;
	run; quit;

%done:
%if &abort %then %put ERROR: The GOODFIT macro ended abnormally.;

	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;

%mend;

%macro words(string,root=);
%*--------------------------------------------------;
%* Return number of words in string. If root ^' ',  ;
%* then create global variables starting with root. ;
%*--------------------------------------------------;
   %local count word;
   %let count=1;
   %let word = %scan(&string,&count,%str( ));
   %do %while(&word^= );
	%*put WORDS: word=&word;
       %if &root^=  %then %do;
          %global &root&count;
          %let &root&count=&word;
       %end;
       %let count = %eval(&count+1);
       %let word = %scan(&string,&count,%str( ));
   %end;
   %eval(&count-1)
%mend words;
%* put words = %words(A B C,root=W);
%* put W1= &W1 W2= &W2 W3= &W3 ;
%* put words = %words(AA BB CC);
