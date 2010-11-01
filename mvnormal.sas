 /*--------------------------------------------------------------*
  *    Name: mvnormal.sas                                        *
  *   Title: Generate multivariate normal samples                *
        Doc: http://www.datavis.ca/sasmac/mvnormal.html    
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 21 Sep 2006 11:51:06                                *
  * Revised: 28 Jan 2007 15:25:44                                *
  * Version: 0.6-0                                               *
  * 0.6  Fixed assorted bugs relating to type=COV                *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The MVNORMAL macro generates random samples from multivariate normal
 distributions with a specified covariance matrix and one or more
 specified mean vectors.

==Method:

 The macro uses the CALL VNORMAL routine in SAS/IML to generate the
 samples.  

=Usage:

 The MVNORMAL macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%mvnormal(data=mycorr, var=X1 X2 X3 X4);
 
==Parameters:

* DATA=       The name of the input data set containing either correlations
              (and standard deviations)  or variances and covariances, as
              specified by the TYPE= parameter.  This must be in a form
              similar to that produced by PROC CORR with the OUTP= 
              option, and possibly the COV option as well.  For a correlation
               matrix, this will contain correlations
              (_TYPE_='CORR') and standard deviations (_TYPE_='STD');
              a covariance matrix will contain observations of _TYPE_='COV'.
			  
* VAR=        A list of names of variables to be generated.  Mut be an explicit
              list of blank-separated names, rather than an abbreviated list
              like X1-X5.

* TYPE=       Type of input data set: CORR or COV. [Default: TYPE=COV]

* MEANS=      Name of an optional input dataset containing mean vector(s) for the
              VAR= variables. If not supplied, all varibles are given means of 0.
              If the MEANS= dataset is the same as the DATA= data set, means are
              read from the _TYPE_='MEAN' observation.  Otherwise,
              one group of N obsrvations is generated for
              each observation in the MEANS= dataset.

* CLASS=      Name of class/group variable in means data set.  The MEANS=
              dataset should contain one observation for each group.

* N=          Sample size (for each group). [Default: N=10]

* SEED=       Seed for random number generator.  If not specified, the
              time of day is used as the seed.

* OUT=        The name of the output data set [Default: OUT=SAMPLE]
                
=Example:

  *-- common correlation matrix for all groups;
  data corr;
  	_type_ = 'CORR';
  	input _type_ _name_ $ X1 X2 X3;
  cards;
  CORR  X1    1    .5   .3
  CORR  X2   .5     1   .2
  CORR  X3   .3    .2    1
  STD    .   10     5    7
  ;
  data means;
  	input gp group $  _type_ $  X1 X2 X3;
  cards;
  1  A  MEAN   10  10  10
  2  B  MEAN   20  20  20
  3  C  MEAN   30  30  30
  
  %mvnormal(data=corr, type=CORR, var=X1 X2 X3
  	,means=means
  	,class=group
  	);

 =*/
%macro mvnormal(
   data=,       /* dataset for correlation or var-cov matrix        */
   var=,        /* list of names of variables to be generated       */
   type=COV,    /* type of input data set: CORR or COV              */
   means=,      /* dataset for mean vector(s)                       */
   class=,      /* name of class/group variable in means data set   */
   n=,          /* sample size (for each group)                     */
   seed=,       /* seed for random number generator                 */
   out=sample   /* output dataset name                              */
	);


%let type=%upcase(&type);
%if %length(&var)=0 %then %do;
	%put You must supply a list of variable names from the &data data set.
	%goto exit;
	%end;

%if %length(%scan(&var,2, %str( )))=0 %then %do;
	%put You must specify 2 or more variables from the  &data data set.
	%goto exit;
	%end;

%if %length(&n)=0 %then %do;
	%put N= has not been specified. Using N=10.;
	%let n=10;
	%end;
 /* Generate the multivariate normal data in SAS/IML */

proc iml worksize=100;
	use &data;            /* read variance-covariance matrix */
	read all var{&var} into cov where(_type_="&type");
	p = ncol(cov);
	print cov;
	%if &type = CORR %then %do;
		read all var{&var} into std where(_type_="STD");
		if type(std)='U' then std = j(1,p);
		cov = diag(std) * cov * diag(std);
		%end;

  %if %length(&means) %then %do;
    %if &means = &data %then %do;
      read all var{&var} into mu where(_type_="MEAN");
      grp=1; n=&n;
    	%end;
    %else %do;
   	use &means;             /* read means */
   	read all var{&var} into mu;
   	n=&n;
   	%if %length(&class) %then %do;
   		read all var{&class} into grp;
   		%end;
   	%else %do;
   		grp = t(1:nrow(mu));
   		%end;
	  %end;
	%end;
	
	%else %do;
		mu = j(1, p, 0);
    grp=1; n=&n;
		%end;


	do i=1 to nrow(mu);
		group = repeat(grp[i,], n, 1);
		if type(group)='N'
			then group = char(group);
		%if %length(&seed)
		%then %str(call vnormal(x, mu[i,], cov, n, &seed););
		%else %str(call vnormal(x, mu[i,], cov, n););
		if i=1 then create &out from x[rowname=group colname={&var}];
		append from x[rowname=group]; 
	end;
quit;

%exit:

%mend;
