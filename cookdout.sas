 /*--------------------------------------------------------------*
  *    Name: cookdout.sas                                        *
  *   Title: Regression, deleting observations with large Cook D *
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Donald P. Cram                                     *
  * Created: 13 Jan 2000 09:26:23                                *
  * Revised: 13 Jan 2000 09:26:23                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The COOKDOUT macro

=Usage:

 The COOKDOUT macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%cookdout();
 
==Parameters:


 =*/

%macro cookdout(indata,depvar,ivarlist,nrounds=1,cookpval=.01,
		    title2=' ',b4print=1,intprint=1,prvars=' ');
/* 
    Run PROC REG  nrounds + 1  times on model &depvar = &ivarlist,
    deleting outliers based on Cooks D statistic
    being larger than &cookpval prob value.

    Per article "Influential Observations in Linear Regression"
    by R. Dennis Cook in _Journal of the American Statistical Association_
    (1979), p169, the D_i stat is to be compared to probability points 
    of the central F distribution with p and n-p degrees of freedom.

    Requires macros %_numobs2.sas and %utlwrds.sas to be in calling programs 
    path.  At Stanford, this is satisfied by having calling program include:
       options sasautos=('/afs/ir/class/gsb/lib/sas/macros');
    and having a valid afs token in the current session.  Documented in
    "SAS Macros for Accounting Research" by Donald P. Cram, 1996.

    Adapted from doncrams 12/96 work for Barth Elliott Finn, as regbef2s.
    See also %regbef.sas which evaluates trend in coefficient estimates.
*/

    %if &prvars = ' ' %then %do;
	%let prvars = &depvar &ivarlist;
	%end;

    %let numvars = %utlwrds(&ivarlist);  * apply utility macro to count words;
    %put numvars = &numvars;
    %let params = %eval(&numvars + 1);   * count the intercept too;
    %put params = &params;

* Run initial regression;

* Selective printing not working need to fix xxxx;
	%local print; %let print=noprint;
    %if b4print =1 %then %let print=;

	proc reg data=&indata &print;
	title1 'cookdout.sas:  Regression before any deletion of outliers';
        title2 "&title2";
        model &depvar = &ivarlist;
        output out=c cookd=cookd;
	run;

* Run Nround deletion of outlier rounds;
%do i = 1 %to &nrounds;
    data df;
	set c;
        if not(nmiss(cookd)=0) then delete;
	run;
    %_numobs2(df,_numreg1); 
    %if &_numreg1 = 0 %then %do;
	proc print data=&indata(obs=5);
	    title1 "No valid observations for regression round &i";
	    title2 "&title2";
	    title3 "Regression &depvar on &ivarlist";
	    title4 "First five observations of dataset &indata here:";
	    run;
	    %end;
	
    data cdel ckeep;
	set df; 
	fst=probf(cookd,&params,&_numreg1-&params);
        if fst > &cookpval then output cdel;
	else output ckeep;
    run;

    %_numobs2(cdel,_numdel1);
    
    proc print data=cdel;
	var &prvars cookd;
	title1 "cookdout.sas: Deleting these &_numdel1 round &i outliers (of &_numreg1 obs.) ... ";
	title2 "&title2";
    run;

* selective printing not working need to fix xxxx;
	%let print=;
    %if &intprint ne '1' and &i < &nrounds %then %let print=noprint;
	proc reg &print data=ckeep;
	model &depvar = &ivarlist;
	output out=c cookd=cookd;
	title "cookdout.sas: regression results after &i th of &nrounds rounds of outlier deletions",
	    title2 "   based on Cooks d statistic cutoff p-value > &cookpval";
	title3 "   &title2";
    run;  * this run is needed so title not overridden;
	%end;

    title ' ';
    run;

%mend cookdout;
%macro _numobs2(dsn,outvar);    
/* adapted from SAS Guide to Macro Processing, vers 6 2nd ed, p263
How differs from macro _numobs:  this allows one to set the name
of the macrovariable to receive the number of observations.  Required
by macro regbef2s.sas.
*/
    %global &outvar;
    data _null_;
        if 0 then set &dsn nobs=count;
        call symput("&outvar",trim(left(put(count,8.))));
        stop;
    run;
    %mend _numobs2;

/*=pod
=head1 NAME

COOKDOUT.SAS

=head1 COPYRIGHT

Copyright(c) 1997 by Don Cram.  Version 0.1a

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License version 2 as 
    published by the Free Software Foundation.

=head1 SYNOPSIS

C<
  %cookdout(indsn,depvar,ivarlist);
  %cookdout(indsn,depvar,ivarlist,nrounds=,cookpval=,
		title2=,b4print=,intprint=,prvars=);
>

=head1 DESCRIPTION

Runs PROC REG repeatedly on successively smaller datasets, printing and deleting
influential outliers. Outliers are assessed by Cook's d statistic
having p-value greater than a cutoff.

   Arguments:

=over 4

=item B<indsn> 

name of input SAS dataset

=item B<depvar>

dependent variable to use in regressions

=item B<ivarlist>

independent variables to use in regressions

=item B<nrounds=>

optional number of rounds of outlier deletion to run.  Must be a
positive integer.  Default is 1.

=item B<cookpval=>

optional p-value of Cook's d statistic to use as cutoff in identifying
outliers.  Default is .01, which may be high for most applications.

=item B<title2=>

optional text string to be used as 2nd title line

=item B<b4print=>

optional: If at 1, the default, then PROC REG will print output for
the first regression run (before any outliers have been deleted).

=item B<intprint=>

optional: If at 1, the default, then PROC REG will print output for
intermediate regressions run.

=item B<prvars=>

optional list of variable names to print in lists of outliers that are
deleted.  Default is to print depvar and ivarlist.

=back

=head1 AUTHOR

Donald P. Cram

=head1 REQUIREMENTS

=over 4

=item 

Macros _numobs2.sas and utlnwrds.sas

=back

=head1 EXAMPLES

C<
%cookdout(mydata,y,x1 x2,nrounds=2,title2=My regression of y on x1 and x2);
%cookdout(mydata,y,x1 x4,cookpval=.05);
%cookdout(mydata,y,x1 x2 x3 x4,nrounds=4,cookpval=.10,
   title2='my y on x1, x2, x3, and x4',b4print=0,intprint=0,prvars=y year x4);
>

=head1 BUGS / NOTES

    Per article "Influential Observations in Linear Regression"
    by R. Dennis Cook in _Journal of the American Statistical Association_
    (1979), p169, the D_i stat is to be compared to probability points 
    of the central F distribution with p and n-p degrees of freedom, where
    n = number of observations and p = number of independent variables in
    the regression.

    P-values of significance of regression coefficients and other
    statistics are inflated, of course.

    When describing results after deletion of outliers, it is
    necessary to state the deletion criteria (e.g. Cook's p-value >
    .0001) and the number of deletion rounds.

    Currently repeats regression analysis even when no outliers are
    detected.  Instead should print a report and stop.

    Currently the intprint and b4print options are disabled.

    Currently titles are too long for 76 column output.

=head1 SEE ALSO

=head1 LAST CHANGE

5 November 1997

=cut
*/

