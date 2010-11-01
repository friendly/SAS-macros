
%macro unstack1(indsn, onevar, byvar, outdsn, rvarlist=' ', outfile=, selvar=' ', selval=,ordervar=date);
/* 
    Unstack one variable out of a SAS dataset that consists of data
    blocks by byvar each ordered internally by ordervar.
    Helps in writing to TSP acceptable format, in poli2tsp.sas.
*/

%if &rvarlist ne ' ' %then %do;

data datrpt;
    set &indsn;

proc sort data=datrpt;
    by &byvar &ordervar;

data datrpt;
    set datrpt;    
    if _N_ = 1 then firstby = &byvar;
    if &byvar = firstby;
    retain firstby;
    run;

data datrpt;
    set datrpt;
    keep &rvarlist ;
    run;
    
    proc transpose data=datrpt out=tranrpt;
	
    proc append base=_base data=tranrpt;

run;
* %qaprint(_base);
	
    %end;

data _data0;
    set &indsn;
    %if &selvar ne ' ' %then %do;
    where &selvar = &selval;
    %end;
    keep &byvar &onevar;

proc sort data=_data0;
    by &byvar &ordervar;

data _data01;
    set _data0;
    by &byvar;
    if first.&byvar;
run;

%_numobs2(_data01,_numby);

* %qaprint(_data01);

%do i = 1 %to &_numby;

* get ith byvar value in a kludgy way;
data _oneby;
    N = &i;
    set _data01 point=N;
    byname = compress( "&onevar" || &byvar );
    put _all_;
    call symput('byval',&byvar);
    call symput('byname', byname);
    call symput('_b'||left(&i), left(byname) );
    stop;
    run;

%put 'after oneby at number ' &i ' byval ' &byval ' byname ' &byname ;

data data&i;
    set _data0;
    if &byvar = &byval ;
    rename &onevar = &byname ;
    keep &onevar ;

* %qptailn(data&i , 5);
    
proc transpose data=data&i out=tran&i;

proc append base=_base data=tran&i;
    %end;

proc transpose data=_base out=&outdsn;
run;

data &outdsn;
    length date 7 ;            * kludge xxx ;
    format date YYMMDD6. ;     * kludge xxx ;
    set &outdsn;
    drop _NAME_;

proc datasets;     * in case macro called twice in same program;
    delete _base;
    
%global _rvarlis;    
%let _rvarlis = &rvarlist;

%global _1varlis;
%let _expr = ;  

%do _i = 1 %to &_numby;
    %let _expr = &_expr &&_b&_i ;
%end;

%let _1varlis = &_expr;

data _null_;
    set &outdsn;
    file &outfile ;
    if _N_ = 1 then do;
	put "&_rvarlis &_1varlis";
	end;
    put &_rvarlis &_1varlis; 
    run;

%mend unstack1;
/*=pod
=head1 NAME

UNSTACK1.SAS

=head1 COPYRIGHT

Copyright(c) 1997 by Don Cram.  Version 0.1a.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License version 2 as 
    published by the Free Software Foundation.

=head1 SYNOPSIS

C<
  %unstack1(indsn, onevar, byvar, outdsn);
  %unstack1(indsn, onevar, byvar, outdsn,
      rvarlist=, outfile=, selvar=, selval=);
>

=head1 DESCRIPTION

    Unstack one variable out of a SAS dataset that consists of data
    blocks by byvar each ordered internally by ordervar.
    Helps in writing to TSP acceptable format, in poli2tsp.sas.

   Arguments:

=over 4

=item B<indsn> 

name of a SAS dataset

=item B<onevar>

the one variable that is to be unstacked

=item B<byvar>

the by variable in indsn that divides blocks

=item B<outdsn>

the output dataset to be created    

=item B<rvarlist=>

optional list of variables that repeat in each block, to be kept.
(Output will be only one block long for this and for other data.)

=item B<outfile=>
    
optional filename to receive ASCII version of output (ready for direct
    use in TSP)

=item B<selvar=>

Optional argument:  Variable to use in selecting only certain blocks

=item B<selval=>

Optional argument (required if selvar is given): Value of selvar for
selection.
    
=back

=head1 AUTHOR

Donald P. Cram

=head1 REQUIREMENTS

=over 4

=item 

Macro _numobs2.sas

=back

=head1 EXAMPLES

=head1 BUGS / NOTES

=over 4

=item
    Not tested for case of single block of data, i.e. when
    no repeated variables.  As rest of code changed considerably,
    probably the code for this is off.
    
=item

    After the macro completes, a global macrovariable _varlist
    contains the variable names in the output dataset.

=item

    Outfile written may not have nice formatting, e.g. of date
variable, as formatting lost in PROC TRANSPOSEs.  Add a formatlist
variable to set formatting?
    
=back
    
=head1 SEE ALSO

=head1 LAST CHANGE

10 November 1997

=cut
*/

