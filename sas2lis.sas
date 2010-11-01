/*
title:  Correlation matrix output for LISREL

=Description:

 The SAS2LIS Macro takes an output Correlation or Covariance matrix
 (created as an output dataset by PROC CORR) and write it out in
 a form of a stub of a LISREL program.  It is assumed the version
 of Lisrel which will be used is part of the SPSS package, and the
 stub written conforms to that requirement.
                                                            
=Arguments:   
                                             
 The following KEYWORD parameters may be specified;       
 defaults are indicated by *.                               

* data=        _REQUIRED_ this is the output dataset from
               PROC CORR.  Several correlation datasets  
               may be created; specifying OUTP=dsn will  
               write the Pearson correlations to the SAS 
               file "dsn".  If the COV Option is speci-  
               fied, the file named in OUTP= will also   
               contain the Variance-Covariance matrix.   
               A special variable named "_TYPE_" is      
               created in these datasets...Correlation   
               matrices have a "_TYPE_=CORR" and Co-     
               variance matrices have a "_TYPE_=COV".    

* type=        either COV or CORR (*) with CORR the default.                                  

* out=         The output file...the default is PRINT    
               any other value here will cause the output
               to be written to the file "&out.sps"       
               PRINT files will display in the LISTING   
               file.                                     

* drop=        The variables to be DROPped/DELETEd from  
               the output.  The variables in this list   
               should be separated by spaces; they will  
               not be passed to the Lisrel stub.  By     
               default _all_ numeric variables are used.         

* title=       The TITLE specified on the first line of  
               the LISREL program.                       

* format=      The numeric format used to print the data.
               The default is 10.5 (*)...do not change   
               this unless you need to.                  
                                                            
                     10/18/94    F.J. Kelley                
*/

%macro sas2lis(
	type=corr,
	data=,
	format=10.5,
	drop=,
	out=PRINT,
	title=
	);

  %let type = %upcase(&type) ;
  %if &type = %str(CORR) %then %let matype=KMatrix;
  %else %if &type = %str(COV) %then %let matype =CMatrix ;

  %if %upcase(&out) ^= %str(PRINT) %then
    %let out = "&out..sps" ;

  %let drop = %upcase(&drop);
  %let subscr = 0 ;
  %do %until(&st =  );
    %let subscr = %eval(&subscr + 1) ;
    %let st = %scan(&drop,&subscr);
    %let nam&subscr ="&st";
  %end;
  %let subscr = %eval(&subscr - 1) ;

  %put *---------------------------------------* ;
  %put SAS2LIS: Parameters Used: ;
  %put data:   &data  ;
  %put type:   &type ;
  %put drop:   &drop ;
  %put out:    &out ;
  %put title:  %quote(&title);
  %put format: &format ;
  %put *---------------------------------------* ;

    data _setup_ ; set &data ;

  %if &subscr > 0 %then %do;
        drop &drop ;
        if _NAME_ in (
    %do i = 1 %to &subscr ;
      &&nam&i
    %end;
        ) then delete ;
  %end;

    data _setup_ ; set _setup_ ;
      array stuff (*) _NUMERIC_ ;
      if upcase(_TYPE_) = 'N' then do;
        maxn = 0 ;
        d = dim(stuff) ;
        do i=1 to d;
          if stuff(i) > maxn then maxn = stuff(i) ;   * ???? ;
        end;
        call symput('_NO_',trim(left(put(maxn,6.))) );
        call symput('_NI_',trim(left(put(d,5.))) )   ;
      end;
      if upcase(_TYPE_) = "&type" then output ;
	run;
	%put _NI_=&_NI_;
	%put _NO_=&_NO_;
		
    data _NULL_ ;
      set _setup_      end=last   ;
      file &out   ;
      retain names1-names&_NI_ '        ';
      array stuff(*) _NUMERIC_ ;
      array names(&_NI_) $8 ;
      names(_N_) = _NAME_ ;
      format  default=&format   ;
      if ( _N_ = 1) then do;
        put @1 "LISREL "  /
            @1 "  /""&title""" /
            @1 "  /DAta  NI=&_NI_    NO=&_NO_    MA=&matype  " /
            @1 "  /&matype  SYmmetric  " /
            @1 "  /*  " ;
      end;
      incr = 2  ;
      k = 7 ;
      do j = 1 to _N_ ;
        if  mod(j,6) = 1 then do ;
          if j = 1 then  put @1  '  / ' @ ;
          else put / '  / ' @ ;
        end;
        put stuff(j) +incr @;
      end;
      put ;
      if last then do;
        put @1 '  /LAbel ' ;
        do i = 1 to &_NI_ ;
          if mod(i,7) = 1 then do ;
            if i = 1 then  put @1 '  /' @ ;
            else put / '  /' @ ;
          end;
          put names(i) $8. + incr @ ;
        end;
        put;
      end;

      proc datasets   library=WORK    nolist  ;
        delete  _setup_    ;
    run;
%mend ;
