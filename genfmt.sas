%macro genfmt
             (
              data=_last_,
              out=,
              var=,
              newvar=_index_,
              assign=_n_,
              format=,
              library=,
              sortby=,
              sortout=,
              fileref=fmt,
              fileid=$genfmt$ sas a3
             );
%****************************************************************************
%*
%*  GENFMT - generate a format for use with the dot plot macro.
%*
%*                                            Barry W Grau
%*                                            U42054@UICVM.UIC.EDU
%*                                            U42054@UICVM.BITNET
%*                                            June 1988
%****************************************************************************
;
 
%if &var = %str()
    %then %do;
         %put NOTE: Required parameter, VAR, not supplied.;
         %goto bottom;
         %end;
 
%if &format = %str()
    %then %do;
         %put NOTE: Required parameter, FORMAT, not supplied.;
         %goto bottom;
         %end;
 
x filedef &fileref disk &fileid;
 
%if &sortby ^= %str()
     %then %do;
          proc sort data=&data
               %if &sortout ^= %str()
                    %then %do;
                         out=&sortout;
                         %let data = &sortout;
                         %end;
               ;
               by &sortby;
          run;
          %end;
 
data &out;
     set &data
        end=_eof_;
     _bs_=-1;
     %* next few lines are so i can control the value of
     %* the assignment variable;
     %let assign=%upcase(&assign);
     %if &assign=_N_
          %then %do;
               %let assign=_nn_;
               _nn_+1;
               drop _nn_;
               %end;
     &newvar=&assign;
     file &fileref;
     if _n_=1
          then put
                   "value &format";
     put
         "     "
         &assign
         "= '"
         &var
         +_bs_
         "'";
    drop _bs_;
    run;
 
proc format
%if &library ^= %str()
   %then library=library;;
   %inc &fileref;
   ;
run;
 
%bottom:
   %mend;
