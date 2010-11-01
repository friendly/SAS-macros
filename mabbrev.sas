%macro mabbrev(word,test,length);
%****************************************************************************
%*
%* function: mimic the rexx "abbrev" function.
%* syntax:   %MABBREV(word,test,length)
%*
%*    Return 1 if "test" is equal to the leading characters of "word"
%*    and "test" is at least "length" characters long. Return 0 if
%*    either of these conditions is false.
%*
%* defaults: word = (no default)
%*           test = (no default)
%*           length = 0
%*
%*
%* examples:
%*
%*      %MABBREV('Dataset','Data')   = 1
%*      %MABBREV('DATASET','data')   = 0
%*      %MABBREV('DATASET','DAT',4)  = 0
%*      %MABBREV('DATASET','DAT')    = 1
%*      %MABBREV('DATASET','DAR')    = 0
%*      %MABBREV('DATASET','')       = 1
%*      %MABBREV('DATASET','',1)     = 0
%*
%*    A null string will return 1 if the length is 0 or omitted.
%*    This allows a word to be made the default as follows:
%*
%*      %* word1 is the default;
%*        %if (%mabbrev('word1',choice)) %then...
%*        %else %if (mabbrev('word2',choice)) %then...
%*        ...
%*         .
%*         .
%*                                            Barry W Grau
%*                                            U42054@UICVM.UIC.EDU
%*                                            U42054@UICVM.BITNET
%*                                            June 1988
%****************************************************************************
;
 %* the default length is 0;
 %if &length = %str()
    %then %let length = 0;
 
 %* here is the function definition;
 %if
 (
   (
    %length(&test) >= &length
    &
    %substr(&word,1,%min(%length(&test),%length(&word))) = &test
   )
  |
   (
    &test = %str()
    &
    &length = 0
   )
 )
    %then 1;
    %else 0;
%mend;
 
%macro min(a, b);
  %if &a <= &b %then &a ;
               %else &b ;
%mend;
