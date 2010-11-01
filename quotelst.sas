/*
/ Program   : quotelst
/ Version   : 1.0
/ Author    : Roland Rashleigh-Berry
/ Date      : October 2002
/ Contact   : roland@rashleigh-berry.fsnet.co.uk
/ Purpose   : To quote the elements of a list
/ SubMacros : none
/ Notes     : This is useful to turn a list into a quoted list so that you can
/             use the in() function on it in a data step. Also, if you search for
/             a quoted string among a list of quoted strings then you can avoid
/             matching on a subset of a single element. Note that you can change
/             not only the quote mark but the delimiter as well so you can use
/             this macro for other purposes like putting commas between variable
/             names etc. It is assumed that the elements of the list are
/             delimited by spaces.
/ Usage     : %if %index(%quotelst(varnames),"varname") %then...
/ 
/================================================================================
/ PARAMETERS:
/-------name------- -------------------------description-------------------------
/ str               String to quote elements of (pos)
/ quote=%str(%")    Quote character to use (defaults to double quotation mark)
/ delim=%str( )     Delimiter character to use (defaults to a space)
/================================================================================
/ AMENDMENT HISTORY:
/ init --date-- mod-id ----------------------description-------------------------
/ 
/================================================================================
/ This is public domain software. No guarantee as to suitability or accuracy is
/ given or implied. User uses this code entirely at their own risk.
/===============================================================================*/

%macro quotelst(str,quote=%str(%"),delim=%str( ));
%local i quotelst;
%let i=1;
%do %while(%length(%scan(&str,&i,%str( ))) GT 0);
  %if %length(&quotelst) EQ 0 %then %let quotelst=&quote.%scan(&str,&i,%str( ))&quote;
  %else %let quotelst=&quotelst.&quote.%scan(&str,&i,%str( ))&quote;
  %let i=%eval(&i + 1);
  %if %length(%scan(&str,&i,%str( ))) GT 0 %then %let quotelst=&quotelst.&delim;
%end;
%unquote(&quotelst)
%mend;
