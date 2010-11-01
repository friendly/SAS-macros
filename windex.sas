/*<pre><b>
/ Program   : windex
/ Version   : 1.0
/ Author    : Roland Rashleigh-Berry
/ Date      : November 2002
/ Contact   : roland@rashleigh-berry.fsnet.co.uk
/ Purpose   : To return the word count position in a string
/ SubMacros : %words
/ Notes     : none
/ Usage     : %let windex=%windex(string,target);
/ 
/================================================================================
/ PARAMETERS:
/-------name------- -------------------------description-------------------------
/ str               String (pos) UNQUOTED
/ target            Target string (pos) 
/================================================================================
/ AMENDMENT HISTORY:
/ init --date-- mod-id ----------------------description-------------------------
/ 
/================================================================================
/ This is public domain software. No guarantee as to suitability or accuracy is
/ given or implied. User uses this code entirely at their own risk.
/===============================================================================*/

%macro windex(str,target);
%local i res;
%let res=0;
%do i=1 %to %words(&str);
  %if "%scan(&str,&i,%str( ))" EQ "&target" %then %let res=&i;
%end;
&res
%mend;
