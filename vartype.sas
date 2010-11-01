/*<pre><b>
/ Program   : vartype
/ Version   : 1.0
/ Author    : Roland Rashleigh-Berry
/ Date      : October 2002
/ Contact   : rolandberry@hotmail.com
/ Purpose   : To return a variable type as either C or N
/ SubMacros : %attrv
/ Notes     : This is a shell macro that calls %attrv
/ Usage     : %let vartype=%vartype(dsname,varname);
/ 
/================================================================================
/ PARAMETERS:
/-------name------- -------------------------description-------------------------
/ ds                Dataset name (pos)
/ var               Variable name (pos)
/================================================================================
/ AMENDMENT HISTORY:
/ init --date-- mod-id ----------------------description-------------------------
/ 
/================================================================================
/ This is public domain software. No guarantee as to suitability or accuracy is
/ given or implied. User uses this code entirely at their own risk.
/===============================================================================*/

%macro vartype(ds,var);
%attrv(&ds,&var,vartype)
%mend;
