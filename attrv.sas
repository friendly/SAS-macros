/*
/ Program   : attrv
/ Version   : 1.0
/ Author    : Roland Rashleigh-Berry
/ Date      : October 2002
/ Contact   : roland@rashleigh-berry.fsnet.co.uk
/ Purpose   : To return a variable attribute
/ SubMacros : none
/ Notes     : This is a low-level utility macro that other shell macros will call
/             The full list of variable attributes can be found in the SAS
/             documentation. The most common ones used will be VARTYPE, VARLEN,
/             VARLABEL, VARFMT and VARINFMT.
/ Usage     : %let vartype=%attrv(dsname,varname,vartype);
/ 
/================================================================================
/ PARAMETERS:
/-------name------- -------------------------description-------------------------
/ ds                Dataset name (pos)
/ var               Variable name (pos)
/ attrib            Attribute (pos)
/================================================================================
/ AMENDMENT HISTORY:
/ init --date-- mod-id ----------------------description-------------------------
/ 
/================================================================================
/ This is public domain software. No guarantee as to suitability or accuracy is
/ given or implied. User uses this code entirely at their own risk.
/===============================================================================*/

%macro attrv(ds,var,attrib);

%local dsid rc varnum;

%let dsid=%sysfunc(open(&ds,is));
%if &dsid EQ 0 %then %do;
  %put ERROR: (attrv) Dataset &ds not opened due to the following reason:;
  %put %sysfunc(sysmsg());
%end;
%else %do;
  %let varnum=%sysfunc(varnum(&dsid,&var));
  %if &varnum LT 1 %then %put ERROR: (attrv) Variable &var not in dataset &ds;
  %else %do;
%sysfunc(&attrib(&dsid,&varnum))
  %end;
  %let rc=%sysfunc(close(&dsid));
%end;

%mend;
