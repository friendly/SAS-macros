/*<pre><b>
/ Program   : match
/ Version   : 1.0
/ Author    : Roland Rashleigh-Berry
/ Date      : 01 June 2003
/ Contact   : rolandberry@hotmail.com
/ Purpose   : To return elements of a list that match those in a reference list.
/ SubMacros : %words
/ Notes     : Non-matching list elements are returned in the global macro
/             variable _nomatch_ .
/ Usage     : %let match=%match(aa bb,aa cc);
/ 
/================================================================================
/ PARAMETERS:
/-------name------- -------------------------description-------------------------
/ ref               (pos) Space-delimited reference list
/ list              (pos) Space-delimited list
/ nodup=yes         By default, remove duplicates from the list
/ casesens=no       By default, case sensitivity is not important.
/ fixcase=no        By default, do not make the case of matching items the same
/                   as the item in the reference list.
/================================================================================
/ AMENDMENT HISTORY:
/ init --date-- mod-id ----------------------description-------------------------
/ 
/================================================================================
/ This is public domain software. No guarantee as to suitability or accuracy is
/ given or implied. User uses this code entirely at their own risk.
/===============================================================================*/

%macro match(ref,list,nodup=yes,casesens=no,fixcase=no);

%local error list2 nref nlist i j item match refitem;
%let error=0;

%global _nomatch_;
%let _nomatch_=;

%if not %length(&nodup) %then %let nodup=yes;
%if not %length(&casesens) %then %let casesens=no;
%if not %length(&fixcase) %then %let fixcase=no;

%let nodup=%upcase(%substr(&nodup,1,1));
%let casesens=%upcase(%substr(&casesens,1,1));
%let fixcase=%upcase(%substr(&fixcase,1,1));

%if "&nodup" EQ "Y" %then %let list2=%nodup(&list,casesens=&casesens);
%else %let list2=&list;

%let nref=%words(&ref);
%let nlist=%words(&list2);

%if not &nref %then %do;
  %put ERROR: (match) No elements in reference list;
  %let error=1;
%end;

%if not &nlist %then %do;
  %put ERROR: (match) No elements in list under test;
  %let error=1;
%end;

%if &error %then %goto error;

%do i=1 %to &nlist;
  %let item=%scan(&list2,&i,%str( ));
  %let match=NO;
  %do j=1 %to &nref;
    %let refitem=%scan(&ref,&j,%str( ));
    %if "&casesens" EQ "N" %then %do;
      %if "%upcase(&item)" EQ "%upcase(&refitem)" %then %do;
        %let match=YES;
        %let j=&nref;
      %end;
    %end;
    %else %do;
      %if "&item" EQ "&refitem" %then %do;
        %let match=YES;
        %let j=&nref;
      %end;
    %end;
  %end;
  %if &match EQ YES %then %do;
    %if "&fixcase" EQ "N" %then &item;
    %else &refitem;
  %end;
  %else %let _nomatch_=&_nomatch_ &item;
%end;

%goto skip;
%error: %put ERROR: (match) Leaving match macro due to error(s) listed.;
%skip:
%mend;
