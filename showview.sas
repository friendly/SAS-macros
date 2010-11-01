/*
   Title: Show views of SASHELP library
*/
%macro showview(lib=sashelp, obs=10);
* Create macro vars VIEW1 to VIEW14, which hold the names of the views.
  Also create macro var NVIEW, which is the # of views (used for DO loop
  control later on. ;
data _null_;
set sashelp.vsview(keep=memname) end=eof;
call symput('view' || left(put(_n_,2.)), memname);
if eof then call symput('nview', put(_n_,2.));
run;

%do i = 1 %to &nview.;
    proc contents data=sashelp.&&view&i.;
    title "Dictionary view %upcase(&lib.).&&view&i.";
    run;

    proc print data=sashelp.&&view&i.(obs=&obs.);
    title2 "First &obs. observations";
    run;
%end;
%mend;

%showview;
