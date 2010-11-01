/*
From: "Zack, Matthew M." <mmz1@CCDOSA1.EM.CDC.GOV>
Here's some PROC PRINTTO code within a macro that your client may change.

Matthew Zack
*/
%macro multfreq(data=,row=,col=,stat=);

%let stat=%upcase(&stat);

filename testfile "testfile.out";
proc printto new print=testfile;
run;

proc freq data=&data;
  tables &row*&col / chisq measures;
  weight count;
run;

proc printto;
run;

data list(keep=table statistc);
  length oldline $ 30;
  retain oldline;
  infile testfile pad lrecl=80;
  input @1 line $char80.;
  tablepos=index(upcase(line),"STATISTICS FOR TABLE OF ");
  if (tablepos ne 0) then oldline=substr(line,tablepos+23,30);
  if (index(upcase(line),"&stat") ne 0) then do;
     table=compbl(oldline);
     statistc=compbl(line);
     output list;
  end;
  label table="Cross-tabulated variables";
  label statistc="Statistic and its value";
run;

proc append base=list2 data=list;
run;

proc datasets nolist;
  delete list;
quit;

%mend multfreq;

options pageno=1 pagesize=54;

* Create test data;

data test;
  infile cards;
  input a b count;
  output test;
  c=a;
  if a eq 0 then count=count*3;
  d=b;
  if b eq 1 then count=count*7;
  output test;
cards;
0 0 12
0 1 20
1 0 30
1 1 40
;
run;

option linesize=80 mprint;



%multfreq(data=test, row=a,col=b,stat=Cramer);
%multfreq(data=test, row=c,col=d,stat=Cramer);

proc print data=list2 label;
run;

proc datasets nolist;
  delete list2;
quit;
/*
 ----------------------------------------------------------------------------

The output of this program follows:

 ----------------------------------------------------------------------------  
                                 The SAS System 
                               7
                                                  08:54 Friday, November 8, 
1996

                         Cross-tabulated      Statistic and
                  OBS       variables           its value

                   1         A BY B         Cramer's V -0.089
                   2         C BY D         Cramer's V -0.031

*/