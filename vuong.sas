%macro Vuong(version, data=_last_, response=, freq=, weight=,
             model1=, p1=, dist1=, 
             model2=, p2=, dist2=, 
             nparm1=, nparm2=, test=both,
             pzero1=, pzero2=,
             event=, trials=,
             scale1=, scale2= 
             );
          
%let _version=1.1;
%if &version ne %then %put VUONG macro Version &_version;

%if &data=_last_ %then %let data=&syslast;
%let _opts = %sysfunc(getoption(notes)) 
            _last_=%sysfunc(getoption(_last_));
%if &version ne debug %then %str(options nonotes;);

/* Check for newer version */
 %if %sysevalf(&sysver >= 8.2) %then %do;
  %let _notfound=0;
  filename _ver url 'http://ftp.sas.com/techsup/download/stat/versions.dat' termstr=crlf;
  data _null_;
    infile _ver end=_eof;
    input name:$15. ver;
    if upcase(name)="&sysmacroname" then do;
       call symput("_newver",ver); stop;
    end;
    if _eof then call symput("_notfound",1);
    run;
  %if &syserr ne 0 or &_notfound=1 %then
    %put &sysmacroname: Unable to check for newer version;
  %else %if %sysevalf(&_newver > &_version) %then %do;
    %put &sysmacroname: A newer version of the &sysmacroname macro is available.;
    %put %str(         ) You can get the newer version at this location:;
    %put %str(         ) http://support.sas.com/ctx/samples/index.jsp;
  %end;
 %end;

/* Check arguments. Initialize. */
%if &model1= %then %let model1=Model 1;
%if &model2= %then %let model2=Model 2;
%let dist1=%upcase(&dist1);
%let dist2=%upcase(&dist2);
%let test=%upcase(%substr(&test,1,1));
%if &freq= %then %let f=1;
 %else %let f=&freq;
%if &weight= %then %let w=1;
 %else %let w=&weight;
%let calc=0;

/* DATA= must be specified and data set must exist */
%if &data= or %sysfunc(exist(&data)) ne 1 %then %do;
  %put ERROR: DATA= data set not specified or not found.;
  %goto exit;
%end;

/* RESPONSE= must be specified */
%if &response= %then %do;
  %put ERROR: The RESPONSE= parameter is required.;
  %goto exit;
%end;

/* TEST= must be valid */
%if &test ne B and &test ne V and &test ne C and &test ne L %then %do;
  %put ERROR: TEST= must be VUONG, CLARK, BOTH, or LR.;
  %goto exit;
%end;

/* If LR test, DIST1 must equal DIST2 */
%if &test=L and &dist1 ne &dist2 %then %do;
  %put ERROR: Likelihood ratio test requires same distribution for both models.;
  %goto exit;
%end;

/* If LR test, NPARM1 and NPARM2 must be specified */
%if &test=L and (&nparm1= or &nparm2=) %then %do;
  %put ERROR: NPARM1= and NPARM2= must be specified when TEST=LR.;
  %goto exit;
%end;

%do i=1 %to 2;
/* P1=, P2= must be specified */
  %if &&p&i= %then %do;
    %put ERROR: The P&i= parameter is required.;
    %goto exit;
  %end;
  /* Check for valid DIST1=, DIST2= values */
  %if &&dist&i ne NOR and
      &&dist&i ne BIN and
      &&dist&i ne MULT and
      &&dist&i ne OTH and
      &&dist&i ne GAM and
      &&dist&i ne IG and
      &&dist&i ne NB and
      &&dist&i ne POI and
      &&dist&i ne ZIP and
      &&dist&i ne ZINB 
  %then %do;
    %put ERROR: DIST&i= must be NOR, BIN, MULT, GAM, IG, NB, POI, ZIP, ZINB, or OTH.;
    %goto exit;
  %end;

  /* Must specify SCALE= if NOR, NB, ZINB, GAM, or IG */
  %if (&&dist&i=NOR or &&dist&i=NB or &&dist&i=ZINB or 
       &&dist&i=GAM or &&dist&i=IG) and &&scale&i= %then %do;
    %put ERROR: SCALE&i= must be specified with this distribution.;
    %goto exit;
  %end;
  
  /* Must specify PZERO= with ZIP or ZINB */
  %if (&&dist&i=ZIP or &&dist&i=ZINB) and &&pzero&i= %then %do;
    %put ERROR: PZERO&i= must be specified with this distribution.;
    %goto exit;
  %end;
%end;

/* Must specify either EVENT= or TRIALS= with BIN */
%if (&dist1=BIN or &dist2=BIN) and &trials= and &event= %then %do;
  %put ERROR: For Binomial distribution, specify either EVENT= or TRIALS=.;
  %goto exit;
%end;

%let dsid=%sysfunc(open(&data));
%if &dsid %then %do;
  %if %sysfunc(varnum(&dsid,%upcase(&response)))=0 %then %do;
    %put ERROR: RESPONSE= variable &response not found.;
    %let rc=%sysfunc(close(&dsid));
    %goto exit;
  %end;
  %if %sysfunc(varnum(&dsid,%upcase(&p1)))=0 %then %do;
    %put ERROR: P1= variable &p1 not found.;
    %let rc=%sysfunc(close(&dsid));
    %goto exit;
  %end;
  %if %sysfunc(varnum(&dsid,%upcase(&p2)))=0 %then %do;
    %put ERROR: P2= variable &p2 not found.;
    %let rc=%sysfunc(close(&dsid));
    %goto exit;
  %end;
  %if &pzero1 ne %then %do;
  %if %sysfunc(varnum(&dsid,%upcase(&pzero1)))=0 %then %do;
    %put ERROR: PZERO1= variable &pzero1 not found.;
    %let rc=%sysfunc(close(&dsid));
    %goto exit;
  %end;
  %end;
  %if &pzero2 ne %then %do;
  %if %sysfunc(varnum(&dsid,%upcase(&pzero2)))=0 %then %do;
    %put ERROR: PZERO2= variable &pzero2 not found.;
    %let rc=%sysfunc(close(&dsid));
    %goto exit;
  %end;
  %end;
  %if &trials ne %then %do;
  %if %sysfunc(varnum(&dsid,%upcase(&trials)))=0 %then %do;
    %put ERROR: TRIALS= variable &trials not found.;
    %let rc=%sysfunc(close(&dsid));
    %goto exit;
  %end;
  %end;
  %if &freq ne %then %do;
  %if %sysfunc(varnum(&dsid,%upcase(&freq)))=0 %then %do;
    %put ERROR: FREQ= variable &freq not found.;
    %let rc=%sysfunc(close(&dsid));
    %goto exit;
  %end;
  %end;
  %if &weight ne %then %do;
  %if %sysfunc(varnum(&dsid,%upcase(&weight)))=0 %then %do;
    %put ERROR: WEIGHT= variable &weight not found.;
    %let rc=%sysfunc(close(&dsid));
    %goto exit;
  %end;
  %end;
  %let rc=%sysfunc(close(&dsid));
%end;
%else %do;
  %put ERROR: Could not open DATA= data set.;
  %goto exit;
%end;

/* Verify response is binary if DIST1 or DIST2 is BIN and not events/trials */
%if (&dist1=BIN or &dist2=BIN) and &trials= %then %do;
   ods exclude all;
   ods output nlevels=_nlvls;
   proc freq data=&data nlevels;
    where missing(&response) ne 1;
    table &response;
    run;
   ods select all;
   data _null_;
    set _nlvls;
    call symput ("nlvls",nlevels);
    run;
   %if &nlvls ne 2 %then %do;
    %put ERROR: Binomial response variable, &response, must have exactly two levels.;
    %goto exit;
   %end;
%end;

proc sql;
 reset noprint;
 
 /* If have only one probability in an observation, then response data is
    not the same for the two models and tests cannot be done.
 */
 select
   sum( ((&p1 is missing) and (&p2 is not missing)) or
        ((&p1 is not missing) and (&p2 is missing)) ) as _chk
   into :chk
   from &data;
   
 /* Compute probability of response for specified distribution for each model */
 create table _m(drop=_n _BINresp) as
 select 
   count(&p1) as _n, 
   %if &freq ne %then sum(&freq) as _sumf,;
   %else calculated _n as _sumf,;
   %if (&dist1=BIN or &dist2=BIN) and &trials= %then %do;
     (&response = &event) as _BINresp,
     %let trials=1; %let calc=1;
   %end;
   %else &response as _BINresp,;
   %do _i=1 %to 2;
     &f*&w*
     %if &&dist&_i=ZIP %then 
       log(pdf("poisson",&response,&&p&_i/(1-&&pzero&_i))*
       (1-&&pzero&_i)+(&response=0)*&&pzero&_i);
     %else %if &&dist&_i=POI %then 
       logpdf("poisson",&response,&&p&_i);
     %else %if &&dist&_i=ZINB %then 
       log(pdf("negbinomial", &response, 1/(1+&&scale&_i*&&p&_i/(1-&&pzero&_i)), 1/&&scale&_i) * (1-&&pzero&_i)+(&response=0)*&&pzero&_i);
     %else %if &&dist&_i=NB %then 
       logpdf("negbinomial",&response,1/(1+&&scale&_i*&&p&_i),1/&&scale&_i);
     %else %if &&dist&_i=BIN %then %do;
       logpdf("binomial", %if &calc %then calculated; 
                         _BINresp,&&p&_i,&trials)
       %end;
     %else %if &&dist&_i=NOR %then 
       logpdf("normal",&response,&&p&_i,&&scale&_i);
     %else %if &&dist&_i=GAM %then 
       logpdf("gamma",&response,&&scale&_i,&&p&_i/&&scale&_i);
     %else %if &&dist&_i=IG %then 
       log(1/&&p&_i) + logpdf("igauss",&response/&&p&_i,1/(&&p&_i*&&scale&_i**2));
     %else %if &&dist&_i=MULT or &&dist&_i=OTH %then 
       log(&&p&_i);
     as _l&_i,
   %end;
   /* Compute log probability ratio and Akaike and Schwarz adjustments */
   (calculated _l1) - (calculated _l2) as _m
   %if &nparm1 ne and &nparm2 ne %then %do;
     ,
     (calculated _l1 - &nparm1/(calculated _n)) - 
     (calculated _l2 - &nparm2/(calculated _n)) as _mA,
     
     (calculated _l1 - (log(calculated _sumf)*&nparm1)/(2*(calculated _n))) - 
     (calculated _l2 - (log(calculated _sumf)*&nparm2)/(2*(calculated _n))) as _mS
   %end;
 from &data;

%if &chk=1 %then %do;
  %put ERROR: Predicted values from both models must exist in every observation.;
  %goto exit;
%end;

/* Compute components of Vuong statistics and Clarke statistics and p-values */
ods exclude all;
proc univariate data=_m vardef=n; 
 %if &nparm1 ne and &nparm2 ne %then %do;
   var _m _mA _mS _l1 _l2; 
   output out=_VC 
        sum=sumM sumMA sumMS LL1 LL2 n=n std=stdm 
        msign=signM signMA signMS probm=prsignM prsignMA prsignMS;
 %end;
 %else %do;
   var _m _l1 _l2; 
   output out=_VC 
        sum=sumM LL1 LL2 n=n std=stdm 
        msign=signM probm=prsignM;
 %end;
 run;
 
/* Compute Vuong statistics and p-values */
data _VC;
 set _VC;
 length type $16 PrefModC $40 PrefModV $40;
 %if &test ne L %then %do;
   type="Unadjusted";
   Z = summ/(sqrt(n)*stdm);  PrZ = 2*(1-probnorm(abs(Z)));
   if Z>0 then PrefModV="&model1"; if Z<0 then PrefModV="&model2";
   sign=signM;  prsign=prsignM;
   if sign>0 then PrefModC="&model1"; if sign<0 then PrefModC="&model2";
   output;
   %if &nparm1 ne and &nparm2 ne %then %do;
     type="Akaike Adjusted";
     Z = sumMA/(sqrt(n)*stdm);  PrZ = 2*(1-probnorm(abs(Z)));
     if Z>0 then PrefModV="&model1"; if Z<0 then PrefModV="&model2";
     sign=signMA;  prsign=prsignMA;
     if sign>0 then PrefModC="&model1"; if sign<0 then PrefModC="&model2";
     output;
     type="Schwarz Adjusted";
     Z = sumMS/(sqrt(n)*stdm);  PrZ = 2*(1-probnorm(abs(Z)));
     if Z>0 then PrefModV="&model1"; if Z<0 then PrefModV="&model2";
     sign=signMS;  prsign=prsignMS;
     if sign>0 then PrefModC="&model1"; if sign<0 then PrefModC="&model2";
     output;
   %end;
 %end;
 %if &test=L %then %do;
   type="Likelihood Ratio";
   ChiSq=2*abs(sumM);
   DF=abs(&nparm1-&nparm2);
   PrChi=1-probchi(ChiSq,DF);
 %end;
 run;
ods select all;

/* Display header table with data and model information */
title "The Vuong Macro";
title2 " ";
data _head;
  if _n_=1 then set _VC;
  length item $40 value $40;

  item="Data Set"; value="&data"; output;
  item="Response"; value="&response"; output;
  %if &dist1=BIN %then %do;
    %if &event ne %then %do;
      item="Event Level"; value=cats(&event); output;
    %end;
    %if &trials ne %then %do;
      item="Number of Trials"; value="&trials"; output;
    %end;
  %end;
  %if &freq ne %then %do;
    item="Frequency Variable"; value="&freq"; output;
  %end;
  %if &weight ne %then %do;
    item="Weight Variable"; value="&weight"; output;
  %end;
  item="Number of Observations Used"; value=cats(n); output;

  item="Model 1"; value="&model1"; output;
  item="   Distribution"; value="&dist1"; output;
  item="   Predicted Variable"; value="&p1"; output;
  %if &nparm1 ne %then %do;
    item="   Number of Parameters"; value="&nparm1"; output;
  %end;
  %if &scale1 ne %then %do;
    item="   Scale"; value="&scale1"; output;
  %end;
  %if &pzero1 ne %then %do;
    item="   Zero-inflation Probability"; value="&pzero1"; output;
  %end;
  item="   Log Likelihood"; value=cats(put(LL1,best10.)); output;

  item="Model 2"; value="&model2"; output;
  item="   Distribution"; value="&dist2"; output;
  item="   Predicted Variable"; value="&p2"; output;
  %if &nparm2 ne %then %do;
    item="   Number of Parameters"; value="&nparm2"; output;
  %end;
  %if &scale2 ne %then %do;
    item="   Scale"; value="&scale2"; output;
  %end;
  %if &pzero2 ne %then %do;
    item="   Zero-inflation Probability"; value="&pzero2"; output;
  %end;
  item="   Log Likelihood"; value=cats(put(LL2,best10.)); output;
  stop;
  run;
proc print noobs data=_head label;
  var item value;
  label item='00'x value='00'x;
  title3 "Model Information";
  run;
  
/* Display unadjusted and adjusted statistics and p-values */
%if &test=L %then %do;
proc print data=_VC noobs label;
 var type ChiSq DF PrChi;
 format ChiSq 8.4 PrChi pvalue6.;
 label type="Test" ChiSq="Chi-Square" PrChi="Pr>Chi-Square";
 title3 "Likelihood Ratio Test";
 title4 "H0: models are equivalent";
 title5 "Ha: models differ";
 run;
%end;

%if &test=B or &test=V %then %do;
proc print data=_VC noobs label;
 var type Z PrZ PrefModV;
 format Z 8.4 PrZ pvalue6.;
 label type="Vuong Statistic" PrZ="Pr>|Z|"
       PrefModV="Preferred Model";
 title3 "Vuong Test";
 title4 "H0: models are equally close to the true model";
 title5 "Ha: one of the models is closer to the true model";
 run;
%end;
 
%if &test=B or &test=C %then %do;
proc print data=_VC noobs label;
 var type sign prsign PrefModC;
 format sign 8.4 prsign pvalue6.;
 label type="Clarke Statistic" sign="M" prsign="Pr>=|M|"
       PrefModC="Preferred Model";
 title3 "Clarke Sign Test";
 title4 "H0: models are equally close to the true model";
 title5 "Ha: one of the models is closer to the true model";
 run;
%end;

%exit:
options &_opts;
title;
%mend;


