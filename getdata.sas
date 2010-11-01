*** YorkArts version with logdir=\\lancelot\home01\psych-shared\courses\dat\jorp\logs\;
*** Updated for 2001  3/26/01;
*** Updated for 2002  3/18/02 -- no longer need to change year;
*** Updated to remove V9 bug with pib8.    5/25/05;
*** (test from psych205 account);
*** 1.3 Updated to set userid from NWUSERNAME 2/8/06

options mstored sasmstore=store;
*libname store clear;
%let ver = %substr(&sysver,1,1); 
*libname store  "n:\storev&ver" ;
*libname store  "c:\sasuser\storev&ver";
/*
%macro getdata(
      run=0, design=design, cms=,
      lib=HOME, out=RUN&run,
      verify=OFF, debug=OFF) / store des='Getdata V1.3';
*/

%macro getdata(
      run=0, design=design, cms=,
      lib=HOME, out=RUN&run,
      verify=OFF, debug=OFF);

%if %upcase(&DEBUG)=OFF %then %do;
options nosource2 nomacrogen nomlogic nomprint errors=0;
%end;

%put **** John Thomas Experiment **** (v1.3);
%if &sysscp=WIN %then %fortune(35);
%put ;

options nonotes;
run;

%global userid;
%if &sysscp = WIN
   %then %do;
   	%let userid=%sysget(userid);
   	%if %length(&userid)=0 %then %let userid=%sysget(nwusername);
   	%end;
   %else %let userid=%sysget(USER);
%let abort=0;

/*
%if %substr(&userid,1,3)=LAB %then %do;
   %put ERROR: GETDATA must be run from an individual userid, not a LABnn account;
   %put ERROR: Login with your own account.;
   %let abort=1;
   %goto done;
   %end;
*/

%let error=0;
%if %length(&run)=0 %then %do;
   %put ERROR: Experimental RUN not specified. Use RUN=0,1, or 2.;
   %let abort=1;
   %goto done;
   %end;
%if %length(&run)>1 or %index(0129,&run) = 0 %then %do;
   %put ERROR: Experimental RUN is invalid. Use RUN=0,1, or 2.;
   %let abort=1;
   %goto done;
   %end;


proc iml;
   use &design;
   read all var{delay} into delay;
   read all var{dose } into dose ;
   read all var{task } into task ;
   d1 = design(delay);
   d2 = design(dose );
   d3 = design(task );
   levels = ncol(d1)|| ncol(d2) || ncol(d3);
   lev = char(levels,2,0);
   lev = lev[1]+','+lev[2]+','+lev[3];
   lev = trim(rowcatc(lev));
   call symput('LEVELS',lev);
   quit;

data _null_;
   userid = symget("userid");
   now = time();
   day = day(date());
   hour= hour(now);
   min = minute(now);
   sec = second(now);
   year = put(mod(year(date()),100),z2.);

   file = put(day,z2.) || put(hour,z2.) || put(min,z2.)||
          put(sec,z2.) || '.' ||substr(userid,6,3);
   time = put(hour,z2.) || ':' || put(min,z2.)|| ':' ||
          put(sec,z2.);
   *put  userid=  time= file=;
   call symput('time',time);
   call symput('file',file);
   call symput('year',year);
 run;

%if &out ^= %str() %then %do;
   %if %index(&out,.) = 0 %then
      %let out = &lib..&out;
   %if &sysver > 6.10 %then %do;
      %if %sysfunc(libref(&lib)) %then %do;
      %*-- libname has not been assigned;
         %if &sysscp = WIN %then %do;
            libname &lib "f:\";
         %end;
         %else %do;
            libname &lib "~/sasuser";
         %end;
         %if &syslibrc ne 0 %then %do;
            %put WARNING: Could not allocate &lib library.;
            %put %str(         ) Data will not be copied to permanent file.;
            %let error=1;
         %end;
      %end;
   %end;
%end;

%if &sysscp = WIN %then %do;
   %let logdir =c:\temp\;

   %if &sysver > 6.11
      %then %let logdir=\\lancelot\home01\psych-shared\courses\dat\jorp\logs\;
      %else %let logdir=c:\temp\;
   %end;

%else %if &sysscp = RS6000 %then %do;
   %let logdir=~friendly/psy3030/jorp/logs/;
   %end;
%else %do;
   %let logdir=/tmp/;
   %end;
%let logs = &logdir.&file;
%let logall = &logdir.Proj&year..run;
filename runlog "&logs";
filename alllog "&logall" mod;

%if %upcase(&debug)=ON %then %do;
%put GETDATA: runlog will be written to &logs;
%put GETDATA: alllog will be written to &logall;
%end;

%let verify=%upcase(&verify);
%if &verify=ON or &verify=YES %then %do;
   %put WARNING: VERIFY=ON means no data will be generated.   Your design;
   %put %str(        ) will only be checked for validity and cost;
%end;

data &design;
   keep task dose delay nobs run;
   length task $8;
   set &design  end=eof;               * read the design specs;
   retain error 0 date time userid cms run levels out OK;
   file print;                  /* Put to LISTING file  */

   if _N_ = 1 then do;
      OK = 1;                      /* Assume the run is OK */
      if symget('run') eq ' ' then run=.;
         else run=symget('run');
      userid = symget("userid");
      levels = symget("levels");
      out = compress(symget("out"));

      DATE = DATE();
      TIME = TIME();
      logfn= symget('file');
      put @20 'John Thomas ECS Study' / @20 21*'=' /;
      PUT @5  'Run:     ' run 3. /
          @5  "Levels:    " levels "(Delay,Dose,Task)"/
          @5  'Run on:'   DATE WEEKDATE. ' at ' TIME TIME8. ;
      PUT @5  'For:       ' userid  '  [' logfn ']' //;
      put @5  'GROUP'  @12 'DELAY'  @22 'DOSE'  @32 'TASK'
          @42 'NOBS' / ;
      select(run);
         when(0,1,2,9) ;
         otherwise do;
            msg = 'Invalid experimental RUN (use RUN=0, 1, or 2).';
            link ERRMSG;
            end;
         end; /* select (run) */
      end;   * End of _n_=1 checks;

   group + 1;            * Increment # of groups;

   if run = 0 then do;
      if NOBS= . then do;
         NOBS=4;
         put 'WARNING: NOBS was missing and was set to NOBS=4 for '
         group=;
         end;
      else if NOBS>4 then do;
         put 'WARNING: NOBS was >4 and was set to NOBS=4 for ' group=;
         NOBS = min(NOBS,4);
         end;
      end;

   else if run = 9 then NOBS=1;
   else do;
      if NOBS= . then do;
         msg = 'NOBS must be specified';
         link ERRMSG;
         end;
      end;

   TOTOBS + NOBS;         * Sum number of observations;

   task = upcase(task);
   if length(task) < 8 then do;
      if task =: 'EASY' then task='EASYMAZE';
      if task =: 'HARD' then task='HARDMAZE';
   end;
   if task not in ('EASYMAZE', 'HARDMAZE') then do;
      msg = 'Invalid TASK value = ' || task;
      link ERRMSG;
      end;

   if dose<0 or dose>3 or mod(dose,1)^=0 then do;
         msg = 'Invalid DOSE value = ' || put(dose, 8.1);
         link ERRMSG;
         end;
   if delay<0 or delay>60 then do;
         msg = 'Invalid DELAY value = ' || put(delay, 8.1);
         link ERRMSG;
         end;

   put @5 group 3.  @12 delay 3.  @22 dose 2.  @30 task $8.
      @42 NOBS 3. ;

   if EOF then do;
      if ERROR > 0 then do;
         _ERROR_=1;                * Set error status;
         call symput('ERROR', '1');
         msg = put(ERROR,5.) || ' errors were detected in your '
            || 'design specifications';
         link ERRMSG0;
         end;
      else do;
      put @42 '----' / @42 TOTOBS 4. ' Total observations' ;
      end;

      COST = 5 + .25 * TOTOBS + .5*group;
      put / 'The cost of data collection for your experiment will be '
            COST DOLLAR7.2
          / 'Please print and save this receipt.';
      end;

      RETURN;        * End of normal processing;

*-------------------- Error handling ------------------------;
ERRMSG0:
   ERROR + 1;
   file log;
   put 'ERROR: ' msg ' GROUP=' group;
   file print;
   put '    Error: ' msg ;
   return;

ERRMSG:
   link errmsg0;
   if OK>0 then do;
      Put '*** Experiment Terminating: The rest of your design will'
            ' be checked,'/ '*** but no new observations will be'
            ' collected.';
      end;
      OK = 0;
     return;
run;

%if &error > 0 %then %do;
   %goto done;
   %end;
%if &verify=ON or &verify=YES %then %do;
   %goto done;
   %end;


data results;
   keep run group subj delay dose task log score;
   length subj dose delay 4 log $ 2 userid $ 8;
   set &design  end=eof;               * read the design specs;

   retain seed 1234567 error 0 date time userid cms run levels out;
   array prime{*} p1-p20;
   retain p1   2  p2   3  p3   5  p4   7  p5  11  p6  13 p7 17
       p8  19  p9  23  p10 29  p11 31  p12 37  p13 41 p14 43
       p15 47  p16 53  p17 59  p18 61  p19 67  p20 71;
*     drop userid cms seed teffect nobs msg error totobs cost mse;
*     drop b0-b2  date time trip state base ok l wild td;
*     drop signatur accn sign p1-p20 levels out logfn;

   label score = 'Performance measure on maze'
      dose  = 'Dose of Adrenaline'
      task  = 'Type of maze'
      delay = 'Delay between learning and ECS'
      group = 'Treatment combination'
      run   = 'Experimental Run'
      subj  = 'Subject'
      log   = 'Log Book Comment';

*       file print;                  /* Put to LISTING file  */

   OK = 1;                      /* Assume the run is OK */
   if _N_ = 1 then do;
      userid = symget("userid");
      levels = symget("levels");
      out = compress(symget("out"));

      DATE = DATE();
      TIME = TIME();
      logfn= symget('file');

      if verify(substr(userid,4),'0123456789') = 0
         then accn = input(substr(userid,4), 5.);
         else accn = input(userid,pib2.0);
      accn  = mod(accn,100000);
      if symget('cms') eq ' ' then do;
         if run=9 then cms   = 6;
                  else cms = accn;
         end;
      else cms=symget('cms');

      seed = 2*(1000*run+cms) + 10001;

   end;   * End of _n_=1 checks;

   group + 1;            * Increment # of groups;
   TOTOBS + NOBS;         * Sum number of observations;

   STATE = mod (CMS,2);
   task = upcase(task);
   select(task);
      when ('EASYMAZE')  TEFFECT=10;
      when ('HARDMAZE')  TEFFECT=-10;
      otherwise          TEFFECT=0;
      end;

   signatur + NOBS * (1+delay) * (1+dose) * (1+(task='HARDMAZE'));
   sign + prime(nobs) * prime(1+int(delay/5)) *
        prime(1+dose) * prime(1+(task='HARDMAZE'));
   if STATE=0 then do;
      TD = TEFFECT * (dose-1.5);
      B0 = 20 + 22 * dose;
      B1 = 2  -.25 * dose;
      B2 = 0;
      MSE=30;
      end;

   else do;
      TD = 0;
      B0 = 19.9142  + 22.4972*dose   - 2.464*dose**2 ;
      B1 = 1.1614   - .26028 *dose   - .0136*dose**2 ;
      B2 =-.0082375 + .00128595*dose - 7.925E-05*dose**2;
      MSE=25;
      end;

   BASE=0*(STATE=0) + 10*(STATE=1);
   TRIP = 0;
   do subj= 1 to NOBS;
      WILD = 0;
      log  = '  ';
      if TRIP=0 and uniform(SEED)<0.085 then do;
         WILD = 80*uniform(SEED) - 40;
         TRIP = 1;
         end;
      if WILD^=0 & uniform(SEED) < .8 then do;
         L = 1 + 2*int(3*uniform(SEED));
         log = substr('AFDHSA',L,2);
         end;
      else if uniform(SEED) < .07 then do;
         L = 1 + 2*int(3*uniform(SEED));
         log = substr('EIAAEE',L,2);
         end;

      score = BASE + TEFFECT  + TD
         + B0 + B1 * delay + B2 * delay**2
         + MSE* (run^=9) * NORMAL(SEED)
         + (run^=9)*WILD;
      score = round (score);
      score = max(score, 0);
      if OK then output;
      end;

        if EOF then do;
          if ERROR > 0 then do;
              _ERROR_=1;                * Set error status;
              call symput('ERROR', '1');
              end;

          else do;
          put 'GETDATA: ' TOTOBS 4. ' observations were generated in your design.';

          put 'GETDATA: Your data have been saved in the '
                 "dataset WORK.RESULTS ";
          if out ^= ' ' then do;
          put '         and copied to the dataset ' out;
            end;
          COST = 5 + .25 * TOTOBS + .5*group;
          put  'GETDATA: The cost of data collection for your experiment is '
                 COST DOLLAR7.2 ;

        if OK then do;
          file runlog;
          put  userid $8. +1 CMS Z6. +1 run 1.
                      sign     8. +1 seed 7.
                   +1 date date7. +1 time time5.
                   +1 COST 6.2  group 3. NOBS 4. TOTOBS 4.
                   +1 levels ;
          file alllog mod;
          put  userid $8. +1 CMS Z6. +1 run 1.
                      sign     8. +1 seed 7.
                   +1 date date7. +1 time time5.
                   +1 COST 6.2  group 3. NOBS 4. TOTOBS 4.
                   +1 levels ;
                        end;
          end;
      end; /* if EOF */
run;

%if &error=0 %then %do;
proc print n;
        id group subj;
        var delay dose task log score;
        Title 'Generated data from your Design';
%end;

%if &out ^= %str() %then %do;
data &out;
   set results;
%end;

%done:
options source notes;
%if &abort or &error %then %do;
%put ERROR:  GETDATA found errors in your DESIGN, or some other problem occurred.;
%end;
run;

%mend;

/*
proc catalog cat=store.sasmacr;
   contents;
   title 'Stored compiled macros';
   run;quit;
*/
