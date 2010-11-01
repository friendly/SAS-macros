/*
From: dmclerra@MY-DEJA.COM (Dale McLerran)
Newsgroups: comp.soft-sys.sas
Subject: Re: Random Proc Print
Date: 27 Jun 01 17:29:39 GMT
Sender: saslmnt@LISTSERV.UGA.EDU
Reply-To: Dale McLerran <dmclerra@MY-DEJA.COM>
Lines: 167

Bruce,

This follows a long line of questions about obtaining a random sample
of explicit size from a specified dataset.  Here is a macro which
can be used for your needs.  Previous replies which I have seen have
suggested using a where statement in which a uniform random number
is compared to a cutoff point which has value 50/nobs.  That approach
is valid if you are willing to settle for a random sample of
approximately 50 observations.  The macro below will return exactly
50 observations.
*/

%macro ransamp(indat=, outdat=, view=NO, nout=, seed=0, weight=1);

/************************************************************************/
/*                                                                      */
/*  Generate a random sample of fixed size &NOUT from dataset &INDAT    */
/*  in one pass through the data.  This algorithm is preferred to       */
/*  generating random numbers, sorting the data by the random numbers,  */
/*  and inputting the first &NOUT of the sorted dataset.  Output the    */
/*  file to &OUTDAT.                                                    */
/*                                                                      */
/*  If a weight variable is employed, then two passes through the data  */
/*  are required, the first to determine the total of all weights in    */
/*  the dataset, and the second to do the sampling.                     */
/*                                                                      */
/*  AUTHOR:   Unknown (pulled from SAS-L)                               */
/*  MODIFIED: Dale McLerran, 5/12/2000 to incorporate weights           */
/*                                                                      */
/************************************************************************/
/*                                                                      */
/* Macro variables:                                                     */
/*                                                                      */
/*    INDAT   - names the input dataset from which to draw the random   */
/*              sample (required)                                       */
/*    OUTDAT  - names the output dataset containing a random sample of  */
/*              records from INDAT (required)                           */
/*    VIEW    - create OUTDAT as a view rather than a dataset.  This    */
/*              allows random sampling at the time that the data are    */
/*              used in a procedure.  (Not required, default is to      */
/*              create a dataset for later use.)                        */
/*    NOUT    - specifies how many records to select from INDAT and     */
/*              output to OUTDAT (required)                             */
/*    SEED    - seed to initialize the random number process.  Default  */
/*              is to use the computer clock to start the process, but  */
/*              it is highly recommended that you specify a seed of     */
/*              your own.  By specifying your own seed, you can always  */
/*              replicate the process, if necessary.  Using the clock   */
/*              to start the sampling process is not replicable.  Seed  */
/*              values can be any positive integer value less than or   */
/*              equal to (2**31) - 1.  (Not required)                   */
/*    WEIGHT  - Specifies a variable containing weights for weighted    */
/*              random sampling.  If a weight variable is specified, a  */
/*              preliminary step is performed which allows normalization*/
/*              of the total weights to the frequency of the number of  */
/*              records in the input dataset.                           */
/*                                                                      */
/************************************************************************/
/*                                                                      */
/* Example: Print two different random subsets of 50 observations from  */
/*          the input dataset MYDATA.  The RANSAMP macro is called      */
/*          only once, but it generates a view in which a clock time    */
/*          seed is employed.  Each time the view is employed, the      */
/*          clock time will be different, generating a different random */
/*          sample.  If you wanted to have one random sample from the   */
/*          data that was used repeatedly, then you would probably want */
/*          to write the random sample out as a dataset rather than a   */
/*          view.  Further control over the sampling is obtained by     */
/*          specifying a non-zero seed for initializing the random      */
/*          number generator.                                           */
/*                                                                      */
/*          %ransamp(indat=mydata,                                      */
/*                   outdat=random50,                                   */
/*                   view=yes,                                          */
/*                   nout=50)                                           */
/*          * Note that no sample has yet been generated, only;         */
/*          * instructions for how to generate a random sample;         */
/*          proc print data=random50;                                   */
/*          run;                                                        */
/*          * The second proc print will demonstrate that use of a;     */
/*          * view and seed=0 results in a different random sample;     */
/*          * each time the view is accessed.                           */
/*          proc print data=random50;                                   */
/*          run;                                                        */
/*                                                                      */
/************************************************************************/


%let view=%upcase(&view);

%if %upcase(&weight)=P %then %let compare=prob;
%else                        %let compare=p;

%if &weight^=1 %then %do;
  data _null_;
    set &indat end=lastrec;
    __tprob + &weight;
    if lastrec then do;
      call symput("totprob",__tprob);
    end;
  run;
%end;

data &outdat
  %if &view=YES %then %do;
    / view=&outdat
  %end;
  ;
  set &indat nobs=big_n;
  retain k v;
  if _n_=1 then do;
    k = &nout;
    %if &weight=1 %then v = big_n;
    %else               v = &totprob;
    ;
  end;
  &compare = ranuni(&seed);
  if &compare <= &weight*k/v then do;
    k = k - 1;
    output;
  end;
  v = v - &weight*1;
  drop k v p;
run;

%mend;


