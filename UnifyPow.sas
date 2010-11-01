/*

------------------------------------------------------------------------
FOR INFORMATION AND LATEST VERSION: http://www.bio.ri.ccf.org/power.html
------------------------------------------------------------------------

UnifyPow.sas, version 97.10.beta
1997 Copyright (c) by Ralph G. O'Brien, PhD
Department of Biostatistics and Epidemiology
Cleveland Clinic Foundation
Cleveland, OH 44195
Voice: 216-445-9451
Fax: 216-444-8023
robrien@bio.ri.ccf.org


This single file contains all components of UnifyPow. You simply put
it in an appropriate directory on your system and %include it in your
SAS run. It is distributed so that it runs in the way I have been
demonstrating in my workshops. Technically, it is not a true macro
in this form, but there are instructions soon below explaining how
to easily convert it to one. (There is usually no compelling reason
to do this.)


-------------------------------------------
RUNNING UnifyPow AS DISTRIBUTED (NON-MACRO)
-------------------------------------------

Regardless of the platform, to run this "non-macro" version of
UnifyPow and get the standard tables, simply follow this template:
----------------------------------------------------------------------
options ls=78 nosource2;
%let UnifyPow = file_specification;

%include "&UnifyPow";
title1 "A Title for Problem 1.";
datalines;
{set of UnifyPow statements here}
%tables;

%include "&UnifyPow";
title1 "A Title for Problem 2.";
datalines;
{second set of UnifyPow statements here}
%tables;
----------------------------------------------------------------------


--------------------------------
THE FILE SPECIFICATION STATEMENT
--------------------------------

UNIX is my everyday platform); I use something like:
----------------------------------------------------------------------
%let UnifyPow = /home/robrien/SASmacros/UnifyPow9710.sas;
----------------------------------------------------------------------

Windows 95 is pretty mysterious to me, but I put UnifyPow9710.sas
in the SAS directory and use something like this:
----------------------------------------------------------------------
%let UnifyPow = UnifyPow9710.sas;
----------------------------------------------------------------------


-----------------------
MAKING UnifyPow A MACRO
-----------------------

To make UnifyPow a macro, search for "MAKE MACRO" in the
code below to find instructions for changing the few lines needed.


-----------------------------------------------
RUNNING UnifyPow AFTER YOU HAVE MADE IT A MACRO
-----------------------------------------------

Regardless of the platform, to run the macro version of UnifyPow
and get the standard tables, simply follow this template:
----------------------------------------------------------------------
options ls=78 nosource2;
%include file_specification;  {described above}

title1 "A Title for Problem 1.";
%readspec; datalines;
{set of UnifyPow statements here}
%UnifyPow;

title1 "A Title for Problem 2.";
%readspec; datalines;
{second set of UnifyPow statements here}
%UnifyPow;
----------------------------------------------------------------------


---------------------------------------------------
EXPERIENCED SAS USERS MAY CUSTOMIZE UnifyPow OUTPUT
---------------------------------------------------

All results from each UnifyPow problem are stored in a temporary SAS
data set called PowData. Knowing this, experienced SAS users may
easily customize their output by merging results from two or more
problems and by using their own PROC TABULATE or SAS/GRAPH code. The
examples.sas file I distribute should have one or two examples of
this. I recommend that you first examine the structure of PowData
by just seeing what it holds.

Non-macro version:
----------------------------------------------------------------------
options ls=78 nosource2;
%let UnifyPow = file_specification;

%include "&UnifyPow";
datalines;
{set of UnifyPow statements here}
proc print data=PowData;
----------------------------------------------------------------------

Macro version:
----------------------------------------------------------------------
options ls=78 nosource2;
%include file_specification;

%readspec; datalines;
{set of UnifyPow statements here}
%UnifyPow;
proc print data=PowData;
----------------------------------------------------------------------


----------------------
UnifyPow LEGAL NOTICES
----------------------

THIS SOFTWARE IS MADE AVAILABLE "AS IS".

UnifyPow is a trademark of Ralph G. O'Brien. No commercial use
of this trademark may be made without prior written permission of
Ralph O'Brien.

All UnifyPow software and its included text and accompanying
documentation are Copyright 1997 by Ralph G. O'Brien.

Permission to use, copy, modify, and distribute this software
and its documentation for any purpose and without fee to Ralph
O'Brien is hereby granted, provided that these legal notices appear in
all copies and supporting documentation, that the name "UnifyPow" is
retained, and that the names of Ralph O'Brien and the Cleveland Clinic
Foundation are not used in advertising or publicity pertaining to
distribution of the software without the specific, written prior
permission of Ralph O'Brien.

Although the above trademark and copyright restrictions do not convey
the right to redistribute derivative works, Ralph O'Brien encourages
unrestricted distribution of patches or ancillary code which can be
applied to or used in conjunction with Ralph O'Brien's distribution.

If this software is modified for local use, please denote this on all
modified versions of the software by appending the letter "L" to the
current version number and by noting the changes in the code itself
and in the associated documentation.

    RALPH G. O'BRIEN AND THE CLEVELAND CLINIC FOUNDATION DISCLAIM
    ALL WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO THIS SOFTWARE,
    INCLUDING WITHOUT LIMITATION ALL IMPLIED WARRANTIES OF
    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, AND IN NO
    EVENT SHALL RALPH G. O'BRIEN OR THE CLEVELAND CLINIC FOUNDATION
    BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA, OR
    PROFITS, WHETHER IN AN ACTION OF CONTRACT, TORT (INCLUDING
    NEGLIGENCE) OR STRICT LIABILITY, ARISING OUT OF OR IN CONNECTION
    WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.



-----------------------------------------
REPORTING PROBLEMS AND MAKING SUGGESTIONS
-----------------------------------------

UnifyPow's computations are checked thoroughly using multiple
methods: comparing its results to those obtained by using other
software, to those in published tables and examples, and to
results obtained from Monte Carlo simulation. No true bugs have
yet been reported to me about UnifyPow's numerical accuracy, but
no software can be said to be totally free of problems. I really
do appreciate knowing if you encounter difficulties or have
suggestions for improvements. Most of the additions to UnifyPow
come about this way. On the other hand, I cannot promise to
address everyone's queries, especially those that are mostly
related to consulting advice. I correspond best via email.

Do not modify this code without obtaining written permission from Ralph
O'Brien.  If changes are needed for some purpose, have the wisdom and
courtesy to consult him.



---------------
CITING THE WORK
---------------

If you use this work, please cite it--something like
     ... using UnifyPow, a macro for the SAS System (O'Brien, 1997).

This reference is
     O'Brien, RG (1997), "UnifyPow: A SAS Macro for Sample-Size Analysis,"
       Proceedings of the 22nd SAS Users Group International Conference,
       Cary NC: SAS Institute, 1353-1358.

Another key reference for this work is:
     O'Brien, RG and Muller, KE (1993), "Unified Power Analysis
       for t Tests through Multivariate Hypotheses," in Edwards, EK.
       (Ed.), Applied Analysis of Variance in the Behavioral Sciences,
       New York: Marcel Dekker, pp. 297-344.

Actually, that chapter covers freeware called OneWyPow.sas and other
%include modules that are now unified in this macro. UnifyPow offers a
simpler interface and a far larger set of methods.



---------------------------------------
OBTAINING FREEWARE SAS CODE AND UPDATES
---------------------------------------

UnifyPow is freeware distributed primarily via the anonymous ftp
site at the Department of Biostatistics and Epidemiology at the
Cleveland Clinic Foundation. Downloading new versions periodically
ensures that you are getting the latest version I feel is safe for
public distribution.

You can use your web browser (e.g., Netscape Navigator/Communicator)
to get the files. Just go to
                  http://www.bio.ri.ccf.org/power.html
and follow the instructions.

You can also download the files directly from the relevant ftp address,
                          ftp.bio.ri.ccf.org
and directory,
                             UnifyPow.all
in the root directory (not in /pub). If you know how to obtain
documents via anonymous ftp, this is all the information you need.


------------------------
UnifyPow.sas SOURCE CODE
------------------------


*/

%macro tables;
/*
All kinds of tables for displaying results.
Easy to customize.
*/

/*
First do the trimming of results for one- or two-tailed tests.
Both types are always computed when appropriate, so when I
decided to add the TAILS/SIDES option it was just much easier to strip
the results, rather than prevent their computation to begin
with.  This is wasteful of computing cycles, so someday I should redo 
this.
*/
%TrimTail;

/*
The tables are constructed so that they will handle either "Power" or
"NTotal" (TotalN or TotalPairs or PairsTotal) statments.
*/

%if &ResltVar = power %then %do;
  %let SpecVar = NTotal;
  %let result = %str(Power="Power"*mean=" ");
  %let spec = %str(Ntotal="Total N");
  %if &ProbType = McNemar %then
    %let spec = %str(Ntotal="Pairs");
  %if &ProbType = PairedMu %then
    %let spec = %str(Ntotal="Total Pairs");
  %let format = 4.3;
  %end;
%if &ResltVar = NTotal %then %do;
  %let SpecVar = NomPower;
  %let result = %str(Ntotal="Total N"*mean=" ");
  %if &ProbType = McNemar %then
    %let result = %str(Ntotal="Pairs"*mean=" ");
  %if &ProbType = PairedMu %then
    %let result = %str(Ntotal="Total Pairs"*mean=" ");
  %let spec = %str(NomPower="Minimum Power");
  %let format = 6.0;
  %end;
%if %index(&TablType, WlcxnPow) > 0 %then
  %let AddRow2 = %str(* parent = "Parent");

%if &TablType = GnrlPow %then %do;
/* all alphas in one table, no SD or parent */
proc tabulate format = &format order=data;
 class alpha effctitl testtype &SpecVar ProbStmt;
 var &ResltVar;
 table
  ProbStmt="Scenario:",
  effctitl="Method" * testtype="Type",
  alpha * &spec * &result
           /rtspace=28;
%if &fnote=2pi %then %do;
  footnote1
"*The Approximate Unconditional corresponds to the Ordinary Pearson";
  footnote2
"chi-square test for a 2 x 2 table.  Technically, the method here  ";
  footnote3
"uses a regular t test with Y = 0 (no) or 1 (yes), which is known  ";
  footnote4
"to offer more accurate p-levels and can be done with any standard ";
  footnote5
"t-test routine. See D'Agostino, Chase, and Belanger (1988),       ";
  footnote6
"American Statistician, 1988, 42:198-202.                          ";
  footnote7 " ";
  footnote8
"**The Exact Unconditional corresponds to the test proposed by     ";
  footnote9
"Suissa and Shuster (1985), J Royal Stat Soc A, 148:317-327).      ";
  run;
  %end; /* 2pi */
run;
footnote1; footnote2; footnote3; footnote4; footnote5;
footnote6; footnote7; footnote8; footnote9;
%end; /* GnrlPow */


%if &TablTyp3 = Pi1Specl %then %do;
/* tables of TruAlpha and critical values for 1-group binomial problem */
  proc tabulate format=6.0 order=data;
  class SpcTitl3 effctitl alpha testtype &SpecVar;
  var TruAlpha LoCrit HiCrit;
  table SpcTitl3 = " ",
    effctitl="Method" * &spec * testtype="Type",
    alpha*(TruAlpha = "Actual Alpha"*mean=" "*F=6.3
           LoCrit="Lower Crit Value"*mean=" "
           HiCrit="Upper Crit Value"*mean=" ")
  /rtspace = 28;

  footnote1
"These critical values are part of the rejection region.";
  footnote2
"The note above describes how they are set.";
run;
footnote1; footnote2;

%end;  /* pi1Specl */       

%if &TablType = tPow %then %do;
/* all alphas in one table, does not give parent */
  proc tabulate format = &format order=data;
    class alpha effctitl testtype SD &SpecVar ProbStmt;
    var &ResltVar;
    table
     ProbStmt="Scenario:"* effctitl=" ",
     alpha= "Alpha" * testtype="Type",
     SD="Standard Deviation" * &spec * &result
           /rtspace=28;
   %end; /* tpow */


%if &TablType = WlcxnPow %then %do;
   proc tabulate format = 4.3 order=data;
     class parent SD ProbStmt;
     var Wp1 Wp2 W&p3or4;
     table ProbStmt="Scenario:", parent="Parent",
       mean="Nonparametric Moments"*(Wp1="p1" Wp2="p2" W&p3or4="&p3or4")*
              SD="&SDType" / rtspace = 11;

/* separate tables for each alpha, gives parent */
   proc tabulate format= &format order=data;
     class alpha effctitl testtype parent SD &SpecVar ProbStmt;
     var &ResltVar;
     table
       ProbStmt="Scenario:"*alpha="Alpha:",
       effctitl="Method"* testtype="Type" *parent="Parent",
       SD="Standard Deviation" * &spec * &result
           /rtspace=28;
     %end;  /* TablType = WlcxnPow */


%if &TablType = 1or2WlcxnPow %then %do;
 %if &TblMmnts = yes %then %do;
  %if &SDtype ne none %then %do;
    proc tabulate format=4.3 order=data;
      class parent SD ProbStmt;
      var Wp1 Wp2 W&p3or4;
      table ProbStmt="Scenario:", parent="Parent",
      mean="Nonparametric Moments"*(Wp1="p1" Wp2="p2" W&p3or4="&p3or4")*
      SD="&SDType" / rtspace = 11;

    %end; /* SDtype ne none (1) */
 
 %if &SDType = none %then %do;
   proc tabulate format=4.3 order=data;
    class parent ProbStmt;
    var Wp1 Wp2 W&p3or4;
    table ProbStmt="Scenario:", parent = "Parent",
       mean="Nonparametic Moments" * (Wp1="p1" Wp2="p2" W&p3or4="&p3or4")
       /rtspace=11;
   %end;  /* SDType = none (1) */
 %end; /* TblMmnts = yes */

 %if &SDtype ne none %then %do;
/* separate tables for each alpha, gives parent */
   proc tabulate format= &format order=data;
     class alpha effctitl testtype parent SD &SpecVar ProbStmt;
     var &ResltVar;
     table ProbStmt="Scenario:"*alpha="Alpha:",
       effctitl="Method"* testtype="Type" *parent="Parent",
       SD="&SDtype" * &spec * &result
           /rtspace=28;
   %end; /* SDtype ne none (2) */

 %if &SDtype = none %then %do;
  proc tabulate format=&format order=data;
    class alpha effctitl &DistList testtype parent &SpecVar ProbStmt;
    var &ResltVar;
    table ProbStmt="Scenario:" &OthScen,
      effctitl="  Method"* testtype="Type",
      alpha="Alpha" * &spec * &result
           /rtspace=28;
  %end;  /* SDType = none (2) */
 %end; /* TablType = 1or2WlcxnPow */


%if &TablType = WlcxMuPpow %then %do;

  data PowDatBB; set PowData; if Wp1 ne .;
  proc tabulate data=PowDatBB format=6.3 order=data;
    class parent SDP Trials ProbStmt;
    var Wp1 Wp2 W&p3or4;
    table ProbStmt="Scenario:", parent="Parent",
      mean="Nonparametric Moments"*(Wp1="p1" Wp2="p2" W&p3or4="&p3or4")*
      SDP="SD(P)" * Trials="&TrlName" / rtspace = 11;

/* separate tables for each alpha, gives parent */
  proc tabulate data=PowData format=&format order=data;
    class alpha effctitl testtype parent SDP Trials &SpecVar ProbStmt;
    var &ResltVar;
    table ProbStmt="Scenario:"*alpha="Alpha:",
      effctitl="Method"* testtype="Type" *parent="Parent",
     SDP="SD(P)" * Trials="&TrlName" * &spec * &result
           /rtspace=28;
  %end;  /* TablType = WlcxMuPpow */


%if &TablType = WlcxPaMuPow %then %do;
  proc tabulate format=4.3 order=data;
    class parent parent SDMult Corr ProbStmt;
    var Wp1 Wp2 W&p3or4;
    table ProbStmt="Scenario:",
      parent="Parent",
      SDMult="x SD Multiplier" * Corr="Corr(Y1, Y2)" *
      mean="Nonparametric Moments"*(Wp1="p1" Wp2="p2" W&p3or4="&p3or4")
      / rtspace = 11;

  proc tabulate format=&format order=data;
    class alpha effctitl testtype parent SDMult Corr &SpecVar ProbStmt;
    var &ResltVar;
    table ProbStmt="Scenario:" * alpha= "Alpha",
      effctitl="Method" * testtype="Type" * parent="Parent",
      SDMult="x SD (SD Multiplier)" * Corr="Corr(Y1, Y2)" * &spec *
        &result
           /rtspace=28;
  %end;  /* TablType = WlcxPaMuPow */


%if &TablType = tPaMuPow %then %do;
  proc tabulate format=&format order=data;
    class alpha effctitl testtype SDMult Corr &SpecVar ProbStmt;
    var &ResltVar;
    table ProbStmt="Scenario:"* effctitl=" ",
      alpha= "Alpha" * testtype="Type",
      SDMult="x SD (SD Multiplier)" * Corr="Corr(Y1, Y2)" * &spec * &result
           /rtspace=28;
  %end;  /* TablType = tPaMuPow */


%if &TablType = FPaMuPow %then %do;
  proc tabulate format=&format order=data;
    class alpha effctitl testtype SDMult Corr &SpecVar ProbStmt;
    var &ResltVar;
    table ProbStmt="Scenario:"* effctitl=" ",
      alpha= "Alpha" * testtype="Type",
      SDMult="x SD (SD Multiplier)" * Corr="Corr(Y1, Y2)" * &spec * &result
            /rtspace=28;
  %end;  /* TablType = FPaMuPow */


%if &TablType = tmuP_Pow %then %do;
  proc tabulate format=&format order=data;
    class alpha effctitl testtype SDP Trials &SpecVar ProbStmt;
    var &ResltVar;
    table ProbStmt="Scenario:"* effctitl=" ",
      alpha= "Alpha" * testtype="Test Type",
      SDP="SD(P)" * Trials="&TrlName" * &spec * &result
           /rtspace=28;
  %end;  /* TablType = tmuP_Pow */

%if &TablType = FMuP_Pow %then %do;
  proc tabulate format=&format order=data;
    class alpha effctitl testtype SDP Trials &SpecVar ProbStmt;
    var &ResltVar;
    table ProbStmt="Scenario:",
      effctitl=" " * alpha= "Alpha" * testtype="Test Type",
      SDP="SD(P)" * Trials="&TrlName" * &spec * &result
           /rtspace=28;
  %end;  /* TablType = FMuP_Pow */


%if &TablType = FPow %then %do;
/* condensed tables for F tests */
  proc tabulate  format = &format  order=data;
    class alpha effctitl testtype SD &SpecVar ProbStmt;
    var &ResltVar;
    table ProbStmt="Scenario:",
      effctitl="Test" * alpha="Alpha" * testtype="Type",
      SD="Standard Deviation" * &spec * &result
            /rtspace=28;
  %end;  /* TablType = FPow */


%if &TablType = 1betaOLS %then %do;
  proc tabulate format=&format order=data;
    class alpha BetaWt tolernce SDx testtype SD &SpecVar;
    var &ResltVar;
    table alpha="Alpha:"*BetaWt="Beta Coefficient:",
      tolernce="Tol(X)" *SDx="SD(X)" * testtype="Type",
      SD="SD(Resid)" * &spec * &result

           /rtspace=28;
  %end;  /* TablType = 1betaOLS */

%if ((&TablTyp2 = FindNPow) and 
     (&TablType ne none)) %then %do;
/* Tables the powers after doing FindN problem  */
proc tabulate format = 4.3 order=data;
 class TitlDlta alpha effctitl testtype NomPower parent ProbStmt;
 var power;
 table TitlDlta="",
  effctitl= "Test" * testtype="Type" &AddRow2,
  alpha * NomPower="Minimum Power" * Power="Power"*mean=" "
           /rtspace=28;
%end; /* FindNPow */


%if &TablType = none %then %do;
  %put ..............................................;
  %put The user specified that no tables be produced.;
  %put ..............................................;
  %end;  /* TablType = none */

  run;
%mend tables;


%macro readspec (fname=temp);
/* Simple utility for input used when UnifyPow is a macro. */
data _null_;  file &fname; input; put _infile_;
%mend readspec;

%macro FindN (GetPow, Cntinu);
/*
Secant method to find Ntotal such that power(Ntotal) = NomPower.
Starting values are carefully found to ensure convergence.

GetPow is the statement label to begin power computation.
Cntinu is the statement label to go to after convergence.
*/
  if Step = 1 then do;
    FindingN = 1;
    MinPossN = NumGrps + 1;
    if rhoProb then MinPossN = NumGrps*4;
    Ntotal = MinPossN;
    Step = 2;
    go to &GetPow;
    end;

  if Step = 2 then do;
    if (Ntotal = MinPossN) and (power > NomPower) then do;
      put //
       "WARNING: Power exceeds " NomPower "using smallest possible NTotal.";
      put
       "         Are your specifications correct?";
      go to &Cntinu;
      end;

    Diff = power - NomPower;
    if Diff < 0 then do;
      Ntotal = 2*Ntotal;
      OldDifLo = Diff;
      go to &GetPow;
      end;
    NtotHi = Ntotal;
    DiffHi = Diff;
    NtotLo = Ntotal/2;
    DiffLo = OldDifLo;

    Ntotal = NtotLo + (NtotHi - NtotLo)/(1 - DiffHi/DiffLo);
    if not(Ntotal > 0) then do; %TroubMsg; end;
    Step = 3;
    go to &GetPow;
    end;

  if Step = 3 then do;
    Diff = power - NomPower;
    if abs(Diff) < .00049 then do;
      FindingN = 0;
      go to &Cntinu;
      end;


    if Diff < 0 then do;
      NtotLo = Ntotal;
      DiffLo = Diff;
      end;
    else do;
      NtotHi = Ntotal;
      DiffHi = Diff;
      end;

    Ntotal = NtotLo + (NtotHi - NtotLo)/(1 - DiffHi/DiffLo);
    if not(Ntotal > 0) then do; %TroubMsg; end;
    go to &GetPow;
    end;
  %mend FindN;


  %macro TroubMsg;
    put // "WARNING: Despite extensive testing, the iterative method";
    put    "         to find NTtotal for a specified power has failed";
    put    "         to converge.  NTotal was set to missing.";
    put / "         Please report this to";
    put    "                        Ralph O'Brien";
    put    "                        robrien@bio.ri.ccf.org";
    Ntotal = .;
    power = .;
    go to &Cntinu;
    %mend TroubMsg;



%macro FindNBnl (GetPow, Cntinu);
/*
Finds minimum N = Ntotal for binomial such that power(N) > NomPower.

GetPow is the statement label to begin power computation.
Cntinu is the statement label to go to after convergence.
*/
  if Step = 1 then do;
    FindingN = 1;
    N_try = 1;
    Step = 2;
    go to &GetPow;
    end;

  if Step = 2 then do;
    if (N_try = 1) and (power > NomPower) then do;
      put //
       "WARNING: Power of " NomPower "is reached with NTotal = 1.";
      put
       "         Are your specifications correct?";
      FindingN = 0;
      go to &Cntinu;
      end;

    if power < NomPower then do;
      N_try = N_try*2;
      go to &GetPow;
      end;

    N_tryHi = N_try;
    N_tryLo = N_try/2;
    if (N_tryHi - N_tryLo) < 5 then Step = 4;
    else do;
      Step = 3;
      N_try = N_tryHi - .5*(N_tryHi - N_tryLo);
      go to &GetPow;
      end;
    end;

  if Step = 3 then do;
    if power > NomPower then N_tryHi = N_try;
    else N_tryLo = N_try;
    if (N_tryHi - N_tryLo) < 5 then do;
      Step = 4;
      N_try = N_tryLo + 1;
      go to &GetPow;
      end;
    else do;
      N_try = N_tryHi - .5*(N_tryHi - N_tryLo);
      go to &GetPow;
      end;
    end;

  if Step = 4 then do;
    if power > NomPower then do;
      FindingN = 0;
      go to &Cntinu;
      end;
    N_try = N_try + 1;
    go to &GetPow;
    end;
  %mend FindNBnl;


%macro TrimTail;
/* Keeps only results specified by TAILS/SIDES statement */
data PowData; set PowData;
if &KeepTail = 3 then go to outTrimT;
if tails = &KeepTail;
outTrimT:
%mend TrimTail;


%macro CheckWp (Wp1, Wp2, Wp3or4);
  if ((NumGrps = 1) and ((&Wp2 < &Wp1) or (&Wp2 < &Wp3or4))) or
     ((NumGrps = 2) and ((&Wp2 ge &Wp1) or (&Wp3or4 ge &Wp1))) then do;
       DoLH = 0;
       DoNoe = 1;
       if NumGrps = 1 then put
         // "WARNING: A situation makes p2 < p1 or p2 < p4.";
       if NumGrps = 2 then put
         // "WARNING: A situation fails to make p2 < p1 and p3 < p1.";
    put     "         Only Noether's method will be used."
        /   "         Use results with caution.  Perhaps try different"
        /   "         numbers of items.";
    end;
%mend CheckWp;


%macro SDBetBnl (muP, SDP, Trials, p_beta, q_beta, SDBB);
/*
SDBB is std dev of Beta-Binomial outcome, Y/Trials, defined as follows.

Subject's "true score" success rate, P, is distributed as standard
beta random variable with mean MuP and std dev SDP. A check is made
that this beta density is unimodal.

Given P, Y is dist'd as binomial(Trials, P).  Y is called a beta-binomial
random variable, also known as the negative hypergeometric.

The subject's success probability, Y/Trials, is the random variable of interest.
*/

/*
Theory in Johnson, Kotz, Balakrishnan, [Continuous Univariate
Distributions, Vol. 2, 2nd Ed., 1995], Equations 25.28 & 25.29.
Note: JKB's "p" = p_beta and "q" = q_beta.
*/
pPLUSq = &muP*(1 - &muP)/&SDP**2 - 1;
&p_beta = (&muP**2)*(1 - &muP)/(&SDP**2) - &muP;
&q_beta = pPLUSq - &p_beta;
if (&p_beta lt 1) or (&q_beta lt 1) then do;
  put // "ERROR: With mu(P) = " &muP "and SD(P) = " &SDP;
  put    "       the resulting beta distribution for P is not unimodal.";
  stop; end;

/*
Theory in Johnson, Kotz, and Kemp [Univariate Discrete
Distributions, 2nd Ed., 1992], Equation 6.48.
Note: JKK's "alpha" = p_beta and "beta" = q_beta.
*/
&SDBB = &Trials*&p_beta*&q_beta*(&p_beta+&q_beta+&Trials);
&SDBB = &SDBB/(((&p_beta+&q_beta)**2)*(&p_beta+&q_beta+1));
&SDBB = sqrt(&SDBB)/&Trials;

%mend SDBetBnl;


%macro ProbBBnl(X, p_beta, q_beta, N, PrX);
/*
Computes probabilities for beta-binomial random variable X:
  PrX = Prob[X = x | P, N] where N fixed, P distd as beta(p_beta, q_beta).
Uses (6.18) in Johnson, Kotz, and Kemp [Univariate Discrete
Distributions, 2nd Ed., 1992].
*/
%NBnlCoef(&p_beta, &X, result1);
%NBnlCoef(&q_beta, &N-&X, result2);
%NBnlCoef(&p_beta+&q_beta, &N, result3);
&PrX = (result1/result3)*result2;
%mend ProbBBnl;


%macro NBnlCoef (NBn, NBr, NBresult);
/*
   Finds /-n\
         \ r/, using Eq. 1.10 in Johnson, Kotz, Kemp, 1992.
*/
%BnmlCoef(&NBn+&NBr-1, &NBr, &NBresult);
&NBresult = ((-1)**(&NBr))*&NBresult;
%mend NBnlCoef;


%macro BnmlCoef(BCn, BCr, n_over_r);
/*
   Finds /n\
         \r/, using gamma function.
*/
&n_over_r = log(gamma(&BCn+1)) - log(gamma(&BCn-(&BCr)+1))
            - log(gamma(&BCr+1));
&n_over_r = exp(&n_over_r);
%mend BnmlCoef;


%macro SetCumBB(p, q, Trials, CumDist);
/*
Sets the cumulative distribution function of a beta-binomial with
parameters P ~ beta(p, q) and Y ~ binomial(Trials, P).
*/
%let i = i_CumDst;
do &i = 0 to &Trials;
  %ProbBBnl(&i, &p, &q, &Trials, prob_i);
  if &i = 0 then &CumDist{0} = prob_i;
  else &CumDist{&i} = &CumDist{&i-1} + prob_i;
  end;
%mend SetCumBB;


%macro StWlBB2G(p_betaX, q_betaX, p_betaY, q_betaY, Trials, Wp1, Wp2, Wp3);
/*
Set p1, p2, p3 parameters for 2-group Wilcoxon problem for beta-binomial
parent. Lots of ties require adjustments:
   p1 = Pr[Y > X] + .5*P[Y = X],
   p2 = Pr[{Yi > Xk} and {Yi' > Xk}] + 0.50*Pr[{Yi = Xk} and {Yi' > Xk}]
          + 0.25*Pr[{Yi = Xk} and {Yi' = Xk}],
   p3 = Pr[{Yi > Xk} and {Yi > Xk'}] + 0.50*Pr[{Yi = Xk} and {Yi > Xk'}]
          + 0.25*Pr[{Yi = Xk} and {Yi = Xk'}].

*/
%let pX = pX_BB2G;  %let qX = qX_BB2G;
%let pY = pY_BB2G;  %let qY = qY_BB2G;
%let muX = muX_BB2G;  %let muY = muY_BB2G;
&pX = &p_betaX;  &qX = &q_betaX; &pY = &p_betaY;  &qY = &q_betaY;

redoBB2G:
&muX = &pX/(&pX+&qX);  &muY = &pY/(&pY+&qY);
%SetCumBB(&pX, &qX, &Trials, cumX);
%SetCumBB(&pY, &qY, &Trials, cumY);
%let X = X_SetCBB;  %let Y = Y_SetCBB;

do &Y = 0 to &Trials;
  if &Y = 0 then &Wp1 = .5*cumY{0}*cumX{0};
  else &Wp1 = &Wp1 + (cumY{&Y} - cumY{&Y-1})*(0.5*(cumX{&Y} - cumX{&Y-1})
    + cumX{&Y-1});
  end;

/* check if Wp1 makes sense */
&Wp1 = round(&Wp1, .001);
if (&muX le &muY) and (&Wp1 < 0.500) or
   (&muX ge &muY) and (&Wp1 > 0.500) then do;
  put // "WARNING: The specified beta-binomial distributions";
  put    "         are such that power computations for the";
  put    "         Wilcoxon test would be misleading. Try";
  put    "         increasing the effect size.";
  &Wp1 = .;  &Wp2 = .;  &Wp3 = .;
  go to outBB2G;
  end;

/* UnifyPow is set only for Wp1 > 0.50 */
else if (&muX ge &muY) and (&Wp1 < 0.500) then do;
  /*"Reflect" problem by switching X and Y */
  temp = &pX; &pX = &pY; &pY = temp;
  temp = &qX; &qX = &qY; &qY = temp;
  go to redoBB2G;
  end;

do &X = 0 to &Trials;
  if &X = 0 then
    &Wp2 = cumX{0}*(.25*cumY{0}**2 + .5*2*cumY{0}*(1-cumY{0})
      + (1-cumY{0})**2);
  else &Wp2 = &Wp2 + (cumX{&X} - cumX{&X-1})*(.25*(cumY{&X} - cumY{&X-1})**2
      + .5*2*(cumY{&X} - cumY{&X-1})*(1-cumY{&X}) + (1-cumY{&X})**2);
  end;

do &Y = 0 to &Trials;
  if &Y = 0 then
    &Wp3 = .25*cumY{0}*cumX{0}**2;
  else &Wp3 = &Wp3 + (cumY{&Y} - cumY{&Y-1})*(.25*(cumX{&Y} - cumX{&Y-1})**2
      + .5*2*(cumX{&Y} - cumX{&Y-1})*cumX{&Y-1} + cumX{&Y-1}**2);
  end;
outBB2G: %mend StWlBB2G;


%macro StWlBB1G(p_beta, q_beta, Trials, NullVal, Wp1, Wp2, Wp4);

/*
Set p1, p2, p4 parameters for 1-group Wilcoxon problem for beta-binomial
parent. Lots of ties require adjustments:
   p1 = Pr[Y > muYnull] + .5*P[Y = muYnull] > .50
   p2 = Pr[(Yi + Yi')/2 > muYnull] + 0.50*Pr[(Yi + Yi')/2 = muYnull],
   p4 = Pr[{(Yi + Yi')/2 > muYnull)} and {(Yi + Yi'')/2 > muYnull)}]
         + 0.50*Pr[{(Yi + Yi')/2 > muYnull)} and {(Yi + Yi'')/2 = muYnull)}]
         + 0.25*Pr[{(Yi + Yi')/2 = muYnull)} and {(Yi + Yi'')/2 = muYnull)}].
*/

%let pY = pY_BB1G;  %let qY = qY_BB1G;  %let NullValu = NV_BB1G;
%let muY = muY_BB1G;
&pY = &p_beta;  &qY = &q_beta; &NullValu = &NullVal;

if &NullValu = 0 then do;
  put // "ERROR: NULL of 0 is not allowed."; stop; end;
if &NullValu = 1 then do;
  put // "ERROR: NULL of 1 is not allowed."; stop; end;

SetCumY: %SetCumBB(&pY, &qY, &Trials, cumY);
&muY = &pY/(&pY+&qY);

%let muYnull = muYnull_;
&muYnull = &Trials*&NullValu;

if floor(&muYnull) = &muYnull then
  &Wp1 = .5*(cumY{&muYnull} - cumY{&muYnull - 1}) + (1 - cumY{&muYnull});
else &Wp1 = 1 - cumY{floor(&muYnull)};

/* check if Wp1 makes sense */
&Wp1 = round(&Wp1, .001);
if ((&muY ge &NullValu) and (&Wp1 < 0.500)) or
   ((&muY le &NullValu) and (&Wp1 > 0.500)) then do;
  put // "WARNING: The specified beta-binomial distribution";
  put    "         is such that power computations for the";
  put    "         Wilcoxon test would be misleading. Try";
  put    "         increasing the effect size.";
  &Wp1 = .;  &Wp2 = .;  &Wp4 = .;
  goto outBB1G;
  end;

if ((&muY < &NullValu) and (&Wp1 < 0.500)) then do;
  /* UnifyPow is set only for Wp1 > 0.50, so "reflect" the problem. */
  temp = &pY;  &pY = &qY;  &qY = temp;  &NullValu = 1 - &NullValu;
  go to SetCumY;
  end;

%let Y1 = Y1_SetBB;
%let MGvY1 = MGvY1_;
%let Wp2GvY1 = Wp2GvY1_;
do &Y1 = 0 to &Trials;
  &MGvY1 = 2*&muYnull - &Y1;
  if &MGvY1 < 0 then &Wp2GvY1 = 1;
  else if &MGvY1 > &Trials then &Wp2GvY1 = 0;
  else if &MGvY1 = 0 then &Wp2GvY1 = .5*cumY{0} + (1 - cumY{&MGvY1});
  else if floor(&MGvY1) = &MGvY1 then
    &Wp2GvY1 = .5*(cumY{&MGvY1} - cumY{&MGvY1 - 1})
               + (1 - cumY{&MGvY1});
  else &Wp2GvY1 = (1 - cumY{floor(&MGvY1)});
  if &Y1 = 0 then &Wp2 = &Wp2GvY1*(cumY{0});
  else &Wp2 = &Wp2 + &Wp2GvY1*(cumY{&Y1} - cumY{&Y1-1});
  end;

%let Wp4GvY1 = Wp4GvY1_;
do &Y1 = 0 to &Trials;
  &MGvY1 = 2*&muYnull - &Y1;
  if &MGvY1 < 0 then &Wp4GvY1 = 1;
  else if &MGvY1 > &Trials then &Wp4GvY1 = 0;
  else if &MGvY1 = 0 then &Wp4GvY1 = .25*cumY{0}**2 + (1 - cumY{0})**2;
  else if floor(&MGvY1) = &MGvY1 then
    &Wp4GvY1 = .25*(cumY{&MGvY1} - cumY{&MGvY1 - 1})**2 +
        + 2*.5*(cumY{&MGvY1} - cumY{&MGvY1 - 1})*(1 - cumY{&MGvY1})
        + (1 - cumY{&MGvY1})**2;
  else &Wp4GvY1 = (1 - cumY{floor(&MGvY1)})**2;
  if &Y1 = 0 then &Wp4 = &Wp4GvY1*(cumY{0});
  else &Wp4 = &Wp4 + &Wp4GvY1*(cumY{&Y1} - cumY{&Y1-1});
  end;
outBB1G: %mend StWlBB1G;



%macro StWlCat1 (NullVal, Wp1, Wp2, Wp4);
/*
Set p1, p2, p3 parameters for 1-group Wilcoxon problem for general
discrete parent specified by user. Lots of ties require adjustments:
   p1 = Pr[Y > NullVal] + .5*P[Y = NullVal] > .50
   p2 = Pr[(Yi + Yi')/2 > NullVal] + 0.50*Pr[(Yi + Yi')/2 = NullVal],
   p4 = Pr[{(Yi + Yi')/2 > NullVal)} and {(Yi + Yi'')/2 > NullVal)}]
         + 0.50*Pr[{(Yi + Yi')/2 > NullVal)} and {(Yi + Yi'')/2 = NullVal)}]
         + 0.25*Pr[{(Yi + Yi')/2 = NullVal)} and {(Yi + Yi'')/2 = NullVal)}].
*/

%let NullValu = NV_Cat1;
&NullValu = &NullVal;

if not(0 < &NullValu < NumCat) then do;
  put // "ERROR: NULL not between 0 and number of categories."; stop; end;

do iCat = 1 to NumCat;
  if iCat = 1 then cumY{iCat} = ProbY{iCat};
  else cumY{iCat} = cumY{iCat-1} + ProbY{iCat};
  end;
  if abs(cumY{NumCat} - 1) > .001 then do;
    put // "ERROR: DISTRIBUTION probabilities do not sum to 1.000";
    stop;  end;

if floor(&NullValu) = &NullValu then
  &Wp1 = .5*(cumY{&NullValu} - cumY{&NullValu - 1}) + (1 - cumY{&NullValu});
else &Wp1 = 1 - cumY{floor(&NullValu)};

if (&Wp1 < 0.500) then do;
   &Wp1 = 1 - &Wp1;
  /* UnifyPow is set only for Wp1 > 0.50, so "reflect" the problem. */
  do iCat = 1 to NumCat;
    if iCat = NumCat then cumX{iCat} = 1;  /* using cumX as temp */
    else cumX{iCat} = 1 - cumY{NumCat - iCat};
    end;
  do iCat = 1 to NumCat; cumY{iCat} = cumX{iCat};  end;
   &NullValu = (1 + NumCat) - &NullValu;
  end;

%let Y1 = Y1SetCat;
%let MGvY1 = MGvY1_;
%let Wp2GvY1 = Wp2GvY1_;
do &Y1 = 1 to NumCat;
  &MGvY1 = 2*&NullValu - &Y1;
  if &MGvY1 < 1 then &Wp2GvY1 = 1;
  else if (&MGvY1 > NumCat) then &Wp2GvY1 = 0;
  else if &MGvY1 = 1 then &Wp2GvY1 = .5*cumY{1} + (1 - cumY{&MGvY1});
  else if floor(&MGvY1) = &MGvY1 then
    &Wp2GvY1 = .5*(cumY{&MGvY1} - cumY{&MGvY1 - 1})
               + (1 - cumY{&MGvY1});
  else &Wp2GvY1 = (1 - cumY{floor(&MGvY1)});
  if &Y1 = 1 then &Wp2 = &Wp2GvY1*(cumY{1});
  else &Wp2 = &Wp2 + &Wp2GvY1*(cumY{&Y1} - cumY{&Y1-1});
  end;

%let Wp4GvY1 = Wp4GvY1_;
do &Y1 = 1 to NumCat;
  &MGvY1 = 2*&NullValu - &Y1;
  if &MGvY1 < 1 then &Wp4GvY1 = 1;
  else if &MGvY1 > NumCat then &Wp4GvY1 = 0;
  else if &MGvY1 = 1 then &Wp4GvY1 = .25*cumY{1}**2 + (1 - cumY{1})**2;
  else if floor(&MGvY1) = &MGvY1 then
    &Wp4GvY1 = .25*(cumY{&MGvY1} - cumY{&MGvY1 - 1})**2 +
        + 2*.5*(cumY{&MGvY1} - cumY{&MGvY1 - 1})*(1 - cumY{&MGvY1})
        + (1 - cumY{&MGvY1})**2;
  else &Wp4GvY1 = (1 - cumY{floor(&MGvY1)})**2;
  if &Y1 = 1 then &Wp4 = &Wp4GvY1*(cumY{1});
  else &Wp4 = &Wp4 + &Wp4GvY1*(cumY{&Y1} - cumY{&Y1-1});
  end;
%mend StWlCat1;



%macro StWlCat2(Wp1, Wp2, Wp3);
/*
Set p1, p2, p3 parameters for 2-group Wilcoxon problem for general
discrete parents specified by user. Lots of ties require adjustments:
   p1 = Pr[Y > X] + .5*P[Y = X],
   p2 = Pr[{Yi > Xk} and {Yi' > Xk}] + 0.50*Pr[{Yi = Xk} and {Yi' > Xk}]
          + 0.25*Pr[{Yi = Xk} and {Yi' = Xk}],
   p3 = Pr[{Yi > Xk} and {Yi > Xk'}] + 0.50*Pr[{Yi = Xk} and {Yi > Xk'}]
          + 0.25*Pr[{Yi = Xk} and {Yi = Xk'}].
*/

  cumX{1} = ProbX{1};
  cumY{1} = ProbY{1};
  do iCat = 2 to NumCat;
    cumX{iCat} = cumX{iCat-1} + ProbX{iCat};
    cumY{iCat} = cumY{iCat-1} + ProbY{iCat};
    end;

  if abs(cumX{NumCat} - 1) > .001 then do;
    put // "ERROR: 1st DISTRIBUTION probabilities do not sum to 1.000";
    stop;  end;
  if abs(cumY{NumCat} - 1) > .001 then do;
    put // "ERROR: 2nd DISTRIBUTION probabilities do not sum to 1.000";
    stop;  end;

%let X = XSetWCat;  %let Y = YSetWCat;

ReDoCat2:
do &Y = 1 to NumCat;
  if &Y = 1 then &Wp1 = .5*cumY{1}*cumX{1};
  else &Wp1 = &Wp1 + (cumY{&Y} - cumY{&Y-1})*(0.5*(cumX{&Y} - cumX{&Y-1})
    + cumX{&Y-1});
  end;

/* UnifyPow is set only for Wp1 > 0.50 */
if (&Wp1 < 0.500) then do;
  /*"Reflect" problem by switching X and Y */
  do iCat = 1 to NumCat;
    temp = CumX{iCat};
    CumX{iCat} = CumY{iCat};
    CumY{iCat} = temp;
    end;
  go to ReDoCat2;
  end;

do &X = 1 to NumCat;
  if &X = 1 then
    &Wp2 = cumX{1}*(.25*cumY{1}**2 + .5*2*cumY{1}*(1-cumY{1})
      + (1-cumY{1})**2);
  else &Wp2 = &Wp2 + (cumX{&X} - cumX{&X-1})*(.25*(cumY{&X} - cumY{&X-1})**2
      + .5*2*(cumY{&X} - cumY{&X-1})*(1-cumY{&X}) + (1-cumY{&X})**2);
  end;

do &Y = 1 to NumCat;
  if &Y = 1 then
    &Wp3 = .25*cumY{1}*cumX{1}**2;
  else &Wp3 = &Wp3 + (cumY{&Y} - cumY{&Y-1})*(.25*(cumX{&Y} - cumX{&Y-1})**2
      + .5*2*(cumX{&Y} - cumX{&Y-1})*cumX{&Y-1} + cumX{&Y-1}**2);
  end;
%mend StWlCat2;



%global ProbType TablType TablTyp2 TablTyp3 SDType TblMmnts DistList OthScen;
%global TrlName KeepTail fnote WriteBox ResltVar format p3or4;
%let ProbType = To be defined;
%let KeepTail = 3;
%let WriteBox = 0;
%let OthScen = ;
%let DistList = DistLst1 DistLst2;
%let TablTyp2 = none;
%let TablTyp3 = none;
%let fnote = none;
%let SDType = Std Dev;
%let DltaPowr = .02;
%let AddRow2 = ;

/* MAKE MACRO: Remove commenting around next statement. */
 %macro UnifyPow(fname="temp.dat");   /* Activate for macro version. */

data PowData;
%let WriteBox = %eval(&WriteBox+1);

/*
MAKE MACRO: Remove commenting around first infile statement and add
commenting around the second one.
*/
 infile &fname missover eof=EOFfound;  /* Activate for macro version. */
/* infile cards missover eof=EOFfound;   De-activate for macro version. */


if _n_ = 1 then do;
file print;
if &WriteBox = 1 then do;
 put /;
 put
"--------------------------------------------------------------------";
 put
"| UnifyPow 97.10.13.beta   1997 Copyright (c) by Ralph G. O'Brien  |";
 put
"| For information, see http://www.bio.ri.ccf.org/power.html        |";
 put
"--------------------------------------------------------------------";
 end;
 put // "Specifications processed:";
end;

keep alpha power NomPower SD NTotal effctitl dfH testtype tails;
keep parent PrimSSHe ProbStmt BetaWt SDx tolernce SDP Trials;
keep Corr SDMult Wp1 Wp2 Wp3 Wp4 DistLst1 DistLst2 NullValu;
keep TitlDlta TruAlpha LoCrit HiCrit SpcTitl3;
array value {40} value1-value40;  /* temporary parameter values */
array mu {20} mu1-mu20;
array pi {20} pi1-pi20;
array C{20,20} c1-c400;
array w{20} w1-w20;
array wDesign{20} wDes1-wDes20;
array alphaV {10} alpha1-alpha10;
array SDMultV{10} SDMlt1-SDMlt10;
array CorrV{10} CorrV1-CorrV10;
array SDMultVV{100} SDMVV1-SDMVV100;
array CorrVV{100} CrVV1-CrVV100;
array SDV {100} SD1-SD100;
array SDPV {10} SDP1-SDP10;
array SDPVV {100} SDP_1-SDP_100;    do i = 1 to 100; SDPVV{i} = 0; end;
array TrialsV {10} Trials1-Trials10;
array TrialsVV {100} Trial1-Trial100;  do i = 1 to 100; TrialsVV{i} = 0; end;
array p_betaV{200} pbeta1-pbeta200;
array q_betaV{200} qbeta1-qbeta200;
array probX{1:25};
array probY{1:25};
array cumX {0:1000};
array cumY {0:1000};
array BetaWtV{10} BetaWt1-BetaWt10;
array SDxV{10} SDx1-SDx10;
array TolV{10} Tol1-Tol10;
array NtotalV {10} Ntotal1-Ntotal10;
array NomPowrV {10} NomPow1-NomPow10;
array Cmu{20,1} Cmu1-Cmu20;
array CmuT{1,20} CmuT1-CmuT20;
array CT{20,20} CT1-CT400;
array A{20,20} A1-A400;
array Y{20,20} Y1-Y400;
array invCWCT{20,20} inCWC1-inCWC400;
array scmu{1,20} scmu1-scmu20;
array indx{20} indx1-indx20;
array VV{20} VV1-VV20;
array DistList{2} $78 DistLst1 DistLst2 ("  "  "  ");
array Comment{40} $78 ComLin01-ComLin40;
length ProbStmt $78;     /* holds entire problem statement */
length effctitl $78;     /* effect title, up to 78 characters */
length TitlDlta $78;
format TitlDlta $78.;
length SpcTitl3 $78;
format SpcTitl3 $78.;
length onechar $1;
length keyword $15;
length parent $8;
length NomParnt $8;
length CmntLine $78;
length TestType $11;
format Wp1-Wp4 5.3;
format NomPower 4.3;

/***** set defaults on specifications *****/
/* indicator variables: 0=no, 1=yes */
  muProb = 0;        /* test of means? */
  PaMuProb = 0;      /* test comparing paired (2 correlated) means? */
  WlcxProb = 0;      /* Wilcoxon one or 2-group test of location? */
  piProb = 0;        /* test of proportions? */
  muP_Prob = 0;      /* test diffs in mu(P)s, where Y bnml with random pi? */
  DoChiSq = 0;       /* do ChiSq instead of F in GetPower link? */
  McNrProb = 0;      /* McNemar test? */
  rhoProb = 0;       /* test of correlations? */
  RSqProb = 0;       /* test comparing R**2 values? */
  SSH_Prob = 0;      /* Exemplary data method inputting SSH values */
  Chi2Prob = 0;      /* Exemplary data method inputting Chi**2 values */
  betLSPrb = 0;      /* test single BetaWt in OLS reg model */
  wIn = 0;           /* weights input yet? */
  alphaIn = 0;       /* alpha levels input yet? */
  SDvalid = 1;       /* Is the SD statement valid for this problem? */
  SDIn = 0;          /* standard deviations input yet? */
  SDPIn = 0;         /* std deviations for pi [mu(P) problem] input yet? */
  putSDwrn = 0;      /* used SD or SIGMA, not SD(P); put note */
  CorrIn = 1;        /* correlation for PairedMu problem input yet? */
  TrialsIn = 0;      /* N_items or N_trials for mu(P) problem input yet? */
  NtotalIn = 0;      /* total N input yet? */
  PowerIn = 0;       /* power input yet? */
  FindingN = 0;      /* currently interating to find Ntotal?  */
  NoOveral = 0;      /* skip overall test? */
  NmParmIn = 0;      /* Number of parameters statement in? */
  TolIn = 0;         /* Tolerence values in for 1betaOLS problem? */
  SDxIn = 0;         /* SDx values in for 1betaOLS problem? */
  NullIn = 0;        /* null value in yet? */
  NullValu = 0;      /* null value for one- and two-group tests */
  NexIn = 0;         /* N for exemplary SSH or CHI**2 values in? */
  WlcxCstm = 0;      /* User specified p1, p2, and p3 or p4 for Wilcoxon? */
  WlcxOrCt = 0;      /* User specified Ordered Categ pr's for Wilcoxon? */
  ARE = 1;           /* asymp. rel. efficiency for given test vs. t */
  DoNoe = 0;         /* Compute Wilcoxon power using Noether's crude approx */
  DoLH = 1;          /* Do Lehmann-Hettsmanperger for Wilcoxon power */
  DoARE = 0;         /* Compute Wilcoxon power via asymp rel eff vs t */
  muP2Spcl = 0;      /* Special Wilcoxon for 2-grp MU(P) with NullValu ne 0 */
  NumTrls = 1;       /* This allows looping on Trials even when irrelevant*/
  NumGrps = 1;       /* Number of groups */
  NumRSq = 0;        /* Number of R**2 values entered. */
  parent = "Normal"; /* Default that underlying distribution is Normal */
  CntrstIn = 0;      /* Has CONTRASTS statement been read?
  EffectIn = 0;      /* Has EFFECTS statement been read?
  LimitsIn = 0;      /* Has LIMITS statemnt been read? */
  RvrsTail = 0;      /* Reversed tail for binomial calculations */
  TitlDlta = 
 "Some actual powers are &DltaPowr greater than specified minimum powers.";
  NumCmnts = 0;      /* Number of comment statements */
  SpcTitl3 = 
 "Critical values and actual alpha levels using binomial distribution.";
  NoNotes = 0;       /* Suppress printing of notes */


/*
First statements may be:
                          /#
                          UnifyPow notes lines (up tp 40)
                          If any contain ";" then use DATALINES4 statement
                          and follow all input with ";;;;".
                          #/
Then use one of the following Problem Statements:
                          mu
                          1Wilcoxon, 2Wilcoxon (or Mann-Whitney)
                          pi
                          mu(P)
                          McNemar
                          rho
                          R**2
                          1betaOLS
                          Exemplary
*/

 InProbSt: input ProbStmt $ 1-78 @;
 if index(ProbStmt,"/#") > 0 then do;
   link GetComnt;
   go to InProbSt;
   end;
 ProbStmt = trim(ProbStmt);
 input @1 keyword $ @;
 keyword = upcase(keyword);
 put +2 keyword @;

/* parse the MU statement */
  if keyword = 'MU' then do;
   call symput('ProbType','mu');
   call symput('TablType',' FPow');
   muProb = 1;
   link GetValus;
   NumGrps = count;
   if NumGrps le 2 then call symput('TablType','tPow');
   do i = 1 to NumGrps;
      mu{i} = value{i};
   end;
   go to NextSpec;
  end;

/* parse the PairedMu statement */
/*
The PairedMu problem handles J groups of paired means, (mu_j1, mu_j2),
which are input as

   PairedMu  mu_11  mu_12
   >         mu_21  mu_22
   ...
   >         mu_J1  mu_J2

with each ">" in column 1. These are transformed to J means of the difference
score, (Y1 - Y2), giving mu{j} = mu_j1 - mu_j2. Only one pair of SDs are
allowed

   SD  SD_1  SD_2

corresponding to the common "base" SDs for Y1 and Y2. A set of possible
correlations between Y1 and Y2 is input via the CORR statement,

   Corr  # # # #

The base SD of (Y1 - Y2) is

          SDbase = sqrt(SD_1**2 + SD_2**2 - 2*Corr*SD_1*SD_2).

SDbase is then varied using the SDMULTiplier statement,

   SDMULT # # # #

which supplies a set of values, m, to adjust SDbase up or down via

          SD = m*SD_base.

If no SDMULTiplier statement is given, then m = 1.

The TotalN (NTotal) statement is used in the usual way to give the
total number of PAIRS of cases. Accordingly, the statements

   TotalN # # # #

and

   TotalPairs # # # #

are identical. (I.e., "TOTAL" is the operative key string for this statement.)
*/
  if index(keyword,'PAIRED') then do;
   call symput('ProbType','PairedMu');
   call symput('TablType','FPaMuPow');
   PaMuProb = 1;
   NumSDmlt = 1; SDMultV{1} = 1;
   link GetValus;
   if (floor(count/2) ne count/2) then do;
     put // "ERROR: The PairedMu problem requires even number of means.";
     stop; end;
   NumGrps = count/2;
   do i = 1 to NumGrps;
     mu{i} = value{2*i} - value{2*i-1};
     end;
   if NumGrps = 1 then do;  /* check for more groups */
   MoreGrps:
     input anochar $1. @;
     input @1 @;
     if anochar = '>' then do;
       NumGrps = NumGrps + 1;
       put @4 '> ' @6 @;
       input @2 @;
       link GetValus;
       if count ne 2 then do;
         put // "ERROR: Means are not in pairs.";  stop;  end;
       ProbStmt = trim(ProbStmt)||';';
       do i = 1 to 2;
         ProbStmt = trim(ProbStmt)||' '||compress(value{i});
         end;
       mu{NumGrps} = value{2} - value{1};
       go to MoreGrps;
       end;
     end;
   if NumGrps le 2 then call symput('TablType','tPaMuPow');
   go to NextSpec;
   end;

/*
Parse the 1WILCOXON AND 2WILCOXON/MANN-WHITNEY statements.

This requires input of parameters defined by Noether (1987, Sample size
determination for some common nonparametric tests, JASA 82:645-647) and
Lehmann (1975, Nonparametrics: Statistical Methods Based on Ranks, New York:
Wiley, pp. 65-81, 164-171, 196, 400-401). These comments follow
Hettsmansperger (1984, Statistical Inference Based On Ranks, New York: Wiley,
pp. 47-62, 157-159), who presents methods identical to Lehmann's, but in a
notation more suitable to SAS coding.

For the one-group case, we take Y1, Y2, ...Yn to be i.i.d. continuous
with a symmetric parent distribution centered at delta. The null
hypothesis is
     Ho: delta = delta0,
which equates to
     Ho: Prob[Y > delta0] = .5,
Thus, the first parameter is the effect size,
     p1 = Prob[Y > delta0].  We take p1 > .5. It is the only parameter
used by Noether. The Lehmann-Hettsmansperger method uses two more
parameters,
     p2 = Prob[(Y1 + Y2)/2 > delta0],
and
     p4 = Prob[{(Y1 + Y2)/2 > delta0} and {(Y1 + Y3)/2 > delta0}].
Note: Hettsmansperger's p3 = (p2 + p1**2)/2 is superfluous and so it is not
an input parameter in UnifyPow. If the variance of Y is finite, p2 >= p1 and
p4 < p2.

Note that in the SAS coding, p1 is Wp1 (Wilcoxon p1), etc.

For the two-group case, we take X1, X2, ..., Xm and Y1, Y2, ...,Yn to
be independent random variables from parent distributions having
identical spreads and shapes, but not necessarily symmetric. Let delta
be the difference in the two medians, thus the null hypothesis is
     Ho: delta = 0.0,
which equates to
     Ho: Prob[Y > X] = 0.5.
Thus, the first parameter is the effect size
     p1 = Prob[Y > X].  We take p1 > .5. It is the only parameter used
by Noether. The Lehmann-Hettsmansperger method uses two more
parameters,
     p2 = Prob[{Yi > Xk} and {Yi' > Xk}],
and
     p3 = Prob[{Yi > Xk} and {Yi > Xk'}].  Both p2 and p3 are less than
p1. If the parent distribution is symmetric, p2 = p3.

NOTE: The Lehmann-Hettsmansperger method employed here is NOT the same
as the "Lehmann" method investigated by Lesaffre E, Scheys I, Frohlich
J, Bluhmki E (1993, Calculation of power and sample size with bounded
ourcome scores, Statistics in Medicine, 12:1063-1078). That method is
a crude one based on a Normal parent and Lehmann gave it almost as an
aside without recommending it over his more complex one.  The LH
method used here is a nonparametic one, like Noether's but more
refined in that it uses all three "moments," not just p1. Thus it
conforms to the Lesaffre, et. al. recommendations given in their final
paragraph.

For either the one or two-group case, UnifyPow requires only p1,
the effect size. If one of the other parameters is not specified, it
will automatically determine all of them based on theory for Normal,
Logistic, and Laplace (double exponential) distributions, which are
all symmetric, and have kurtoses of 0.0, 1.2, and 3.0, respectively.
*/

 if (index(keyword,'WILC') gt 0) or index(keyword,'MANN') then do;
   call symput ('TablType', '1or2WlcxnPow');
   call symput ('SDType', 'none');
   call symput ('TblMmnts','yes');
   call symput ("DistList", " ");
   call symput ("OthScen", " ");
   SDIn = 1; NumSD = 1; SDV{1} = 1;
   WlcxCstm = 1; NomParnt = "Custom";
   link GetValus;
   if count > 2 then do;
     SDvalid = 0;
     call symput ('TblMmnts','no');
     end;
   if count = 0 then go to OrderCat;
   if count = 1 then do;
     p1OnlyIn = 1;
     NomParnt = "NORMAL";  /* Default is Normal parent */
     WlcxCstm = 0;
     end;
   else p1OnlyIn = 0;
   do i = 1 to count;
     if not(0 < value{i} < 1) then go to WlcxErr;
     end;
   if (value{1} < .50) and (count = 1) then do;
     temp = 1 - value{1};
     put // "WARNING: UnifyPow requires p1 > 0.50.";
     put    "         It has been reset to p1 = 1 - " value{1} "= " temp;
     value{1} = temp;
     end;
   else if (value{1} < .50) and (count > 1) then do;
     put // "ERROR: UnifyPow requires p1 > 0.50.";
     stop;
     end;

 if index(keyword,'1WILC') then do;
   call symput('ProbType','1Wilcoxon');
   WlcxProb = 1; NumGrps = 1;
   if not(0 < count < 4) then go to WlcxErr;
   Wp1 = value1; Wp2 = value2; Wp3 = (Wp2 + Wp1**2)/2; Wp4 = value3;
   if (Wp2 = .) or (Wp4 = .) then do;
     Wp2 = .;  Wp3 = .;  Wp4 = .;
     p1OnlyIn = 1;
     WlcxCstm = 0;
     NomParnt = "NORMAL";
     end;
   if (Wp2 ne .) and (Wp2 < Wp1) then do;
     put // "ERROR: p2 must be greater than p1."; stop; end;
   if (Wp2 ne .) and (Wp4 ne .) and (Wp2 le Wp4) then do;
     put // "ERROR: p4 must be less than p2."; stop; end;
   Wp1Custm = Wp1;  Wp2Custm = Wp2;  Wp3Custm = Wp3;  Wp4Custm = Wp4;
   go to NextSpec;
   end; /* 1WIL do */

 if index(keyword,'2WILC') or index(keyword,'MANN') then do;
   call symput('ProbType','2Wilcoxon');
   WlcxProb = 2; NumGrps = 2;
   if not(0 < count < 4) then go to WlcxErr;
   Wp1 = value1; Wp2 = value2; Wp3 = value3;
   if (Wp2 = .) or (Wp3 = .) then do;
     Wp2 = .;  Wp3 = .;
     p1OnlyIn = 1;
     WlcxCstm = 0;
     NomParnt = "NORMAL";
     end;
     if (Wp2 ne .) and (Wp2 ge Wp1) then do;
       put // "ERROR: p2 must be less than p1."; stop; end;
     if (Wp3 ne .) and (Wp3 ge Wp1) then do;
       put // "ERROR: p3 must be less than p1."; stop; end;
     Wp1Custm = Wp1;  Wp2Custm = Wp2;  Wp3Custm = Wp3;  Wp4Custm = Wp4;
     go to NextSpec;
     end; /* 2WIL or MANN do */

OrderCat:
/*

Ordered categorical problems via Wilcoxon testing, as per Lesaffre,
et. al (1993); see not above.  For 2-group problem, user specifies
      2Wilcoxon
      # # # # # # # #
      # # # # # # # #
where each row defines a probability dist'n, in this case with NumCat = 8
categories.

For the 1-group problem, the category values are assumed to be interval in
scale and are taken to be Y = {1, 2, ..., NumCat}.  The user, however, may
specify a different LOWer and UPPer value, but the sequence is assumed to be
an arithmetic progression. The user must specify a NULL statement, where
0 < NullValu < NumCat, if LOW and UPP are not specified or
LOWer < NullValu < UPP, if they are specified.
*/
  if index(keyword,'1WILC') then do;
    NumGrps = 1;
    call symput("ProbType","1WilcOrdCat");
    end;
  else if index(keyword,"2WILC") then do;
    call symput("ProbType","2WilcOrdCat");
    NumGrps = 2;
    end;
  else go to WlcxErr;

  WlcxProb = NumGrps;
  WlcxOrCt = 1;
  SDvalid = 0;
  if NumGrps = 1 then do;
    call symput ("DistList", "DistLst1 NullValu");
    call symput ("OthScen", "* DistLst1='' * NullValu='NULL:'");
    end;
  if NumGrps = 2 then do;
    call symput ("DistList", "DistLst1 DistLst2");
    call symput ("OthScen", "* DistLst1=''*DistLst2=''");
    end;

  do iGrp = 1 to NumGrps;

    input DistList{iGrp} $ 1-78 @1 @;
    distlist{iGrp} = "{" || trim(distlist{iGrp}) || "}";
    WlcxOrCt = 2; link GetValus;  WlcxOrCt = 1;
    if iGrp = 1 then NumCat = count;
    else if (NumCat ne count) then do;
      put // "ERROR: Number of categories is not identical.";
      stop; end;

    do iCat = 1 to NumCat;
      if not(0 le value{iCat} le 1) then do;
        put "ERROR: Improper probability value.";  stop;  end;
      if NumGrps = 1 then probY{iCat} = value{iCat};
      else if (NumGrps = 2) and (iGrp = 1) then probX{iCat} = value{iCat};
      else probY{iCat} = value{iCat};
      end;
    end;
    go to NextSpec;
 WlcxErr: put // "ERROR: Wilcoxon specification is bad.";  stop;
 end; /* either WIL or MAN do */

/* parse the PI statement */
  if keyword = 'PI' then do;
   call symput('ProbType','pi');
   call symput('TablType','GnrlPow');
   piProb = 1;  parent = "Bernoulli";
   link GetValus;
   NumGrps = count;
   if NumGrps = 1 then do;
     call symput ('TablTyp3','Pi1Specl');
     end;

   do i = 1 to NumGrps;
      pi{i} = value{i};
      if (pi{i} lt 0) or (pi{i} gt 1) then do;
        put // "ERROR: Improper pi: " pi{i}; stop; end;
      if (pi{i} = 0) then do;
        pi{i} = .001;
        put // "WARNING: pi = 0 is not permissable.";
        put    "         It has been automatically reset to pi = 0.001";
        end;
      if (pi{i} = 1) then do;
        pi{i} = .999;
        put // "WARNING: pi = 0 is not permissable.";
        put    "         It has been automatically reset to pi = 0.999";
        end;
      mu{i} = log(pi{i}/(1 - pi{i}));          /* create logits */
   end;
   SDIn = 1;  /* SD is fixed by the pis and weights */
   NumSD = 1;
   SDV{1} = 1;
   go to NextSpec;
  end;

/* Parse the MU(P) statement.

Handles designs in which subjects' data, Y, are binomial based
on TRIALS trials per subject (fixed) and success probability, P,
fixed to each subject but random across subjects. P is taken
to be a standard beta(p_,q_) random variable with mean mu(P) and
standard deviation SD(P). Thus, the user first specifies a set
of J mu(P) values and also sets one or more values for
SD(P) and Trials.

Input is checked to assure that
     mu(P) = muP, 0 < MuP < 1
     SD(P) = SDP, 0 < SDP < sqrt(1/12),
with sqrt(1/12) limit imposed because SD(P) > sqrt(1/12) gives beta
distributions for P that are not unimodal, a condition that makes
little sense in building scenarios for power analyses.

Power is determined as if the analysis will use ordinary Normal-theory
methods on P_hat = Y/Trials.  E(P_hat) = mu(P), but SD(P_hat) must be
computed. For each mu(P) and pairing of SD(P) and Trials
specifications, SD(P_hat) is determined using macro SDBetBnl, which
contains the relevant references to distribution theory. So that all
beta densities are unimodal, UnifyPow will abort runs if it finds that
[mu(P), SD(P)] give (p_ - 1)*(q_ - 1) le 0. The common SD(P_hat) comes
from the weighted average of Var(p_hat) across the J groups.

WILCOXON can also be specified for the MU(P) problem. In this case, the
Lehmann-Hettsmansperger parameters (p1 = Wp1, p2, p3, p4) are determined
directly from exact beta-binomial distributions and the NULL statement.

*/

  if keyword = 'MU(P)' then do;
   call symput('ProbType','muP');
   call symput('TablType','FmuP_Pow');
   muP_Prob = 1;
   link GetValus;
   NumGrps = count;
   if NumGrps = 1 then NullValu = .;  /* Must specify NullValu for this */
   if NumGrps le 2 then call symput('TablType','tmuP_Pow');
   do i = 1 to NumGrps;
      mu{i} = value{i};
      if not(0 < mu{i} < 1) then do;
        put // "ERROR: Improper mu(P): " mu{i}; stop; end;
      if (mu{i} = 0) then do;
        mu{i} = .001;
        put // "WARNING: mu(P) = 0 is not permissable.";
        put    "         It has been automatically reset to mu(P) = 0.001";
        end;
      if (mu{i} = 1) then do;
        mu{i} = .999;
        put // "WARNING: mu(P) = 0 is not permissable.";
        put    "         It has been automatically reset to mu(P) = 0.999";
        end;
      end;
   go to NextSpec;
  end;

/* parse the MCNEMAR statement */
  if keyword = 'MCNEMAR' then do;
   call symput('ProbType','McNemar');
   call symput('TablType','GnrlPow');
   McNrProb = 1;  parent = "CorrBern";
   link GetValus;
   if count ne 2 then do;
      put // "ERROR: not exactly 2 pi values for McNemar's test."; stop; end;
    pi12 = value{1};  pi21 = value{2};
    if not(0 < pi12 < 1) or not(0 < pi21 < 1) then do;
      put // "ERROR: A pi value is not between 0 and 1."; stop; end;
    if pi12 + pi21 > 1 then do;
      put // "ERROR: The sum of the pi values exceeds 1.0."; stop; end;
    SDIn = 1;  /* SD is fixed by the the pis and weights */
    go to NextSpec;
    end;

/* parse the RHO statement */
  if keyword = 'RHO' then do;
   call symput('ProbType','rho');
   call symput('TablType','GnrlPow');
   rhoProb = 1;
   link GetValus;
   NumGrps = count;
   if NumGrps > 1 then effctitl = "Comparing correlations (r-to-Z)";
   do i = 1 to NumGrps;
      if i = 1 then rho1 = value{i};
      if (value{i}**2 ge 1) then do;
        put "Improper rho: " value{i}; stop; end;
      else mu{i} = .5*log((1 + value{i})/(1 - value{i}));
   end;

/* For r-to-Z, the effective SD is fixed at SD = 1.0 */
   SDIn = 1;
   NumSD = 1;
   SDV{1} = 1;
   go to NextSpec;
   end;


/* parse the EXEMPLARY statement */
if index(keyword, 'EXEMPLARY') then do;
   call symput('ProbType','exemplary');
  input keyword $;
  keyword = upcase(keyword);
  put keyword;
  if index(keyword, 'SSH') then do;
      SSH_Prob = 1;
      call symput('TablType','FPow');
      end;
  else if index(keyword, 'CHI') then do;
      Chi2Prob = 1;
      call symput('TablType','GnrlPow');
      SDIn = 1; NumSD = 1;  SDV{1} = 1;
      end;
  else do;
    put // "ERROR: EXEMPLARY type must be SSH or CHI**2.";  stop;  end;
  go to NextSpec;
  end;


/* parse the R**2 statement */
  if keyword = 'R**2' then do;
   call symput('ProbType','R2');
   call symput('TablType','GnrlPow');
   RSqProb = 1;
   link GetValus;
   NumRSq = count;
   if (NumRSq ne 1) and (NumRSq ne 2) then do;
     put // "ERROR: You must give 1 or 2 R**2 values."; stop; end;
   if NumRSq = 1 then do;
     R2redc = 0;  R2full = value{1};
     end;
   if NumRSq = 2 then do;
     R2full = max(value{1}, value{2});
     R2redc = min(value{1}, value{2});
     end;
   if not(0 < R2full < 1) or not(0 le R2redc < 1) then do;
     put // "ERROR: R**2 values must be non-negative and less than 1."; 
     stop; 
     end;

/* For R**2, the effective SD is fixed at SD = 1.0 */
   SDIn = 1;
   NumSD = 1;
   SDV{1} = 1;
  go to NextSpec;
  end;

/* parse the 1betaOLS statement */
  if index(keyword,'1BETA') then do;
   call symput('TablType','1betaOLS');
   betLSPrb = 1;
   link GetValus;
   NumBetas = count;
   do i = 1 to NumBetas;
      BetaWtV{i} = value{i};
   end;
   go to NextSpec;
   end;

/* Shouldn't get to here */
put // "ERROR: " keyword $ " is not a legitimate problem keyword.";
stop;

NextSpec:
  input keyword $ @;
  if index(keyword,"/#") > 0 then do;
    link GetComnt;
    go to NextSpec;
    end;
  keyword = upcase(keyword);
  if not(index(keyword, 'CONTR') or index(keyword, 'EFFECT')) then
    put +2 keyword @;

NxtSpec2:;

/* parse the WILCOXON option for mu and mu(P) problems */
/*
For the one or two group MU problem, this initiates a Normal approximation
to set p1 = Wp1 (defined above). For MU(P), this initiates exact theory
to set p1, p2, p3, p4 unless NumGrps = 2 and NullValu NE 0.  In that
case, only the Normal approximation is used.
*/
  if index(keyword,'WILCOX') > 0 or
     index(keyword,'MANN') > 0 then do;
     if WlcxProb = 0 then do; input; put; end;
  if ((muProb=0) and (muP_Prob=0) and (PaMuProb=0)) or (NumGrps > 2)
     then do;
     put // "ERROR: Rank test options not allowed here."; stop; end;
   WlcxProb = NumGrps;
   if MuProb then call symput('TablType','WlcxnPow');
   if muP_Prob then call symput('TablType','WlcxMuPpow');
   if PaMuProb then call symput('TablType','WlcxPaMuPow');
   go to NextSpec;
  end;

/* parse the METHOD option for Wilcoxon problem */
/* Default is to only do Lehmann-Hettesmansberger */
  if index(keyword,'METHOD') then do;
  input keyword $; keyword = upcase(keyword);
  put keyword;
  if index(keyword,'NOETHER') then DoNoe = 1;
  else if index(keyword,'ARE') then DoARE = 1;
  else if index(keyword,'ALL') then do; DoNoe = 1; DoARE = 1; end;
  else do;
  put // "ERROR: METHOD statement has wrong specification."; stop; end;

  if WlcxCstm and DoARE then
    put // "NOTE: ARE method inappropriate for this problem.";
  if WlcxProb = 0 then do;
    WlcxProb = NumGrps;
    keyword = 'WILCOXON';
    go to NxtSpec2;
    end;
  go to NextSpec;
  end;

/* parse the PARENT option for 1Wilcoxon and 2Wilcoxon/Mann-Whitney problem */
/* Default is to assume NomParnt = "NORMAL." */
  if index(keyword,'PARENT') then do;
    input keyword $; keyword = upcase(keyword);
    put keyword;
  if index(keyword,'NORMAL') then NomParnt = "NORMAL";
  else if index(keyword,'LOGISTIC') then NomParnt = "LOGISTIC";
  else if index(keyword,'LAPLACE') then NomParnt = "LAPLACE";
  else do;
    put // "ERROR: Parent must be Normal (default), Logistic, or Laplace.";
    stop;
    end;
  if WlcxProb = 0 then do;
    WlcxProb = NumGrps;
    keyword = 'WILCOXON';
    go to NxtSpec2;
    end;
  go to NextSpec;
  end;


/* parse the LIMITS statement */
  if keyword = 'LIMITS' then do;
   LimitsIn = 1;
   link GetValus;
   if count ne 2 then do;
    put // "ERROR: Not exactly 2 LIMITS."; stop; end;
   UppLimit = max(value{1}, value{2});
   LowLimit = min(value{1}, value{2});
   go to NextSpec;
   end;


/* parse the Nexemplary statement for SSH_Prob and Chi2Prob */
  if index(keyword,'NEXE') then do;
   NexIn = 1;
   if SSH_Prob = 0 and Chi2Prob = 0 then do;
     put // "ERROR: EXEMPLARY statement is missing."; stop; end;
   link GetValus;

   if count ne 1 then do;
     put "There is not exactly 1 Nexemplary values."; stop; end;

   Nxmplr = value{1};
   go to NextSpec;
   end; /* Nexemplary parsing */


/* parse the NumParm statement for R**2, 1betaOLS, and SSH problems */
  if index(keyword,'NUMPARM') then do;
   NmParmIn = 1;
   if RSqProb = 0 and betLSPrb = 0 and SSH_Prob = 0 then do;
    put // "ERROR: R**2 or 1betaOLS statement is missing."; stop; end;
   link GetValus;

   if RSqProb then do;
     if NumRSq ne count then do;
       put // "ERROR: Wrong number of NumParm values."; stop; end;
     if NumRSq = 1 then do;
       rXfull = value{1};
       rXredc = 1;
       end;
     if NumRSq = 2 then do;
       rXfull = max(value{1}, value{2});
       rXredc = min(value{1}, value{2});
       end;
     if (rXfull = rXredc) or (rXredc < 1) or (rXfull ne round(rXfull))
        or (rXredc ne round(rXredc)) then do;
        put // "ERROR: Improper NumParm value."; stop; end;
     go to NextSpec;
     end;

   if betLSPrb or SSH_Prob then do;
     if count ne 1 then do;
       put "Specify exactly 1 NumParm value."; stop; end;
     if (value{1} < 1) or (value{1} ne round(value{1})) then do;
       put // "ERROR: Improper NumParm value"; stop; end;
     rXfull = value{1};
     go to NextSpec;
     end;

 end; /* NumParms parsing */


/* parse the SDX statement (SD of X variable in 1betaOLS Problem) */
  if keyword = 'SDX' or keyword = 'SD_X' then do;
   if betLSPrb ne 1 then do;
     put // "SD_X statement must follow 1betaOLS statement."; stop; end;
   link GetValus;
   SDxIn = 1;
   NumSDx = count;
   do i = 1 to NumSDx;
     SDxV{i} = value{i};
     if not(SDxV{i} > 0) then do;
       put // "ERROR: SDX value is not positive."; stop; end;
     end;
   go to NextSpec;
  end;

/* parse the Tolerance statement (1 - R^2 of Xj predicted from other Xs) */
  if index(keyword,'TOLER') then do;
   if betLSPrb ne 1 then do;
     put // "Tolerance statement must follow 1betaOLS statement.";
     stop; end;
   link GetValus;
   TolIn = 1;
   NumTol = count;
   do i = 1 to NumTol;
     TolV{i} = value{i};
   end;
   go to NextSpec;
  end;

/* parse the SDMULT statement (SD Multiplier for PairedMu Problem) */
  if index(keyword, 'MULT') then do;
   if PaMuProb ne 1 then do;
     put // "SDMULTiplier statement must follow PairedMu statement.";
     stop; end;
   link GetValus;
   NumSDmlt = count;
   do i = 1 to NumSDmlt;
     SDMultV{i} = value{i};
     if SDMultV{i} le 0 then do;
       put // "ERROR: Negative SDMULTiplier."; stop; end;
     end;
   go to NextSpec;
  end;

/* parse the WEIGHT statement */
/*
Note: Weights are relative. They are summed and converted to fractions.  
Thus use
   WEIGHT .333 .666
or, equivalently,
   WEIGHT 1 2
This is not the same as
   WEIGHT .333 .667
which may cause problems, especially when finding NTotal given a Power
specification.
*/
  if index(keyword,'WEIGHT') then do;
   link GetValus;
   wIn = 1; NumWght = count;
  if NumGrps ne NumWght then do;
    put // "ERROR: Improper number of WEIGHTS specified.";  stop;  end;

  SumWts = 0;
  do i = 1 to NumWght;
    w{i} = value{i};
    SumWts = SumWts + w{i};
    end;
  do i = 1 to NumWght;
    w{i} = w{i}/SumWts;
    wDesign{i} = w{i};
    end;
  go to NextSpec;
  end;

/* parse the TRIALS (or ITEMS) statement for the mu(P) problem */
  if index(keyword,'TRIALS') or index(keyword,'ITEMS') then do;
   if index(keyword,'TRIALS') then call symput('TrlName','Trials');
   if index(keyword,'ITEMS') then call symput('TrlName','Items');
   if muP_Prob = 0 then do;
     put // "ERROR: " keyword $ "statement must be part of mu(P) problem.";
     stop; end;
   link GetValus;
   TrialsIn = 1; NumTrls = count;
   do i = 1 to NumTrls;
     TrialsV{i} = value{i};
     if not(TrialsV{i} > 0) or (floor(TrialsV{i}) ne TrialsV{i}) then do;
        put // "ERROR: Improper " keyword $ "specification: " TrialsV{i};
        stop; end;
     end;
   go to NextSpec;
   end;

/* parse the ALPHA statement */
  if keyword = 'ALPHA' then do;
   link GetValus;
   alphaIn = 1; NumAlpha = count;
   MaxAlpha = 0;
   do i = 1 to NumAlpha;
     alphaV{i} = value{i};
     if not(0 < alphaV{i} < 1) then do;
       put "Improper alpha: " alphaV{i}; stop; end;
     if PowerIn and not(0 < alphaV{i} < MinPower) then do;
       put // "ERROR: Alpha must not exceed power."; stop; end;
     if (alphaV{i} gt .20) then
       put // "WARNING: " alphaV{i} "is a large alpha level.";
     MaxAlpha = max(of MaxAlpha alphaV{i});
   end;
   go to NextSpec;
  end;

/* parse the TAILS statement */
   if index(keyword,'TAIL') or index(keyword,'SIDE') then do;
   link GetValus;
   do i = 1 to count;
     if not((value{i} = 1) or (value{i} = 2)) then go to ErrTAILS;
     end;
   if (count = 2) then do;
     if (value{1} + value{2} = 3) then go to NextSpec;
     else go to ErrTAILS;
     end;

   if (value{1} = 2) then call symput('KeepTail', '2');
   if (value{1} = 1) then call symput('KeepTail', '1');

   go to NextSpec;
   ErrTAILS: put // "ERROR: Invalid TAILS statement."; stop;
   end; /* TAILS do */


/* parse the standard deviations statements: SD or SIGMA */
  if (keyword = 'SD') or (keyword = 'SIGMA') then do;
    if SDvalid = 0 then do;
      put // "WARNING: SD (SIGMA) statement is invalid for this problem.";
      put    "       It is being ignored.";
      go to NextSpec;
      end;
    if p1OnlyIn then call symput ('SDType','Relative Std Dev');
    if (muP_Prob = 1) then do;
      keyword = 'SD(P)';  putSDwrn = 1; go to getSDP;
      end;
    link GetValus;
    SDIn = 1;
    NumSD = count;
    if WlcxCstm = 1 then do;
      NumSD = 1;
      SDV{1} = 1;
      put // "WARNING: " keyword "is not appropriate for this problem.";
      put    "         Statement is ignored.";
      end;

    if (PaMuProb = 1) and (NumSD ne 2) then do;
      put // "ERROR: The PairedMu problem requires exactly";
      put    "       2 standard deviations.";
      stop;
      end;
    do i = 1 to NumSD;
      SDV{i} = value{i};
      if not(SDV{i} > 0) then do;
        put // "ERROR: A standard deviation is not positive."; stop; end;
      end;
    if PaMuProb then do;
      ProbStmt = trim(ProbStmt)||'  &  '||compress(keyword);
      do i = 1 to NumSD;
        ProbStmt = trim(ProbStmt)||' '||compress(SDV{i});
        end;
      end;
    go to NextSpec;
    end;


/* parse the SD(P) statement */
  if keyword = 'SD(P)' then do;
getSDP:   link GetValus;
   if putSDwrn = 1 then put // 
     "NOTE: SD (or SIGMA) values assumed to be for pi" / 
     "      as part of mu(P) problem." //;
   SDPIn = 1;
   NumSDP = count;
   do i = 1 to NumSDP;
     SDPV{i} = value{i};
     if (SDPV{i} > sqrt(1/12)) then do;
       put // "ERROR: SD(P) = " SDPV{i} "exceeds sqrt(1/12), resulting in ";
       put    "       beta distribution for P that is not unimodal.";
       stop; end;
   end;
   go to NextSpec;
  end;


/* parse the CORR statement (for PairedMU problem) */
  if index(keyword,'CORR') gt 0 then do;
    link GetValus;
    CorrIn = 1;
    NumCorr = count;
    do i = 1 to NumCorr;
      CorrV{i} = value{i};
      if not(-1 < CorrV{i} < 1) then do;
        put // "ERROR: Correlation is not between -1 and 1.";
        stop;
        end;
      end;
    go to NextSpec;
    end;


/* parse the TotalN or NTotal (total sample size) statement */
  if (index(keyword,'TOTAL') gt 0) then do;
    call symput('ResltVar','power');
    link GetValus;
    NtotalIn = 1;
    NumNtotl = count;
    do i = 1 to NumNtotl;
      NtotalV{i} = value{i};
      if not(NtotalV{i} > 0) or not(floor(NtotalV{i}) = NtotalV{i}) then do;
        put // "ERROR: Total sample size is not correct.";
        stop;
        end;
      end;
    NumPower = 1;  NomPowrV{1} = 0;
    go to NextSpec;
    end;


/* parse the Power statement */
  if keyword = 'POWER' then do;
    call symput('ResltVar','NTotal');
    link GetValus;
    PowerIn = 1;
    NumPower = count;
    MinPower = 1;
    do i = 1 to NumPower;
      NomPowrV{i} = value{i};
      MinPower = min(of MinPower NomPowrV{i});
      if AlphaIn and not(MaxAlpha < NomPowrV{i} < 1) then do;
        put // "ERROR: Power must be between alpha and 1.";
        stop;
        end;
      end;
    NumNtotl = 1; NTotalV{1} = 1;
    go to NextSpec;
    end;


/* parse the NULL statement */
  if keyword = 'NULL' then do;
    NullIn = 1;
    input NullValu; put +2 NullValu;
    if (RSqProb = 1) or ((rhoProb = 1) and (NumGrps > 1)) or (NumGrps > 2)
      then do;
       put // "ERROR: NULL is not defined/implemented for this problem.";
       stop; end;
    if ((rhoProb = 1) and (NullValu**2 ge 1)) then do;
      put // "ERROR: Improper NULL for rho problem."; stop; end;
    go to NextSpec;
    end;

/* parse the NoOverall statement */
  if index(keyword,'NOOVERALL') then do;
    NoOveral = 1;
    put; input;
    go to NextSpec;
    end;

/* parse the NoTables statement */
  if index(keyword,'NOTABLE') then do;
    call symput('TablType','none');
    put;  input;
    go to NextSpec;
    end;

/* parse the NoNotes statement */
  if index(keyword,'NONOTE') then do;
    NoNotes = 1;
    put;  input;
    go to NextSpec;
    end;

  if index(keyword,'END') then do;
    put; input; go to CheckIt; end;
  if index(keyword,'CONTRAST') then do;
    put; input; CntrstIn = 1; go to CheckIt; end;
  if index(keyword,'EFFECT') then do;
    put; input; EffectIn = 1; go to CheckIt; end;
  else do;
    put // "ERROR: " keyword $ "statement not understood.";  stop;  end;

EOFfound: EndOFile = 1;
  if CntrstIn then go to ContPowr;
  if OvAlDone or EffectIn then go to AllDone;

CheckIt:
 OvAlDone = 1;
 /* Check to see if parameters are in */

   if NtotalIn and PowerIn = 1 then do;
     put // "ERROR: Cannot use both Ntotal and Power statements.";
     stop;  end;
   if not(NtotalIn or PowerIn) then do;
     put // "ERROR: Missing Ntotal or Power statement for this problem.";
     stop;  end;

   if WlcxOrCt and (NullIn = 0) and NumGrps = 1 then do;
     put // "ERROR: One-group ordered categorial problem requires NULL.";
     stop;  end;

   if (betLSPrb or RSqProb or SSH_Prob) and (NmParmIn = 0)
     then do;
     put // "ERROR: Missing NumParms statement for this problem.";
     stop;  end;

   if (SSH_Prob or Chi2Prob) and (NexIn = 0) then do;
     put // "ERROR: Missing Nexemplary statement for this problem.";
     stop; end;

   if betLSPrb and SDxIn=0 then do;
     put // "ERROR: Missing SD_X statement for this problem.";
     stop;  end;

   if betLSPrb and TolIn=0  then do;
     put // "ERROR: Missing TOLERANCE statement for this problem.";
     stop;  end;

   if (muP_Prob ne 1) and (SDIn ne 1) then do;
     put // "ERROR: Missing SD (or SIGMA) statement for this problem.";
     stop;  end;

   if muP_Prob and (SDPIn ne 1) then do;
     put // "ERROR: Missing SD(P) statement for this problem.";
     stop;  end;

   if muP_Prob and (TrialsIn ne 1) then do;
     put // "ERROR: Missing Trials or Items statement for this problem.";
     stop;  end;

   if (muP_Prob = 1) and (NumGrps = 1) and (NullIn = 0) then do;
     put // "ERROR: Must have NULL statement for this problem.";
     stop;  end;

   if AlphaIn = 0 then do;
     AlphaIn = 1; NumAlpha = 1; alphaV{1} = .05;
     put / "NOTE: No ALPHA statement. Default is alpha = 0.05 only."; end;
   
   if NumRSq = 1 then do;
     put / "NOTE: Assuming that R**2 = 0 for reduced model.";
     put  / 
  "NOTE: Assuming test of models with " rXfull "vs. " rXredc "parameters.";
     end;

   if wIn = 0 then do;  /* force balanced design */
     wIn = 1;
     if NumGrps > 1 then
     put / "NOTE: No WEIGHTS statement. Default is balanced design.";
     do i = 1 to NumGrps;
       w{i} = 1/NumGrps;
       wDesign{i} = w{i};
       end;
     end;


   if NtotalIn then do;
     do i = 1 to NumGrps;
       do j = 1 to NumNtotl;
         if abs(round(w{i}*NtotalV{j}) - w{i}*NtotalV{j}) gt
                   .003*round(w{i}*NtotalV{j}) then do;
           put // "WARNING: Total sample sizes and cell weights produce";
           put    "         cell sizes that are not close to integers.";
           put    "         Group: " i "     NTotal: " NtotalV{j};
           end;
         end;
       end;
     end;

if SSH_Prob or Chi2Prob then go to cntrasts;


/* For McNemar problem */
   if McNrProb = 1 then do;
     if NullIn = 0 then do;
       put / "Testing Ho: pi12 - pi21 = 0 using McNemar's test.";
       piHo = .50;
       end;
     else do;
       put / "Testing Ho: pi12/pi21 = " NullValu "using McNemar's test.";
       piHo = 1/(1 + 1/NullValu);
       end;
     link getMcNpw;
     go to AllDone;
     end;

/* For R**2 problem */
   if RSqProb = 1 then do;
     put / "Testing Ho: " @;
       do j = rXredc + 1 to rXfull; put "Beta_" j "= " @; end; put "0.";
     effctitl = "Comparing nested R**2 values";
     dfH = rXfull - rXredc;
     PrimSSHe = (R2full - R2redc)/(1 - R2full);
     link GetPower;
     go to AllDone;
     end;

/* For 1betaOLS problem */
   if betLSPrb = 1 then do;
     put /
    "Testing Ho: Beta_j = " NullValu "in OLS model with " rXfull "parameters.";
     effctitl = "Testing one reg coefficient";
     dfH = 1;
     do i_BetaWt = 1 to NumBetas;   BetaWt = BetaWtV{i_BetaWt};
     do i_SDx = 1 to NumSDx;  SDx = SDxV{i_SDx};
     do i_tol = 1 to NumTol;  tolernce = TolV{i_tol};
       PrimSSHe = ((BetaWt - NullValu)**2)*(tolernce*SDx**2);
       link GetPower;
       end; end; end;
     go to AllDone;
   end;


/* Working with pi problem */
 if piProb ne 0 then do;

if NumGrps = 1 then do;  /* special handling for one pi */
  if NullIn = 0 then piHo = .5;
  else piHo = NullValu;
  put / "Testing Ho: pi = " piHo;
  if NoNotes then go to BinPw; 
  put /
  / 'NOTE: SETTING 2-TAILED CRITICAL REGIONS FOR THE BINOMIAL DISTRIBUTION.'
  / '      Denote the critical regions for a 2-tailed test as "major" and'
  / '      "minor" depending on which one is consistent with the true pi.'
  / '      Thus for Ho: pi = .35 with a conjecture of true pi = .20, the'
  / '      major critical region would be in the lower tail of the'
  / '      binomial(Ntotal, .35) distribution.  Let alpha_major and'
  / '      alpha_minor be the Type I error rates in these tails.  UnifyPow'
  / '      first finds the largest minor critical region such that'
  / '                    alpha_minor LE alpha/2,'
  / '      where "LE" stands for "less than or equal." Then it finds the'
  / '      largest major region such that'
  / '                    alpha_major LE alpha - alpha_minor.'
  / '      This ensures that'
  / '                    alpha_minor + alpha_major LE alpha,' 
  / '      and yet favors the major tail, thus increasing power.'
  / ' '
  / '      The critical values tabled below are in the rejection region.'
  / '      For example, the alpha = .05, two-tailed test of Ho: pi = .35 with'
  / '      NTotal = 40 gives lower and upper critical values of 8 and 21'
  / '      if the conjectured true pi is less than .35.  Thus, the major'
  / '      critical region is r = 0, 1, ..., 8 and the minor one is'

  / '      r = 21, 22, ..., 40.  If the conjectured true pi exceeds .35,'
  / '      then the major region is r = 20, 21, ..., 40 and the minor one is'
  / '      r = 0, 1, ..., 7.';

BinPw: piProb = -1;
  link getbinpw;
  go to AllDone;
  end;

if NumGrps > 2 then go to DoLogit;

/* Power for unconditional and conditional tests of two proportions */
   call symput('fnote','2pi');
/* Pooled t, like ordinary chi-square test of 2 x 2 table */
  put / "Testing Ho: pi1 - pi2 = " NullValu;
  NumSD = 1;
  dfH = 1;
  PrimSSHe = w1*w2*(pi1 - pi2 - NullValu)**2;
  SD1 = sqrt(w1*pi1*(1-pi1) + w2*pi2*(1-pi2));
  if not(NoOveral) then link GetPower;
/* Unpooled t, like exact unconditional test of 2 x 2 table */
  piProb = 2;
  SD1 = sqrt(w2*pi1*(1-pi1) + w1*pi2*(1-pi2));
  if not(NoOveral) then link GetPower;

  if NullValu ne 0 then go to AllDone;

/* Fisher's Exact Test */
  piProb = 2.5;
  link GetPower;

 DoLogit: piProb = 3;
/* Likelihood-ratio test for logit equality */
  SD1 = 1;
  dfH = NumGrps - 1;
  piNull = 0;
  do i = 1 to NumGrps; piNull = piNull + w{i}*pi{i}; end;
  PrimSSHe = 0;
  do i = 1 to NumGrps;
    PrimSSHe = PrimSSHe + 2*w{i}*(pi{i}*log(pi{i}/piNull) +
                          (1-pi{i})*log((1-pi{i})/(1-piNull)));
    end;
  if not(NoOveral) then link GetPower;
  put;
  go to cntrasts;
 end; /* piProb ne 0 do */


/* shouldn't get to here unless it is a mu, PairedMu, Wilcoxon,
   or mu(P) problem */
/* compute power for overall tests of means */

/* Compute SDs for difference scores for PairedMu problem */
   if PaMuProb then do;
     ij = 0;
     V1pV2 = SDV{1}**2 + SDV{2}**2;
     SD1tSD2 = SDV{1}*SDV{2};
     do i = 1 to NumSDmlt;
     do j = 1 to NumCorr;
       ij = ij + 1;
       SDMultVV{ij} = SDMultV{i};
       CorrVV{ij} = CorrV{j};
       SDV{ij} = SDMultVV{ij}*sqrt(V1pV2 - 2*CorrVV{ij}*SD1tSD2);
       end; end;
     NumSD = NumSDmlt*NumCorr;
     end;

/* revise SDs for mu(P) problem */
if muP_Prob = 1 then do;
  iSDY = 0;
  do iSDP = 1 to NumSDP; do iTrials = 1 to NumTrls;
    iSDY = iSDY + 1;
    varY = 0;
    do iGrp = 1 to NumGrps;
      %SDBetBnl(mu{iGrp}, SDPV{iSDP}, TrialsV{iTrials},
        p_beta, q_beta, SDYGrp);
      VarY = VarY + w{iGrp}*SDYGrp**2;
      if NumGrps le 2;
      p_betaV{100*(iGRP-1)+iSDY} = p_beta;
      q_betaV{100*(iGRP-1)+iSDY} = q_beta;
      end;
    SDV{iSDY} = sqrt(VarY);
    SDPVV{iSDY} = SDPV{iSDP};
    TrialsVV{iSDY} = TrialsV{iTrials};
  end; end;

  NumSD = NumSDP*NumTrls;
end;

 if NumGrps = 1 then do;
   if muProb then do;
     put / "Testing location of single group:";
     put   "   <Parametric>    Ho: mu = " NullValu;
     end;
   if PaMuProb then do;
     put / "Testing difference of single pair of correlated measures:";
     put   "   <Parametric>    Ho: mu(Y1 - Y2) = " NullValu;
     end;
   if muP_Prob then do;
     put   "Testing location of single beta-binomial distribution:";
     put / "   <Parametric>    Ho: mu(P) = " NullValu;
     end;
   if rhoProb then do;
     if NullValu = 0 then do;
     /* special t test of Ho: rho = 0 */
       put   "Testing single correlation coefficient:";
       put / "   <Parametric>    Ho: rho = 0";
       effctitl = "t test of rho = 0";
       PrimSSHe = (rho1**2)/(1-rho1**2);
       dfH = 1;
       link GetPower;
       go to AllDone;
       end;
     else do;
       put   "Testing single correlation coefficient using Fisher's r-to-Z:";
       put / "   <Parametric>    Ho: Z(rho) =  Z(" NullValu ")";
       effctitl = "Fisher's    r-to-Z test of one rho";
       NullValu = .5*log((1 + NullValu)/(1 - NullValu));
       link rZpower;
       go to Alldone;
       end;
     end;

   dfH = 1;
   PrimSSHe = (mu1 - NullValu)**2;

   if WlcxProb = 1 then do;
     if not(muProb or muP_prob) then put 
      / "Testing location of a single group:";
     put   "  <Nonparametric>  Ho: p1 = .50";
     if not(PaMuProb) then do;
       put   "      where p1 = Pr[Y{i} > " Nullvalu "]" @;
       put   " + .50*Pr[Y{i} = " Nullvalu "].";
       end;
     if PaMuProb then do;
       put   "      where p1 = Pr[D{i} > " Nullvalu "]" @;
       put   " + .50*Pr[D{i} = " Nullvalu "].";
       put   "            D{i} = Y1{i} - Y2{i}.";
       end;
     link WlcxPow;
     if p1OnlyIn then PrimSSHe = psiZ**2;
     if not(WlcxCstm) then link AREtPowr;
     end;

/* I'm wondering if this is needed */
/*
   if WlcxProb = 0 then do;
     dfH = 1;
     PrimSSHe = (mu1 - NullValu)**2;
     end;
*/
   parent = 'Normal  ';
   effctitl = 't test of one mean';
   if PaMuProb then effctitl = "Matched-pairs t test";
   if not(WlcxCstm) then link GetPower;
   go to AllDone;
   end; /* NumGrps = 1 do */


 if NumGrps = 2 and NoOveral = 0 then do;
   if muProb or p1OnlyIn or WlcxCstm or WlcxOrCt then
     put / "Testing location difference between 2 groups:";
   if muProb or p1OnlyIn then
     put   "   <Parametric>    Ho: mu1 - mu2 = " NullValu;
   if PaMuProb then do;
     put / "Comparing differences of two pairs of correlated measures:";
     put   "   <Parametric>    Ho: mu1(Y1 - Y2) - mu2(Y1 - Y2) = " NullValu;
     end;
   if muP_Prob then do;
     put / "Testing location difference between two beta-binomial means:";
     put / "   <Parametric>    Ho: mu1(P) - mu2(P) = " NullValu;
     end;
   if rhoProb then do;
     put / "Testing correlations using Fisher's r-to-Z:";
     put / "   <Parametric>    Ho: Z(rho1) - Z(rho2) = 0";
     effctitl = "Comparing two correlations (r-to-Z)";
     link rZpower;
     go to cntrasts;
     end;

   if WlcxProb = 2 then do;
     put   "  <NonParametric>  Ho: p1 = .50";
    if muProb or muP_prob or p1OnlyIn or WlcxCstm or WlcxOrCt then do;
     put   "      where p1 = Pr[Y{i,1} - Y{i',2} > " Nullvalu "]" @;
     put   " + .50*Pr[Y{i,1} - Y{i',2} = " Nullvalu "].";
     put   "            Y{i,g} = Y for case i in group g.";
     end;
    if PaMuProb then do;
     put   "      where p1 = Pr[D{i,1} - D{i',2} > " Nullvalu "]" @;
     put   " + .50*Pr[D{i,1} - D{i',2} = " Nullvalu "].";
     put   "            D{i,g} = Y1{i,g} - Y2{i,g}.";
     put   "            Y1{i,g} = Y1 for case i in group g.";
     end;

     link WlcxPow;
     dfH = 1;
     if muProb or muP_Prob or PaMuProb then
       PrimSSHe = w1*w2*(mu1 - mu2 - NullValu)**2;
     if p1OnlyIn then PrimSSHe = w1*w2*psiZ**2;
     if not(WlcxCstm) then link AREtPowr;
    end;


   if WlcxProb = 0 then do;
     dfH = 1;
     PrimSSHe = w1*w2*(mu1 - mu2 - NullValu)**2;
     end;

    parent = 'Normal  ';
    effctitl = 'Ordinary t test';
    if PaMuProb then effctitl = "Comparing means of difference scores";
    if not(WlcxCstm or NoOveral) then link GetPower;
    go to cntrasts;
 end; /* NumGrps = 2 do */


 if NumGrps > 2 then do;
   if muProb then put / "ANOVA testing on " NumGrps "means.";
   if PaMuProb then do;
     put / "ANOVA testing on differences of pairs of correlated measures: ";
     put   "     " NumGrps "groups x 2 'repeated' measures design.";
     end;
   if MuP_Prob then put / "Comparing " NumGrps "mu(P) means.";
   if rhoProb then put / "Comparing " NumGrps "r-to-Z correlations.";

   if NoOveral then go to cntrasts;
   effctitl = 'Overall test';
   dfH = NumGrps - 1;
   
   if rhoProb then do;
     do i = 1 to dfH; do j = 1 to NumGrps;
       if i = j then c{i,j} = 1;
       else if i = (j-1) then c{i,j} = -1;
       else c{i,j} = 0;
       end; end;
       link GetPower;
       go to cntrasts;
     end;  

/* standard overall anova */
   mubar = 0;
   do i = 1 to NumGrps;
     mubar = mubar + w{i}*mu{i};
   end;
   PrimSSHe = 0;
   do i = 1 to NumGrps;
     PrimSSHe = PrimSSHe + w{i}*(mu{i} - mubar)**2;
   end;
   link GetPower;
 end; /* NumGrps > 2 do */


cntrasts:;

/* This section also handles SSH and CHI**2 effects statements */

if CntrstIn or EffectIn then put // keyword $;
else go to AllDone;

/* Perform C*mu = 0 general contrasts (without proc iml) */
if CntrstIn or EffectIn then do;
/* reset SD and weights if logit analysis */
  if piProb > 0 then do;
    piProb = 4;
    SDV{1} = 1;
    do i = 1 to NumGrps; w{i} = wDesign{i}*pi{i}*(1 - pi{i}); end;
    end;

NewCont:  /* begin contrast or effects specification */
/* read title between double quotes (") */
  quote = 0;
  do col = 1 to 80;
  input onechar $1. @;
  if onechar = '"' then do;
    if quote = 0 then do;
      col1 = col; quote = 1; go to nextcol; end;
    else do;
      col2 = col; quote = 0; go to readtitl; end;
    end;
  nextcol: end;
  put 'Missing double quote (") in effect title'; stop;
  readtitl: titlelng = col2 - col1 - 1;
  input @(col1+1) effctitl $varying40. titlelng @;
  input @(col2+1) @;

  if EffectIn then put @4 effctitl @;
  if CntrstIn then put @4 effctitl / @6 @;
  
  GetRow1: link GetValus;
  if count = 0 then go to GetRow1; /* look for values on next line */

  if EffectIn then do;
    if (count ne 2) and (count ne 3) then do;
      put // 
       "ERROR: Effects statement requires 2 or 3 values.";  
      stop;  
      end;
    DFH = value{1};
    if count = 2 then value{3} = 0;
    PrimSSHe = abs(value{2} - value{3})/Nxmplr;
    if Chi2Prob then DoChiSq = 1;
    if SSH_Prob then NumGrps = rXfull;
    link GetPower;
    go to NewCont;
    end;

/* Parse the C matrix */

   cIn = 1; Nc = count;
   if (Nc ne NumGrps) then do;
     put // "ERROR: Wrong number of contrast coefficients in " effctitl;
     stop; end;

   sumcij = 0;
   do j = 1 to Nc;
     sumcij = sumcij + value{j};
     c{1,j} = value{j};
     end;
   if abs(sumcij) > .002 then do;
     put // "WARNING: Contrast coefficients do not sum to 0.0.";
     put "  Row: 1   Effect title: " effctitl;
     end;

   dfH = 1;
   nn = 1;

   Readmore:
    input anochar $1. @;
    input @1 @;
    if anochar = '>' then do;
      nn = nn + 1;
      put @4 '> ' @6 @;
      input @2 @;
      link GetValus;

      sumcij = 0;
      do j = 1 to Nc;
        sumcij = sumcij + value{j};
        c{nn,j} = value{j};
        end;

   if abs(sumcij) > .002 then do;
     put "WARNING: Contrast coefficients do not sum to 0.0.";
     put "  Row: " nn  "    Effect title: " effctitl;
     end;


      dfH = nn; go to Readmore;
    end;

  ContPowr: if rhoProb = 0 then link PrimSSHC;

  link GetPower;
  if EndOFile then go to AllDone;
  go to NewCont;
  end; /*  end of C*mu = 0 general contrasts */

AllDone:
  if NumCmnts > 0 then do;
    put _page_;
    do i = 1 to NumCmnts;
      put Comment{i} $78.;
      end;
    end;
  return;


/*
PrimSSHC: Computes PrimSSHe for C*mu contrasts, where muhat_j has variance
sigma**2/(Ntotal*w{j}).
*/
PrimSSHC:

if rhoProb then do;
/* reset weights */
    do i = 1 to NumGrps; w{i} = wDesign{i} - 3/Ntotal; end;
    end;

   If dfH > 1 then do;
/*  calculate C*mu and (C*mu)'  */
    do i = 1 to dfH;
       Cij = 0;
       do j = 1 to NumGrps;
          Cij = Cij + C{i,j} * mu{j};
       end;
       Cmu{i,1} = Cij;
       CmuT{1,i} = Cij;
    end;
/*  calculate C'  */
    do i = 1 to NumGrps;
       do j =1 to  dfH;
          CT{i,j} = C{j,i};
       end;
    end;
/*  calculate C*inv(W)*C'  */
    do i = 1 to dfH;
       do j = 1 to dfH;
          A{i,j} = 0;
          do k = 1 to NumGrps;
             A{i,j} = A{i,j} + C{i,k} * C{j,k} / w{k};
          end;
       end;
    end;
/*  calculate inv(C*inv(W)*C')  */
    n = dfH;
    link inverse;
    do i = 1 to dfH;
       do j = 1 to dfH;
          invCWCT{i,j} = Y{i,j};
       end;
    end;
/*  calculate (C*mu)'*inv(C*inv(W)*C')*(C*mu)  */
    do j = 1 to dfH;
       scmuij = 0;
       do k = 1 to dfH;
          scmuij = scmuij + CmuT{1,k} * invCWCT{k,j};
       end;
       scmu{1,j} = scmuij;
    end;
    PrimSSHe = 0;
    do j = 1 to dfH;
       PrimSSHe = PrimSSHe + scmu{1,j} * Cmu{j,1};
    end;
   end;

  else do;
    sumcmu = 0;
    bottom = 0;
    do i = 1 to NumGrps;
     sumcmu = sumcmu + c{1,i}*mu{i};
     bottom = bottom + c{1,i}**2/w{i};
    end;
    PrimSSHe = sumcmu**2/bottom;
  end;
return;

/* Matrix inverse for C*mu = 0 general contrasts */
inverse: /*  Y = inv(A) */
nrow = dim1(A);
ncol = dim2(A);
/*  array indx{&max};  */
/*  ARRAY  VV{&max};  */

TINY = 1.0E-20;

do I = 1 to n;
     do j = 1 to n;
          Y{i,j} = 0;
     end;
     Y{i,i} = 1;
end;
link ludcmp;
do m = 1 to n;
link lubksb;
end;
return;

ludcmp: /*  LU DECOMPOSITION */
IF NROW NE NCOL
     THEN PUT "THE MATRIX IS NOT SQUARE";
DO I = 1 TO n;
     AAMAX = 0;
     DO J = 1 TO n;
          IF ABS(A{I,J}) > AAMAX THEN AAMAX = ABS(A{I,J});
     END;
     IF AAMAX = 0 THEN do;
          put "ERROR: A matrix singularity has suspended computations.";
          stop;
          end;
     VV{I} = 1.0 / AAMAX;
END;

D = 1;

     do j = 1 to n;
          do i = 1 to j-1;
               sum = A{i,j};
               do k = 1 to i-1;
                    sum = sum - A{i,k} * A{k,j};
               end;
               A{i,j} = sum;
          end;
          aamax = 0;
          do i = j to n;
                sum = A{i,j};
               do k = 1 to j-1;
                    sum = sum - A{i,k} * A{k,j};
               end;
               A{i,j} = sum;
               dum = vv{i} * abs(sum);

               if dum >= aamax then
               do;
                    imax = i;
                    aamax = dum;
               end;
          end;
          if j ne imax then
          do;
               do k = 1 to n;
                     dum = A{imax, k};
                    A{imax,k} = A{j,k};
                    A{j,k} = dum;
               end;
               d = -d;
               vv{imax} = vv{j};
          end;
          indx{j} = imax;
          if A{j,j} = 0 then A{j,j} = tiny;
               if j ne n then do;
               dum = 1.0 / A{j,j};
               do i = j+1 to n;
                    A{i,j} = A{i,j} * dum;
               end;
          end;
     end;
return;

lubksb: /* LU BACKSUBSTITUTION */
     II = 0;
     DO I = 1 TO n;
          LL = indx{I};
          SUM = Y{LL,m};
          Y{LL,m} = Y{i,m};
          IF II NE 0 THEN
               DO j = II TO (I-1);
                    SUM = SUM - A{I,j}*Y{j,m};
               END;
          else IF SUM NE 0 THEN II = I;
           Y{i,m} = SUM;

     END;
     DO I = n TO 1 BY -1;
               SUM = Y{i,m};
               DO j = I+1 TO n;
                    SUM = SUM - A{I,j} * Y{j,m};
               END;
               Y{i,m} = SUM / A{I,I};

     END;
return;
/* end of matrix inverse  */


GetPower:;  /* link for power for F, t, chi-square, and piProb */

/* If DoChiSq = 1, then do chi-square & Z instead of F & t */

do ialpha = 1 to NumAlpha;
   alpha = alphaV{ialpha};

do iSD = 1 to NumSD;
   SD = SDV{iSD};
   if PaMuProb then do;  /* get SDMult and Corr that led to SD */
     SDMult = SDMultVV{iSD};
     Corr = CorrVV{iSD};
     end;
   if muP_Prob then do;  /* get SD(P) and Trials that led to SD */
     SDP = SDPVV{iSD};
     Trials = TrialsVV{iSD};
     end;
   else do;  SDP = .;  Trials = .;  end;
   if rhoProb = 0 then
     PrimLmda = PrimSSHe/(SD**2);  /* Primary noncentrality */

do i_Ntotal = 1 to NumNtotl;
   Ntotal = NtotalV{i_Ntotal};
do i_Power = 1 to NumPower;
   NomPower = NomPowrV{i_Power};

   if piProb = 1 then effctitl = 'Approximate Uncondit''l "chi^2*"';
   if piProb = 2 then effctitl = "Exact Uncondit'l**";
   if piProb = 2.5 then do;
     effctitl = "Fisher's exact conditional";
     go to XactCond;
     end;
   if piProb = 3 then do;
    DoChiSq = 1;
    if NumGrps = 2 then effctitl = 'Likhd Ratio for Log Odds Ratio ';
    if NumGrps > 2 then effctitl = 'Likhd Ratio for Equal Logits';
    end;
   if piProb = 4 then DoChiSq = 1;
   if rhoProb then DoChiSq = 1;

if DoChiSq then do;
/* compute power or Ntotal for chi-square */
   testtype = 'Chi-square';
   if DFH = 1 then testtype = '2-tail Z';
   tails = 2;
   ccrit = cinv( 1-alpha, DFH, 0.0);
   if PowerIn then do;
      Step = 1;
      %FindN(GetPow01, Cntinu01);
      end;
  GetPow01: if rhoProb then do;
        link PrimSSHC;
        PrimLmda = PrimSSHe;
        end;
     lambda = ARE*Ntotal*PrimLmda;
     link C_TRAP;
     if TRAP = 1 then power = .9999;
     else power = 1 - probchi(ccrit, DFH, lambda);
     if FindingN then do; 
       %FindN(GetPow01, Cntinu01); 
  Cntinu01: if NTotal ne . then do;
         link Adjust_N;
         go to GetPow01;
         end; 
       end;
     end;

else do;
/* compute power for F */
   testtype = 'Regular F ';
   if dfH = 1  then testtype = '2-tail t';
   tails = 2;
   if (piProb = 1)  or (piProb = 2) then testtype = '2-tld t aprx';
   if (WlcxProb ne 0) and (ARE ne 1) then testtype = '2-tail W';
   if PowerIn then do;
      Step = 1;
      %FindN(GetPow02, Cntinu02);
      end;
 GetPow02: lambda = ARE*Ntotal*PrimLmda;
   if NumGrps ne . then dfE = ARE*Ntotal - NumGrps;
   else dfE = Ntotal - rXfull;
   if dfE < 1 then DFE = 1;
   fcrit = finv(1-alpha, dfH, dfE, 0.0);
   link F_TRAP;
   if trap = 1 then power = .9999;
   else power = 1 - probf(fcrit, dfH, dfE, lambda);
   if FindingN then do; 
     %FindN(GetPow02, Cntinu02);
 Cntinu02: if NTotal ne . then do;
       link Adjust_N;
       go to GetPow02;
       end;
     end;
   end;

link OutPower;


/* if dfH = 1, compute power for one-tailed tests */
if dfH = 1 then do;
  tails = 1;

  if DoChiSq = 1 then do;
    testtype = '1-tail Z';
    Zcrit = probit(1-alpha);
    if PowerIn then do;
      if rhoProb ne 1 then do;
        Zpower = probit(1 - NomPower);
        Ntotal = ((Zpower - Zcrit)**2)/(PrimLmda*ARE);
        link Adjust_N;
        go to GetPow1a;
        end;
      Step = 1;
      %FindN(GetPow1a, Cntinu1a);
      end;
  GetPow1a: if rhoProb then do;
      link PrimSSHC;
      PrimLmda = PrimSSHe;
      end;
    lambda = ARE*Ntotal*PrimLmda;
    power = 1 - probnorm(Zcrit - sqrt(lambda));
    if FindingN then do;
      %FindN(GetPow1a, Cntinu1a);
  Cntinu1a: if NTotal ne . then do;
         link Adjust_N;
         go to GetPow1a;
         end;
       end;
     end; /* DoChiSq */

 else do;
   testtype = '1-tail t';
   if (piProb = 1) or (piProb = 2) then testtype = '1-tld t aprx';
   if (WlcxProb ne 0) and (ARE ne 1) then testtype = '1-tail W';
   if PowerIn then do;
      Step = 1;
      %FindN(GetPow03, Cntinu03);
      end;
 GetPow03: lambda = ARE*Ntotal*PrimLmda;
   if NumGrps ne . then dfE = ARE*Ntotal - NumGrps;
   else dfE = Ntotal - rXfull;
   if dfE < 1 then DFE = 1;
   tcrit = tinv(1-alpha, dfE, 0);
   link T_TRAP;
   if TRAP = 1 then power = .9999;
   else power = 1 - probt( tcrit, dfE, sqrt(lambda));
   if FindingN then do; 
     %FindN(GetPow03, Cntinu03); 
 Cntinu03: if NTotal ne . then do;
       link Adjust_N;
       go to GetPow03;
       end;
     end;
   end; 

link OutPower;

end; /* dfH = 1 */


XactCond: if piProb = 2.5 then do;
/*
Power for Fisher's exact conditional test using arcsin transformation
of Walters (The Statistican, 1979, 28:219-232), noted by Gordon and
Watson (Controlled Clinical Trials, 1994, 15:77-79) to be quite
accurate even for values of pi near 0 or 1.
*/

do tails = 2 to 1 by -1;
  if tails = 2 then testtype = '2-tld aprx';
  else testtype ='1-tld aprx';
  link WaltPow;
  end; /* tails */
end; /* piProb = 2.5 */


/* old code for Fisher's Exact Test, now commented out */
/*
if piProb = 2.5 then do;

**
Compute power for conditional (Fisher-Irwin) test using the
solution advanced in stages by
    Casagrande, Pike, and Smith (1978), Biometrics, p483-486.
    Fleiss, Tytun, and Ury (1980), Biometrics, p343-346.
    Diegert and Diegert (1981), Biometrics, p595.
**

  effctitl = 'Fisher-Irwin exact cond. (CPS)';

  r = w2/w1;
  m0 = Ntotal/(r + 1);
  pibar = w1*pi1 + (1-w1)*pi2;
  term1 = sqrt((r+1)*pibar*(1-pibar));
  term2 = sqrt(r*pi1*(1-pi1) + pi2*(1-pi2));
  term3 = r*(pi1 - pi2)**2;

  * nondirectional test';
  tails = 2;
  testtype = '2-tld aprx';
  Zalpha = probit(1 - alpha/2);
  link DDPow;
  output;

  * directional test;
  tails = 1;
  testtype ='1-tld aprx';
  Zalpha = probit(1 - alpha);
  link DDPow;
  output;
 end;
 */  /* end of old code for Fisher's Exact Test */

end; /*  power loop  */
end; /*  Ntotal loop */
end; /* SD loop */
end; /* alpha loop */
return;  /* GetPower */


/* Finds power for Fisher's r-to-Z for 1- and 2-group problems */
rZpower:
  dfH = 1;
  do iAlpha = 1 to NumAlpha;
    alpha = alphaV{iAlpha};
    ccrit = cinv(1-alpha, 1, 0.0 );
    Zcrit = probit(1-alpha);
  do i_NTotal = 1 to NumNtotl;
    NTotal = NtotalV{i_NTotal};
  do i_Power = 1 to NumPower;
    NomPower = NomPowrV{i_Power};
  
  do tails = 2 to 1 by -1;
    if tails = 2 then testtype = '2-tail Z';
    else testtype = '1-tail Z';
    if PowerIn then do;
      Step = 1;
      %FindN(GetPow04, Cntinu04);
      end;
  GetPow04:
    if NumGrps = 1 then lambda = (NTotal - 3)*(mu1 - NullValu)**2;
    if NumGrps = 2 then 
     lambda = ((mu1 - mu2)**2)/(1/(wDes1*NTotal - 3) + 1/(wDes2*NTotal - 3));
     if lambda < 0 then do;
       power = 0;
       go to ifFndN04;
       end;

    if tails = 2 then do;
      link C_TRAP;
      if TRAP then power = .9999;
      else power = 1 - probchi(ccrit, 1, lambda);
      end;
    if tails = 1 then do;
      if TRAP then power = .9999;
      else power = 1 - probnorm(Zcrit - sqrt(lambda));
      end;
 ifFndN04: if FindingN then do; 
      %FindN(GetPow04, Cntinu04);
 Cntinu04: if NTotal ne . then do;
        link Adjust_N;
        go to GetPow04;
        end;
      end;   /* FindingN */
    link OutPower;
    end;  /* tails */
    end;  /* NomPower loop */
    end;  /* NTtotal loop */
    end;  /* Alpha loop */
  return;  /* rZpower */



GetValus:  /* a link subroutine */
 count = 0;
 do i = 1 to dim(value);  value{i} = .;  end;
 newvalue: input v @;
 if count=0 and v=. then do; input; return; end;
 if v = . then do; 
   input; put; 
   return; end;
 else do;
   In = 1;
   count = count + 1;
   value{count} = v;
   if (count = 1) and WlcxOrCt = 2 then put @3 @;
   if EffectIn then put +4 @;
   put v +1 @;
   go to newvalue;
   end;
 return;  /* GetValus */


WlcxPow:;  /* a link subroutine for Wilcoxon power */

/*
Compute Wilcoxon power parameters assuming different parent
distributions, but constant psi = abs(mu1-NullValu)/SD = delta/SD
[1-group case] or psi = abs(mu1-mu2-NullValu)/SD [2-group case].
This means that the probability effect size, p1, will differ
across parents. Because the SDs differ across the standard forms
of the Normal [S = 1], Logistic [S = 3.1416/sqrt(3)], and Laplace
[S = sqrt(2)] distributions, we need to adjust delta using delta*S
in order to apply the formulas given by Lehmann (pp 400-410)--
results that I (RO'B) verified.  Lehmann p 196 gives simple translations
between the 2-group to 1-group case.


*/

/*
Note: Extensive Monte Carlo study (by RO'B, unpublished) showed that the
Lehmann-Hettsmanberger method is superior to both the Noether and ARE t
methods for Logistic and Laplace parents. With the Normal parent, the
ARE t method was slightly superior to the LH method in some cases. The
Noether approximation is generally inferior across all three
parents. Thus only the LH is given by default.  The other two can be
requested.
*/

if WlcxOrCt then do;
  if NumGrps = 1 then do;
    if LimitsIn then do;
      RangeLim = UppLimit - LowLimit;
      aa = 1 + (1-NumCat)*LowLimit/RangeLim;
      bb = (NumCat-1)/RangeLim;
      NullAdj =  aa + bb*NullValu;

      put / "Category values (interval scale):" / @3 @;
      do iCat = 1 to NumCat;
        Ymetric = (iCat - aa)/bb;
        put Ymetric @;
        end;
      put;
      end;

    else NullAdj = NullValu;
    %StWlCat1(NullAdj, Wp1Custm, Wp2Custm, Wp4Custm);
    Wp3Custm = (Wp2Custm + Wp1Custm**2)/2;
    %CheckWp(Wp1Custm, Wp2Custm, Wp4Custm);
    end;
    else if NumGrps = 2 then do;
      %StWlCat2(Wp1Custm, Wp2Custm, Wp3Custm);
      %CheckWp(Wp1Custm, Wp2Custm, Wp3Custm);
      end;
    end;

if (muP_Prob = 1) and (WlcxProb = 2) and (NullValu ne 0) then muP2Spcl = 1;
if (muP_Prob = 1) and (muP2Spcl = 1) then go to SD_LoopW;

if (NoNotes = 0) and (not(WlcxCstm) or WlcxOrCt) then do;
  put / "Nonparametric Moments (if no ties possible)"
  /     "-------------------------------------------";

  if NumGrps = 1 then put
     "Let Y{i} be the outcome score for case i.  (For the PairedMu"
   / "problem, Y{i} is a difference score.)  Then,"
   / "  p1 = Pr[Y{i} > Null]"
   / "  p2 = Pr[Y{i} + Y{i'} > 2*Null]"
   / "  p3 = (p2 + p1**2)/2"
   / "  p4 = Pr[(Y{i} + Y{i'} > 2*Null) and (Y{i} + Y{i''} > 2*Null)]";


  if NumGrps = 2 then put
     "Let Y{i,g} be the outcome score for case i in group g.  (For the"
   / "PairedMu problem, Y{i,g} is a difference score.)  Then,"
   / "  p1 = Pr[Y{i,1} - Y{i',2} > Null]"
   / "  p2 = Pr[(Y{i,1} - Y{k,2} > Null) and (Y{i',1} - Y{k,2} > Null)]"
   / "  p3 = Pr[(Y{i,1} - Y{k,2} > Null) and (Y{i,1} - Y{k',2} > Null)]";

  put // "UnifyPow will reverse ordering relations in order to force p1 > .5"
   / "Ties are handled by partitioning probabilities appropriately, e.g.,"
   / "  p1 = Pr[Y{i,1} - Y{i',2} > Null] + .50*Pr[Y{i,1} - Y{i',2} = Null]";
   end;

if NumGrps = 1 then do;
  call symput('p3or4','p4');
  if NoNotes = 0 then put
    / "The (default) Lehmann-Hettsmansperger method uses p1, p2, and p4"
    / "(as well as p3), whereas 'METHOD NOETHER' uses only p1.";
    end;

if NumGrps = 2 then do;
  call symput('p3or4','p3');
  if NoNotes = 0 then put
    / "The (default) Lehmann-Hettsmansperger method uses p1, p2, and p3,"
    / "whereas 'METHOD NOETHER' uses only p1.";
    end;

if WlcxCstm = 1 then go to SD_LoopW;
if NoNotes = 0 then do;
put
  / "'METHOD ARE' does not use the p-type moments, but rather uses"
  / "the asymptotic relative efficiences of the Wilcoxon versus the t-test.";

if muProb or PaMuProb or p1OnlyIn then put
 / "Parent Distributions"
 / "--------------------"
 / "Powers for the Wilcoxon will be approximated assuming Normal,"
 / "Logistic, and Laplace parent distributions, thus giving a range of"
 / "tail thicknesses (kurtoses):"
 / 
 / "   Parent     Kurtosis"
 / "  --------    --------"
 / "  Normal        0.0"
 / "  Logistic      1.2"
 / "  Laplace       3.0";

end; /* NoNotes do */

/* Set psi, nature's effect size in distance. */
/* Use this method if MU(P) 2-grp problem has NullValue ne 0. */

 if muP2Spcl = 1 then put
  // "WARNING: For the two-group MU(P) problem, WILCOXON with a"
  /  "         non-zero NULL value results in cruder approximations"
  /  "         than if the null is 0.0 (default).";

 if muProb or muP_Prob or PaMuProb then do;
   if WlcxProb = 1 then psiZ = abs(mu1-NullValu);
   if WlcxProb = 2 then psiZ = abs(mu1-mu2-NullValu);
   end;

if p1OnlyIn then do;
/*
The user has requested a Wilcoxon test and supplied only the effect
size, p1, defined above. The other parameters--p2, p3, and p4--will be
determined for Normal, Logistic, and Laplace parents, keeping the distance
effect size, psi (defined above), constant across them. psi is determined from
p1, assuming either a Normal (default), Logistic, or Laplace parent with
SD = 1. Note that the SD statement can still be used to alter psi up or down,
i.e. SD = 0.50 will double psi.
*/
  if NomParnt = "NORMAL" then do;
    if WlcxProb = 1 then delNorml = probit(Wp1);
    if WlcxProb = 2 then link SlvDelta;
    psiZ = DelNorml;
    end;
  if NomParnt = "LOGISTIC" then do;
    if WlcxProb = 1 then delLogst = log(Wp1/(1 - Wp1));
    if WlcxProb = 2 then link SlvDelta;
    psiZ = DelLogst/(3.1416/sqrt(3));
    end;
  if NomParnt = "LAPLACE" then do;
    if WlcxProb = 1 then delLaplc = -log(2*(1 - Wp1));
    if WlcxProb = 2 then link SlvDelta;
    psiZ = DelLaplc/sqrt(2);
    end;

  put / "You specified the " NomParnt $ "distribution and p1 = " Wp1 5.3 ".";
    if WlcxProb = 1 then
      put "This equates to psi = (mu - NullValue)/SD = " psiZ best6. ".";
    if WlcxProb = 2 then
      put "This equates to psi = (mu2 - mu1 - NullValue)/SD = " psiZ best6.
        ".";
    end; /* muProb = 0, muP_Prob = 0 do */


SD_LoopW:
do iSD = 1 to NumSD;

   SD = SDV{iSD};

   if WlcxCstm then go to LoopWlPw;

   SDP = SDPVV{iSD};  Trials = TrialsVV{iSD};
   Corr = CorrVV{iSD};  SDMult = SDMultVV{iSD};

if not(MuP_Prob and muP2Spcl = 0) then do;

  psi = psiZ/SD;

  if WlcxProb = 1 then do;
/* set parameters given Normal parent */
   delNorml = psi;
   deltta = 2*delNorml; link WpNorml;
   Wp4Norml = Wp2Norml;
   Wp2Norml = Wp1Norml;
   Wp1Norml = probnorm(delNorml);
   Wp3Norml = (Wp2Norml + Wp1Norml**2)/2;

/* set parameters given Logistic parent */
   delLogst = psi*(3.1416/sqrt(3));
   deltta = 2*delLogst; link WpLogst;
   Wp4Logst = Wp2Logst;
   Wp2Logst = Wp1Logst;
   Wp1Logst = 1/(1 + exp(-delLogst));
   Wp3Logst = (Wp2Logst + Wp1Logst**2)/2;

/* set parameters given Laplace parent */
   delLaplc = psi*sqrt(2);
   deltta = 2*delLaplc; link WpLaplc;
   Wp4Laplc = Wp2Laplc;
   Wp2Laplc = Wp1Laplc;
   Wp1Laplc = 1 - exp(-delLaplc)/2;
   Wp3Laplc = (Wp2Laplc + Wp1Laplc**2)/2;

end; /* WlcxProb = 1 do */


if WlcxProb = 2 then do;
/* set parameters given Normal parent */
   delNorml = psi;
   deltta = delNorml; link WpNorml;
   Wp3Norml = Wp2Norml;

/* set parameters given Logistic parent */
   delLogst = psi*(3.1416/sqrt(3));
   deltta = delLogst; link WpLogst;
   Wp3Logst = Wp2Logst;

/* set parameters given Laplace parent */
   delLaplc = psi*sqrt(2);
   deltta = delLaplc; link WpLaplc;
   Wp3Laplc = Wp2Laplc;
  end; /* WlcxProb = 2 do */
end; /* (muProb = 1) or (muP2Spcl = 1) do */

/*
For MU(P) WILCOXON problem, determine p1, p2, p3, p4.  For NumGrps = 2,
these can only be determined exactly if NullValu = 0.

Otherwise the Normal, Logistic, and Laplace parents are assumed based on mu1,
mu2, and SD of the.beta-binomia random variable. In essence, then the
beta-binomial parent is dropped in favor of these others, just to give a
rough answer. (An specifice beta-binomial determination is possible
for NullValu NE 0 in the two-group case, but this has not been coded.)
*/

if (muP_Prob = 1) and (muP2Spcl = 0) then do;
  if WlcxProb = 1 then do;
    %StWlBB1G(p_betaV{iSD}, q_betaV{iSD}, Trials, NullValu,
              muP_Wp1, muP_Wp2, muP_Wp4);
    muP_Wp3 = (muP_Wp2 + muP_Wp1**2)/2;
    %CheckWp(muP_Wp1, muP_Wp2, muP_Wp4);
    end;
  if (WlcxProb = 2) then do;
    %StWlBB2G(p_betaV{iSD}, q_betaV{iSD}, p_betaV{100+iSD},
              q_betaV{100+iSD}, Trials, muP_Wp1, muP_Wp2, muP_Wp3);
    %CheckWp(muP_Wp1, muP_Wp2, muP_Wp3);
    end;
  end;


LoopWlPw:

do ialpha = 1 to NumAlpha;
   alpha = alphaV{ialpha};

do i_Ntotal = 1 to NumNtotl;
   Ntotal = NtotalV{i_Ntotal};
do i_Power = 1 to NumPower;
   NomPower = NomPowrV{i_Power};

do tails = 2 to 1 by -1;
   if tails = 2 then testtype = '2-tail W';
   else testtype ='1-tail W';

Zalpha = probit(1 - alpha/tails);

if WlcxCstm = 1 then do;
  parent = 'Custom  ';
  Wp1 = Wp1Custm;  Wp2 = Wp2Custm;  Wp3 = Wp3Custm;  Wp4 = Wp4Custm;
  if DoLH then link LHpower;
  link NoePower;
  go to endtails;
  end;

if (muP_Prob = 1) and (muP2Spcl = 0) then do;
  parent = 'beta bnl';
  Wp1 = muP_Wp1;  Wp2 = muP_Wp2;  Wp3 = muP_Wp3;  Wp4 = muP_Wp4;
  if DoLH then link LHpower;
  link NoePower;
  go to endtails;
  end;

/* do 3 parents */
parent = 'Normal  ';
Wp1 = Wp1Norml;  Wp2 = Wp2Norml;  Wp3 = Wp3Norml;  Wp4 = Wp4Norml;
if DoLH then link LHpower;
link NoePower;

parent = 'Logistic';
Wp1 = Wp1Logst;  Wp2 = Wp2Logst;  Wp3 = Wp3Logst;  Wp4 = Wp4Logst;
if DoLH then link LHpower;
link NoePower;

parent = 'Laplace ';
Wp1 = Wp1Laplc;  Wp2 = Wp2Laplc;  Wp3 = Wp3Laplc;  Wp4 = Wp4Laplc;
if DoLH then link LHpower;
link NoePower;

endtails: end; /*  tails */
end; /* NomPower loop */
end; /* Ntotal loop */
end; /* alpha loop */
end; /* SD loop */
Wp1 = .;  Wp2 = .;  Wp3 = .;  Wp4 = .;
return;  /* WlcxPow */


OutPower: if power > .999 then power = .999;
  else power = round(power,.001);
  output;
  if PowerIn and ((power - NomPower) > &DltaPowr) then do;
    if piProb > 0 then do; call symput ('TablTyp2','FindNPow'); end;
    end;
  return; /* OutPower */


Adjust_N:
  NTotal = ceil(NTotal);
  Check_N: do i_Grps = 1 to NumGrps;
    if abs(round(wDesign{i_Grps}*Ntotal) 
      - wDesign{i_Grps}*Ntotal) > .01 then do;
        Ntotal = Ntotal + 1;
        go to Check_N;
        end;
    end;
  return;  /* Adjust_N  */



F_TRAP:;
/*
*********************************************************************
*                            F_TRAP                                 *
* This link module screens the arguments to the PROBF function in   *
* order to determine whether they will result in a near zero value  *
* to be returned by the PROBF function, due to a SAS bug that's     *
* been around since at least 1985. Assessment is achieved with the  *
* with Severo-Zelen normal approximation to the noncentral F, as    *
* described in Johnson and Kotz, vol. 2, 1970, p. 195.              *
*********************************************************************

*/

crit = fcrit;
TRAP:;
   trap = 0;
   a1 = dfH*crit/(dfH+lambda);
   a2 = 2*(dfH + 2*lambda)/(9*(dfH + lambda)**2);
   a3 = 2/(9*dfE);

   SJz = ((1 - a3)*a1**(1/3) - (1 - a2))/sqrt(a2 + a3*a1**(2/3));

   IF SJz < -3.80 then trap = 1;
   return; *F_TRAP;

T_TRAP:;
/* a check on whether delta is too high due for PROBT due to SAS bugs */
crit = tcrit**2;
dfH = 1;
link TRAP;
return;

C_TRAP:;
/* a check on whether lambda is too high for PROBCHI due to SAS bugs */
dfE = 5000;
crit = ccrit/dfH;
link TRAP;
return;


WaltPow:;
/* link function for Fisher's Exact Test using arcsin */
CCsign = sign(pi1 - pi2);
Zcrit = probit(1 - alpha/tails);
if PowerIn then do;
  Step = 1;
  %FindN(GetPow05, Cntinu05);
  end;
GetPow05: term1 = pi1 - CCsign/(2*w1*Ntotal);
if term1 < 0 then do;
  power = 0;
  go to ifFndN05;
  end;
term1 = arsin(sqrt(term1));
term1 = abs(term1 - arsin(sqrt(pi2 + CCsign/(2*w2*Ntotal))));
term1 = 2*term1*sqrt(Ntotal*w1*w2);
Zpower = Zcrit - term1;
power = 1 - probnorm(Zpower);
ifFndN05: if FindingN then do;
  %FindN(GetPow05, Cntinu05);
  Cntinu05: if NTotal ne . then do;
    link Adjust_N;
    go to GetPow05;
    end;
  end;
link OutPower;
return;


/* old code for Fisher's Exact Test */
/*

DDPow:;
* From Diegert and Diegert (1981), Biometrics, p595;
h = (r+1)/(r*abs(pi1-pi2));  * h is c in DD(1981);
m = h**2/m0 - h + m0;  * m is m_prime of DD(1981);
Zpower = (Zalpha*term1 - sqrt(m*term3))/term2;
power = 1 - probnorm(Zpower);
link OutPower;
return;
      * end of old code for Fisher's Exact;
*/


getbinpw:;  /* power for binomial */
if pi1 > piHo then do;
  RvrsTail = 1;
  piHo = 1 - piHo;
  pi1 = 1 - pi1;
  end;

SD = sqrt(pi1*(1 - pi1));
varfactr = (piHo*(1 - piHo))/(pi1*(1 - pi1));
PrimSSHe = (pi1 - piHo)**2;
PrimLmda = PrimSSHe/(SD**2);  /*  Primary noncentrality */

do ialpha = 1 to NumAlpha;
   alpha = alphaV{ialpha};

do i_Ntotal = 1 to NumNtotl;
   Ntotal = NtotalV{i_Ntotal};
   N = Ntotal;
do i_Power = 1 to NumPower;
   NomPower = NomPowrV{i_Power};

do tails = 2 to 1 by -1;
  alphtail = alpha/tails;
  LoCrPoss = 1;  HiCrPoss = 1;
  if tails = 1 then do;
    if RvrsTail then LoCrPoss = 0;
    else HiCrPoss = 0;
    end;

  if PowerIn then do;
    Step = 1;
    %FindNBnl(GetPow06, Cntinu06);
    end;

 GetPow06: if FindingN then N = N_try;
  lambda = N*PrimLmda;
  link MainBnPw;
  power = ActulPow;
  if FindingN then do;
    %FindNBnl(GetPow06, Cntinu06);
    end;
 Cntinu06: Ntotal = N;
  link OutPower;

end; /* tails */
end; /* i_Power */
end; /* i_Ntotal */
end; /* ialpha */

return; /* getbinpw */


getMcNpw:;  * a link subroutine to compute power for McNemar;
  effctitl = "McNemar test";
  piD = pi12 + pi21;  *Prob discordant;
  pi1 = pi12/piD;
  if pi1 > piHo then do;
    RvrsTail = 1;
    piHo = 1 - piHo;
    pi1 = 1 - pi1;
    end;

  SD = sqrt(pi1*(1 - pi1));
  varfactr = (piHo*(1 - piHo))/(pi1*(1 - pi1));
  PrimSSHe = (pi1 - piHo)**2;
  PrimLmda = PrimSSHe/(SD**2);  * Primary noncentrality;

do ialpha = 1 to NumAlpha;
   alpha = alphaV{ialpha};

do i_Ntotal = 1 to NumNtotl;
   Ntotal = NtotalV{i_Ntotal};
do i_Power = 1 to NumPower;
   NomPower = NomPowrV{i_Power};

do tails = 2 to 1 by -1;
  if PowerIn then do;
    Step = 1;
    %FindNBnl(GetPow09, Cntinu09);
    end;

GetPow09: if FindingN then NTotal = N_try;
  MidNdis = round(Ntotal*piD);
  lambda = NTotal*PrimLmda;
  cumprobN = 0;
  cumpower = 0;
  delta = 0;
new_Ndis: Ndis = MidNdis + delta;
  if (Ndis ge 0) and (Ndis le Ntotal) then link prNbyPow;
  if delta = 0 then go to newdelta;
  Ndis = MidNdis - delta;
  if (Ndis ge 0) and (Ndis le Ntotal) then link prNbyPow;

newdelta:
  if cumprobN < .9999 then do;
    delta = delta + 1;
    go to new_Ndis;
    end;
  power = cumpower;
if FindingN then do;
  %FindNBnl(GetPow09, Cntinu09);
  end;
Cntinu09: effctitl = "McNemar    (virtually   exact)";
  if tails = 1 then TestType = "1-tailed";
  else TestType = "2-tailed"; 
  link OutPower;

end; *tails;
end; /* i_Power */
end; * i_Ntotal;
end; * ialpha;
return; *getMcNpw;


prNbyPow: *link to compute Pr[Ndis]*Pr[power|Ndis] in getMcNpw;
  pii = piD;  N = Ntotal;  i_r = Ndis;
  link prBnml;
  prob_N = prob_r;
  cumprobN = cumprobN + prob_N;
  N = Ndis;
  link MainBnPw;
  cumpower = cumpower + prob_N*ActulPow;
  return; *prNbyPow;


MainBnPw:;  * a link subroutine;
*given active values of N, pi1, alpha, and tails, finds ActulPow;
if (N*min(pi1, 1 - pi1) > 80) then do;
  effctitl = 'Approx. Binomial';
if tails = 2 then do;
* compute ActulPow for chi-square;
   testtype = '2-tail Z';
   ccrit = cinv( 1-alpha, 1, 0.0 );
   ccrit = ccrit*varfactr;
   dfH = 1;
   link C_TRAP;
   if TRAP = 1 then ActulPow = .9999;
   else ActulPow = 1 - probchi( ccrit, 1, lambda );
end; *tails = 2;

if tails = 1 then do;
   testtype = '1-tail Z';
   Zcrit = probit( 1-alpha);
   Zcrit = Zcrit*sqrt(varfactr);
   ActulPow = 1 - probnorm(Zcrit - sqrt(lambda));
end; *tails = 1;

end; *approx binomial;

else do;
  effctitl = 'Exact Binomial';
if tails = 2 then do;
  testtype = '2-tail bnml';
  link crit_r;
  link BnmlPow;
end; *tails = 2 exact;

if tails = 1 then do;
   testtype = '1-tail bnml';
   link crit_r;
   link BnmlPow;
end; *tails = 1 exact;

end; *exact binomial;

return; *MainBnPw;


crit_r:;
/*
link fnctn to find the crit values for r for Binl(N, piHo).

  For tails = 2:
    r_hi: min r such that
        Pr[X gt r_hi | N, piHo] = p_hi < alpha/2
    r_lo: max r such that
        Pr[X le r_lo | N, piHo] = p_lo < alpha - p_hi.

  For tails = 1:
    r_hi = N + 1; impossible, so p_hi = 0.
    r_lo: max r such that
        Pr[X le r_lo | N, piHo] = < alpha.

  Note that this requires problem to be preset so that true pi < piHo.
*/


r_lo = -1;  r_hi = N + 1;
if tails = 1 then do;
  p_hi = 0;
  go to findr_lo;
  end;

CumProb = 0;
do i_r = N to 0 by -1;
  pii = piHo; link PrBnml;  *returns prob_r;
  p_hi = CumProb;
  CumProb = p_hi + prob_r;
  if CumProb > alpha/2 then go to findr_lo;
  r_hi = i_r;
  end; *find r_hi;

findr_lo: CumProb = 0;
do i_r = 0 to N;
  pii = piHo; link PrBnml;  *returns prob_r;
  p_lo = CumProb;
  CumProb = p_lo + prob_r;
  if CumProb > alpha - p_hi then go to SavCrits;
  r_lo = i_r;
  end; *find r_lo;

SavCrits: if not(piProb = -1) then go to ExCrit_r; 
if tails = 2 then TruAlpha = p_hi + p_lo;
else TruAlpha = p_lo;
if RvrsTail then do;
  if tails = 2 then LoCrit = N - r_hi; else LoCrit = .;
  HiCrit = N - r_lo;
  end;
else do;
  LoCrit = r_lo;
  if tails = 2 then HiCrit = r_hi; else HiCrit = .;
  end;

if LoCrit = -1 and LoCrPoss and not(FindingN) then do;
  put // 
    "NOTE: No critical value will set " alphtail 4.3 " in lower tail.";
  LoCrit = .;
  end;
if HiCrit = N+1 and HiCrPoss and not(FindingN) then do;
  put //
    "NOTE: No critical value will set " alphtail 4.3 " in upper tail.";
  HiCrit = .;
  end;
ExCrit_r: return; *crit_r;


BnmlPow:;  *link fcntn for ActulPow of binomial;
ActulPow = 1;
do i_r = r_lo + 1 to r_hi - 1;
  pii = pi1; link prBnml;
  ActulPow = ActulPow - prob_r;
  end;
return; *BnmlPow;


prBnml:;  *link fnctn for individual terms of binomial;
*link fnctn to find exact Pr[X = i_r | N, pii];
*first find N choose i_r;
if (i_r = 0 or i_r = N) then combo = 0;
else do;
  rtemp = min(i_r, N - i_r);
  combo = 0;
  do i = 0 to rtemp - 1;
    combo = combo + log((N - i)/(rtemp - i)); end;
  end;
prob_r = exp(combo + i_r*log(pii) + (N - i_r)*log(1 - pii));
return; *prBnml;


/*
A suite of link functions for computing p1(deltta) and p2(deltta) for
Wilcoxon _two-group_ problem for Normal, Logistic, and Laplace.  For
the one-group problem, the mapping is:
                 one-group                             two-group
                 p1(delta) = P(Y > NullValu)
                 p2(delta)                    <==   p1(deltta=2*delta)
                 p3 = (p2 + p1**2)/2
                 p4(delta)                    <==   p2(deltta=2*delta)

Ref: Lehmann p 69, 196, 400. Hettsmansperger, p 47, 158.
*/



WpNorml:
Wp1Norml = probnorm(abs(deltta)/sqrt(2));


/*

Converts deltta = (mu2 - mu1)/SD GE 0 to p2 = P[(Y1 > X1) and (Y2 >
X1)] GE 0.33 assuming the Normal shift model as per Lehmann
(p. 71). Then X is N(mu, SD**2) and Y1 and Y2 are N(mu + D, SD**2), D
= (mu2 - mu1) GE 0. The more general form has deltta = (mu2 - mu1 -
NullValu)/SD GE 0 and p2 = P[(Y1 > X1 + NullValu) and (Y2 > X1 +
NullValu)], for NullValu GE 0. It will also find p2 for the one-group
case by using deltta = 2*abs(mu1-NullValu)/SD.

Let:
  Za = deltta/sqrt(2)) be the noncentrality for the t with n1 = n2 = 1.
   D = (mu2-mu1) - NullValu > 0

The problem converts to
   p2 = P[(Z1 < Za) and (Z2 < Za)],
where
   Z1 = (X1/SD - Y1/SD - deltta)/sqrt(2)
and
   Z2 = (X1/SD - Y2/SD - deltta)]/sqrt(2)
are standard bivariate Normals with corr = 0.5.;

Lehmann cites now-archaic tables to solve
    p2 = 1 - Pr[(Z1 > Za) or (Z2 > Za)]
        = 1 - {Pr[Z1 > Za] + Pr[Z2 > Za] - Pr[(Z1 > Za) and (Z2 > Za)]};

UnifyPow uses a trick of transforming to Z1 and Z2 to their principal
components, PC1 and PC2, which are independent N(0,1):
    PC1 = (Z1 + Z2)/sqrt(3),
    PC2 = Z1 - Z2;
If Z1 > Za and Z2 > Za then PC1 < 2*Za/sqrt(3) and
2*Za - PC1*sqrt(3) < PC2 < PC1*sqrt(3) - 2*Za.  The area is integrated
numerically.
*/

intvals = 100;
Za = deltta/sqrt(2);
/* first compute PrUpQuad = Pr[(Z1i > Za) and (Z2i > Za)] */
lowlmPC1 = 2*Za/sqrt(3);
lowlmCP1 = probnorm(lowlmPC1);
delPrPC1 = (1 - lowlmCP1)/intvals;
PrUpQuad = 0;
do iPC1 = 1 to intvals;
  PC1 = probit(lowlmCP1 + (iPC1-0.5)*(1 - lowlmCP1)/intvals);
  uplmPC2 = PC1*sqrt(3) - 2*Za;
  PrUpQuad = PrUpQuad +
     delPrPC1*(2*(probnorm(uplmPC2) - .5));
  end;
Wp2Norml = 1 - (2*(1 - probnorm(Za)) - PrUpQuad);
return;

WpLogst:
if deltta = 0 then Wp1logst = .5;
else do;
  Wp1logst = (1 - (1+deltta)*exp(-deltta))/((1 - exp(-deltta))**2);
  end;
/*Wp2Logst:*/
if deltta = 0 then Wp2logst = .3333;
else do;
  Wp2logst = (1 - 2*deltta*exp(-deltta) - exp(-2*deltta))/((1-exp(-
deltta))**3);
  end;
return;

WpLaplc:
Wp1Laplc = 1 - exp(-deltta)*(1 + deltta/2)/2;
Wp2Laplc = 1 - (7/12 + deltta/2)*exp(-deltta) - exp(-2*deltta)/12;
return;

/*
End of suite of link functions for computing p1(deltta) and p2(deltta)
*/

NoePower: ;
/*
Method by Noether (JASA, 1987, p. 645).  Uses only effect size, Wp1, thus
does not adjust Var[T] or Var[W] for shape of parent distribution.
*/
if DoNoe = 0 then return;
if WlcxProb = 1 then effctitl = "Wilcoxon Signed Rank [Noether (p1) aprx]";
if WlcxProb = 2 then effctitl = "Wilcoxon-Mann-Whitney [Noether (p1) aprx]";
if PowerIn then do;
  Step = 1;
  %FindN(GetPow07, Cntinu07);
  end;

GetPow07:
if WlcxProb = 1 then do;
   Qterm = (Ntotal*(Wp1-.5) + Ntotal*(Ntotal-1)*(Wp2-.5)/2)**2;
   Qterm = Qterm/(Ntotal*(Ntotal+1)*(2*Ntotal+1)/24);
   /* Note: Most people in effect use the cruder     */
   /* Qterm = Ntotal*3*(Wp2 - 0.5)**2;               */
   end;
if WlcxProb = 2 then 
     Qterm = (Ntotal**2/(Ntotal+1))*12*w1*(1-w1)*(Wp1 - 0.5)**2;
   /* Note: Most people in effect use the cruder    */
   /* Qterm = Ntotal*12*w1*(1-w1)*(Wp1 - 0.5)**2; */
Zbeta = sqrt(Qterm) - Zalpha;
power = probnorm(Zbeta);
if FindingN then do; 
  %FindN(GetPow07, Cntinu07);
 Cntinu07: if NTotal ne . then do;
     link Adjust_N;
     go to GetPow07;
     end;
   end;
link OutPower;
return;


LHpower: ;
/*
Lehmann-Hettsmansperger power approx for Wilcoxon problems.
Requires p1, p2, p3, p4 (Wp1, Wp2, Wp3, Wp4).
Ref: Hettsmansperger, p 47, 158, and 2.2.9; agrees with Lehmann.
*/
if WlcxProb = 2 then
  effctitl = "Wilcoxon-Mann-Whitney [Lehmann (p1, p2, p3) aprx]";
else effctitl = "Wilcoxon Signed Rank [Lehmann (p1, p2, p3, p4) aprx]";
if PowerIn then do;
  Step = 1;
  %FindN(GetPow08, Cntinu08);
  end;

GetPow08:
if WlcxProb ne 2 then do;
  n = Ntotal;
  ET = n*Wp1 + n*(n-1)*Wp2/2;
  VT = n*Wp1*(1-Wp1) + n*(n-1)*Wp2*(1-Wp2)/2;
  VT = VT + 2*n*(n-1)*(Wp3-Wp1*Wp2) + n*(n-1)*(n-2)*(Wp4-Wp2**2);
  Wcrithi = n*(n+1)/4 + .5 + Zalpha*sqrt(n*(n+1)*(2*n+1)/24);
  power = 1 - probnorm((Wcrithi - ET)/sqrt(VT));

  if tails = 2 then do;
    Wcritlo = n*(n+1)/4 - .5 - Zalpha*sqrt(n*(n+1)*(2*n+1)/24);
    power = power + probnorm((Wcritlo - ET)/sqrt(VT));
    end;
  end;

if WlcxProb = 2 then do;
  m = Ntotal*w1;
  n = Ntotal*w2;
  EW = m*n*Wp1;
  VW = m*n*Wp1*(1-Wp1) + m*n*(n-1)*(Wp2-Wp1**2);
  VW = VW + m*n*(m-1)*(Wp3-Wp1**2);
  Wcrithi = m*n/2 + .5 + Zalpha*sqrt(m*n*(m+n+1)/12);
  power = 1 - probnorm((Wcrithi - EW)/sqrt(VW));

  if tails = 2 then do;
    Wcritlo = m*n/2 - .5 - Zalpha*sqrt(m*n*(m+n+1)/12);
    power = power + probnorm((Wcritlo - EW)/sqrt(VW));
    end;
  end;

if FindingN then do; 
  %FindN(GetPow08, Cntinu08);
 Cntinu08: if NTotal ne . then do;
    link Adjust_N;
    go to GetPow08;
    end;
  end;
link OutPower;
return;



AREtPowr:;
/*
Approximates power of Wilcoxon one- and two-group tests using
the asymptotic relative efficiencies of them vs. corresponding t tests.
Same method as one used in the PASS program.
*/
if DoARE = 0 then return;
if WlcxProb = 1 then
   effctitl = "Wilcoxon Signed Rank [aprx via ARE W vs. t]";
if WlcxProb = 2 then
   effctitl = "Wilcoxon Mann-Whitney [aprx via ARE W vs. t]";
parent = 'Normal  '; ARE = 3/3.1416; link GetPower;
parent = 'Logistic'; ARE = (3.1416**2)/9; link GetPower;
parent = 'Laplace '; ARE = 1.5; link GetPower;
ARE = 1;
return;


SlvDelta:;
/*
Uses Newton-Raphson to find deltas (delta >= 0) for specified
2-group p1 (p1 >= 0.50) based on Lehmann's results for

        Normal: p1 = sqrt(2)*Z[p1], where Z = probit(p1);

                     1 - (1 + delta)*exp(-delta)
      Logistic: p1 = ----------------------------
                         (1 - exp(-delta))**2

       LaPlace: p1 = 1 - exp(-delta)*(1 + delta/2)/2
*/

if NomParnt = "NORMAL" then do;
  DelNorml = sqrt(2)*probit(Wp1);
  return;
  end;

if NomParnt = "LOGISTIC" then do;
 if Wp1 = .5 then do;  DelLogst = 0;  return;  end;
 /* do Newton-Raphson */
 resid_p1 = .5;
 DelLogst = sqrt(2)*probit(Wp1)*(3.1416/sqrt(3));
 do until (abs(resid_p1/Wp1) < .001);
  Numatr = 1 - (1 + DelLogst)*exp(-DelLogst);
  dNumatr = DelLogst*exp(-DelLogst);
  Denomtr = (1 - exp(-DelLogst))**(-2);
  dDenomtr = -2*exp(-DelLogst)*(1 - exp(-DelLogst))**(-3);
  resid_p1 = Numatr*Denomtr - Wp1;
  deriv_p1 = dNumatr*Denomtr + Numatr*dDenomtr;
  DelLogst = DelLogst - resid_p1/deriv_p1;
  end;
 return;
 end;

if NomParnt = "LAPLACE" then do;
 if Wp1 = .5 then do;  DelLaplc = 0;  return;  end;
 /* do Newton-Raphson */
 resid_p1 = .5;
 DelLaplc = 2*probit(Wp1);
 do until (abs(resid_p1/Wp1) < .001);
  resid_p1 = 1 - exp(-DelLaplc)*(1 + DelLaplc/2)/2 - Wp1;
  deriv_p1 = exp(-DelLaplc)*(1 + DelLaplc)/4;
  DelLaplc = DelLaplc - resid_p1/deriv_p1;
  end;
 return;
 end; /* Laplace */


/*
  Link function to input and store comment lines.  Format is
    /#
    Comments lines, at most 78 chars per line, 40 lines.
    If any contain ";" then use DATALINES4 statement and follow all
    UnifyPow input with ";;;;".
    #/
*/
GetComnt: input;
  NumCmnts = NumCmnts + 1;
  Comment{NumCmnts} = "  ";
NxtComnt: input CmntLine $char78.;
  if index(CmntLine,"#/") > 0 then go to EndComnt;
  NumCmnts = NumCmnts + 1;
  Comment{NumCmnts} = CmntLine;
  go to NxtComnt;
EndComnt: NumCmnts = NumCmnts + 1;
  Comment{NumCmnts} = "  ";
  return;


/* MAKE MACRO: Remove commenting signs around next 3 lines. */

run;
%tables;
%mend UnifyPow;

