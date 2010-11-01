
/*
Date: Wed, 20 Nov 1996 10:25:23 -0500 (EST)
From: pat@po.CWRU.Edu (Paul A. Thompson)
*/
%macro _bootgen(
	_b=200,
	_base=WORK,
	_bootout=BOOTSAMP,
	_bvar=BOOT,
	_btype=S,
	_dv=,
	_idv=IDN,
	_ivs=,
	_sdhld=SEEDHLD,
	_seed=SEEDS,
	_seedv=SD1,
	_srcinv=SRCSET);

%* Vars _b:# bootstrap resample||_base:database||_bootout:dataset with resamples *;
%* _bvar:bootstrap identifier||_btype:type of selection-(S)imple,(R)egression    *;
%* _dv:dependent variable||_idv:ID variable||_ivs:independent variables          *;
%* _sdhld:seed archive||_seed:seed file||_seedv:seed var||_srcinv:sample         *;

PROC APPEND DATA=&_base..&_seed. BASE=&_base..&_sdhld.;
	RUN;
PROC MEANS DATA=&_base..&_srcinv.;* NOPRINT;
	VAR &_idv.;
	OUTPUT OUT=_CNT N=OCNT;
	RUN;

DATA &_base..&_bootout.(KEEP=&_idv. &_bvar. SUBCNT)/* Results dataset           */
     &_base..&_seed.(KEEP=&_seedv.);
	if _n_=1 then set _cnt;
	set  &_base..&_seed.;
*--	  MERGE &_base..&_seed. _CNT;
     DO &_bvar.=1 TO &_b.;
			DO SUBCNT=1 TO OCNT;
				CALL RANUNI(&_seedv.,&_idv.);
				&_idv.=CEIL(&_idv.*OCNT);
				OUTPUT &_base..&_bootout.;
			END;
	  END;  
	  OUTPUT &_base..&_seed.;
	  RUN;

	/* Order by subject number   */
PROC SORT DATA=&_base..&_bootout.;
	BY &_idv.;
	RUN;

%if (&_btype. = R) %then %do;                   /* Regression-style          */

PROC REG DATA=&_base..&_srcinv.(DROP=R&_dv. P&_dv.) NOPRINT;
     MODEL &_dv.=&_ivs.;
	  OUTPUT OUT=&_base..&_srcinv. P=P&_dv. R=R&_dv.;
	  RUN;
DATA &_base..&_bootout.;                        /* Merge in residual-error   */
     MERGE &_base..&_srcinv.(KEEP=R&_dv. &_idv. &_dv.) &_base..&_bootout.;
	  BY &_idv.;
	  RUN;

	/* Order by sequential #     */
PROC SORT DATA=&_base..&_bootout.;
	BY SUBCNT;	
	RUN;

DATA &_base..&_bootout.;                        /* Get in Yhat, and set up   */
     MERGE &_base..&_srcinv.(KEEP=P&_dv. &_ivs. &_idv. &_dv. RENAME=(&_dv.=H&_dv.))
           &_base..&_bootout.(RENAME=(&_idv.=_S&_idv. SUBCNT=&_idv.));
     BY &_idv.;
	  &_dv.=P&_dv.+R&_dv.;
	  RENAME &_idv.=SUBCNT _S&_idv.=&_idv.;
	  RUN;
%end;

%else %do;
	/* Merge source into bootout */
DATA &_base..&_bootout.;
     MERGE &_base..&_bootout.(IN=IN_B) &_base..&_srcinv.(IN=IN_D);
	  BY &_idv.;
	  RUN;
%end;
	/* Order by boot sample count*/
PROC SORT DATA=&_base..&_bootout.;
	BY &_bvar.;
	RUN;
%mend _bootgen;

%*--- Second macro;

%macro _itemcnt(_items=a b, _delim=1, _n=_nit);
	%let _delx=%str( ,*\~);
	%let &_n.=0;
	%let _ksep=%quote(%substr(&_delx., &_delim., 1));
	%do %while(%length(%scan(%str(&_items.),%eval(&&&_n.+1), %str(&_ksep.))) > 0);
	%let &_n.=%eval(&&&_n. + 1);
	%end;
%mend _itemcnt;

%macro _bootsql(
	_b=200,
	_base=WORK,
	_bootout=BOOTSAMP,
	_bvar=BOOT,
	_btype=S,
	_vlist=,
	_dv=,
	_idv=IDN,
	_ivs=,
	_sdhld=SEEDHLD,
	_seed=SEEDS,
	_seedv=SD1,
	_srcinv=SRCSET);
%* _vlist:var list        *;

PROC APPEND DATA=&_base..&_seed. BASE=&_base..&_sdhld.;
RUN;
PROC MEANS DATA=&_base..&_srcinv. NOPRINT;
	VAR &_idv.;
	OUTPUT OUT=_CNT N=OCNT;
	RUN;
DATA BOOTINT(KEEP=&_idv. &_bvar. RANIDV) &_base..&_seed.(KEEP=&_seedv.);
	MERGE &_base..&_seed. _CNT;
   DO &_bvar.=1 TO &_b.;
	DO &_idv.=1 TO OCNT;
		CALL RANUNI(&_seedv.,RANIDV);
      RANIDV=CEIL(RANIDV*OCNT);
		OUTPUT BOOTINT;
		END;
		END;
		OUTPUT &_base..&_seed.;
	RUN;

%if (&_btype. = R) %then %do;
%let _ci=;%_itemcnt(_items=&_ivs.,_n=_ci);
PROC REG DATA=&_base..&_srcinv.(DROP=R&_dv. P&_dv.) NOPRINT;
     MODEL &_dv.=&_ivs.;
	  OUTPUT OUT=&_base..&_srcinv. P=P&_dv. R=R&_dv.;
	  RUN;
%end;

%else %do;
	%let _ci=;
	%_itemcnt(_items=&_vlist.,_n=_ci);
	%put _ci[&_ci.];
	%end;

PROC SQL; 
	CREATE TABLE &_base..&_bootout. AS ORDER BY 1,2
%if (&_btype. = R) %then %do;
		SELECT F.&_bvar.,F.&_idv.,F.RANIDV,F.YHAT,
			R.EPSEST,F.YHAT+R.EPSEST AS NEWDEP,
			%do _bi=1 %to &_ci.;F.%scan(&_ivs.,&_bi),%end;
			F.&_dv.
		FROM (SELECT B.&_bvar., B.&_idv., B.RANIDV,D.P&_dv. AS YHAT,
			%do _bi=1 %to &_ci.;D.%scan(&_ivs.,&_bi),%end;
			D.&_dv.
		FROM &_base..&_srcinv. D, BOOTINT B WHERE D.&_idv.=B.&_idv.) F,
				(SELECT B.&_bvar., B.&_idv., B.RANIDV, D.R&_dv. AS EPSEST
		FROM &_base..&_srcinv. D, BOOTINT B WHERE D.&_idv.=B.RANIDV) R
		WHERE F.&_bvar.=R.&_bvar. & F.&_idv.=R.&_idv.;
		QUIT;
%end;
%else %do;
      SELECT B.&_bvar., B.&_idv., B.RANIDV,
      	%do _bi=1 %to &_ci.;D.%scan(&_vlist.,&_bi),%end;
			D.&_idv. AS G&_idv.
     FROM &_base..&_srcinv. D, BOOTINT B WHERE D.&_idv.=B.RANIDV;
	  QUIT;
%end;
%mend  _bootsql;


%*--------------- 3rd Macro;
%macro _bootiml(
	_b=200,
	_base=WORK,
	_bootout=BOOTSAMP,
	_bvar=BOOT,
	_btype=S,
	_dv=,
	_idv=IDN,
	_ivs=,
	_sdhld=SEEDHLD,
	_seed=SEEDS,
	_seedv=SD1,
	_srcinv=SRCSET);

PROC APPEND DATA=&_base..&_seed. BASE=&_base..&_sdhld.;
	RUN;
PROC IML;
%if (&_btype.=R) %then %do;USE &_base..&_srcinv.;
     READ ALL VAR {&_ivs.} INTO XMAT[COLNAME=CNZX];
	  READ ALL VAR {&_dv. } INTO YMAT[COLNAME=CNZY];
     READ ALL VAR {&_idv.} INTO IDMAT[COLNAME=CNZI];
	  DATASRC=YMAT||XMAT||IDMAT;CNZ=CNZY||CNZX||CNZI;
     BETA=INV(XMAT`*XMAT)*XMAT`*YMAT;YPRED=XMAT*BETA;
	  EPRED=YMAT-YPRED;
	  %end;

%else %do;
	USE &_base..&_srcinv.;
	READ ALL INTO DATASRC[COLNAME=CNZ];
	%end;
     CNZ="&_bvar."||CNZ;
	  CLOSE &_base..&_srcinv.;
	  NRX=NROW(DATASRC);
     USE &_base..&_seed.;
	  READ ALL INTO SDXZ[COLNAME=SEEDZ];
	  SDZ=SDXZ[1,1]; CLOSE &_base..&_seed.;
     SDV=J(NRX,1,1);
	  DO I=1 TO NRX;
	  A=1;
	  CALL RANUNI(SDZ, A);
	  SDV[I]=A;
	  END;
     SDV=CEIL(SDV*3.141593827*100000)-25;
	  RESAMP=J(NRX,1,1);
     DO I=1 TO &_b.;
	  CALL RANUNI(SDV,RESAMP);
	  RESAMPN=CEIL(RESAMP*NRX);
%if (&_btype.=R) %then %do;
        YANAL=EPRED[RESAMPN,]+YPRED;RESVAL=J(NRX,1,I)||YANAL||XMAT||IDMAT;
%end;
%else %do;
        RESVAL=DATASRC[RESAMPN,];
		  RESVAL=J(NRX,1,I)||RESVAL;
%end;
        if (i=1) then create &_base..&_bootout. from resval[colname=cnz];
        append from resval;
     end; 
	  close &_base..&_bootout.;
     create &_base..&_seed. from sdxz[colname=seedz];
	  sdxz[1,1]=sdz;
     append from sdxz;
	 close &_base..&_seed.;
	  quit;
%mend _bootiml;

%*--------------- 4th Macro;
%macro _calcci(
	_b=,
	_base=work,
	_bfinal=bres,
	_bvar=boot,
	_cil=95,
	_vlist=,
	_ostat=,
	_bstat=);

%* _bfinal:Final dataset with CIs||_cil:Level of CI (given as 1-alpha)            *;
%* _ostat:original statistics dataset||_bstat:bootstrap statistics dataset        *;

%let _nvr=;
%_itemcnt(_items=&_vlist., _n=_nvr);
%let _dv=%eval(100-&_cil.);
%let _hv=%eval(&_dv./2);
%if (%eval(&_hv.*2) ^= &_dv.) %then %let _pctl=&_hv..5 %eval(100-&_hv.-1).5;
%else                               %let _pctl=&_hv. %eval(100-&_hv.);

DATA BIASCHK;
	SET &_base..&_bstat. END=EOF;
	RETAIN _OS1-_OS&_nvr.;
	RETAIN PC1-PC&_nvr. 0;
   IF (_N_ = 1) THEN SET &_base..&_ostat.(
        RENAME=(%do _inv=1 %to &_nvr.; %scan(&_vlist.,&_inv.)=_OS&_inv.%end;));

     ARRAY VOS _OS1-_OS&_nvr.; ARRAY VBS &_vlist; ARRAY VPC PC1-PC&_nvr.;
     DO I=1 TO &_nvr.;
	  VPC{I}=VPC{I}+(VBS{I} < VOS{I});
	  END;
     IF (EOF) THEN DO;
	  DO I=1 TO &_nvr.;BCPCT=VPC{I}/_N_;VPC{I}=PROBIT(VPC{I}/_N_);
        BCLO=PROBNORM(PROBIT(%scan(&_pctl.,1," ")/100)+2*VPC{I});
        BCHI=PROBNORM(PROBIT(%scan(&_pctl.,2," ")/100)+2*VPC{I});
        CALL SYMPUT(COMPRESS("_bc"||PUT(I,4.)), PUT(BCLO*100,5.2)||" "||PUT(BCHI*100,5.2));
        OUTPUT;
		  END;END;
		  KEEP BCLO BCHI BCPCT;RUN;

%do _i=1 %to &_nvr.;
PROC UNIVARIATE DATA=&_base..&_bstat. NOPRINT;
	VAR %scan(&_vlist.,&_i.);
	OUTPUT OUT=CI&_i. 
		PCTLPTS=&_pctl. &&_bc&_i. 
		PCTLPRE=PCT_ 
		PCTLNAME=OL OH BL BH
        MEAN=BBAR STD=BSTD;
		  RUN;
PROC APPEND DATA=CI&_i. BASE=ALLCI;
	RUN;
%end;

PROC TRANSPOSE DATA=&_ostat. 
	OUT=TOSTAT(RENAME=(_NAME_=VARNM VX1=OSTAT)) PREFIX=VX;VAR &_vlist.;
	RUN;
DATA &_base..&_bfinal.;
	LENGTH VARNM $8;
	MERGE ALLCI TOSTAT BIASCHK;
   ZV=PROBIT(%scan(&_pctl.,2," ")/100);
   LABEL PCT_BL="Lower/BC BootEst/&_cil. pct CI" 
	  	PCT_BH="Upper/BC BootEst/&_cil. pct CI"
		NAP_LO="Lower/ParmEst/&_cil. pct CI"
		NAP_HI="Upper/ParmEst/&_cil. pct CI"
		PCT_OL="Lower/OrderSt/&_cil. pct CI"
		PCT_OH="Upper/OrderSt/&_cil. pct CI";
     NAP_LO=OSTAT-ZV*BSTD;NAP_HI=OSTAT+ZV*BSTD;
RUN;
%mend  _calcci;
