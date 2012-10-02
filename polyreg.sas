 /*
***********************************************************************
*                                                                     *
*   MACRO NAME :  REGRESS                                             *
*                                                                     *
*   PURPOSE    :  PERFORM POLYNOMIAL REGRESSION ANALYSIS ON XVAR      *
*                   WITH THE OPTION OF FIRST ELIMINATING OUTLIERS.    *
*                   PROVIDE DESCRIPTIVE STATISTICS ON XVAR AND YVAR,  *
*                   AND PERFORM TESTS OF MODELING ASSUMPTIONS.        *
*                                                                     *
*   AUTHORS    :  C. D. STERTZ & S. L. DAOOD                          *
*   DATE       :  08/26/88                                            *
*                                                                     *
*   CALL                                                              *
*   STATEMENT  :  %REGRESS(DSN, OPT, XVAR, YVAR, ID, C1)              *
*                                                                     *
*   PARAMETERS :  DSN  - INPUT DATA SET NAME.                         *
*                 OPT  - OUTLIER ELIMINATION OPTION.                  *
*                           OFFORD - FLAG OUTLIERS BUT INCLUDE IN     *
*                                    ANALYSIS.                        *
*                           OBRIEN - FLAG OUTLIERS AND EXCLUDE FROM   *
*                                    ANALYSIS (CAUTION: MAY NOT BE    *
*                                    APPROPRIATE WITH 'SMALL'         *
*                                    DATASETS).                       *
*                 XVAR - NAME OF X VARIABLE.                          *
*                 YVAR - NAME OF Y VARIABLE.                          *
*                 ID   - NAME OF ID VARIABLE (I.E. CLINIC).           *
*                 C1   - X INTERVAL CUT-OFF POINTS (I.E. 5 10 15).    *
*                        IF OMITTED, MACRO WILL CHOOSE 4 INTERVALS    *
*                        BASED ON RANGE OF X VALUES.                  *
*                                                                     *
*   PROCESSING :  OUTLIERS - A VALUE IS FLAGGED AS AN OUTLIER IF      *
*                   IT IS MORE THAN ONE STANDARD DEVIATION FROM THE   *
*                   NEXT HIGHEST (LOWEST) VALUE.  (AT MOST 5 VALUES   *
*                   ARE CHECKED.)                                     *
*                                                                     *
*   OUTPUT     :  PAGE 1 -                                            *
*                  DESCRIPTIVE STATISTICS -                           *
*                   EXTREME VALUES WITH OUTLIERS FLAGGED.  MEANS &    *
*                   STD DEVS ARE PROVIDED FOR EACH X INTERVAL AND     *
*                   OVERALL.  P-VALUES FOR ANOVA & SPEARMAN'S TESTS.  *
*                  POLYNOMIAL REGRESSION ANALYSIS -                   *
*                   POLYNOMIAL COEFFICIENTS FOR LINEAR THRU QUINTIC   *
*                   MODELS ALONG WITH R-SQUARE AND P VALUES FOR       *
*                   MODEL COMPARISONS.                                *
*                  TESTS OF MODELING ASSUMPTIONS -                    *
*                   SKEWNESS, KURTOSIS, NORMALITY, SPEARMAN AND       *
*                   LEVENE TESTS ARE PERFORMED ON EACH OF THE MODELS. *
*                 PAGE 2 -                                            *
*                   PLOT OF Y VS. X AND MEAN-Y VS. MEAN-X.            *
*                                                                     *
*   ERROR MSG  :  MESSAGES ARE OUTPUT IF AN INPUT PARAMETER IS        *
*                 MISSING OR OUT OF RANGE.                            *
*                                                                     *
***********************************************************************
  */

%MACRO REGRESS(DSN,OPT,XVAR,YVAR,ID,C1);
     %GLOBAL QUIT I N4 NOBSERV;
     OPTIONS DQUOTE;
     %IF &DSN=  %THEN %DO;
         %PUT "ERROR: NO DATASET NAME WAS SUPPLIED IN PARM LIST.";
         %GO TO ENDUP;
     %END;
     %IF &OPT=  %THEN %DO;
         %PUT "ERROR: NO OPTION (OBRIEN/OFFORD) WAS SUPPLIED.";
         %GO TO ENDUP;
     %END;
     %IF ^(&OPT=OBRIEN | &OPT=OFFORD) %THEN %DO;
         %PUT "ERROR: OPTION MUST BE OBRIEN OR OFFORD.";
         %GO TO ENDUP;
     %END;
     %IF &XVAR=   %THEN %DO;
         %PUT "ERROR: NO X VARIABLE SUPPLIED";
         %GO TO ENDUP;
     %END;
     %IF &YVAR=   %THEN %DO;
         %PUT "ERROR: NO Y VARIABLE SUPPLIED";
         %GO TO ENDUP;
     %END;
     %LET _TIT=    ;
     %IF &OPT=OBRIEN %THEN %LET _TIT="OUTLIERS ELIMINATED";

****  COUNT & ELIMINATE MISSINGS  ****;
     DATA _DSN;  SET &DSN END=EOF;
        KEEP &ID &XVAR &YVAR;
        RETAIN  DEL_CNT 0;
        IF (&XVAR=. | &YVAR=.)  THEN DEL_CNT = DEL_CNT+1;
        IF (EOF)  THEN DO;
          DELC = PUT(DEL_CNT,3.);
          CALL SYMPUT('_DELS',DELC);
        END;
        IF (&XVAR=. | &YVAR=.)  THEN DELETE;

     %IF &C1=   %THEN %DO;
         %LET NUM=4;
         DATA _NULL_; SET _DSN END=EOF;
            RETAIN BOTTOM TOP;
            IF (_N_ =1)  THEN DO;
               BOTTOM=&XVAR; TOP=&XVAR;
            END;
            IF (&XVAR<BOTTOM)  THEN BOTTOM=&XVAR;
            IF (&XVAR>TOP)  THEN TOP=&XVAR;
            IF (EOF)  THEN DO;
               INTERVAL=(TOP - BOTTOM)/4;
               T1=CEIL(BOTTOM + INTERVAL);
               T2=CEIL(T1 + INTERVAL);
               T3=CEIL(T2 + INTERVAL);
               TT1='     '; TT2='     ';   TT3='     ';
               TT1=PUT(T1,Z5.);
               TT2=PUT(T2,Z5.);
               TT3=PUT(T3,Z5.);
               NEWVAL='                    ';
               NEWVAL=TT1 || ' ' || TT2 || ' ' || TT3;
               CALL SYMPUT('C1',NEWVAL);
            END;
     %END;

     DATA _DSN(KEEP=&ID GROUP WEIGH X X2 X3 X4 X5 Y);
            SET _DSN END=EOF;
       RETAIN COUNT 0 DEL_CNT 0;
       GROUP="            ";
       %LET BE=X;
       %LET I=1;
       X  = &XVAR;
       X2 = X*X;
       X3 = X2*X;
       X4 = X3*X;
       X5 = X4*X;
       Y  = &YVAR;
       COUNT = COUNT + 1;
       %DO %UNTIL(&BE= );
            %LET BI=%SCAN(&C1,&I,' ');
            %LET BE=%SCAN(&C1,&I+1,' ');
            %IF &I=1 %THEN %DO;
               IF &XVAR <&BI THEN DO;
                 GROUP="    X<&BI";
                 WEIGH=0;
               END;
            %END;
            %IF &BE^=  %THEN %DO;
               %IF &BE<&BI %THEN %DO;
                 PUT "ERROR: NON-INCREASING INTERVALS IN PARM LIST";
                 %GO TO ENDUP;
               %END;
               IF &BI <= &XVAR < &BE THEN DO;
                 GROUP="&BI<=X<&BE";
                 WEIGH=1;
               END;
            %END;
            %ELSE %DO;
               IF &XVAR>=&BI THEN DO;
                 GROUP="&BI<=X    ";
                 WEIGH=2;
               END;
            %END;
            %LET I=%EVAL(&I+1);
       %END;
     OUTPUT _DSN;
     %LET NUM=%EVAL(&I);
     IF EOF THEN DO;
       CC = PUT(COUNT,6.);
       CALL SYMPUT('QUIT',CC);
     END;

  DATA _DSN; SET _DSN END=EOF;
   %IF &QUIT>=6 %THEN %DO;

     RETAIN COUNT 0;
     COUNT = COUNT + 1;
     ID = &ID;
     %LET I=1;
     %DO II=1 %TO &NUM;
        G&I=0;
        %LET I=%EVAL(&I+1);
     %END;
     %LET BE=X;
     %LET I=1;
       %DO %UNTIL(&BE= );
            %LET BI=%SCAN(&C1,&I,' ');
            %LET BE=%SCAN(&C1,&I+1,' ');
            %IF &I=1 %THEN %DO;
                %LET II=1;
                IF X < &BI THEN G&II=1;
            %END;
            %LET II=%EVAL(&II+1);
            %IF &BE^=  %THEN %DO;
                %IF &BE<=&BI %THEN %DO;
                   PUT "ERROR: NON-INCREASING INTERVALS IN PARM LIST";
                   %GO TO ENDUP;
                %END;
                IF &BI <= X < &BE THEN G&II=1;
            %END;
            %ELSE %DO;
                IF X >= &BI THEN G&II=1;
            %END;
            %LET I=%EVAL(&I+1);
       %END;
       IF EOF THEN DO;
         NN = PUT(COUNT,Z5.);
         DO I=1 TO 5;
            IF (SUBSTR(NN,I,1)^='0') THEN DO;
               INDEX = I;
               I=5;
            END;
         END;
         NNN = SUBSTR(NN,INDEX,5-INDEX+1);
         CALL SYMPUT('NOBSERV',NNN);
      END;

****  DETERMINE AND THROW OUT OUTLIERS IF REQUESTED  ****;
%MACRO STD_DEV;
  SM = 0;  SM_SQ = 0;
  DO I=START TO END;
    SM = SM + OBS(I);
    SM_SQ = SM_SQ + (OBS(I)**2);
  END;
  SQ_SM = SM**2 / (END - START + 1);
  S2 = (SM_SQ - SQ_SM) / (END - START);
  STDEV = SQRT(S2);
%MEND STD_DEV;

%MACRO STD_CHK;
    START = 1;
    END = &NOBSERV;
    %STD_DEV;
    IF (&NOBSERV>=2  &  ABS(OBS(1) - OBS(2)) > STDEV) THEN DO;
      OUTL(1) = '*';
      START = 2;
      %STD_DEV;
    END;
    IF (&NOBSERV>=3  &  ABS(OBS(2) - OBS(3)) > STDEV) THEN DO;
      OUTL(1) = '*';
      OUTL(2) = '*';
      START = 3;
      %STD_DEV;
    END;
    IF (&NOBSERV>=4  &  ABS(OBS(3) - OBS(4)) > STDEV)  THEN DO;
      OUTL(1) = '*';
      OUTL(2) = '*';
      OUTL(3) = '*';
      START = 4;
      %STD_DEV;
    END;
    IF (&NOBSERV>=5  &  ABS(OBS(4) - OBS(5)) > STDEV)  THEN DO;
      OUTL(1) = '*';
      OUTL(2) = '*';
      OUTL(3) = '*';
      OUTL(4) = '*';
      START = 5;
      %STD_DEV;
    END;
    IF (&NOBSERV>=6  &  ABS(OBS(5) - OBS(6)) > STDEV)  THEN DO;
      OUTL(1) = '*';
      OUTL(2) = '*';
      OUTL(3) = '*';
      OUTL(4) = '*';
      OUTL(5) = '*';
      START = 6;
      %STD_DEV;
    END;
    IF (&NOBSERV>=7  &
               ABS(OBS(&NOBSERV) - OBS(&NOBSERV-1)) > STDEV) THEN DO;
      OUTL(10) = '*';
      END = &NOBSERV - 1;
      IF (END > START)  THEN DO;
        %STD_DEV;
      END;
    END;
    IF (&NOBSERV>=8  &
               ABS(OBS(&NOBSERV-1) - OBS(&NOBSERV-2)) > STDEV) THEN DO;
      OUTL(10) = '*';
      OUTL(9) = '*';
      END = &NOBSERV - 2;
      IF (END > START)  THEN DO;
        %STD_DEV;
      END;
    END;
    IF (&NOBSERV>=9  &
               ABS(OBS(&NOBSERV-2) - OBS(&NOBSERV-3)) > STDEV) THEN DO;
      OUTL(10) = '*';
      OUTL(9) = '*';
      OUTL(8) = '*';
      END = &NOBSERV - 3;
      IF (END > START)  THEN DO;
        %STD_DEV;
      END;
    END;
    IF (&NOBSERV>=10  &
               ABS(OBS(&NOBSERV-3) - OBS(&NOBSERV-4)) > STDEV) THEN DO;
      OUTL(10) = '*';
      OUTL(9) = '*';
      OUTL(8) = '*';
      OUTL(7) = '*';
      END = &NOBSERV - 4;
      IF (END > START)  THEN DO;
        %STD_DEV;
      END;
    END;
    IF (&NOBSERV>=11  &
               ABS(OBS(&NOBSERV-4) - OBS(&NOBSERV-5)) > STDEV) THEN DO;
      OUTL(10) = '*';
      OUTL(9) = '*';
      OUTL(8) = '*';
      OUTL(7) = '*';
      OUTL(6) = '*';
      END = &NOBSERV - 5;
      IF (END > START)  THEN DO;
        %STD_DEV;
      END;
    END;
%MEND STD_CHK;

%MACRO OUTLR(L);
  PROC SORT DATA=_DSN; BY DESCENDING &L;

  PROC TRANSPOSE DATA=_DSN OUT=_TRANSID  PREFIX=ID&L._;
    VAR ID;

  DATA _TRANSID; SET _TRANSID;
    %LET N4 = 6;
    %IF (&NOBSERV > 10)  %THEN %DO;
      %LET N4 = %EVAL(&NOBSERV-4);
    %END;
    KEEP ID&L._1-ID&L._5 ID&L._&N4-ID&L._&NOBSERV;

  PROC TRANSPOSE DATA=_DSN OUT=_TRANS  PREFIX=&L._;
    VAR &L.;

  DATA _OUTL&L; SET _TRANS;
    LENGTH  OUTL&L._1-OUTL&L._10  $1.;
    ARRAY OBS(*)  &L._1-&L._&NOBSERV;
    ARRAY OUTL(10)  OUTL&L._1-OUTL&L._10;
    %LET N4 = 6;
    %IF (&NOBSERV > 10)  %THEN %DO;
      %LET N4 = %EVAL(&NOBSERV-4);
    %END;
    KEEP &L._1-&L._5  &L._&N4-&L._&NOBSERV  OUTL&L._1-OUTL&L._10;
    %STD_CHK;

  DATA _MNMX&L;
    IF _N_=1 THEN SET _TRANSID;
    SET _OUTL&L;
%MEND OUTLR;

****  REMOVE OUTLYING X  ****;
%OUTLR(X);

****  REMOVE OUTLYING Y  ****;
%OUTLR(Y);

DATA _T_DAT;
  IF _N_=1 THEN DO;
    MERGE _MNMXX _MNMXY;
  END;
  SET _DSN;
  KEEP ID X Y WEIGH GROUP X2-X5 G1-G&NUM;
  %LET N4 = %EVAL(&NOBSERV-4);
  %LET N3 = %EVAL(&NOBSERV-3);
  %LET N2 = %EVAL(&NOBSERV-2);
  %LET N1 = %EVAL(&NOBSERV-1);
  %IF (&OPT = OBRIEN) %THEN %DO;
      IF ((X=X_1 & OUTLX_1='*')   | (X=X_2 & OUTLX_2='*')   |
          (X=X_3 & OUTLX_3='*')   | (X=X_4 & OUTLX_4='*')   |
          (X=X_5 & OUTLX_5='*')   | (X=X_&N4 & OUTLX_6='*') |
          (X=X_&N3 & OUTLX_7='*') | (X=X_&N2 & OUTLX_8='*') |
          (X=X_&N1 & OUTLX_9='*') | (X=X_&NOBSERV & OUTLX_10='*') |
          (Y=Y_1 & OUTLY_1='*')   | (Y=Y_2 & OUTLY_2='*')   |
          (Y=Y_3 & OUTLY_3='*')   | (Y=Y_4 & OUTLY_4='*')   |
          (Y=Y_5 & OUTLY_5='*')   | (Y=Y_&N4 & OUTLY_6='*') |
          (Y=Y_&N3 & OUTLY_7='*') | (Y=Y_&N2 & OUTLY_8='*') |
          (Y=Y_&N1 & OUTLY_9='*') | (Y=Y_&NOBSERV & OUTLY_10='*'))
          THEN DELETE;
  %END;

 PROC SORT DATA=_T_DAT; BY WEIGH GROUP;

 DATA _GRPS; SET _T_DAT END=EOF; BY WEIGH GROUP;
    RETAIN G_NUM 0;
    KEEP _GRPS;
    IF FIRST.GROUP THEN DO;
        _GRPS=GROUP;
        G_NUM= G_NUM + 1;
        OUTPUT;
    END;
    IF EOF THEN DO;
         NN = PUT(G_NUM,Z2.);
         DO I=1 TO 2;
            IF (SUBSTR(NN,I,1)^='0') THEN DO;
               INDEX = I;
               I=2;
            END;
         END;
         NNN = SUBSTR(NN,INDEX,2-INDEX+1);
         CALL SYMPUT('NUM',NNN);
    END;

 DATA _GRPS; SET _GRPS END=EOF;
    KEEP _GRPS;
    OUTPUT;
    IF EOF THEN DO;
       CT=_N_; _GRPS="            ";
       DO WHILE(CT<10);
          CT = CT + 1;
          OUTPUT;
       END;
       _GRPS='  TOTAL';
       OUTPUT;
    END;

DATA _DSN; SET _T_DAT END=EOF;
  IF (EOF)  THEN DO;
    OUTS = &NOBSERV - _N_;
    OUTSPUT = PUT(OUTS,3.);
    CALL SYMPUT('_OUTS',OUTSPUT);
    NN = PUT(_N_,Z5.);
    DO I=1 TO 5;
      IF (SUBSTR(NN,I,1) ^= '0') THEN DO;
        INDEX = I;
        I = 5;
      END;
    END;
    NNN = SUBSTR(NN,INDEX,5-INDEX+1);
    CALL SYMPUT('NOBSERV',NNN);
  END;
  %LET KK = %EVAL(&NUM-1);

****  CALCULATE GROUP & TOTAL STATS  ****;
PROC SORT DATA=_DSN; BY GROUP WEIGH;

PROC MEANS NOPRINT DATA=_DSN;
  BY GROUP WEIGH;
  VAR Y X;
  OUTPUT OUT=_GRPSTAT MEAN=MN_Y MN_X
                      STD=STD_Y
                      N=N;

PROC SORT DATA=_GRPSTAT; BY WEIGH GROUP;

PROC MEANS  NOPRINT DATA=_DSN;
  VAR Y X;
  OUTPUT OUT=_TOTSTAT(KEEP=MN_Y MN_X STD_Y STD_X CSS_Y CSS_X N)
                      MEAN=MN_Y MN_X
                      STD=STD_Y STD_X
                      CSS=CSS_Y CSS_X
                      N=N;

DATA _NULL_;  SET _TOTSTAT;
  CALL SYMPUT('MEANX', MN_X);

DATA _MEANS;  SET _GRPSTAT(IN=INGP) _TOTSTAT(IN=INSMN);
  DROP CT SAVE1-SAVE7;
  IF INGP THEN OUTPUT;
  IF INSMN  THEN DO;
    SAVE1=N; SAVE2=MN_Y; SAVE3=MN_X; SAVE4=STD_Y;
    SAVE5=STD_X; SAVE6=CSS_Y; SAVE7=CSS_X;
    N=.; MN_Y=.; MN_X=.; STD_Y=.; STD_X=.; CSS_Y=.; CSS_X=.;
    GROUP='       ';
    CT=_N_;
    DO WHILE (CT<=10);
       OUTPUT;
       CT=CT + 1;
    END;
    GROUP='  TOTAL';
    N=SAVE1; MN_Y=SAVE2; MN_X=SAVE3;  STD_Y=SAVE4;
    STD_X=SAVE5;  CSS_Y=SAVE6;  CSS_X=SAVE7;
    OUTPUT;
  END;

****  TRANSFORM X VALUES AROUND MEAN  ****;
DATA _DSNSAV;  SET _DSN;

DATA _DSN;  SET _DSN;
  X = X - &MEANX;
  X2 = X * X;
  X3 = X2 * X;
  X4 = X3 * X;
  X5 = X4 * X;

****  FIT POLYNOMIAL MODELS - OUTPUT COEFF & RESIDUALS ****;
PROC SORT DATA=_DSN; BY X Y ID;
PROC REG  DATA=_DSN NOPRINT;
  MEAN:      MODEL Y=;  OUTPUT OUT=_RES_0 R=R_0;
PROC REG  DATA=_DSN NOPRINT OUTEST=_OUTREGR;
  LINEAR:    MODEL Y=X;  OUTPUT OUT=_RES_1 R=R_1;
  QUADRATC:  MODEL Y=X X2;  OUTPUT OUT=_RES_2 R=R_2;
  CUBIC:     MODEL Y=X X2 X3;  OUTPUT OUT=_RES_3 R=R_3;
  QUARTIC:   MODEL Y=X X2 X3 X4;  OUTPUT OUT=_RES_4 R=R_4;
  QUINTIC:   MODEL Y=X X2 X3 X4 X5;  OUTPUT OUT=_RES_5 R=R_5;


****  ANOVA ON Y & MODIFIED LEVENE TEST ****;
DATA _LEV; MERGE  _RES_0 _RES_1 _RES_2 _RES_3 _RES_4 _RES_5;  BY X Y ID;

PROC UNIVARIATE  NOPRINT DATA=_LEV;
  VAR  R_0  R_1  R_2  R_3  R_4  R_5;
  OUTPUT OUT=R_STAT MEDIAN=LMD_0  LMD_1  LMD_2  LMD_3  LMD_4  LMD_5
                    MEAN = MNR_0  MNR_1  MNR_2  MNR_3  MNR_4  MNR_5
                    STD  = STDR_0 STDR_1 STDR_2 STDR_3 STDR_4 STDR_5;

DATA _LEV;
  IF _N_=1  THEN SET R_STAT;
  SET _LEV;
  LEV_0 = ABS(R_0 - LMD_0);
  LEV_1 = ABS(R_1 - LMD_1);
  LEV_2 = ABS(R_2 - LMD_2);
  LEV_3 = ABS(R_3 - LMD_3);
  LEV_4 = ABS(R_4 - LMD_4);
  LEV_5 = ABS(R_5 - LMD_5);
  IF (&NOBSERV>50)  THEN DO;
    Z_0 = PROBNORM((R_0 - MNR_0)/STDR_0);
    Z_1 = PROBNORM((R_1 - MNR_1)/STDR_1);
    Z_2 = PROBNORM((R_2 - MNR_2)/STDR_2);
    Z_3 = PROBNORM((R_3 - MNR_3)/STDR_3);
    Z_4 = PROBNORM((R_4 - MNR_4)/STDR_4);
    Z_5 = PROBNORM((R_5 - MNR_5)/STDR_5);
  END;

%MACRO GG;
  %DO I=1 %TO &KK;
    G&I
  %END;
%MEND GG;

PROC RSQUARE  NOPRINT DATA=_LEV  OUTEST=_OUTRSQ SSE MSE;
        MODEL Y = %GG     / INCLUDE=&KK;
        MODEL LEV_0 = %GG / INCLUDE=&KK;
        MODEL LEV_1 = %GG / INCLUDE=&KK;
        MODEL LEV_2 = %GG / INCLUDE=&KK;
        MODEL LEV_3 = %GG / INCLUDE=&KK;
        MODEL LEV_4 = %GG / INCLUDE=&KK;
        MODEL LEV_5 = %GG / INCLUDE=&KK;

DATA _AONY _LEV_P;  SET _OUTRSQ;
  SSR = _RSQ_ * _SSE_ / (1 - _RSQ_);
  MSB = SSR / (&NUM-1);
  F = MSB / _MSE_;
  P = 1 - PROBF(F,&NUM-1,&NOBSERV-&NUM);
  IF _N_=1  THEN OUTPUT _AONY;
  IF _N_^=1  THEN OUTPUT _LEV_P;

****  NORMALITY  ****;
%LET W_P_VAL1 = .753/.687/.686/ .713/.730/.749/.764/.781/
      .792/.805/.814/.825/.835/ .844/.851/.858/.863/.868/
      .873/.878/.881/.884/.888/ .891/.894/.896/.898/.900/
      .902/.904/.906/.908/.910/ .912/.914/.916/.917/.919/
      .920/.922/.923/.924/.926/ .927/.928/.929/.929/.930;
%LET W_P_VAL2 = .756/.707/.715/ .743/.760/.778/.791/.806/
      .817/.828/.837/.846/.855/ .863/.869/.874/.879/.884/
      .888/.892/.895/.898/.901/ .904/.906/.908/.910/.912/
      .914/.915/.917/.919/.920/ .922/.924/.925/.927/.928/
      .929/.930/.932/.933/.934/ .935/.936/.937/.937/.938;
%LET W_P_VAL3 = .767/.748/.762/ .788/.803/.818/.829/.842/
      .850/.859/.866/.874/.881/ .887/.892/.897/.901/.905/
      .908/.911/.914/.916/.918/ .920/.923/.924/.926/.927/
      .929/.930/.931/.933/.934/ .935/.936/.938/.939/.940/
      .941/.942/.943/.944/.945/ .945/.946/.947/.947/.947;
%LET W_P_VAL4 = .789/.792/.806/ .826/.838/.851/.859/.869/
      .876/.883/.889/.895/.901/ .906/.910/.914/.917/.920/
      .923/.926/.928/.930/.931/ .933/.935/.936/.937/.939/
      .940/.941/.942/.943/.944/ .945/.946/.947/.948/.949/
      .950/.951/.951/.952/.953/ .953/.954/.954/.955/.955;

%MACRO NORM;
  %IF (&NOBSERV<=50)  %THEN %DO;
      PROC UNIVARIATE  NOPRINT DATA=_LEV;
        VAR R_0 R_1 R_2 R_3 R_4 R_5;
        OUTPUT OUT=_NORML  NORMAL=W_0 W_1 W_2 W_3 W_4 W_5;

      %MACRO PN;
        %DO I=0 %TO 5;
          PNORM_&I = '      P>.10';
          IF (W_&I <= &W4) THEN PNORM_&I = ' .05<P<=.10';
          IF (W_&I <= &W3) THEN PNORM_&I = ' .02<P<=.05';
          IF (W_&I <= &W2) THEN PNORM_&I = ' .01<P<=.02';
          IF (W_&I <= &W1) THEN PNORM_&I = '     P<=.01';
        %END;
      %MEND PN;

      DATA _NORML;  SET _NORML;
        %LET INDX = %EVAL(&NOBSERV - 2);
        %LET W1 = %SCAN(&W_P_VAL1, &INDX, '/');
        %LET W2 = %SCAN(&W_P_VAL2, &INDX, '/');
        %LET W3 = %SCAN(&W_P_VAL3, &INDX, '/');
        %LET W4 = %SCAN(&W_P_VAL4, &INDX, '/');
        %PN;
  %END;

  %IF (&NOBSERV>50)  %THEN %DO;
    %DO I=0 %TO 5;
      PROC SORT DATA=_LEV;  BY Z_&I;
      PROC TRANSPOSE DATA=_LEV OUT=_ZTRAN(DROP=_NAME_) PREFIX=Z_;
        VAR Z_&I.;

      %MACRO A2_CALC;
        ARRAY Z(&NOBSERV)  Z_1 - Z_&NOBSERV;
        A2_&I. = 0;
        DO J=1 TO &NOBSERV;
          A2_&I = A2_&I +
                (2*J-1) * (LOG(Z(J)) + LOG(1 - Z(&NOBSERV-J+1)));
        END;
        A2_&I = (-1 * A2_&I / &NOBSERV) - &NOBSERV;
        A2_&I = A2_&I * (1 + .75/&NOBSERV + 2.25/(&NOBSERV**2));
        IF (A2_&I >= 1.035) THEN PNORM_&I = '     P<=.01';
        IF (A2_&I < 1.035)  THEN PNORM_&I = '.01<P<=.025';
        IF (A2_&I < .873)   THEN PNORM_&I = '.025<P<=.05';
        IF (A2_&I < .752)   THEN PNORM_&I = ' .05<P<=.10';
        IF (A2_&I < .631)   THEN PNORM_&I = '      P>.10';
      %MEND A2_CALC;

      DATA _ZA2_&I;  SET _ZTRAN;
        KEEP  A2_&I  PNORM_&I;
        RETAIN A2_&I;
        %A2_CALC;
    %END;

    DATA _NORML; MERGE _ZA2_0 _ZA2_1 _ZA2_2 _ZA2_3 _ZA2_4 _ZA2_5;
  %END;
%MEND NORM;

%NORM;

****  SPEARMAN R  ****;
DATA _RES;  SET _RES_0 _RES_1 _RES_2 _RES_3 _RES_4 _RES_5;
  ABS_R0 = ABS(R_0);
  ABS_R1 = ABS(R_1);
  ABS_R2 = ABS(R_2);
  ABS_R3 = ABS(R_3);
  ABS_R4 = ABS(R_4);
  ABS_R5 = ABS(R_5);

PROC CORR SPEARMAN  NOSIMPLE NOPRINT DATA=_RES OUTS=_OUTSPR;
  VAR X;
  WITH  Y  ABS_R0  ABS_R1  ABS_R2  ABS_R3  ABS_R4  ABS_R5;

DATA _OUTSPR;  SET _OUTSPR;
  IF (_TYPE_='CORR')  THEN DO;
    T = SQRT(&NOBSERV - 2) * X / SQRT(1 - X**2);
    P = (1 - PROBT(ABS(T),&NOBSERV-2)) * 2;
  END;

****  SKEWNESS, KURTOSIS & NORMALITY COEFFICIENTS & Z  ****;
PROC UNIVARIATE DATA=_RES NOPRINT;
  VAR  X Y R_0 R_1 R_2 R_3 R_4 R_5;
  OUTPUT OUT=_COEFF
       N=N_X N_Y N_R0 N_R1 N_R2 N_R3 N_R4 N_R5
       SKEWNESS=SKW_X SKW_Y SKW_R0 SKW_R1 SKW_R2 SKW_R3 SKW_R4 SKW_R5
       KURTOSIS=KRT_X KRT_Y KRT_R0 KRT_R1 KRT_R2 KRT_R3 KRT_R4 KRT_R5
       NORMAL=NML_X NML_Y NML_R0 NML_R1 NML_R2 NML_R3 NML_R4 NML_R5;

%MACRO Z_P;
  %DO I=0 %TO 5;
    Z_SKW_R&I = SKW_R&I / SE_S;
    Z_KRT_R&I = KRT_R&I / SE_K;
  %END;
%MEND Z_P;

DATA _ZP;  SET _COEFF;
  SE_S = SQRT(6 / &NOBSERV);
  SE_K = SQRT(24 / &NOBSERV);
  Z_SKW_X = SKW_X / SE_S;
  Z_KRT_X = KRT_X / SE_K;
  Z_SKW_Y = SKW_Y / SE_S;
  Z_KRT_Y = KRT_Y / SE_K;
  %Z_P;

****  POLYNOMIAL REGRESSION ANALYSIS  ****;
DATA MEAN_PGA;
  SET _TOTSTAT;
  EDF_MN = &NOBSERV - 1;
  SSE_MN = CSS_Y;
  RMSE_MN = STD_Y;

DATA _PGANAL;
  IF _N_=1  THEN SET MEAN_PGA;
  SET _OUTREGR;
  IF (X=.)  THEN X = 0;
  IF (X2=.)  THEN X2 = 0;
  IF (X3=.)  THEN X3 = 0;
  IF (X4=.)  THEN X4 = 0;
  IF (X5=.)  THEN X5 = 0;
  B0 = INTERCEP - (X * &MEANX) + (X2 * &MEANX**2) - (X3 * &MEANX**3) +
       (X4 * &MEANX**4) + (X5 * &MEANX**5);
  B1 = X  - (2 * X2 * &MEANX) + (3 * X3 * &MEANX**2) -
       (4 * X4 * &MEANX**3) + (5 * X5 * &MEANX**4);
  B2 = X2 - (3 * X3 * &MEANX) + (6 * X4 * &MEANX**2) -
       (10 * X5 * &MEANX**3);
  B3 = X3 - (4 * X4 * &MEANX) + (10 * X5 * &MEANX**2);
  B4 = X4 - (5 * X5 * &MEANX);
  B5 = X5;
  IF (_N_=1)  THEN DO;  B2=.;  B3=.;  B4=.;  B5=.;  END;
  IF (_N_=2)  THEN DO;  B3=.;  B4=.;  B5=.;  END;
  IF (_N_=3)  THEN DO;  B4=.;  B5=.;  END;
  IF (_N_=4)  THEN DO;  B5=.;  END;
  %if &sysver=5.18 %then %do;
       _RMSE_ = _SIGMA_;
  %end;
  _EDF_ = &NOBSERV - _N_ - 1;
  _SSE_ = _EDF_ * (_RMSE_**2);
  _RSQ_ = 1 - (_SSE_/SSE_MN);
  L_SSE = LAG(_SSE_);
  IF _N_=1  THEN L_SSE = SSE_MN;
  F_I_1 = (L_SSE - _SSE_) / (_RMSE_**2);
  P_I_1 = 1 - PROBF(F_I_1 , 1 , _EDF_);
  F_0 = ((SSE_MN - _SSE_) / _N_) / (_RMSE_**2);
  P_0 = 1 - PROBF(F_0 , _N_ , _EDF_);

DATA __AONY; SET _AONY;
  KEEP _RMSE_ AONY_P;
     AONY_P=P;

DATA _SPEAR; SET _OUTSPR;
  KEEP X SPEAR_P;
     IF _N_=4;
     SPEAR_P=P;

DATA _P_SPEAR; SET _OUTSPR;
  RETAIN P_R0-P_R5;
     KEEP P_R0-P_R5;
       IF _N_=5 THEN P_R0=P;
       ELSE IF _N_=6 THEN P_R1=P;
       ELSE IF _N_=7 THEN P_R2=P;
       ELSE IF _N_=8 THEN P_R3=P;
       ELSE IF _N_=9 THEN P_R4=P;
       ELSE IF _N_=10 THEN P_R5=P;
       IF _N_=10 THEN OUTPUT;

  DATA _WOW; MERGE _MNMXX _MNMXY _GRPS _MEANS __AONY _SPEAR;

  DATA __LEV_P; SET _LEV_P;
    KEEP LEVP0-LEVP5;
    RETAIN LEVP0-LEVP5;
       IF _N_=1 THEN LEVP0=P;
       ELSE IF _N_=2 THEN LEVP1=P;
       ELSE IF _N_=3 THEN LEVP2=P;
       ELSE IF _N_=4 THEN LEVP3=P;
       ELSE IF _N_=5 THEN LEVP4=P;
       ELSE IF _N_=6 THEN LEVP5=P;
       IF _N_=6 THEN OUTPUT;

  /************************************************************/
  /*   PRINT RESULTS ON SPECIFIED FORMAT                      */
  /************************************************************/

 TITLE7  "P O L Y N O M I A L   R E G R E S S I O N " ;
 DATA _NULL_; SET _WOW;
   FILE PRINT N=PS;
     PUT #1 @1 130*"="
         #2 @1 "| DESCRIPTIVE STATISTICS |"   @30  &_TIT
         #3 @2 "------------------------"
            @35 "# OF OBS DELETED DUE TO : MISSING VALUES = " "&_DELS"
            @95 "DATASET USED: " "&DSN"
         #4 @2 "X="  "&XVAR"  @14 "Y=" "&YVAR"
            @35 "                        : OUTLIERS       = " "&_OUTS"
            @95 "OPTION      : " "&OPT";

   PUT @1 130*"=";
      PUT @5 "EXTREME VALUES  (* INDICATES OUTLIER)" @50 "|"
          @53 "X INTERVAL" @69 "N" @77 "MEAN X"  @89 "MEAN Y"
          @102 "STD Y" @108 "|";
      PUT @6 "X" @ 31"Y" @50 "|"  @52 _GRPS $CHAR12. @65 N 6.
          @73 MN_X 10.2 @85 MN_Y 10.2  @97 STD_Y 10.2
          @108 "|" @115 "ANOVA ON Y" /
          @4 "HIGH" @12 "&ID"  @29 "HIGH" @37 "&ID" @50 "|"
          @108 "|    USING X INTERVALS" /
          @2 X_1 8.3 @10 "(" @11 IDX_1 10. @21 ")" @23 OUTLX_1 $1.
          @27 Y_1 8.3 @35 "(" @36 IDY_1 10. @46 ")" @48 OUTLY_1 $1.
          @50 "|" @108 "| SQRT(MSE)=" @120 _RMSE_ 10.2 /
          @2 X_2 8.3 @10 "(" @11 IDX_2 10. @21 ")" @23 OUTLX_2 $1.
          @27 Y_2 8.3 @35 "(" @36 IDY_2 10. @46 ")" @48 OUTLY_2 $1.
          @50 "|" @108 "| P        =" @120 AONY_P 10.3 /
        %DO I=3 %TO 5;
          @2 X_&I 8.3 @10 "(" @11 IDX_&I 10. @21 ")" @23 OUTLX_&I $1.
          @27 Y_&I 8.3 @35 "(" @36 IDY_&I 10. @46 ")" @48 OUTLY_&I $1.
          @50 "|"  @108 "|" /
        %END;
        %DO I=1 %TO 1;
          @4 "LOW"   @29 "LOW" @50 "|" @108 "|" /
        %END;
        %LET I=6;
        %LET II=%EVAL(&I);
        %LET OLDN = %EVAL(&NOBSERV + &_OUTS);
        %DO I=&N4 %TO &OLDN %BY 1;
          @2 X_&I 8.3 @10 "(" @11 IDX_&I 10. @21 ")" @23 OUTLX_&II $1.
          @27 Y_&I 8.3 @35 "(" @36 IDY_&I 10. @46 ")" @48 OUTLY_&II $1.
          @50 "|"  @108 "|" /
          %LET II=%EVAL(&II+1);
        %END;
          ;
      PUT #12 @109 22*"-" // @110 "SPEARMANS (Y ON X)"/
             @114 "R=" X 10.2 / @114 "P=" SPEAR_P 10.3;
      PN=8;
      DO I=1 TO 11;
         SET _WOW;
         IF I=11 THEN DO;
            PUT #PN @53 "---------" @65 "------" @73 "----------"
                @85 "----------" @97 "----------" ;
            PN=PN+1;
         END;
         IF I>1 THEN DO;
            PUT #PN @52 _GRPS $CHAR12. @65 N 6. @73 MN_X 10.2 @85 MN_Y
                10.2 @97 STD_Y 10.2 ;
            PN=PN+1;
         END;
      END;
   PUT #20 @1 130*"=";
   PUT @1 "| POLYNOMIAL REGRESSION ANALYSIS |"/
       @2 "--------------------------------" @117 "P-VALUE" /
       @43 "COEFFICIENTS" @87"ERROR" @112 "MODEL(I)  MODEL(I)"/
       @16 65*"-" @83 14*"-" @104 "2" @114 "VS        VS"/
       @2 "MODEL(I)" @24 "B0" @35 "B1" @46 "B2" @57 "B3" @68 "B4"
       @79 "B5" @84 "DF"  @92 "RMSE" @103 "R"
       @110 "MODEL(I-1)  MODEL(0)"/
       @2 130*"-";
      SET MEAN_PGA;
       PUT @2 "MEAN(0)" @16 MN_Y 10.3 @82 EDF_MN 4. @88 RMSE_MN 8.2;
   DO I=1 TO 5;
      SET _PGANAL(drop=_type_);
      IF _MODEL_="LINEAR" THEN _MODEL="LINEAR(1)    ";
         ELSE IF _MODEL_="QUADRATC" THEN _MODEL="QUADRATIC(2)";
         ELSE IF _MODEL_="CUBIC" THEN _MODEL="CUBIC(3)";
         ELSE IF _MODEL_="QUARTIC" THEN _MODEL="QUARTIC(4)";
         ELSE IF _MODEL_="QUINTIC" THEN _MODEL="QUINTIC(5)";
      PUT
       @2 _MODEL  @16 B0 10.3 @27 B1 10.4  @38 B2 10.4 @49 B3 10.5
       @60 B4 10.5 @71 B5 10.5 @82 _EDF_ 4. +2 _RMSE_  8.2 +2
       _RSQ_  8.3  @112 P_I_1 8.3  +2 P_0 8.3;
   END;
   PUT @1 130*"=";
   PUT @1  "| TEST OF MODELLING ASSUMPTIONS USING RESIDUALS "
       @49 "FROM THE FOLLOWING MODELS : |";
   PUT @2 75*"-" /
       @39 "MEAN        LINEAR     QUADRATIC         CUBIC       "
       @92 "QUARTIC       QUINTIC";
   SET _ZP;
   PUT @3 "SKEWNESS : COEFFICIENT" @35 SKW_R0 8.2 +6 SKW_R1 8.2
       +6 SKW_R2 8.2 +6 SKW_R3 8.2 +6 SKW_R4 8.2  +6 SKW_R5 8.2 /
       @18 "Z" @35 Z_SKW_R0 8.2 +6 Z_SKW_R1 8.2 +6 Z_SKW_R2 8.2
       +6 Z_SKW_R3 8.2 +6 Z_SKW_R4 8.2 +6 Z_SKW_R5 8.2 //
       @3 "KURTOSIS : COEFFICIENT" @35 KRT_R0 8.2 +6 KRT_R1 8.2
       +6 KRT_R2 8.2 +6 KRT_R3 8.2 +6 KRT_R4 8.2  +6 KRT_R5 8.2 /
       @18 "Z" @35 Z_KRT_R0 8.2 +6 Z_KRT_R1 8.2 +6 Z_KRT_R2 8.2
       +6 Z_KRT_R3 8.2 +6 Z_KRT_R4 8.2 +6 Z_KRT_R5 8.2 /;
   SET _NORML;
   PUT @3 "NORMALITY :    P      " @32 PNORM_0 $CHAR11. +3
       PNORM_1 $CHAR11.  +3 PNORM_2 $CHAR11.  +3 PNORM_3 $CHAR11.
       +3 PNORM_4 $CHAR11.  +3 PNORM_5 $CHAR11. //
       @3 "COMMON VARIANCE :" ;
     SET _P_SPEAR;
       PUT @4 "P VALUE FROM SPEARMANS R" @35 P_R0 8.3 +6 P_R1 8.3
           +6 P_R2 8.3 +6 P_R3 8.3 +6 P_R4 8.3 +6 P_R5 8.3/
           @6 "USING ABSOLUTE RESIDUALS" /;
     SET __LEV_P;
       PUT @4 "P VALUE USING MODIFIED" @35 LEVP0 8.3 +6 LEVP1 8.3
           +6 LEVP2 8.3 +6 LEVP3 8.3 +6 LEVP4 8.3 +6 LEVP5 8.3 /
           @6 "LEVENE TEST";
    STOP;

  DATA _ME; SET _MEANS;
    IF GROUP='     ' | GROUP='  TOTAL' THEN DELETE;

  DATA _PLOT; MERGE _DSNSAV _ME;
  PROC PLOT DATA=_PLOT; PLOT Y*X MN_Y*MN_X="*"/OVERLAY;
    LABEL Y=%UNQUOTE(%QUOTE(%'&YVAR%'))
          X=%UNQUOTE(%QUOTE(%'&XVAR%'));

  %END;
  %ELSE %DO;
    PUT 'INSUFFICIENT NON-MISSING OBSERVATIONS (N<6)';
  %END;
  %ENDUP:
  OPTIONS NODQUOTE;
%MEND REGRESS;
