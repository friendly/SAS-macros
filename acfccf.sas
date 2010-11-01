*
          ACFCCF - THE AUTO/CROSS CORRELATION ANALYSIS MACRO
 
          For more information:
 
          Dr. Dan Jacobs                         BITNET: DAN@UMDC
          Maryland Sea Grant College Program
          University of Maryland
          H. J. Patterson Hall, Room 1224
          College Park, Maryland  20742
          (301) 454-5690
          ;
 
* developed using sas 5.xx on vm/cms.
* procs: iml, plot.
* other: sas macro language.
*           bwg
*
;
%MACRO ACFCCF(TIMEVAR,VARLIST,LAGS,DSN,ACFCCF,T);
  %PUT NOTE: AUTO/CROSS-CORRELATION ANALYSIS MACRO.;
 
  %IF &T =  %THEN %LET T = 1.96;        * STUDENT T VALUE FOR ACF/CCF CI;
  %IF &ACFCCF =  %THEN %LET ACFCCF = CCF;         * AUTO AND CROSS CORR.;
  %IF &ACFCCF NE CCF %THEN %LET ACFCCF = ACF;           * ONLY AUTOCORR.;
  %IF &DSN =   %THEN %LET DSN = %SUBSTR(&SYSDSN,9);   * LAST CREATED DSN;
  %IF &LAGS =  %THEN %LET LAGS = 1;
  %ELSE %LET LAGS = %EVAL(&LAGS + 1);    *  LAGS = 1 - JUST SIMPLE CORR.;
  %IF &VARLIST =  %THEN %LET VARLIST = _NUMERIC_;     * ALL NUMERIC VAR.;
  %IF &TIMEVAR =   %THEN %DO;             * ONLY REQUIRED MACRO VARIABLE;
    %LET SS = %STR(     );
    %PUT NOTE: NO AUTO/CROSS-CORRELATION ANALYSIS DONE.;
    %PUT &SS THE CALL IS: ;
    %PUT &SS ;
    %PUT &SS %NRSTR(%ACFCCF(TIMEVAR,VARLIST,LAGS,DSN,ACFCCF,T););
    %PUT &SS ;
    %PUT &SS WHERE:;
    %LET SS = %STR(       );
    %PUT &SS TIMEVAR  - TIME/SPACE VARIABLE IN DATA SET (REQUIRED);
    %PUT &SS VARLIST  - VARIABLE LIST (DEFAULT = ALL NUMERIC VARIABLES);
    %PUT &SS LAGS     - NUMBER OF LAG PERIODS (DEFAULT = 0);
    %PUT &SS DSN      - DATA SET TO USE (DEFAULT = LAST CREATED);
    %PUT &SS ACFCCF   = CCF - AUTO AND CROSS CORRELATIONS CALCULATED (DEFAULT);
    %PUT &SS          = ACF - ONLY AUTOCORRELATIONS CALCULATED;
    %PUT &SS T        - STUDENT T FOR CONFIDENCE INTERVAL(S) (DEFAULT = 1.96);
    %PUT &SS ;
    %PUT &SS LEAVE A FIELD BLANK TO USE A DEFAULT VALUE;
    %PUT &SS ;
  %END;
  %ELSE %IF &DSN =   OR &DSN = _NULL_ OR &DSN = CPT %THEN
    %PUT NOTE: NO DATA SET AVAILABLE;
  %ELSE %DO;
    OPTIONS NONOTES DQUOTE;
    DATA _NULL_; SET &DSN (OBS=1);
      %IF %INDEX(&VARLIST,-) = 1 OR &VARLIST = _NUMERIC_ %THEN %DO;
        * FIND THE NUMBER OF VARIABLES IN THE LIST AND
          CONVERT SHORTHAND VARIABLE LIST TO LONG FORM;
        LENGTH NNN $ 8 VLIST $ 200;
        ARRAY X(CPTNUM) &VARLIST;
        NNN = ' ';
        DO OVER X;
          CALL VNAME(X,NNN);
          IF NNN NE "&TIMEVAR" AND NNN NE 'CPTNUM' THEN DO;
            NUMVAR + 1;
            IF NUMVAR = 1 THEN STARTPT = 1;
             ELSE STARTPT = LENGTH(VLIST) + 2;
            ENDPT = LENGTH(NNN);
            SUBSTR(VLIST,STARTPT,ENDPT) = NNN;
          END;
        END;
        CALL SYMPUT('VARLIST',VLIST);
      %END;
      %ELSE %DO;
        * FIND THE NUMBER OF VARIABLES IN THE LIST;
        NUMVAR = N(OF &VARLIST);
      %END;
      CALL SYMPUT('NUMVAR',TRIM(LEFT(NUMVAR)));
    RUN;
 
    %IF &NUMVAR = 1 %THEN %LET ACFCCF = ACF;
    * FIND MEANS FOR PLOT;
    PROC MEANS NOPRINT MEAN DATA=&DSN;
      VAR &VARLIST;
      OUTPUT OUT=CPT MEAN=&VARLIST N=ACFNOBS;
    RUN;
 
    * SAVE MEANS IN MACRO VARIABLES;
    DATA CPT; SET CPT;
      * OUTPUT THE VARIABLE MEAN(S) AS MACRO VARIABLE(S);
      LENGTH NNN $ 8 ;
      ARRAY X(I) &VARLIST;
      NNN = ' ';
      DO OVER X;
        CALL VNAME(X,NNN);
        CALL SYMPUT(NNN,TRIM(LEFT(X)));
      END;
      * OUTPUT THE ACF AND CCF ESTIMATED STD AND CONFIDENC INTERVAL;
      ACFNOBS = 1/(SQRT(ACFNOBS));
      CALL SYMPUT('STD',TRIM(LEFT(ACFNOBS)));
      ACFNOBS = &T * ACFNOBS;
      CALL SYMPUT('CI',TRIM(LEFT(ACFNOBS)));
    RUN;
 
    * CONNECT THE 'DOTS';
    DATA CPT; SET &DSN; LENGTH CPT $ 1;
      ARRAY X(J) &VARLIST;
      ARRAY LAGX(J) X1-X&NUMVAR;
      ARRAY B(J) B1-B&NUMVAR;
      LAGTV = LAG(&TIMEVAR);
      DO OVER X;
        LAGX = LAG(X);
      END;
      CPT = 'A'; OUTPUT;
      IF _N_ > 1 THEN DO;
        C = (&TIMEVAR - LAGTV)/11;
        DO OVER X;
          B = (X - LAGX)/(&TIMEVAR - LAGTV);
        END;
        CPT = '*';
        DO I = 1 TO 10 BY 1;
          DO OVER X;
            X = LAGX + B*C*I;
          END;
          &TIMEVAR = LAGTV + C*I;
          OUTPUT;
        END;
      END;
      KEEP &TIMEVAR &VARLIST CPT;
    RUN;
 
    %DO I = 1 %TO &NUMVAR;
      %LET PLOTVAR = %SCAN(&VARLIST,&I,' ');
      PROC PLOT NOLEGEND;
        PLOT &PLOTVAR * &TIMEVAR = CPT / VREF = &&&PLOTVAR;
        FOOTNOTE "&PLOTVAR MEAN = &&&PLOTVAR";
      RUN;
      FOOTNOTE ;
    %END;
 
    * find the auto/cross-correlation matrix;
    DATA CPT;
       SET &DSN;
       KEEP &VARLIST;
       RUN;
*include acfiml;
proc iml;
reset autoname ;
 
start main;
  use cpt ;
  read all into x [colname=varname];
  COV = COVLAG(X,&LAGS);               * AUTO/CROSS COVARIANCE MATRIX;
  COLNAME = 'LAG' || VARNAME;
  %IF &NUMVAR = 1 %THEN %DO;           * JUST ACF, NO CCF;
     ACFCCF = (COV/ COV[1,1])`;
     LAG = NCOL(COV) - 1;
     LAG = (0: LAG[1,1]);
     LAG = LAG`;
     ACFCCF = LAG || ACFCCF;
     _TMP_ROW = 'ROW1    ' : compress('ROW'+char(nrow(ACFCCF)));
     CLOSE CPT;
     CREATE CPT ( RENAME=(_TMP_ROW=ROW  )) FROM ACFCCF [ROWNAME=_TMP_ROW
        COLNAME=COLNAME ];
     APPEND FROM ACFCCF [ROWNAME=_TMP_ROW];
     %END;
  %ELSE %DO;
        DENOM = VECDIAG( COV[,1:NROW(COV)]);
        DENOM = SQRT(DENOM * DENOM`);        * TO CONVERT COV TO CORR;
        JJ = 1;
        %IF &ACFCCF = CCF %THEN %DO;
          DO I = 1 TO &LAGS;
             K = I - 1;
             J = &NUMVAR * I;
             ACFCCF = COV[,JJ:J]/ DENOM;
             IF ( I= 1) THEN  ROWNAME = VARNAME;
             ELSE  ROWNAME = ROWNAME || VARNAME;
             JJ = J + 1;
             J = J(&NUMVAR,1,K);
             ACFCCF = J || ACFCCF;
             IF ( I= 1) THEN  ACF = ACFCCF;
             ELSE  ACF = ACF // ACFCCF;
             END;
           EDIT CPT ;APPEND FROM ACF [ROWNAME=ROWNAME];
        %END;
        %ELSE %DO;
          JJ = 1;
          DO I = 1 TO &LAGS;
            J = &NUMVAR * I;
            ACFCCF = COV[,JJ:J]/ DENOM;
            ACFCCF = VECDIAG(ACFCCF)`;
            JJ = J + 1;
            J = I - 1;
            ACFCCF = J || ACFCCF;
            IF ( I= 1) THEN  ACF = ACFCCF;
             ELSE  ACF = ACF // ACFCCF;
            END;
         EDIT CPT ;APPEND FROM ACF [ROWNAME=_TMP_ROW];
 
        %END;
   %END;
FINISH MAIN;
RUN MAIN;
quit;
 
    %IF &ACFCCF = ACF %THEN %DO;
      PROC PRINT DATA=CPT;
        ID LAG; VAR &VARLIST;
        TITLE3 'AUTOCORRELATIONS';
      RUN;
    %END;
    %ELSE %DO;
      PROC PRINT DATA=CPT LABEL; BY LAG NOTSORTED;
        ID ROW; VAR &VARLIST;
        LABEL ROW = 'x/y';
        TITLE3 'AUTO- AND CROSS-CORRELATIONS';
      RUN;
    %END;
 
    * PLOT THE ACF AND/OR CCF;
    %IF &ACFCCF = CCF %THEN %STR(DATA CPT CCF;);
    %ELSE %STR(DATA CPT;);
      SET CPT; LENGTH NNN $ 8 VARNAME $ 22;
      ARRAY X(I) &VARLIST;
      DO OVER X;
        NNN = ' ';
        CALL VNAME(X,NNN);
        DO J = 0 TO 10 BY 1;
          ACFCCF = X - (X/10)*J;
          %IF &ACFCCF = CCF %THEN %DO;
            IF ROW = NNN THEN DO;
              SUBSTR(VARNAME,2) = TRIM(LEFT(ROW)); OUTPUT CPT;
            END;
            ELSE DO;
              SUBSTR(VARNAME,2) = TRIM(LEFT(ROW))||' with '||TRIM(LEFT(NNN));
              OUTPUT CCF;
            END;
          %END;
          %ELSE %DO;
            SUBSTR(VARNAME,2) = TRIM(LEFT(NNN)); OUTPUT CPT;
          %END;
        END;
      END;
      KEEP ACFCCF VARNAME LAG;
    RUN;
    PROC SORT DATA=CPT; BY VARNAME; RUN;
    PROC PLOT NOLEGEND DATA=CPT; BY VARNAME;
      PLOT ACFCCF*LAG='*'/ VREF=0 &CI -&CI
                               VAXIS = -1 TO 1 BY .1;
      FOOTNOTE "AUTOCORRELATION STANDARD DEVIATION = &STD";
      LABEL VARNAME = 'AUTOCORRELATION VARIABLE'
            ACFCCF = 'AUTOCORRELATION';
    RUN;
    %IF &ACFCCF = CCF %THEN %DO;
      PROC SORT DATA=CCF; BY VARNAME; RUN;
      PROC PLOT NOLEGEND DATA=CCF; BY VARNAME;
        PLOT ACFCCF*LAG='*'/ VREF=0 &CI -&CI
                                 VAXIS = -1 TO 1 BY .1;
        FOOTNOTE "CROSSCORRELATION STANDARD DEVIATION = &STD";
        LABEL VARNAME = 'CROSSCORRELATION VARIABLES'
              ACFCCF = 'CROSSCORRELATION';
      RUN;
    %END;
  %END;
  OPTIONS NOTES;
  FOOTNOTE ; RUN;
%MEND ACFCCF;
