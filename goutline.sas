*===GOUTLINE MEMBER FROM $1700.PUBLIC.SASMACRO BEGINS===*;
*---EDITED 2-28-90 TO REPLACE ALL SPECIAL EBCDIC CHARACTERS,
    ELIMINATE THE /STMT OPTION, AND FIX THE X AND Y VARIABLE
    RENAMING CODE SO THAT WE DO NOT RENAME X TO X AND Y TO Y--;
 /********************************************************************/
 /* NAME: GOUTLINE     SYSTEM: SCADS   PGMR: JOHNSON/BLODGETT        */
 /* FUNCTION: CONVERT GMAP POLYGON DATASETS TO ANNOTATE MOVE/DRAW    */
 /*        COMMANDS TO GENERATE OUTLINES.                            */
 /* STATUS: TEST UNDER PC-DOS, PRODUCTION UNDER MVS.                 */
 /* RELATED MODULES: GENLABS, ANNOGEN.                               */
 /* DOCUMENTATION:                                                   */
 /* NOTES: HAS LIMITED CLIP OPTION TO STOP OUTLINES FROM LEAVING A   */
 /*    RECTANGULAR LIMIT.                                            */
 /*    THIS CLIPPING FEATURE (XOPT PARM) IS NOT YET IMPLEMENTED<===  */
 /********************************************************************/
 
 %GLOBAL REVDATE;  %LET REVDATE=28FEB90; *<----DATE OF LAST REVISION;
 
 /********************************************************************/
 /* BEGIN MACRO DEFINITION                                           */
 /********************************************************************/
%MACRO GOUTLINE(POLYDS  =_LAST_, /* INPUT POLYGON DATA SET           */
               ANNODS  =ANNODS, /* OUTPUT ANNOTATE DATA SET          */
               POLIDS  =,       /* POLYGON ID VARIABLES FROM HI-LOW  */
               ID      =,       /* ID VARIABLES TO BE KEPT ON OUTPUT */
               X       =X,      /* NAME OF HORIZONTAL COORD. VARIABLE*/
               Y       =Y,      /* NAME OF VERTICAL COORD VAR.       */
               SEGMENT =,       /* NAME OF VARIABLE DESIGNATING MULTI-
                                   PART POLYGONS IF PRESENT IN DATA  */
               SORT    =,       /* LIST OF SORT VARIABLES, IF D.S.
                                   REQUIRES PRE-SORTING              */
               SLCTEDIT=,       /* USUAL USER-SUPPLIED MACRO NAME    */
               XSYS    =2,      /* X-VARIABLE REFERENCE SYSTEM       */
               YSYS    =2,      /* Y-VARIABLE REFERENCE SYSTEM       */
               COLOR   =BLACK,  /* COLOR OF LABEL                    */
               SIZE    =1,      /* LINE SIZE                         */
               LINE    =1,      /* LINE TYPE                         */
               WHEN    =A,      /* ANNOTATE WHEN OPTION              */
               XMIN=-9999999,XMAX=9999999,YMIN=-9999999,YMAX=9999999,
                 /* X,Y EXTREMA DEFINING A PLOT WINDOW. POINTS OUTSIDE
                   THIS WINDOW WILL BE HANDLED AS PER XOPT VALUE */
               XOPT=,  /* HOW TO HANDLE X,Y VALUES OUTSIDE CLIPPING
                    WINDOW DEFINED BY XMIN,..,YMAX.  VALUES ARE:
                  (NULL=DEFAULT): DO NOTHING-DO NOT EVEN CHECK FOR IT.
                  OMIT,SKIP: DELETE POINTS OUTSIDE THE WINDOWN FROM THE
                  OUTPUT "POLYGON". (POSSIBLY OMIT ENTIRE POLYGON).
                  ABORT: STOP THE JOB IF THIS OCCURS, I.E. TREAT IT AS
                   A SERIOUS ERROR CONDITION.                       */
               DEBUG=0);        /* DEBUG=1 TO PRODUCE DIAGNOSTICS    */
  %PUT %STR( );
  %PUT **************************************************************;
  %PUT *       GOUTLINE MACRO REV &REVDATE BEGIN EXECUTION            *;
  %PUT ***************************************************************;
  %PUT %STR( );
 
  %IF &DEBUG %THEN %DO;
    %PUT %STR( );
    %PUT; %PUT *****PARAMETERS SPECIFIED*****;
    %PUT ANNODS= &ANNODS NAME OF OUTPUT ANNOTATE SAS DATASET;
    %PUT POLIDS= &POLIDS VARIABLES THAT ID POLYGONS (REQUIRED);
    %PUT ID= &ID ID VARIABLES (USUALLY NONE) TO BE KEPT ON OUTPUT.;
    %PUT X= &X   Y= &Y NAMES OF X-Y COORDINATE VARIABLES.;
    %PUT SEGMENT= &SEGMENT    NAME OF SEGMENT ID VARIABLE (E.G. SEGMENT=
SEGMENT).;
    %PUT SORT= &SORT LIST OF VARIABLES BY WHICH INPUT DATASET NEEDS TO B
E SORTED (POLIDS, SEGMENT);
    %PUT XSYS= &XSYS YSYS= &YSYS REFERENCE SYSTEMS FOR X-Y VALUES.;
    %PUT SIZE= &SIZE SIZE (THICKNESS) OF LINES;
    %PUT LINE= &LINE ANNOTATE LINE TYPE.;
%PUT WHEN=&WHEN ANNOTATE WHEN OPTION (DRAW OUTLINE BEFORE OR AFTER MAP);
    %PUT %STR( );
    %END;
  *OPTIONS DQUOTE;
  %IF &SORT NE %THEN %DO;
     PROC SORT DATA=&POLYDS OUT=TEMP;
     BY &SORT;RUN;
     %LET POLYDS = TEMP;
     %END;
  %LET LOOP=1;
  %IF &SEGMENT NE %STR() %THEN %LET BRKVAR=&SEGMENT;
  %ELSE %DO %WHILE(%SCAN(&POLIDS,&LOOP)>%STR());
     %LET BRKVAR=%SCAN(&POLIDS,&LOOP);
     %LET LOOP=%EVAL(&LOOP+1);
     %END;
  DATA &ANNODS (KEEP=X Y XSYS YSYS HSYS SIZE LINE COLOR
                     WHEN FUNCTION &ID);
    %*--FIX PROBLEM WITH RENAMING X TO X AND/OR Y TO Y FOR VERS 6-;
    %IF &X=X AND &Y=Y %THEN %LET RENAME= ;  %ELSE %DO;
      %IF &X NE X %THEN %LET RENAME=%STR(&X=X );
      %IF &Y NE Y %THEN %LET RENAME=&RENAME %STR(&Y=Y);
      %END;
    SET &POLYDS  %IF %BQUOTE(&RENAME) NE %THEN %DO;
                   RENAME=(&RENAME))
                   %END;
    ; BY &POLIDS &SEGMENT NOTSORTED;
    %IF &SLCTEDIT NE %THEN %&SLCTEDIT;
    *--- CONSTANTS SUPPLIED BY DEFAULT AND/OR MACRO USER;
    LENGTH FUNCTION COLOR $ 8 LINE SIZE 8 WHEN
           XSYS YSYS HSYS $1;
    RETAIN XSYS "&XSYS" YSYS "&YSYS" COLOR "&COLOR" WHEN "&WHEN"
           HSYS ' '
           FUNCTION "LABEL" SIZE &SIZE LINE &LINE;
 
    %IF &XOPT NE %THEN %DO;
     RETAIN XMIN &XMIN XMAX &XMAX YMIN &YMIN YMAX &YMAX;
     %LET XOPT=%UPCASE(&XOPT);
     IF X LT XMIN THEN DO;  X=XMIN;  XOUT=1;  END;
      ELSE IF X GT XMAX THEN DO;  X=XMAX;  XOUT=1;  END;
     IF Y LT YMIN THEN DO;  Y=YMIN;  YOUT=1;  END;
      ELSE IF Y GT YMAX THEN DO;  Y=YMAX;  YOUT=1;  END;
     %IF &XOPT=ABORT %THEN %DO;
       IF XOUT | YOUT THEN DO;
         PUT /'***X AND/OR Y COORDINATE OUT OF WINDOW...RUN ABORTING***'
          /_ALL_;
         ABORT 20;
         END;
       %END;
     %END;
 
    *--- RETAIN FIRST X & Y TO COMPLETE POLYGON IF NECESSARY;
    RETAIN FIRSTX FIRSTY LASTX LASTY;
 IF FIRST.&BRKVAR OR (LASTX=. & LASTY=.) THEN DO;
    NPTSOUT=0;
    FIRSTX=X; FIRSTY=Y;
    FUNCTION='MOVE';
    END;
 ELSE FUNCTION='DRAW';
 IF X NE . & Y NE . THEN DO;
    LASTX=X; LASTY=Y;
    LINK OUTPUT;
    *---CLOSE LAST ELEMENT OF POLYGON, SEGMENT OR INTERNAL SEGMENT---*;
    IF LAST.&BRKVAR THEN DO;
       IF X NE FIRSTX OR Y NE FIRSTY THEN DO;
          X=FIRSTX; Y=FIRSTY;
          LINK OUTPUT;
          END;
       END;
    END;
 *---BE CERTAIN MAIN POLYGON CLOSES BEFORE PROCESSING INTERNALS---*;
 ELSE IF LASTX NE FIRSTX OR LASTY NE FIRSTY THEN DO;
    LASTX=X; LASTY=Y;
    X=FIRSTX; Y=FIRSTY;
    LINK OUTPUT;
    END;
 ELSE DO; *---SAVE MISSING VALUES AS LASTX & LASTY---*;
    LASTX=X; LASTY=Y;
    END;
RETURN;
OUTPUT:
 NPTSOUT+1;
 OUTPUT;  RETURN;
 RUN;
%MEND GOUTLINE;
