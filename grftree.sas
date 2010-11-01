%MACRO GRFTREE(CLUSDSN=,ITEMS=,AXIS=,GROUPAT=,TRIML=,TRIMR=,ITEMLIST=N,
               SPLIT=N,FONT=duplex, LABEL=,CTREE=BLACK,CAXIS=BLACK,
               GOUT=,GNAME=,GDES=);
  /*
    GRFTREE - DRAWS DENDROGRAMS USING PROC CLUSTER OUTPUT DATA SET
 
    Written:  October 5, 1989     Modified:  October 16, 1989
                                               March  9, 1990
                                               April 29, 1990
                                            November 27, 1990
                                             October  6, 1994
                                            November 16, 1994
    Originally developed using SAS 5.18 on VM/CMS
    Procs:  PROC GPLOT, PROC MEANS, PROC PRINT, PROC SORT, PROC TREE
    Other:  SAS MACRO language
    Macros: GRFTREE, ORDER (both included in this file)
    Note:   Do not use TREEDATA, TREESTUF, ANNOSTUF or ANNOSPLT
            as a data set name.
            Titles should be specified prior to the macro call.
            No footnotes are allowed.
 
    Converts output data set from PROC CLUSTER into an easy to
    inteprete dendrogram.  The X axis scale is in distance or
    similarity (depending on data used in PROC CLUSTER), and the
    Y axis scale is the item names that were clustered.
 
    MACRO VARIABLE      DESCRIPTION
    In Request:
    CLUSDSN             Name of SAS data set created by PROC CLUSTER
    ITEMS               Number of objects that you clustered
    AXIS                How to scale the X axis
                        S - similarity
                        D - distance
    DEBUG               YES for some debugging prints (default is NO)
    GROUPAT             Value to join items into a cluster and
                        a filled triangle is used (optional)
    TRIML               Value to truncate the left side of the X axis.
                        Used with TRIMR to 'zoom in' on a part of
                        the tree. Both TRIML and TRIMR are optional
    TRIMR               Value to truncate the right side of the X axis
    FONT                Font to print all text (default is SIMPLEX)
    LABEL               Text to label the X axis (default is SIMILARITY
                        if AXIS = S, DISTANCE if AXIS = D)
    CTREE               Color of the tree (default is BLACK)
    CAXIS               Color of the axes (default is BLACK)
    GOUT                Name of a graphics catalog (optional)
    GNAME               Name of the tree is stored in GOUT (optional)
    GDES                Description of the tree stored in GOUT (optional)
    ITEMLIST            Print the tree item order (default is N)  (Y/N)
    SPLIT               Spread tree over several pages (default N) (Y/N)
 
    Internal:
    GROUP               group some of the items? (YES or NO)
    MAXL                length of longest item label
    MINVAL              minimum (rightmost) X axis value used to
                        scale similarity axis in 1/10ths
    WINDOW              zooming done? (YES or NO)
    S                   FROM GRFTREE: (leader for notes from the macro)
    ST                  items - 1
    SCALE               scale of the X axis (_HEIGHT_)
 
    For more information:
 
    Dr. Dan Jacobs                       Internet: DAN@UMDD.UMD.EDU
    Maryland Sea Grant College Program
    University of Maryland
    0102 Skinner Hall
    College Park, Maryland  20742
    (301) 405-6379
 
    Please acknowledge Dan Jacobs as the provider of the macro code
    in any publication that uses the results of this macro.  Thank you.
  */
  %LET S = %STR(GRFTREE:  );
  OPTIONS NONOTES;
  %PUT %STR(   );
  %PUT &S Please remember to acknowledge Dan Jacobs as the provider;
  %PUT &S of the dendrogram macro software in any publication that;
  %PUT &S uses the results of this macro.  Thank you.;
  %PUT %STR(  );
  %PUT &S Starting to figure out how to draw the tree . . . ;
  %* CHECK MACRO CALL ENTRIES;
  %IF &CLUSDSN =   %THEN %GOTO BADDSN;
  %IF &TRIML =  AND &TRIMR =  %THEN %LET WINDOW = NO;
  %ELSE %LET WINDOW = YES;
  %IF &GROUPAT =   %THEN %LET GROUP = NO;
  %ELSE %LET GROUP = YES;
  %LET AXIS = %UPCASE(%SUBSTR(&AXIS,1,1));
  %IF &AXIS = S OR &AXIS = D %THEN %LET SCALE = _HEIGHT_;
  %ELSE %DO;
    %PUT &S  AXIS can only be S or D and not &AXIS;
    %PUT &S  No dendrogram will be done.;
    %GOTO ENDALL;
  %END;
  %LET ST = %EVAL(&ITEMS - 1);
  %IF &LABEL =  %THEN %DO; %* DEFAULT X AXIS LABEL;
    %IF &AXIS = S %THEN %LET LABEL = SIMILARITY;
    %ELSE %LET LABEL = DISTANCE;
  %END;
  %IF %UPCASE(%SUBSTR(&ITEMLIST,1,1)) NE Y %THEN %LET ITEMLIST = N;
  %IF %UPCASE(%SUBSTR(&SPLIT,1,1)) NE Y %THEN %LET SPLIT = N;
  %* CHECK IF DATA SET EXISTS AND CALC. MAX. ITEM LABEL LENGTH;
  %LET MAXL = 0;
  DATA TREEDATA; SET &CLUSDSN END=EOF;
    RETAIN MAXL 0;
    MAXL = MAX(MAXL,LENGTH(_NAME_));
    %IF &WINDOW = YES %THEN %DO;
      %* ZOOM IN TO PART OF RANGE TO PLOT, IF REQUESTED;
      %IF &AXIS = D %THEN %DO;
        %IF &TRIML NE  %THEN %DO;
          IF &SCALE < &TRIML THEN &SCALE = &TRIML; %END;
        %IF &TRIMR NE  %THEN %DO;
          IF &SCALE > &TRIMR THEN &SCALE = &TRIMR; %END;
      %END;
      %ELSE %DO;
        %IF &TRIML NE  %THEN %DO;
          IF &SCALE > &TRIML THEN &SCALE = &TRIML; %END;
        %IF &TRIMR NE  %THEN %DO;
          IF &SCALE < &TRIMR THEN &SCALE = &TRIMR; %END;
      %END;
    %END;
    IF EOF THEN CALL SYMPUT('MAXL',TRIM(PUT(MAXL,2.)));
  RUN;
  %IF &MAXL = 0 %THEN %GOTO BADDSN;
  %IF &GROUP = YES %THEN %DO;
    %IF &AXIS = S %THEN %DO;
      PROC TREE H=HEIGHT SIM LEVEL=&GROUPAT NOPRINT
        DATA=TREEDATA OUT=TREESTUF;
    %END;
    %ELSE %DO; %* DISTANCE;
      PROC TREE H=HEIGHT DIS LEVEL=&GROUPAT NOPRINT
        DATA=TREEDATA OUT=TREESTUF;
    %END;
    RUN; %* CLOSE OFF PROC TREE;
    DATA TREESTUF; SET TREESTUF;
      IF _NAME_ NE CLUSNAME;
    RUN;
    PROC SORT DATA=TREEDATA; BY _NAME_; RUN;
    PROC SORT DATA=TREESTUF; BY _NAME_; RUN;
    DATA TREEDATA(KEEP=_NAME_ FROMHT FROMFREQ FROM CLUSTER)
         TREESTUF(KEEP=_NAME_ TOHT TOFREQ TO);
      MERGE TREEDATA TREESTUF(IN=INB); BY _NAME_;
     IF INB THEN _PARENT_ = CLUSNAME;
  %END;
  %ELSE %DO; %* NO GROUPING AT SOME VALUE DONE;
    DATA TREEDATA(KEEP=_NAME_ FROMHT FROMFREQ FROM CLUSTER)
         TREESTUF(KEEP=_NAME_ TOHT TOFREQ TO);
      SET TREEDATA;
      CLUSTER = .;
  %END;
    %* FINISH OFF SPLITTING THE DATA SET INTO PARENT AND CHILD SETS;
    TO = _NAME_; TOHT = &SCALE; TOFREQ = _FREQ_; OUTPUT TREESTUF;
    FROM = _NAME_; _NAME_ = _PARENT_;
    FROMHT = &SCALE; FROMFREQ = _FREQ_; OUTPUT TREEDATA;
  RUN;
  %PUT &S   . . . determining item order based on the clustering;
  PROC SORT DATA=TREEDATA; BY _NAME_; RUN;
  PROC SORT DATA=TREESTUF; BY _NAME_; RUN;
  DATA TREEDATA;
    MERGE TREESTUF TREEDATA(IN=INA); BY _NAME_;
    IF INA ;
   %IF &GROUP = YES %THEN %DO;
     %IF &AXIS = D %THEN %DO;
       %IF &TRIML =  %THEN %LET TRIML = 0.00;
       IF &TRIML LT TOHT LT &GROUPAT THEN DO;
         IF &TRIML LT FROMHT LT &GROUPAT THEN DELETE;
         IF CLUSTER = . AND FROMHT = &TRIML THEN DELETE;
       END;
     %END;
     %ELSE %DO;
       %IF &TRIML =  %THEN %LET TRIML = 1.00;
       IF &GROUPAT LT TOHT LT &TRIML THEN DO;
         IF &GROUPAT LT FROMHT LT &TRIML THEN DELETE;
         IF CLUSTER = . AND FROMHT = &TRIML THEN DELETE;
       END;
     %END;
   %END;
   CLUSNUM = SUBSTR(_NAME_,3) + 0.0;
   DROP _NAME_;
  RUN;
  %IF &AXIS = S %THEN %DO;
    PROC MEANS NOPRINT MIN DATA=TREEDATA; VAR FROMHT;
      OUTPUT OUT=TREESTUF MIN=FROMHT; RUN;
    DATA TREESTUF; SET TREESTUF;
      %* ROUND DOWN THE LOWEST (RIGHT MOST) VALUE TO SCALE IN TENTHS;
      FROMHT = FLOOR(FROMHT * 10)/10;
      CALL SYMPUT('MINVAL',TRIM(LEFT(FROMHT)));
    RUN;
  %END;
  %ORDER; %* GET ORDER THAT THE ITEMS ARE TO GO ON THE Y AXIS;
  %IF &ITEMLIST = Y %THEN %DO;
    %PUT &S   . . . printing the item order;
    PROC SORT DATA=TREESTUF; BY Y; RUN;
    PROC PRINT DATA=TREESTUF SPLIT=' ';
      VAR FROM Y;
      LABEL FROM = 'ITEM' Y = 'Y COORD.';
      TITLE3 'ITEM ORDER FOR THE DENDROGRAM';
    RUN;
  %END;
  TITLE3 ;
  %PUT &S   . . . determining how to draw the dendrogram (tree);
  PROC SORT DATA=TREESTUF; BY DESCENDING CLUSNUM FROM; RUN;
  PROC SORT DATA=TREEDATA; BY DESCENDING CLUSNUM FROM; RUN;
  DATA TREEDATA(KEEP=X Y FROM TO)
       TREESTUF(KEEP=XSYS YSYS HSYS SIZE POSITION TEXT STYLE
                     COLOR X Y FUNCTION LINE)
       ANNOSTUF(KEEP=XSYS YSYS HSYS SIZE POSITION TEXT STYLE
                     COLOR X Y FUNCTION LINE);
    MERGE TREEDATA TREESTUF; BY DESCENDING CLUSNUM FROM;
    LENGTH FUNCTION STYLE COLOR $ 8
           TEXT $ &MAXL
           XSYS YSYS HSYS WHEN POSITION $ 1;
    ARRAY CLUS{*} CL1-CL&ST.;
    RETAIN CL1-CL&ST. LASTY SAMEHT;
    XSYS = '2'; YSYS = '2'; HSYS = '4'; SIZE = 1;
    IF TOHT = . THEN DO;
      %IF &AXIS = D AND &TRIML = 0.00 %THEN %DO;
         %* MAKE SURE DISTANCE AXIS INCLUDES 0 UNLESS TRIMMED (04/24/90);
         X = 0; Y = 1; OUTPUT TREEDATA;
         X = .; Y = .; OUTPUT TREEDATA;
      %END;
      RETURN; %* THIS OBS NOT NEEDED;
    END;
    IF FROMFREQ = 1 THEN DO; %* EACH ITEM IS ITS OWN CLUSTER;
      I1 = SUBSTR(TO,3) + 0.0;
      %* PUT THE ITEM NAMES IN AN ANNOTATE DATA SET TO LABEL Y AXIS;
      %* X COORDINATE (Y COORD ALREADY IN TREESTUF);
      %* 5/10/93 - WINDOW COORD, NOT DATA;
      POSITION = '6'; XSYS = '5'; X = 0;
      FUNCTION = 'LABEL'; WHEN = 'B';
      COLOR = "&CTREE"; STYLE = "&FONT";
      %IF &ITEMS > 50 AND &SPLIT NE Y %THEN SIZE = 1.0 * 50/&ITEMS;
      %ELSE SIZE = 1.0;;
      SUBSTR(TEXT,1,&MAXL) = TRIM(LEFT(FROM));
      OUTPUT TREESTUF;
      X = FROMHT; XSYS = '2'; %* DATA COORD. SYSTEM, 5/10/93;
      %* DETERMINE HOW THE PLOTTER SHOULD DRAW THE LINES OF THE TREE;
      SIZE = 1;
      IF FIRST.CLUSNUM THEN DO;
        CLUS{I1} = Y;
        LASTY = Y;
        IF CLUSTER = . THEN DO;
          OUTPUT TREEDATA;
          X = TOHT; OUTPUT TREEDATA;
          X = .; Y = .; OUTPUT TREEDATA;
        END;
        ELSE DO;
          %* STORE IN SEPARATE DATA SET (NOT IN TREESTUF) SINCE
            POLY AND POLYCONT OBS. MUST BE IN SEQUENTIAL ORDER AND
            NOT SEPARATED BY AXIS LABEL OBS.;
          FUNCTION = 'POLY'; STYLE = 'S'; COLOR = "&CTREE"; LINE = 1;
          OUTPUT ANNOSTUF;
        END;
      END;
      ELSE IF LAST.CLUSNUM THEN DO;
        IF SAMEHT = 'Y' THEN DO;
          SAMEHT = 'N';
          CLUS{I1} = (CLUS{I1}*(TOFREQ - FROMFREQ) + Y) / TOFREQ;
        END;
        ELSE CLUS{I1} = (CLUS{I1} + Y) / 2;
        IF CLUSTER = . THEN DO;
          OUTPUT TREEDATA;
          X = TOHT; OUTPUT TREEDATA;
          Y = LASTY; OUTPUT TREEDATA;
          X = .; Y = .; OUTPUT TREEDATA;
        END;
        ELSE DO;
          FUNCTION = 'POLYCONT'; COLOR = "&CTREE"; OUTPUT ANNOSTUF;
          X = TOHT; Y = CLUS{I1}; OUTPUT ANNOSTUF;
        END;
      END;
    END;
    ELSE DO; %* DETERMINE HOW TO DRAW THE LINES OF THE TREE;
     I1 = SUBSTR(FROM,3) + 0.0;
     I2 = SUBSTR(TO,3) + 0.0;
     X = FROMHT;
     IF FIRST.CLUSNUM THEN DO;
       IF ROUND(TOHT,.00001) = ROUND(FROMHT,.00001) THEN SAMEHT = 'Y';
       ELSE SAMEHT = 'N';
       CLUS{I2} = CLUS{I1};
       LASTY = CLUS{I1};
       Y = CLUS{I1};
       OUTPUT TREEDATA;
       X = TOHT; OUTPUT TREEDATA;
       X = .; Y = .; OUTPUT TREEDATA;
     END;
     ELSE IF LAST.CLUSNUM THEN DO;
       IF ROUND(TOHT,.00001) = ROUND(FROMHT,.00001) THEN SAMEHT = 'Y';
       IF SAMEHT = 'Y' THEN DO;
         SAMEHT = 'N';
         CLUS{I2} = (CLUS{I1}*FROMFREQ + CLUS{I2}*(TOFREQ-FROMFREQ))
                    / TOFREQ;
       END;
       ELSE CLUS{I2} = (CLUS{I1} + CLUS{I2}) / 2;
       Y = CLUS{I1};
       OUTPUT TREEDATA;
       X = TOHT; OUTPUT TREEDATA;
       Y = LASTY; OUTPUT TREEDATA;
       X = .; Y = .; OUTPUT TREEDATA;
     END;
    END;
  RUN;
  %* PUT POLY AND POLYCONT WITH THE AXIS LABEL ANNOTATE DATA;
  DATA ANNOSTUF; SET ANNOSTUF TREESTUF; RUN;
  %PUT &S   . . . drawing the dendrogram;
  %IF &AXIS NE D AND &TRIML NE    %THEN %DO;
    DATA _NULL_; %* ROUND UP THE LEFT MOST VALUE TO SCALE IN TENTHS;
      CALL SYMPUT('TRIML',CEIL(&TRIML * 10) / 10);
    RUN;
  %END;
  %ELSE %LET TRIML = 1.00;
  GOPTIONS NOCELL NOCHARACTERS HPOS=91 VPOS=53;
  SYMBOL1 L=1 V=NONE C=&CTREE I=JOIN;
  %IF &GOUT NE  %THEN %DO; * OUTPUT GRAPH INFO;
    %LET GOUT = %STR(GOUT=&GOUT);
    %LET GNAME = %STR(NAME="&GNAME");
    %LET GDES = %STR(DES="&GDES");
  %END;
  %* ADDED 10/19/90, MODIFIED 11/16/94;
  DATA _NULL_;  %* CALCULATE THE AMOUNT TO OFFSET THE X AXIS ORIGIN;
    %IF &ITEMS > 50 AND &SPLIT NE Y %THEN ORIGIN = &MAXL * 50/&ITEMS;
    %ELSE ORIGIN = &MAXL.;;
    CALL SYMPUT('ORIGIN',TRIM(LEFT(ORIGIN)));
  RUN;
  %IF &SPLIT = Y %THEN %DO; %* ADDED 10/06/94;
    DATA TREESTUF;
      DO DIV = 40 TO 50 BY 1;
        TIMES =  INT(&ITEMS/DIV);
        NITEMS = INT(&ITEMS/TIMES);
        DONE = TIMES * NITEMS;
        LEFT = &ITEMS - DONE;
        IF TIMES > 1 THEN OUTPUT TREESTUF;
      END;
    RUN;
    PROC SORT DATA=TREESTUF OUT=TREESTUF; BY LEFT TIMES; RUN;
    DATA TREESTUF; SET TREESTUF(FIRSTOBS=1 OBS=1);
      CALL SYMPUT('TIMES',TRIM(LEFT(TIMES)));
      CALL SYMPUT('NITEMS',TRIM(LEFT(NITEMS)));
    RUN;
    %DO I = 1 %TO &TIMES;
      GOPTIONS GSFMODE=
        %IF &I = 1 %THEN REPLACE; %ELSE APPEND;;
      %LET FITEMS = %EVAL(((&I - 1) * &NITEMS) + 1);
      %IF &I = &TIMES %THEN %LET LITEMS = &ITEMS;
      %ELSE %LET LITEMS = %EVAL(&FITEMS + &NITEMS - 1);
      %PUT &S   . . . working on OTUs &FITEMS to &LITEMS (&I of &TIMES);
      DATA TREESTUF; SET TREEDATA;
        LAGY = LAG(Y); LAGX = LAG(X);
        IF X = . AND Y = . THEN OUTPUT;
        ELSE IF ((&FITEMS - 1) < Y < (&LITEMS + 1)) THEN DO;
          OUTPUT;
          IF LAGY <= (&FITEMS - 1) AND LAGY NE . THEN DO;
            IF LAGX = X THEN DO;
              Y = &FITEMS - 1;
              IF Y > 0 THEN DO;
                OUTPUT; Y = .; X = .; OUTPUT; END;
            END;
          END;
          ELSE IF LAGY > &LITEMS THEN DO;
            IF LAGX = X THEN DO;
              Y = &LITEMS + 1; OUTPUT;
              Y = .; X = .; OUTPUT;
            END;
          END;
        END;
        ELSE IF (&FITEMS LE LAGY LE &LITEMS) THEN DO;
          IF Y < &FITEMS AND Y NE . THEN DO;
            IF LAGX = X THEN DO;
              Y = &FITEMS - 1;
              IF Y NE . AND Y NE 0 THEN DO;
                OUTPUT; Y = .; X = .; OUTPUT; END;
            END;
          END;
          ELSE IF Y > &LITEMS THEN DO;
            IF LAGX = X THEN DO;
              Y = &LITEMS + 1; OUTPUT;
              Y = .; X = .; OUTPUT;
            END;
          END;
        END;
        ELSE IF Y < &FITEMS AND LAGY > &LITEMS THEN DO;
          IF LAGX = X THEN DO;
            Y = &FITEMS - 1;
            IF Y NE . AND Y NE 0 THEN DO;
              OUTPUT; Y = &LITEMS + 1;
              OUTPUT; Y = .; X = .; OUTPUT; END;
          END;
        END;
        ELSE IF Y > &LITEMS AND LAGY < &FITEMS THEN DO;
          IF LAGX = X THEN DO;
            Y = &FITEMS - 1;
            IF Y > 0 THEN DO;
              OUTPUT; Y = &LITEMS + 1;
              OUTPUT; Y = .; X = .; OUTPUT; END;
          END;
        END;
      RUN;
      DATA ANNOSPLT; SET ANNOSTUF;
        RETAIN X1 Y1 X2 Y2 FLAG 0;
        Y2 = LAG(Y); X2 = LAG(X); LAGF = LAG(FUNCTION);
        IF FUNCTION = 'LABEL' THEN DO;
          IF (&FITEMS LE Y LE &LITEMS) THEN DO;
            TEXT = TRIM(LEFT(TEXT)); OUTPUT;
            %IF &I NE 1 %THEN %DO;
              IF FLAG = 0 THEN DO;
                XSYS = '3'; YSYS = '3'; HSYS = '4'; SIZE = 0.5;
                X = 2; Y = 2; FUNCTION = 'LABEL'; POSITION = '5';
                TEXT = TRIM(LEFT("&I")); OUTPUT;
                FLAG = 1;
              END;
            %END;
          END;
          RETURN;
        END;
        IF FUNCTION = 'POLY' THEN DO;
          X1 = X; Y1 = Y; RETURN;
        END;
        IF LAGF = 'POLY' THEN RETURN;
        X3 = X; Y3 = Y;
        FUNCTION = 'POLY'; %* FIRST POINT IN POLY;
        IF Y1 < &FITEMS THEN DO;
          IF Y2 < &FITEMS THEN RETURN; %* POLY BELOW CUT;
          X = X1; Y = &FITEMS - 1; OUTPUT;
        END;
        ELSE IF Y1 <= &LITEMS THEN DO;
          X = X1; Y = Y1; OUTPUT;
        END;
        ELSE RETURN; %* POLY ABOVE CUT;
        FUNCTION = 'POLYCONT'; %* REST OF THE POINTS IN POLY;
        IF Y2 <= &LITEMS THEN DO;
          X = X2; Y = Y2; OUTPUT;
        END;
        ELSE DO;
          X = X2; Y = &LITEMS + 1; OUTPUT;
        END;
        IF Y3 < &FITEMS THEN DO;
          IF Y2 > &LITEMS THEN DO;
            X =
              %IF &AXIS = S %THEN 1 -;
              ((&LITEMS + 1) - Y2)/(Y3 - Y2) * ABS(X3 - X2);
            Y = &LITEMS + 1; OUTPUT;
          END;
          X =
            %IF &AXIS = S %THEN 1 -;
            ((&FITEMS - 1) - Y2)/(Y3 - Y2) * ABS(X3 - X2);
          Y = &FITEMS - 1; OUTPUT;
        END;
        ELSE IF Y3 <= &LITEMS THEN DO;
          IF Y2 > &LITEMS THEN DO;
            X =
              %IF &AXIS = S %THEN 1 -;
              ((&LITEMS + 1) - Y2)/(Y3 - Y2) * ABS(X3 - X2);
            Y = &LITEMS + 1; OUTPUT;
          END;
          X = X3; Y = Y3; OUTPUT;
          IF Y1 < &FITEMS THEN DO;
            X =
              %IF &AXIS = S %THEN 1 -;
              ((&FITEMS - 1) - Y1)/(Y3 - Y1) * ABS(X3 - X1);
            Y = &FITEMS - 1; OUTPUT;
          END;
        END;
        ELSE DO;
          X =
            %IF &AXIS = S %THEN 1 -;
            ((&LITEMS + 1) - Y1)/(Y3 - Y1) * ABS(X3 - X1);
          Y = &LITEMS + 1; OUTPUT;
          IF Y1 < &FITEMS THEN DO;
            X =
              %IF &AXIS = S %THEN 1 -;
              ((&FITEMS - 1) - Y1)/(Y3 - Y1) * ABS(X3 - X1);
            Y = &FITEMS - 1; OUTPUT;
          END;
        END;
        DROP X1-X3 Y1-Y3 LAGF FLAG;
      RUN;
      SYMBOL1 L=1 V=NONE W=2 C=BLACK I=JOIN;
      PROC GPLOT DATA=TREESTUF ANNO=ANNOSPLT;
        PLOT Y*X / SKIPMISS HAXIS=AXIS1 VAXIS=AXIS2
                   CAXIS=&CAXIS;
 
        %IF &I = 1 %THEN %DO;
        AXIS1 VALUE=(F=&FONT H=1 C=&CAXIS)
              LABEL=(F=&FONT H=1.5 C=BLACK "&LABEL")
              MINOR=(N=9)
        %END;
        %ELSE %DO;
        AXIS1 VALUE=(F=SIMPLEX H=1 C=&CAXIS
                     %DO DD = 0 %TO 10; "`" %END; )
              LABEL=(F=SIMPLEX H=1.5 C=&CAXIS "`````")
              MAJOR=NONE
              MINOR=NONE
              STYLE = 0
        %END;
              ORDER=&TRIML TO &MINVAL BY -.1
              ORIGIN=(&ORIGIN)
              OFFSET=(0,0)
              COLOR = &CAXIS;
        AXIS2 VALUE=NONE
              LABEL=NONE
              MINOR=NONE
              MAJOR=NONE
              ORDER=
              %IF &I = 1 %THEN 1 TO %EVAL(&LITEMS + 1) BY 1;
              %ELSE %IF &I = &TIMES %THEN %EVAL(&FITEMS - 1) TO &LITEMS BY 1;
              %ELSE %EVAL(&FITEMS - 1) TO %EVAL(&LITEMS + 1) BY 1 ;
              STYLE = 0
              OFFSET=(2,2)
              COLOR = &CAXIS;
        FOOTNOTE ;
      RUN;
    %END;
  %END;
  %ELSE %DO;
    PROC GPLOT DATA=TREEDATA ANNO=ANNOSTUF &GOUT;
      PLOT Y*X / SKIPMISS HAXIS=AXIS1 VAXIS=AXIS2
                 CAXIS=&CAXIS &GNAME &GDES;
      AXIS1 VALUE=(F=&FONT H=1 C=&CAXIS)
            LABEL=(F=&FONT H=1.5 C=&CAXIS "&LABEL")
          %IF &AXIS = S %THEN %DO;
            ORDER=&TRIML TO &MINVAL BY -.1
            MINOR=(N=9)
          %END;
            OFFSET=(0,0)
            ORIGIN=(&ORIGIN)
            COLOR = &CAXIS;
      AXIS2 VALUE=NONE
            LABEL=NONE
            MINOR=NONE
            MAJOR=NONE
            ORDER= 1 TO &ITEMS BY 1
            STYLE = 0
            OFFSET=(2,2)
            COLOR = &CAXIS;
      FOOTNOTE ;
    RUN;
  %END;
  %GOTO ENDALL;
  %BADDSN: %PUT &S No tree produced since an output data set from ;
    %PUT &S PROC CLUSTER was not provided.;
  %ENDALL: %PUT %STR(   );  OPTION NOTES;
%MEND GRFTREE;
%MACRO ORDER;
  PROC SORT DATA=TREEDATA; BY CLUSNUM FROM; RUN;
  DATA TREESTUF; SET TREEDATA; BY CLUSNUM;
    ARRAY CLUS{*} CL1-CL&ST.;
    RETAIN CL1-CL&ST. LASTY;
    IF TOHT = . THEN DO;
      IF MOD(FROMFREQ,2) = 1 THEN Y = FROMFREQ/2;
      ELSE Y = (FROMFREQ + 1)/2;
      CLUS{1} = Y;
    END;
    ELSE DO;
     Y = (TOFREQ - FROMFREQ)/2;
     IF FROMFREQ NE 1 THEN DO;
      I1 = SUBSTR(TO,3) + 0.0;
      I2 = SUBSTR(FROM,3) + 0.0;
      IF FIRST.CLUSNUM THEN DO;
       CLUS{I2} = CLUS{I1} + Y;
       LASTY = CLUS{I2};
      END;
      ELSE CLUS{I2} = CLUS{I1} - Y;
      Y = CLUS{I2};
     END;
     ELSE DO;
      I2 = SUBSTR(TO,3) + 0.0;
      IF FIRST.CLUSNUM THEN DO;
        Y = CLUS{I2} + Y;
        LASTY = Y;
      END;
      ELSE DO;
       IF CLUSTER = . THEN Y = CLUS{I2} - Y;
       ELSE DO;
        Y = LASTY - 1;
        LASTY = Y;
       END;
      END;
     END;
    END;
    IF FROMFREQ = 1 THEN DO;
      IF MOD(Y,1) = 0.5 THEN Y = Y + .5; %* ODD NUMBER OF ITEMS;
      OUTPUT;
    END;
    KEEP CLUSNUM FROM Y;
  RUN;
%MEND ORDER;
