/* 
    Name:  centrds.sas
   Title:  Calculate centroid and area of polygon regions
	
   Generate a dataset containing the physical centroid and area
	associated with a given polygon.  Area values are not correct
	in the case of polygons with islands;
*/
 
%MACRO CENTRDS(INDS    =_LAST_, /* INPUT POLYGON DATA SET            */
               OUTDS   =,       /* OUTPUT ANNOTATE DATA SET          */
               POLIDS  =,       /* VARIABLES IDENTIFYING POLYGONS    */
               X       =X,      /* NAME OF HORIZONTAL VARIABLE       */
               Y       =Y,      /* NAME OF VERTICAL VARIABLE         */
               SEGMENT =SEGMENT,/* NAME OF VARIABLE DESIGNATING
                                   MULTI-PART POLYGONS.              */
               SORT    =,       /* LIST OF SORT VARIABLES, IF D.S.
                                   REQUIRES PRE-SORTING.             */
               SLCTEDIT=,       /* USUAL USER-SUPPLIED MACRO NAME    */
               DEBUG   =0,
               LISTN   =0);     /* INTEGER INDICATING HOW MANY OBS. TO
                                   PRINT FOR DEBUGGING PURPOSES      */
  OPTIONS DQUOTE; RUN;
  %IF &SORT NE %THEN %DO;
    proc sort data=&inds out=temp;
       by &sort; run;
    %LET INDS = TEMP;
    %END;
  %IF &SEGMENT EQ
    %THEN %LET BYVARS = &POLIDS;
  %ELSE %LET BYVARS = &POLIDS%STR( )&SEGMENT;
  %LET LOOP=1;
  %DO %WHILE(%SCAN(&BYVARS,&LOOP)>%STR());
     %LET POLID=%SCAN(&BYVARS,&LOOP);
     %LET LOOP=%EVAL(&LOOP+1);
     %END;
  DATA &OUTDS (KEEP=X Y AREA &BYVARS);
    SET &INDS(RENAME=(&X=INX &Y=INY)) END=EOD;
    BY &BYVARS NOTSORTED;
    %IF &SLCTEDIT ^= %THEN %&SLCTEDIT;
    *--- ACCUMULATOR VARIABLES, ETC.;
    RETAIN AREA SUMX SUMY SKIP LASTX LASTY FIRSTX FIRSTY OBSOUT 0;
    ARRAY SUMVARS AREA SUMX SUMY;
    IF FIRST.&POLID THEN DO;
      SKIP = 0;
      DO OVER SUMVARS; SUMVARS = 0; END;
      FIRSTX = INX; FIRSTY = INY;
      END;
    ELSE IF INX = . & ^SKIP THEN DO;
      *--- islands are handled as follows: 1) use last x-y values
           to calculate a final segment, 2) calculate the centroid
           of the outer part and output it, and 3) skip over the
           island points to the next polygon;
      AREA = (AREA + (FIRSTX-LASTX)*(FIRSTY+LASTY))/2;
      SUMX = SUMX + (FIRSTY-LASTY) * (FIRSTX**2 + FIRSTX*LASTX +
             LASTX**2);
      SUMY = SUMY + (LASTX-FIRSTX) * (FIRSTY**2 + FIRSTY*LASTY +
             LASTY**2);
      LINK CALCCENT;
      SKIP = 1;
      END;
    ELSE IF ^SKIP THEN DO;
      *--- accumulate values;
      AREA = AREA + (INX-LASTX)*(LASTY+INY);
      SUMX = SUMX + (INY-LASTY)*(LASTX**2 + LASTX*INX + INX**2);
      SUMY = SUMY + (LASTX-INX)*(LASTY**2 + LASTY*INY + INY**2);
      IF LAST.&POLID THEN DO;
        *--- do last piece to round-off polygon;
        AREA = (AREA + (FIRSTX-INX)*(FIRSTY+INY))/2;
        SUMX = SUMX + (FIRSTY-INY)*(FIRSTX**2 + FIRSTX*INX + INX**2);
        SUMY = SUMY + (INX-FIRSTX)*(FIRSTY**2 + FIRSTY*INY + INY**2);
        *--- CALCULATE CENTROID AND OUTPUT;
        LINK CALCCENT;
      END;
    END; *--- OF THE ELSE IF ^SKIP;
  LASTX = INX; LASTY = INY;
  %IF &DEBUG EQ 1 %THEN %DO;
    PUT /'*****' _ALL_;
  %END;
  RETURN;
 
CALCCENT:
  X = -SUMX/(6*AREA);
  Y = -SUMY/(6*AREA);
  AREA = ABS(AREA);
  OUTPUT;
  RETURN;
  RUN;
 
  proc print data=_last_(obs=&listn);
    title "FIRST &LISTN RESULTS OF CENTROID CALCULATION STEP";
    run;
  proc summary data=_last_; by &polids;
    var x y area;
    output out=&outds(drop=sumx sumy _type_ _freq_)
           mean=x y sum=sumx sumy area;
    run;
  proc print data=_last_(obs=&listn);
    title "FIRST &LISTN RESULTS OF SUMMARY STEP";
    run;
%MEND CENTRDS;
