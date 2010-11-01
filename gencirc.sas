*---THIS FILE CONTAINS 4 MODULES:
    GENCIRC  MACRO.
    TESTCIRC SAMPle PROGRAM TO INVOKE GENCIRC.
    MLATLON,
    LATLONM: UTILITY MACROS TO CONVERT TO/FROM LAT-LONG AND MILES.
    JGB, 8-20-90;
*===GENCIRC MODULE BEGINS===;
 /********************************************************************/
 /* NAME: GENCIRC      SYSTEM: GMAPDEMO PGMR: BLODGETT               */
 /* FUNCTION: GENERATES ANNOTATE DATA SET OBSERVATIONS TO DRAW A     */
 /*           CIRCLE OF A SPECIFIED RADIUS AT A SPECIFIED POINT.     */
 /* STATUS: TEST UNDER PC-DOS.                                       */
 /* RELATED MODULES: MAY INVOKE LATLONM TO CONVERT LAT-LON TO MILES  */
 /* DOCUMENTATION:                                                   */
 /* NOTES: NEEDS WORK AND DOCUMENTATION BUT IT HANDLES THE BASIC     */
 /*   FUNCTION OF DRAWING CIRCLES.                                   */
 /*                                                                  */
 /********************************************************************/
 
 /********************************************************************/
 /* BEGIN MACRO DEFINITION                                           */
 /********************************************************************/
%MACRO GENCIRC(REVDATE=19AUG90,
   X0=X0,  /* SPECIFY THE NAME OF A SAS VARIABLE CONTAINING THE X
              COORDINATE OF THE CENTER OF THE CIRCLE */
   Y0=Y0,  /* THE CORRESPONDING Y COORDINATE VARIABLE */
   RADIUS=RADIUS, /* THE RADIUS OF THE CIRCLE TO BE DRAWN */
   NSIDES=36, /* THE CIRCLE WILL BE APPROXIMATED WITH AN N-SIDED
               POLYGON.  SPECIFY A HIGHER NUMBER (E.G. 60) TO GET A
               MORE "CIRCULAR" CIRCLE. CANNOT BE A VARIABLE NAME */
   FIRST=1, /* SPECIFY FIRST=0 IF USING THE ROUTINE MORE THAN ONCE IN
              THE SAME DATA STEP.  MACRO WILL NOT GENERATE INITIAL
              CODE ON SUBSEQUENT CALLS.  FIRST=1 MUST BE SPECIFIED ON
              FIRST CALL IN A STEP */
   ANNODS=, /* SPECIFY NAME OF OUTPUT DATA SET TO BE USED IN OUTPUT
               STATEMENTS. LEAVE NULL IF CREATING ONLY 1 DATA SET */
   /* FOR EACH OF THE FOLLOWING PARMS YOU CAN SPECIFY A VALUE EQUAL TO
      THE PARM NAME. IF YOU DO THE MACRO ASSUMES YOU HAVE A VARIABLE
      BY THAT NAME IN YOUR DATA STEP AND IT WILL LEAVE IT ALONE */
   COLOR   =BLACK,  /* COLOR OF CIRCLE                   */
   SIZE    =1,      /* LINE SIZE                         */
   LINE    =1,      /* LINE TYPE                         */
   WHEN    =A,      /* ANNOTATE WHEN OPTION              */
   PUTHDR=1,
   DEBUG=0);        /* DEBUG=1 TO PRODUCE DIAGNOSTICS    */
 
%IF &PUTHDR %THEN %DO;
  %PUT %STR( );
  %PUT **************************************************************;
  %PUT *       GENCIRC MACRO REV &REVDATE BEGIN EXECUTION            *;
  %PUT ***************************************************************;
  %PUT %STR( );
  %END;
 
  %IF &DEBUG %THEN %DO;
    %PUT %STR( );
    %PUT; %PUT *****PARAMETERS SPECIFIED*****;
    %PUT %STR( );
    %END;
 %IF &FIRST %THEN %DO;
   RETAIN _AINCR 0;
   RETAIN _NSIDES 0 _NSDSM1;
   ARRAY _XOFF (&NSIDES) _TEMPORARY_;
   ARRAY _YOFF (&NSIDES) _TEMPORARY_;
   RETAIN XSYS "2" YSYS "2" HSYS "3";
   %END;
 IF _NSIDES NE &NSIDES THEN DO;
   _NSIDES=&NSIDES;   _NSDSM1=_NSIDES - 1;
   _AINCR=8*ATAN(1)/&NSIDES; *-ANGLE IN RADIANS;
   _ANGLE=0;
 
   DO _IGCIRC=1 TO _NSDSM1;
     _ANGLE + _AINCR;
     _XOFF(_IGCIRC)=COS(_ANGLE);
     _YOFF(_IGCIRC)=SIN(_ANGLE);
     END;
   END;
 *--GENERATE THE CIRCLE AS A MOVE FOLLOWED BY A SERIES OF DRAWS;
   X=&X0 + &RADIUS;  Y=&Y0;  FUNCTION='MOVE    ';  OUTPUT &ANNODS;
   FUNCTION='DRAW    ';
   %IF &COLOR NE COLOR %THEN %STR( COLOR="&COLOR"; );
   %IF &LINE NE LINE %THEN %STR( LINE=&LINE; );
   %IF &SIZE NE SIZE %THEN %STR( SIZE=&SIZE; );
   %IF &WHEN NE WHEN %THEN %STR( WHEN="&WHEN";);
   OUTPUT &ANNODS;
   DO _IGCIRC=1 TO _NSDSM1;
     X=&X0 + &RADIUS * _XOFF(_IGCIRC);
     Y=&Y0 + &RADIUS * _YOFF(_IGCIRC);
     OUTPUT &ANNODS;
     END;
   X=&X0 + &RADIUS;  Y=&Y0;  OUTPUT &ANNODS; *--RETURN FULL CIRCLE;
   DROP _NSIDES _NSDSM1 _AINCR _ANGLE _IGCIRC;
 
%MEND GENCIRC;
 
*<=====TESTCIRC MODULE BEGINS=====;
 OPTIONS REPLACE MPRINT;
GOPTIONS DEVICE=PS2EGA CBACK=GRAY;
*---TEST GENCIRC--;
*--CREATE TEST DUMMY MAP DATA SET-;
DATA BOX; INPUT X Y;  ID='5-MILE BOX';
 CARDS;
0 0
5 0
5 5
0 5
DATA CIRCLE;
 INPUT X0 Y0 RADIUS LINE  COLOR $;
 %GENCIRC(COLOR=COLOR,RADIUS=RADIUS)
 NCIRC+1;
 IF _N_=2 THEN DO;
   HSYS="3";  X=3; Y=2; SIZE=2; TEXT='*'; COLOR='BLUE';
   FUNCTION='LABEL';  POSITION='5';
   OUTPUT;
   END;
 CARDS;
1.5 1.5 1  2  RED
3   2   2  6  BLUE
  RUN;
 
PROC GMAP DATA=BOX(OBS=1) MAP=BOX ANNOTATE=CIRCLE ALL;
 CHORO X / NOLEGEND ;
 ID ID;
 PATTERN1 VALUE=E;
RUN;
 
*=======MLATLON AND LATLONM MACROS BEGIN=======;
 /********************************************************************/
 /* NAME: MLATLON      SYSTEM: MSCDC   PGMR: BLODGETT                */
 /* FUNCTION: CONVERT X-Y COORDS FROM MILES TO LAT-LONG              */
 /*                                                                  */
 /* STATUS: TEST UNDER MVS                                           */
 /* RELATED MODULES: LATLONGM, TIGERLN, ETC.                         */
 /* DOCUMENTATION:                                                   */
 /* NOTES: DEFAULT PARMS SET UP TO ALLOW AUTOMATIC REVERSAL OF       */
 /*   CONVERSIONS FOR "MISSOURI MILES".  MACRO IS WRITTEN TO BE      */
 /*   PART OF A SAS DATA STEP AND CAN BE INVOKED MULTIPLE TIMES.     */
 /********************************************************************/
 
 %GLOBAL REVDATE;  %LET REVDATE=06OCT89; *<----DATE OF LAST REVISION;
 
 /********************************************************************/
 /* BEGIN MACRO DEFINITION                                           */
 /********************************************************************/
%MACRO MLATLON( /*<---AVOID 8-CHAR NAMES..CMS CANNOT HANDLE THEM---*/
  X0=96,  /* LONGITUDE ORIGIN, DEGREES WEST. SAME AS WESTLONG      */
  Y0=35,  /* LATITUDE ORIGIN, DEGREES NO. SAME AS NORTHLAT         */
  XFACT=54.9838, /* FACTOR USED TO CONVERT MILES TO DEGREES.       */
  X=X,Y=Y,XLONG=XLONG,YLAT=YLAT, /* NAMES OF VARIABLE CONTAINING:
     THE (INPUT) X,Y COORDS IN MILES, THE (OUTPUT) Y-LATITUDE AND
     X-LONGITUDE COORDS.  IF YOU WANT TO CONVERT COORDS IN (XM,YM) TO
     (LONGITD,LATTD) THEN USE: X=XM,Y=YM,XLONG=LONGITD,YLAT=LATTD  */
  DEBUG=0);
 /* THE VALUE OF THESE PARMS CAN BE OBTAINED FROM THE FORMULA PRINTED
    ON THE SAS LOG WHEN THE TIGERLN OR LATLONM MACROS ARE INVOKED  */
 
 /* END                                                              */
 
 %GLOBAL MLATLON_ ; %IF &MLATLON_ NE YES %THEN %DO;
  %LET MLATLON_=YES;
  %PUT %STR( );
  %PUT ***************************************************************;
  %PUT *       MLATLON MACRO REV &REVDATE BEGIN EXECUTION             *;
  %PUT *           MISSOURI STATE CENSUS DATA CENTER                 *;
  %PUT ***************************************************************;
  %PUT %STR( );
 
  %IF &DEBUG %THEN %DO;
    %PUT %STR( );
    %PUT; %PUT *****PARAMETERS SPECIFIED*****;
    %PUT  X0=  &X0  WEST LONGITUDE (DEGREES) CORRESPONDING TO X=0;
    %PUT  Y0=  &Y0  NORTH LATITUDE (DEGREES) CORRESPONDING TO Y=0;
    %PUT  XFACT= &XFACT # OF MILES IN 1 DEG. OF LONGITUDE;
    %PUT CONVERSION FORMULA IS AS FOLLOWS: ;
    %PUT  &XLONG = &X0 - (&X / &XFACT);
    %PUT  &YLAT  = &Y0 + (&Y / 68.824);
    %END;
  %END;
 
  &XLONG = &X0 - (&X / &XFACT);
  &YLAT  = &Y0 + (&Y / 68.824);
 
   %MEND MLATLON;
 /* SAMPLE INVOCATION
 OPTIONS MPRINT;
 DATA LL; SET TIGER.TIGER1(KEEP=OBS1 FXM TXM FYM TYM OBS=5);
  %MLATLON(X=FXM,Y=FYM,XLONG=FLONG,YLAT=FLAT)
  %MLATLON(X=TXM,Y=TYM,XLONG=TLONG,YLAT=TLAT)
   RUN;
 */
 /********************************************************************/
 /* NAME: LATLONM      SYSTEM: MSCDC   PGMR: BLODGETT                */
 /* FUNCTION: CONVERT X-Y COORDS FROM MILES TO LAT-LONG              */
 /*                                                                  */
 /* STATUS: TEST UNDER MVS                                           */
 /* RELATED MODULES: MLATLON, TIGERLN, ETC.                         */
 /* DOCUMENTATION: MSCDC USER'S GUIDE (?)                            */
 /* NOTES: DEFAULT PARMS SET UP TO ALLOW AUTOMATIC CONVERSION TO     */
 /*                   "MISSOURI MILES".  MACRO IS WRITTEN TO BE      */
 /*   PART OF A SAS DATA STEP AND CAN BE INVOKED MULTIPLE TIMES.     */
 /*   THE COMPANION MODULE, MLATLON, WILL "UNDO" THE CONVERSION      */
 /*   THAT THIS MACRO IMPLEMENTS, CONVERTS BACK TO LAT-LONG.         */
 /*   WILL ONLY WORK ON NORTH LATITUDE, WEST LONGITUD, UNSIGNED.     */
 /********************************************************************/
 
 %GLOBAL REVDATE;  %LET REVDATE=31JUL89; *<----DATE OF LAST REVISION;
 
 /********************************************************************/
 /* BEGIN MACRO DEFINITION                                           */
 /********************************************************************/
%MACRO LATLONM(
  X0=96,  /* LONGITUDE ORIGIN, DEGREES WEST. SAME AS WESTLONG      */
  Y0=35,  /* LATITUDE ORIGIN, DEGREES NO. SAME AS NORTHLAT         */
  XFACT=54.9838, /* FACTOR USED TO CONVERT MILES TO DEGREES.       */
  X=X,Y=Y,XLONG=XLONG,YLAT=YLAT, /* NAMES OF VARIABLE CONTAINING:
     THE (OUTPUT) X,Y COORDS IN MILES, THE (INPUT) Y-LATITUDE AND
     X-LONGITUDE COORDS.  IF YOU WANT TO CONVERT COORDS TO (XM,YM) FROM
     (LONGITD,LATTD) THEN USE: X=XM,Y=YM,XLONG=LONGITD,YLAT=LATTD  */
  DEBUG=0);
 /* THE VALUE OF XFACT IS 69.77545 (= # MILES IN 1 LONG DEGREEE ABOUT*/
 /* THE EQUATOR) TIMES THE COSINE OF AN "AVERAGE" LATITUDE VALUE.    */
 /* WE TYPICALLY AVERAGE THE NORTHERNMOST & SOUTHERNMOST LATS FOR    */
 /* TO ARRIVE AT THIS AVERAGE LATITUDE. HENCE WHEN %TIGRLN CONVERTS  */
 /* IT USES PARMS SOTHLAT AND NORTHLAT, WHICH IT AVERAGES AND USED   */
 /* TO DERIVE ITS XFACT VALUE.  THE DEFAULT VALUE USED HERE IS BASED */
 /* ON USING 38 DEGREES NORTH AS THE "AVERAGE LATITUDE". THIS MEANS  */
 /* OUR X DISTANCES ARE BASED ON THE DISTANCE ALONG THE 38TH PARALLEL*/
 /* IN MILES.  THIS IS A CRUDE BUT USEFUL CONVERSION, NOT TO BE USED */
 /* NEAR THE POLES.                                                  */
 /* END                                                              */
 
 %GLOBAL LATLONM_ ; %IF &LATLONM_ NE YES %THEN %DO;
  %LET LATLONM_=YES;
  %PUT %STR( );
  %PUT ***************************************************************;
  %PUT *       LATLONM MACRO REV &REVDATE BEGIN EXECUTION             *;
  %PUT *           MISSOURI STATE CENSUS DATA CENTER                 *;
  %PUT ***************************************************************;
  %PUT %STR( );
 
  %IF &DEBUG %THEN %DO;
    %PUT %STR( );
    %PUT; %PUT *****PARAMETERS SPECIFIED*****;
    %PUT  X0=  &X0  WEST LONGITUDE (DEGREES) CORRESPONDING TO X=0;
    %PUT  Y0=  &YO  NORTH LATITUDE (DEGREES) CORRESPONDING TO Y=0;
    %PUT  XFACT= &XFACT # OF MILES IN 1 DEG. OF LONGITUDE;
    %PUT CONVERSION FORMULA IS AS FOLLOWS: ;
    %PUT  &X = (&X0 - &XLONG) * &XFACT;
    %PUT  &Y = (&YLAT - &Y0) * 68.824;
    %END;
  %END;
 
  &X = (&X0 - &XLONG) * &XFACT;
  &Y = (&YLAT - &Y0) * 68.824;
 
   %MEND LATLONM;
