%macro manyplot(igout=WORK.GSEG, cols=, rows=, border=, gout=);

  /***************************************************************/
  /* Macro to put multiple plots on a page, with user-defined    */
  /* number of rows and columns                                  */
  /*                                                             */
  /* Version 1.0 - 12/21/93                                      */
  /*                                                             */
  /* Written November 1993 by Earl Westerlund                    */
  /*                                                             */
  /* Arguments Type    Description                               */
  /* --------- ------- -----------                               */
  /* igout     input   name of graphics catalog to display       */
  /* cols      input   number of columns across to display plots */
  /* rows      input   number of rows down to display plots      */
  /* border    input   color of border (default=no border)       */
  /* gout      input   name of graphics catalog to save to       */
  /*                                                             */
  /***************************************************************/

  /*** Divide plotting area into grid, based on number of rows and
       columns requested.  Create template specification in a
       temporary text file ***/
  DATA _NULL_;
    ROWPCT = 100 / &rows;
    COLPCT= 100 / &cols;
    FORMAT LX RX LY UY 5.1 PANEL 3.;
    FILE "C:\TEMP\_PANELS.SAS" RECFM=V LRECL=255;
    PUT 'TDEF MANYPLT';
    DO ROW = 1 TO &rows;
      DO COL = 1 TO &cols;
        PANEL + 1;
        LY = (&rows-ROW) * ROWPCT;
        UY = LY + ROWPCT;
        RX = COL * COLPCT;
        LX = RX - COLPCT;
        PUT PANEL '/LLX=' LX 'LLY=' LY 'ULX=' LX 'ULY=' UY /
                  ' LRX=' RX 'LRY=' LY 'URX=' RX 'URY=' UY ;
        %if &border ne %then PUT "COLOR=&border";;
      END;
    END;
    PUT ';';
  RUN;

  /*** Count how many plots in the catalog                    ***/
  /*** Only count the plots (could be other catalog elements) ***/
  PROC CATALOG C=&igout;
    CONTENTS OUT=_1(KEEP=TYPE WHERE=(TYPE='GRSEG'));
  QUIT;
  DATA NULL_;
    DS = OPEN('_1', 'I');
    NOBS = ATTRN(DS, 'NOBS');
    CALL SYMPUT('NOBS', NOBS);
    RC = CLOSE(DS);
  RUN;

  /*** Actually replay the plots, using template defined above ***/
  PROC GREPLAY NOFS IGOUT=&igout CC=CMAP.CMAP CMAP=WHTBLK;
    %if &gout ^= %then GOUT=&gout;
    ;
    TC TEMPLT;
    %include "C:\TEMP\_PANELS.SAS" / NOSOURCE;
    TEMPLATE MANYPLT;
    %let nplots=%eval(&cols*&rows);
    %do i = 1 %to &nobs %by &nplots;
      TREPLAY
      %let box = 1;
      %let plot = &i;
      %do %until (&box>&nplots or &plot>&nobs);
        &box:&plot
        %let box = %eval(&box+1);
        %let plot = %eval(&plot+1);
      %end;
      ;
    %end;
  QUIT;

%mend;
