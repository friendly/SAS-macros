 /********************************************************************/
 /* NAME: INWHICH      SYSTEM: MSCDC   PGMR: STRUTHERS/BLODGETT      */
 /* FUNCTION: GIVEN A SAS DATA SET WITH X-Y COORDS AND A SAS GMAP    */
 /*  (POLYGON BDRY) DATA SET, MACRO DOES A PT-IN-POLYGON SEARCH TO   */
 /*  WHICH (IF ANY) OF THE POLYGONS EACH POINT IS IN/ON.  USES THE   */
 /*  %PTINPOL AND %INSECT UTILITY SUBMACROS.                         */
 /* STATUS: TEST UNDER MVS                                           */
 /* RELATED MODULES: %PTINPOL, %INSECT.                              */
 /*    (SAMPLE USE IN $6386.UICJOBS.CNTL(BG82BG9A))                  */
 /*   Sample apps on AIX in /mscdc/appsunix/inwhich.sas              */
 /*       and /uic/ojobs/o96005d.sas.                                */
 /* DOCUMENTATION: WHY BREAK TRADITION?                              */
 /* NOTES: AS OF 11/91 THERE APPEARS TO BE A PROBLEM WITH %PTINPOL   */
 /*   WORKING WITH POLYGONS HAVING INTERNAL HOLES ("LAKES").         */
 /*                                                                  */
 /********************************************************************/

 %GLOBAL REVDATE;  %LET REVDATE=27FEB96; *<----DATE OF LAST REVISION;

 /********************************************************************/
 /* BEGIN MACRO DEFINITION                                           */
 /********************************************************************/
/* Macro to access a sas gmap data set and a data set
   containing x-y coordinates (a "datapoints file"), and assign the
   identifiers of the polygon(s), if any, in which each point is
   contained, i.e. this is a point-in-polygon encoding routine.
   Utilizes the PTINPOL utility routine.  All parameters are required
*/

 %MACRO INWHICH( 
 PTX=X,      /* variable on &ptsin data set representing x coordinate */
 PTY=Y,      /* companion to ptx */
 POLX=X,     /* variable on &polsin data set representing x coordinate */
 POLY=Y,     /* companion to polx */

 PTSIN=,     /* sas data set containing datapoints to be encoded */
 OUT=points, /* output sas data set, i.e. &ptsin with &polids added  */
 POLSIN=,    /* sas map data set (i.e. suitable for use with gmap) */

 SORTPOLS=0, /* Set sortpols=1 if polsin is not sorted by polids.
             	Set sortpols=2 if you want sort to be permanent, i.e.
             	to store results of sort back into polsin  */
 PTIDS=,     /* variables on &ptsin used to identify points */
 POLIDS=,    /* id variables on &polsin data set */
 KEEPALSO=_OBS,
            /* the ptsout data set will contain the ptids and polids
               variables.  to keep any other vars include them in a
               list as the value of this parm.  you can use the variable
               _obs in this list (created by the macro, it points to
               the ptsin data set and can be used to re-sort output
               points back into original order) */
 WARNINGS=0, /* set warnings=1 to have uncoded polys to gen warning
             	messages.  set warnings=-1 to also turn off warnings
             	about points that are outside the "bounding box" of the
             	polygons set.  added -1 opt 2-96, jgb */
 DEBUG=0);

 /* END                                                              */

  %PUT %STR( );
  %PUT ***************************************************************;
  %PUT *       INWHICH MACRO REV &REVDATE BEGIN EXECUTION             *;
  %PUT ***************************************************************;
  %PUT %STR( );

 data _ptsin;
 	set &ptsin;  _Obs+1;      run;
 proc sort data=_ptsin;
 	by &ptx &pty;run;

%if &sortpols eq 1 %then %let sortout=_polys;
  %else %if &sortpols eq 2 %then %let sortout=&polsin;
%if &sortpols %then %do;
  proc sort data=&polsin out=&sortout; 
  	by &polids;  run;
  %end;

%if &sortpols eq 1 %then %let polsin=_polys; %*<----note reassignment;
 proc summary data=&polsin noprint nway;
 var &polx &poly;
 class &polids;
 output out=_minmax (drop=_type_ _freq_)
        min=minx miny max=maxx maxy n=ptsn nmiss=missn;
 run;
*proc print data=_minmax;

 %let i=1; %*--loop to assign value to breakid, the last id variable;
 %do %while(%scan(&polids,&i)^=%str());
  %let breakid=%scan(&polids,&i);
  %let i=%eval(&i+1);
  %end;

 %let npolys=0;  
 %let maxpts=0; *--these globals assigned via symput
         next step (if nothing has gone wrong)----*;
 data _polys;
  retain maxpts npolys 0;
  *--merge data sets and create macro vars maxpts, npolys--;
  merge &polsin _minmax end=eof;
  by &polids;
  if last.&breakid then do;
   npolys+1;
   npts=ptsn+missn;
   if npts>maxpts then maxpts=npts;
   if eof then do;
	call symput('maxpts',maxpts);
	call symput('npolys',npolys);
	end;
   end;
  drop maxpts npts ptsn missn npolys;
  run;
  %put NOTE: MAXPTS=&maxpts and NPOLYS= &npolys in polygon dataset &polsin;

*--FOLLOWING SORT ALLOWS FOR FASTER SEARCHING--*;
 proc sort data=_polys out=_polys(drop=minx miny maxx maxy);
 	by maxx minx maxy miny;
 run;
 proc sort data=_minmax;
 	by maxx minx maxy miny;
 run;

 data &out;
 length inout $3;
 retain
 &ptids;  *<---used to force these to be "first" vars on output;
 retain loxmin _np_ 0 polobs basepoly 1;

 *---arrays defining windows, i.e. "boxes" around each polygon---*;
 array xlo(&npolys) _temporary_;
 array xlomin(&npolys) _temporary_;
 array ylo(&npolys) _temporary_;
 array xhi(&npolys) _temporary_;
 array yhi(&npolys) _temporary_;

 *--polptr values point to obs on _polsin for each polygon;
 array polptr(&npolys) _temporary_;
 array npolpts(&npolys) _temporary_;

 *---arrays defining polygon vertices (only one stored at a time)---*;
 *---array sizes were assigned in previous step via symput--;
 array ax(&maxpts) _temporary_;
 array ay(&maxpts) _temporary_;

 IF _N_=1 THEN DO;
 *---INPUT POLYGON MAXS & MINS IN MAXX ORDER, #POINTS, STARTING OBS---*;
  do while(^eofwind);
   set _minmax end=eofwind;
   _np_+1;
   xlo(_np_)=minx;
   ylo(_np_)=miny;
   xhi(_np_)=maxx;
   yhi(_np_)=maxy;
   polptr(_np_)=polobs;
   npolpts(_np_)=ptsn+missn;*-calculate #points from n & nmiss-*;
   polobs=polobs+npolpts(_np_);*-keep running ptr to 1st poly obs-*;
   if _error_ then err_at='loading_minmax';
   end;

  *---Now for each point in arrays set the lowest minx remaining---*;
  *---to determine range over which poly candidates must be tested---*;
  *---eg. when lowest x remaining greater than current x quit test---*;

  %IF &DEBUG %THEN %STR(PUT 'Initializing xlomin array';);
  loxmin=xlo(&npolys);
  xlomin(&npolys)=loxmin;
  do _np_=&npolys to 1 by -1;
   if xlo(_np_)<loxmin then do;
    loxmin=xlo(_np_);
    xlomin(_np_)=loxmin;
    end;
   else xlomin(_np_)=loxmin;
   %if &debug %then %str(put xlomin(_np_)=;);
   end;
  end;

 SET _PTSIN;
 _PTX=&PTX; _PTY=&PTY;
 *---Establish 1st possible window for x based on max x---*;
 IF BASEPOLY>&NPOLYS THEN DO;
  %if &warnings ne %str(-1) %then %do;
  PUT 'WARNING: Test point outside polygon file coverage area ' basepoly= &ptx= &pty=;
  PUT _ALL_;
  %end;
  GOTO DONE;
  END;

 DO WHILE(_PTX>XHI(BASEPOLY) & BASEPOLY<=&NPOLYS);
 *-INCREMENT TILL XHI GREATER THAN X-*;
  BASEPOLY+1;
  IF BASEPOLY>&NPOLYS THEN DO;
  %if &warnings ne %str(-1) %then %do;
   PUT 'WARNING: Test point outside polygon file coverage area ' basepoly=;
  %end;
   XMIN=XLO(BASEPOLY);XMAX=XHI(BASEPOLY);
   YMIN=YLO(BASEPOLY);YMAX=YHI(BASEPOLY);
   if err_at=' ' and _error_ then err_at='store_minmax';
   GOTO DONE;
   END;
  END;
 CURPOLY=BASEPOLY;

 *---Test y values over possible x range---*;
 DO WHILE(_PTX>=XLOMIN(CURPOLY));
 *-TEST WHILE X IS EQUAL TO OR GREATER THAN LOWEST REMAINING MIN-*;
  if _ptx>=xlo(curpoly) & ylo(curpoly)<=_pty<=yhi(curpoly) then do;
   npts=npolpts(curpoly);
   xmin=xlo(curpoly);xmax=xhi(curpoly);
   ymin=ylo(curpoly);ymax=yhi(curpoly);
   if err_at=' ' and _error_ then err_at='store_minmax2';
   point=polptr(curpoly);

   do curpoint=1 to npts;
    ptr=point;
    set _polys(keep=&polids &polx &poly) point=ptr;
    ax(curpoint)=&polx;
    ay(curpoint)=&poly;
    if err_at=' ' and _error_ then err_at='store_ax_ay';
    point=point+1;
    end;
   *----INVOKE THE PTINPOL ROUTINE TO APPLY BASIC IS-IT-IN-THE-POLYGON
        ALGORITHM AND ASSIGN VALUE TO VARIABLE INOUT----*;
   %PTINPOL(X=_PTX,Y=_PTY);
   END;
  IF INOUT='IN' | INOUT='ON' THEN GOTO FOUND;

  CURPOLY=CURPOLY+1;
  IF CURPOLY>&NPOLYS THEN DO;
   CURPOLY=&NPOLYS;
   XMIN=XLO(CURPOLY);XMAX=XHI(CURPOLY);
   YMIN=YLO(CURPOLY);YMAX=YHI(CURPOLY);
   if err_at=' ' and _error_ then err_at='circa-209';
   GOTO DONE;
   END;
  END;
 XMIN=XLO(CURPOLY);XMAX=XHI(CURPOLY);
 YMIN=YLO(CURPOLY);YMAX=YHI(CURPOLY);

 DONE:
  %IF %quote(&WARNINGS) eq %str(1) %THEN %DO;
    PUT 'WARNING: Test point does not fall in any polygon';
    %str(put )
    %let i=1;
    %do %while(%scan(&ptids,&i)^=%str());
     %let id=%scan(&ptids,&i);
     %str(&id= )
     %let i=%eval(&i+1);
     %end;
    %str(_ptx= _pty= )
    %let i=1;
    %do %while(%scan(&polids,&i)^=%str());
     %let id=%scan(&polids,&i);
     %str(&id= )
     %let i=%eval(&i+1);
     %end;
    %str(xmin= xmax= ymin= ymax= ;)
    %end;
   %let i=1;
   %do %while(%scan(&polids,&i)^=%str());
    %let id=%scan(&polids,&i);
    %str(&id=.;)
    %let i=%eval(&i+1);
    %end;

 FOUND:
  OUTPUT;
  INOUT=' ';
 %do %while(%scan(&polids,&i)^=%str());
  %let id=%scan(&polids,&i);
  %str(&id=.;)
  %let i=%eval(&i+1);
  %end;
  KEEP &PTIDS &POLIDS INOUT &KEEPALSO;
  RUN;
  %MEND INWHICH;
