/*
textable macro -- revised from Paul Thompson's original by M. Friendly
	- macro parameters renamed so a human can remember what they do
	- some defaults changed; output changed to place all values of each
	row on one line in the tex file.

The current version has
1) preview feature (_rsltab)
2) BY variables which provide rows with hlines and/or separate tables
   for separate values of the by variables
3) an example which, thanks to F. Poppe and his actually trying to 
   run the wretched thing, ACTUALLY WORKS.  Thanks again, Frank.

-- BUG:  Gives macro error if formats not asdsigned
*/
options nosource;
%* ****************************************************************** *;
%* Copyright Paul Thompson, Ph.D., 1996
%* Version: 2.5 - 3/15/96
%*
%* This macro system is wholely owned by Paul A. Thompson
%* 1. It may be used by anyone who retains this copyright notice
%* 2. It may be redistributed to anyone, provided this notice is
%*    also included
%* 3. It may not be sold or distributed in any manner other than
%*    totally free without the express written permission of
%*    Dr. Thompson
%* 4. If used for a commercial enterprise, notice must be given to
%*    Dr. Thompson
%*
%* For the casual user - see macro textable below - this is
%* the primary driver for typesetting an intact rectangular table
%* The other macros are all support
%* ****************************************************************** *;

%* ****************************************************************** *;
%* textable                                                           *;
%* Sets up latex code for typesetting rectangular table               *;
%*                                                                    *;
%* Protocol                                                           *;
%* data      - input dataset                                          *;
%* var       - listing of variables to be set up                      *;
%* byvar     - Variable treated as by variable                        *;
%*             Value output when first.byv, otherwise blank           *;
%* byhline   - # \hline to produce when BY-var isnew (>= 0)           *;
%* tabyfile  - put new tables in new files (1-yes, 0-no)              *;
%* tabyvar   - variable to make new tables with - treated as BY var   *;
%* tabyfmt   - format which is used (if exists) to add values of      *;
%*             tabyvar to end of caption and tabid - if not set,      *;
%*             values are added in an unformated manner               *;
%* caption   - caption for table (enclose in quotes)                  *;

%* outdir    - main subdirectory listing                              *;
%* subdir    - optional subdirectory listing from main                *;
%* dirsep    - subdirectory separator - either "/" or "\" (DOS, UNIX) *;
%* outfile   - file name for final table                              *;
%* ext      - file extension for final table - .tex default           *;

%* formats   - formats for setting up vars - use ~ as separator       *;
%*             If a variable is character, use $.                     *;
%* head1-10  - variables with header information                      *;
%*             _S_ skips that column, and is a mandatory              *;
%*             placeholder                                            *;
%* debug     - internal printing - see forbidden things as the macro  *;
%*             does many disgusting things - prepare for total boredom*;
%* locate    - table location (single letter - see Latex Manual)      *;
%* just      - justification of each variable in vars - single letter *;
%*             see LaTeX manual (rrccll)                              *;
%* miss      - If set to 1 and value is missing, generate a blank     *;
%* _rsltab   - 0-no output: 1-SAS print  2-LaTeX source  3-SAS/LaTeX  *;
%* _strx     - strings for tabular listing - use ~ as separator.      *;
%*             Use _S_ as separator.  These strings are placed AFTER  *;
%*             column listing.                                        *;
%* _ststrx   - string placed at the very start of the tabular list.   *;
%* tabenv    - LaTeX command specifying table environment             *;
%*             Usually, size (\small) or spacing (\singlespace)       *;
%* tabid     - table identification - passed as comment to LaTeX      *;
%* tabname   - table name - set up as label for table                 *;
%*             See LaTeX manual                                       *;
%* tail1-10  - trailer 1-10 - information placed in trailer lines     *;
%* tlwidth   - trailer width - used in parbox specification           *;
%*
%* The separator for arguments is the tilda (~)
%*
%* Note: Two sorts of multipliers may be used in the specifications
%* of a table.
%* 1. In formats, just, _strx, head1-head10, the
%*    text duplicate multiplier may be used.  This is used as
%*       formats=$~@034.@,  ---> formats=$~4.~4.~4.,
%*    In this multiplier, "@" delimits the whole string, first
%*    2 chars are the multiplier count, and rest is the body.  These
%*    may NOT be nested.
%* 2. In head1-head10, multicolumn designations are specified by using
%*    the # specifier.  If the 1st col is a #, the next two columns are
%*    the number of columns to be included in the multicolumn specifier
%* ****************************************************************** *;
%*

%* ****************************************************************** *;
%macro textable(
   data=,
   var=,
   just=,
	formats=,
	byvar=,
	byhline=,
	tabyvar=,
	tabyfmt=,
	tabyfile=0,
	miss=1,

	locate=H,
	sideway=0,
	caption=,
	tabname=tab:&data,
	tabid=table from dataset &data,
	head1=,head2=,head3=,head4=,head5=,
	head6=,head7=,head8=,head9=,head10=,
	tail1=,tail2=,tail3=,tail4=,tail5=,
	tail6=,tail7=,tail8=,tail9=,tail10=,
	tlwidth=,
	tabenv=,
	texalone=0,

   outdir=,
	outfile=,
	ext=tex,
	subdir=,
	dirsep=/,
               _label=,
               debug=0,
               ncol=,
               _rsltab=2,
               _strx=,
               _ststrx=,
					);

%let _zepp=&dirsep.;
%let _cntv=;
%let _cntbyv=;
%if (&sideway.) %then %let _sw=sideways;%else %let _sw=;

%put Unpacking formats....;
%_fillp(_parm=formats, _tilda=1);
%put Unpacking just....;
%_fillp(_parm=just, _tilda=0);
%if  (%length(&_strx.) > 0) %then %do;
	%put Unpacking _strx....;
	%_fillp(_parm=_strx, _tilda=1);
%end;

%do _i=1 %to 10;
%if  (%length(&&head&_i..)) %then %_fillp(_parm=head&_i., _tilda=1);
%end;

%if (%length(&tabyfmt.)) %then %do;
%if (%substr(&tabyfmt.,%length(&tabyfmt.),1) ^= .) %then %let tabyfmt=&tabyfmt..;
%end;

%if (%length(&ncol.)) %then %let _cntv=&ncol.;
%else %_itemcnt(_items=&var., _n=_cntv, _delim=5);
%if (%length(&byvar.)) %then %_itemcnt(_items=&byvar., _n=_cntbyv, _delim=5);
%else %let _cntbyv=0; %let _bystr=;

%do _i=1 %to &_cntbyv.;
	%let _bystr=&_bystr. %scan(&byvar., &_i., ~);
	%let _byhl&_i.=0;
	%if (%length(&byhline.)) %then %let _byhl&_i.=%scan(&byhline.,&_i.,~);
	%end;

%put **************************************************************** *;
%put _cntv[&_cntv.], _cntbyv[&_cntbyv.];
%do _i=1 %to &_cntbyv.;
	%put _byhl&_i.[&&_byhl&_i..];
	%end;
%put **************************************************************** *;

%do _j=1 %to &_cntv.;
	%let _byi&_j.=0;
	%end;
%do _i=1 %to &_cntbyv.;
	%let byvar&_i.=0;
%do _j=1 %to &_cntv.;
	%if (%scan(&_bystr.,&_i.) = %scan(&var.,&_j.,~)) %then %do;
	%let byvar&_i.=1;
	%let _byi&_j.=1;
	%end;
%end;

%if (&&byvar&_i. = 0) %then %do;
	%put ****** ERROR ****** ;
	%put By variable %scan(&_bystr.,&_i.) is not found in the variables list;
	%put This may be an error;
	%put ****** ERROR ****** ;
	%end;
%end;

%let _ext=0;
%if (%length(&tail1.)) %then %let _ext=1;
%if (%length(&_bystr.)) %then %do;
	PROC SORT DATA=&data. OUT=_TSET;
			BY &tabyvar. &_bystr.;
	RUN;
%end;
%else %do;
	DATA _TSET;
	SET &data.;
	RUN;
%end;

%let _byntc=0;
%if (%length(&tabyvar.)) %then %do;
	PROC FREQ DATA=_TSET;
	TABLES &tabyvar./NOPRINT OUT=NEWTAB;
	RUN;
	DATA _NULL_;
	SET NEWTAB END=EOF;
	IF (EOF) THEN CALL SYMPUT("_byntc",COMPRESS(PUT(_N_,4.)));RUN;
%end;

%* put *************************************************************** *;
%put _byntc[&_byntc.];
%* put *************************************************************** *;

PROC CONTENTS DATA=_TSET NOPRINT OUT=BYTYPE;
RUN;

%do _j=1 %to &_cntv.;
 %let _mvt&_j.=0;
 %end;

DATA _NULL_;
	SET BYTYPE;
	%do _j=1 %to &_cntv.;
			IF (UPCASE(NAME) = UPCASE("%scan(&var.,&_j.,~)")) THEN
				CALL SYMPUT(COMPRESS("_mvt&_j."), PUT(TYPE,1.));
		%end;
	RUN;

%if (&debug.) %then %do;
	%put *************************************************************** *;
	%do _j=1 %to &_cntv.;
		%put _var[%scan(&var.,&_j.,~)], _mvt&_j.[&&_mvt&_j.];
		%end;
	%put *************************************************************** *;
	%end;

DATA _XV;
     SET _TSET;
	%if (&_cntbyv. > 0) %then %do;
      BY &tabyvar. &_bystr.;
		%end;
	%else %if (%length(&tabyvar.)) %then %do;
		BY &tabyvar.;
		%end;
     LENGTH _V1-_V&_cntv. $80.;
     ARRAY FVLS _V1-_V&_cntv.;
     _POS=0;
%do _i=1 %to &_cntv.;
%if (&&_byi&_i.) %then %do;
%if (%scan(&formats.,&_i.,~) = $) %then %do;
     IF FIRST.%scan(&_bystr., &_i.) THEN DO;
        FVLS{&_i.}=%scan(&var.,&_i.,~);
        CHKPOS=&&_byhl&_i.;
        _POS=MAX(OF _POS, CHKPOS);
     END;
     ELSE FVLS{&_i.}=" ";
%end;
%else %do;
%if (&&_mvt&_i. = 1) %then %do;
     IF (^FIRST.%scan(&_bystr., &_i.) |
         (&miss. = 1 & %scan(&var.,&_i.,~) = .)) THEN FVLS{&_i.}=" ";
%end;
%if (&&_mvt&_i. = 2) %then %do;
     IF (^FIRST.%scan(&_bystr., &_i.) |
         (&miss. = 1 & %scan(&var.,&_i.,~) = " ")) THEN FVLS{&_i.}=" ";
%end;
        ELSE DO;
           FVLS{&_i.}=PUT(%scan(&var.,&_i.,~), %scan(&formats.,&_i.,~));
           CHKPOS=&&_byhl&_i.;
           _POS=MAX(OF _POS, CHKPOS);
        END;
%end;
%end;
%else %do;
	%if (%scan(&formats.,&_i.,~) = $) %then %do;
			FVLS{&_i.}=%scan(&var.,&_i.,~);
	%end;
%else %do;
%if (&&_mvt&_i. = 1) %then %do;
     IF (&miss. = 1 & %scan(&var.,&_i.,~) = .) THEN FVLS{&_i.}=" ";
%end;
%if (&&_mvt&_i. = 2) %then %do;
     IF (&miss. = 1 & %scan(&var.,&_i.,~) = " ") THEN FVLS{&_i.}=" ";
%end;
     ELSE FVLS{&_i.}=PUT(%scan(&var.,&_i.,~), %scan(&formats.,&_i.,~));
%end;
%end;
%end;
     _CNT=_N_;
RUN;

%if (&_rsltab. = 1 | &_rsltab. = 3) %then %do;
	PROC PRINT DATA=_XV noobs;
		VAR _V1-_V&_cntv. ;
	%if (%length(&tabyvar.)) %then %do;
		BY &tabyvar.;
		PAGEBY &tabyvar.;
		%end;
	TITLE "Output from textable: SAS Preview of final table";
	TITLE2 "Table &tabname.: &caption. -- &tabid.";
	TITLE3 "NOTE: Column headers are not set up, and probably look wrong";
	TITLE4 "To produce final LaTeX table, ensure that _rsltab=2 or 3";
	RUN;
%end;

%if (&_rsltab. = 2 | &_rsltab. = 3) %then %do;
PROC TRANSPOSE DATA=_XV OUT=_XXV(RENAME=(A1=_ZX)) PREFIX=A;
     BY &tabyvar. _CNT _POS;
	%if (&_cntv. > 1) %then %do;
			VAR _V1-_V&_cntv.;
	%end;
	%else %do;
			VAR _V1;
	%end;
	RUN;

%let _ccnt=0;
DATA FINAL;
     LENGTH _OP_ $5 JS $1 TX CAPX TABN $200;
     KEEP _OP_ _NM_ _TY_ TX JS NC &tabyvar.;
     RETAIN XJUST;
     SET _XXV END=EOF;
	%if (%length(&tabyvar.)) %then %do;
		BY &tabyvar.;
		IF (FIRST.&tabyvar.) THEN DO;
	%end;
	%else %do;
		IF (_N_ = 1) THEN DO;
	%end;
        XJUST="&just.";
        CAPX="&caption.";
		  TABN="&tabname.";
%if (%length(&tabyvar.)) %then %do;
        BYCX+1;
%if (%length(&tabyfmt.)) %then %do;
        CAPX=TRIM(CAPX)||PUT(&tabyvar.,&tabyfmt.);
%end;
%else %do;
        CAPX=COMPRESS(TRIM(CAPX)||&tabyvar.);
%end;
        TABN=TRIM(TABN)||COMPRESS(PUT(BYCX,3.));
%end;
        %_tzout(_t=TRIM(CAPX));
        %_tlout(_t=TRIM(TABN));
        %do _i=1 %to &_cntv.;
           %if (%length(&_strx.)<=0) %then %do;
              %_clout(_j=%substr(&just.,&_i.,1));
           %end;
           %else %do;
              %* put scan result[%scan(&_strx.,&_i.,~)];
              %if ("%scan(&_strx., &_i.,~)"="_S_") %then %do;
                 %_clout(_j=%substr(&just.,&_i.,1));
              %end;
              %else %do;
                 %_clout(_j=%substr(&just.,&_i.,1),
                         _t="%scan(&_strx., &_i.,~)");
              %end;
           %end;
        %end;
        %_hlout;
        %do _i=1 %to 10;
           zcnt=0;
           %if (%length(&&head&_i.) > 1) %then %do;
              %put head&_i[&&head&_i.];
              %let _j=1;
              %do %while(%length(%scan(&&head&_i.., %eval(&_j), ~)));
                 %let _px=%scan(&&head&_i..,&_j.,~);
                 %let _pa=;%let _pc=;%let _pb=;%let _pd=;
                 %if (%length(&_px.) >= 2) %then %do;
                    %let _pa=%substr(&_px., 1, 1); %end;
                 %if (%length(&_px.) >= 3) %then %do;
                    %let _pb=%substr(&_px., 2, 2); %end;
                 %if (%length(&_px.) >= 6) %then %do;
                    %let _pc=%substr(&_px., 4, 3); %end;
                 %if (%length(&_px.) >= 4) %then %do;
                    %let _pd=%substr(&_px., 4,
                       %eval(%length(&_px.)-3));     %end;
%*put   _j[&_j.],_px[&_px.],_pa[&_pa.],_pb[&_pb.],_pc[&_pc.],_pd[&_pd.];
                 %if ("&_pa." = "#") %then %do;
                    %if ("&_pc." ^= "_S_") %then
                       %_txout(_j=c,_t="&_pd.",_n=&_pb.);
                    %else
                        %_skout(_n=&_pb.);
                    zcnt+&_pb.;
                 %end;
                 %else %do;
                    %if ("&_pa." = "!") %then %let _px=&_pd.;
                    zcnt+1;
                    %if ("%scan(&&head&_i.,&_j.,~)"^="_S_") %then %do;
                       justv=substr(xjust, zcnt, 1);
                       %_txout(_j=justv, _jt=0,
                               _t="&_px.",
                               _n=1);
                    %end;
                    %else %do;
                       %_skout(_n=1);
                    %end;
                 %end;
                 %let _j=%eval(&_j.+1);
              %end;
           %end;
        %end;
        %_hlout;
        xjust="&just.";
        zcnt=0;
     end;
     if (_name_ = "_V1" & _POS > 0 & _N_ > 0) THEN
        DO Z=1 TO _POS;
		  %_hlout;
		  END;
     ZCNT+1; JUSTV=SUBSTR(XJUST, ZCNT, 1);

     IF (ZCNT = &_cntv.) THEN ZCNT=0;
     %_txout(_n=1, _t=_zx, _j=justv, _jt=0);
	%if (%length(&tabyvar.)) %then %do;
		IF (LAST.&tabyvar.) THEN DO;
	%end;
	%else %do;
		IF (EOF) THEN DO;
	%end;
%_hlout;
%if (%length(&tail1.)) %then %do;
       %_moout(_n=%eval(&_cntv.+1), _t="\strut");
       %_poout(_t="t-&tlwidth.");
%do _i=1 %to 10;
	%if (%length(&&tail&_i.)) %then %do;
		%put Checking _pcout: tail&_i[&&tail&_i.];
		%_pcout(_t="&&tail&_i.");
		%end;
	%end;
        %_peout;
		  %_meout;
		  %_hlout;
%end;
     END;
RUN;

%_table(_byntc=&_byntc.,
        tabyfile=&tabyfile.,
        tabyvar=&tabyvar.,
        _dset=final,
        _extra=&_ext.,
        outfile=&outfile.,
        ext=&ext.,
        locate=&locate.,
        _nc=&_cntv.,
        dirsep=&_zepp.,
        _ststrx=&_ststrx.,
        outdir=&outdir.,
        subdir=&subdir.,
        _sw=&_sw.,
        tabenv=&tabenv.,
        tabid=&tabid.);
%end;
%mend textable;

%macro _table(_byntc=1,
              tabyfile=,
              tabyvar=,
              _dset=,
              _extra=0,
              outfile=,
              ext=,
              locate=H,
              _nc=,
              dirsep=/,
              _ststrx=,
              outdir=,
              subdir=,
              _sw=,
              tabenv=,
              tabid=);

%let outfile=%scan(&outfile,1,.);
proc sort data=&_dset.;
	by &tabyvar. _ty_ _nm_;
	run;

%if (&_nc. = 0 | %length(&_nc.) = 0) %then %do;
	proc means data=&_dset. noprint;
		var nc;
		where (_ty_ = 3 & nc = 1);
		output out=redox sum=totcol;
		run;
	data _null_;
		set redox;
		call symput("_nc", compress(put(totcol, 6.)));
		run;
%end;

%put ********************************************************************* *;
%put _nc[&_nc.],_byntc[&_byntc.],tabyfile[&tabyfile.],tabyvar[&tabyvar.];
%put ********************************************************************* *;

%* ****************************************************************** *;
%* _table                                                             *;
%*                                                                    *;
%* Protocol                                                           *;
%* outdir     - main subdirectory to place final file on            *;
%* subdir     - specific subdirectory, under main, for files        *;
%* outfile    - file name on subdirectory                           *;
%* ext        - file extension                                      *;
%* _prvars    - variable list to print                              *;
%* dirsep     - /                                                   *;
%* _dset        - SAS dataset with data to be set up                  *;
%* locate      - location code - LaTeX table parameter               *;
%* _nc        - number of columns for table - computed in MEANS       *;
%*                                                                    *;
%* This macro was inspired by F. Poppe, who wrote a file which        *;
%* formatted tables in plain TeX.  This macro formats tables in       *;
%* LaTeX, using various LaTeX constructs.                             *;
%*                                                                    *;
%* The macro reads a dataset &_dset.  This dataset has 4 variables    *;
%* plus optional BY variables, which must be specified in the macro   *;
%* call.                                                              *;
%* 1. NC      - Number of columns                                     *;
%* 2. TX      - Text which goes in the column                         *;
%* 3. _TY_    - Entry type (entry may be indicated with initials      *;
%*    1       - Title (placed in caption)                             *;
%*    2       - Table reference (in label)                            *;
%*    3       - Table definition                                      *;
%*    4       - Normal table text                                     *;
%* 4. _NM_    - number within _TY_                                    *;
%* 5. JS      - Justification of entry.  During _TY_="DEFINE",        *;
%*              default justification is set.  If justification for   *;
%*              for the entry is different later, multicolumn will    *;
%*              be used to set the correct justification              *;
%* 6. _OP_    - Operation                                             *;
%*    e       - End of the line (hard space) - output "\\"            *;
%*    h       - Place a horizontal line (\hline) at this point        *;
%*    c       - Place a horizontal line (\cline) at this point        *;
%*              The value of TX will indicate the columns             *;
%*    s       - Skip a column or columns, defined in the NCOL spec    *;
%*    mo      - multicolumn open - starts a multicolumn specification *;
%*    me      - multicolumn end                                       *;
%*    po      - parbox open - enter location and size in TX           *;
%*              as "t-5in" (top-5 inch)                               *;
%*    pc      - parbox continue                                       *;
%*    pe      - parbox end - TX is placed on file - "}" entered too   *;
%*                                                                    *;
%*                                                                    *;
%*                                                                    *;
%* Basic approach                                                     *;
%* _TY_ = 1 - enter caption                                           *;
%* _TY_ = 2 - set up label                                            *;
%*                                                                    *;
%* _TY_ = 3                                                           *;
%* In this section, the basic table is defined.  Each item defines    *;
%* the specification for one column.  If NCOL = 1, the item defines   *;
%* a column.  If NCOL = 0, the item defines either a vertical line    *;
%* at the right of the previous column (if TX = "|") or an            *;
%* @-expression (TX is placed between "@{" and "}").                  *;
%*                                                                    *;
%* _TY_=4                                                             *;
%* In this section, the items are placed into columns in the order    *;
%* in which they are received.  The NCOL specification determines the *;
%* number of columns which will be used -if NCOL > 1, a multicolumn   *;
%* environment will be used to frame the tx.  At the end of each      *;
%* item except the item at the end of the line, an "&" will be        *;
%* written to the file.  The items will be counted, and a "\\" will   *;
%* be written to the file when the number of items in the table is    *;
%* encountered, or _TY_="END" is found.                               *;
%*                                                                    *;
%* Error conditions                                                   *;
%* o If an incorrect number of items is output, a NOTE will be output *;
%* o If a multicolumn item spans the end of a line, a NOTE will be    *;
%*   output                                                           *;
%* o                                                                  *;
%*                                                                    *;
%*                                                                    *;
%* ****************************************************************** *;
%let _fx=;
%if (%length(&outdir)) %then %let _fx=&outdir.&dirsep.;
%if (%length(&subdir)) %then %let _fx=&_fx.&subdir.&dirsep.;
%if (&_byntc. = 0) %then %let _byntc=1;

%if (&tabyfile.) %then %do;
	%do _i=1 %to &_byntc.;
		FILENAME XF&_i. "&_fx.&outfile.&_i..&ext.";
		%end;
	%end;
%else %do;
	FILENAME XF1 "&_fx.&outfile..&ext.";
	%end;

DATA _RNULLX ;
   SET &_dset. END=EOF;
	%if (%length(&tabyvar.)) %then %do;
		BY &tabyvar.;
	%end;;
     TNCOL=&_nc.;
     RETAIN STRTFLG SETFLG 1 MULTIOP PAROP FCNT 0;

	%if (%length(&tabyvar.)) %then %do;
		IF (FIRST.&tabyvar.) THEN DO;
	%end;
	%else %do;
		IF (_N_ = 1) THEN DO;
	%end;
        FCNT+1;
	%if (%length(&tabyvar.) and &tabyfile) %then %do;
	%do _i=1 %to &_byntc.;
		IF (FCNT = &_i.) THEN FILE XF&_i.;
	%end;
	%end;
	%else %do;
		file xf1;
	%end;

	%if &texalone %then %do;
		if _n_=1 then do;
		PUT "\documentclass{article}";
		put "\begin{document}";
		end;
	%end;

        STRTFLG=1; SETFLG=1;
	%if (%length(&tabyvar.)) %then %do;
		PUT "%% &tabid." &tabyvar. &tabyfmt.;
	%end;
	%else %do;
				PUT "%% &tabid.";
	%end;
	%if ("&locate." ^= " ") %then %let locate=[&locate.];
        PUT "\begin{&_sw.table}&locate.";
	%if (%length(&tabenv.)) %then %do;
        PUT "{";
        PUT "&tabenv.";
	%end;
     END;

	%if (%length(&tabyvar.) and &tabyfile) %then %do;
	%do _i=1 %to &_byntc.;
     IF (FCNT = &_i.) THEN FILE XF&_i.;
	%end;
	%end;
	%else %do;
		file xf1;
	%end;

     IF (_TY_ = 1) THEN DO;
	  	PTX=" \caption{"||TRIM(TX)||"}";
		PUT PTX;
		END;
     IF (_TY_ = 2) THEN DO;
	  PTX=" \label{"||TRIM(TX)||"}";
	  PUT PTX;
	  END;
     IF (STRTFLG & _TY_ >= 2) THEN DO;
        STRTFLG=0;
        PUT " \begin{center}";
        PUT "  \begin{tabular}{" @;
%if (%length(&_ststrx.)) %then PUT "&_ststrx." @;;
        TI=0;
     END;

     ARRAY COLJUST {&_nc.} $ 1 _TEMPORARY_;
     ARRAY COLBAR  {&_nc.}     _TEMPORARY_;

     IF (_TY_ = 3) THEN DO;
%* ****************************************************************** *;
%* This is the define section - check conditions for define here      *;
%* after the define section is finished                               *;
%*                                                                    *;
%* If nc = 1, standard column                                         *;
%* If nc = 0, define some special column specifications               *;
%* Otherwise an error is raised                                       *;
%*                                                                    *;
%* coljust - justification for column                                 *;
%* colbar  - place bar after this column                              *;
%* ****************************************************************** *;
        IF (NC = 1) THEN DO;
           PUT JS +(-1) @;
           TI+1;
           IF (TX ^= " " & LENGTH(TX) > 0) THEN DO;
              IF (TX ^= "|") THEN DO;
                 TX="@{"||TRIM(TX)||"}";
                 COLBAR{TI}=0;
              END;
              ELSE DO;
                 TX="|";
                 COLBAR{TI}=1;
              END;
           END;
           COLJUST{TI}=JS;
        END;
        ELSE IF (NC = 0) THEN DO;
           IF (TX = "|") THEN DO;
              IF (TI > 0) THEN
                 COLBAR{TI}=1;
           END;
           ELSE TX="@{"||TRIM(TX)||"}";
        END;
        ELSE ERROR "nc > 1 during define - invalid";
        IF (TX ^= "") THEN PUT TX +(-1) @;
        IF (TI = &_nc. & &_extra.) THEN PUT "l" @;
     END;
     ELSE IF (_TY_ = 4) THEN DO;
%* ****************************************************************** *;
%* This is the table body section                                    *;
%* First check to see if this is the first item - finish tabular     *;
%* ****************************************************************** *;
        IF (SETFLG) THEN DO;
           PUT "}";
           SETFLG=0;
           TI=0;
        END;
%* ****************************************************************** *;
%* Now increment ti - counter for columns                            *;
%* ****************************************************************** *;
        IF (_OP_ ^= "h" & _OP_ ^= "pe" & _OP_ ^= "pc" &
            _OP_ ^= "me") THEN TI+NC;
        IF (_OP_ = "c") THEN DO;
%* ****************************************************************** *;
%* Here set up a cline specification                                 *;
%* ****************************************************************** *;
           PTX="  \cline{"||trim(tx)||"}";
           PUT PTX;
           OSEC=SCAN(TX, 1, "-");
           TIX=INPUT(OSEC,5.);
           IF (TI > TIX) THEN ERROR "Current column is beyond start of cline";
           OSEC=SCAN(TX, 2, "-");
           TI=INPUT(OSEC,5.);
           IF (TI > &_nc.) THEN ERROR "Invalid final column for cline";
        END;
        ELSE IF (_OP_ = "e") THEN DO;
%* ****************************************************************** *;
%* Here end of line specification                                    *;
%* ****************************************************************** *;
           PUT "\\"; TI=0;
        END;
        ELSE IF (_OP_ = "h") THEN DO;
%* ****************************************************************** *;
%* Here set up a hline specification                                 *;
%* ****************************************************************** *;
           IF (TI <= 1) THEN DO;
			  	PUT "  \hline"; TI=0; END;
           ELSE ERROR "Issuing \hline specification in middle of line";
        END;
        ELSE IF (_OP_ = "s") THEN DO;
%* ****************************************************************** *;
%* Here set up a column skip specification                           *;
%* ****************************************************************** *;
           ptx="\multicolumn{"||put(nc,2.)||"}{c}{}";
           put ptx @;
        end;
        else if (_op_ = "pc") then do;
           put tx;
        end;
        else if (_op_ = "pe" | _op_ = "me") then do;
           put tx "}";
           if (_op_ = "me") then do;
              if (multiop) then multiop=0;
              else ERROR "Multiline multicolumn close issued when not open";
           end;
           if (_op_ = "pe") then do;
              if (parop) then parop=0;
              else ERROR "Multiline parbox close issued when not open";
           end;
        end;
        else if (_op_ = "mo") then do;
/*         ptx="\multicolumn{"||put(nc,2.)||"}{"||
*             trim(js)||"}{"||trim(tx);              */
           ptx="\multicolumn{"||put(nc,2.)||"}{l"||
              "}{"||trim(tx);
           put ptx;
           if (multiop) then
              ERROR "Opening second multiline multicolumn specifications";
           multiop=1;
        end;
        else if (_op_ = "po") then do;
           locat=scan(tx, 1, "-");
           if (length(locat) > 0) then
              ptx="\parbox["||trim(locat)||"]";
           else ptx="";
           ptx=trim(ptx)||"{"||trim(scan(tx,2,"-"))||"}{";
           put ptx;
           if (parop) then
              ERROR "Opening second multiline parbox specifications";
           PAROP=1;
        END;
        ELSE DO;
           IF (NC > 1 |
               (COLJUST{TI} ^= JS & JS ^= "")) THEN DO;

              IF (COLBAR{TI}) THEN
                 PTX="\multicolumn{"||PUT(NC,2.)||"}{"||
                    TRIM(JS)||"|"||"}{"||TRIM(TX)||"}";
              ELSE
                 PTX="\multicolumn{"||PUT(NC,2.)||"}{"||
                    TRIM(JS)||"}{"||TRIM(TX)||"}";
              PUT PTX @;
           END;
           ELSE PUT TX @;
        END;
        IF (MULTIOP | PAROP) THEN;
        ELSE IF (0 < TI < &_nc.) THEN PUT "& " @; 	/**?**/
        ELSE DO; IF (_OP_ ^= "h") THEN PUT "\\";
           TI=0; END;
     END;

	%if (%length(&tabyvar.)) %then %do;
	IF (LAST.&tabyvar.) THEN DO;%end;
	%else %do;                        IF (EOF   ) THEN DO;       %end;
        PUT "  \end{tabular}" / " \end{center}";
	%if (%length(&tabenv.)) %then %do;
        PUT "}";
	%end;
        PUT "\end{&_sw.table}";
     end;
	%if &texalone %then %do;
		if eof then put "\end{document}";
	%end;
run;
%mend _table;

%macro _itemcnt(_items=a b, _delim=1, _n=_nit);
%* ************************************************************ *;
%* This macro counts the number of items in a macro var
%* ************************************************************ *;
%let _delx=%str( ,*\~);%let &_n.=0;
%let _ksep=%quote(%substr(&_delx., &_delim., 1));
%do %while(%length(%scan(%str(&_items.),%eval(&&&_n.+1), %str(&_ksep.))) > 0);
	%let &_n.=%eval(&&&_n. + 1);
	%end;
%mend _itemcnt;

%macro _fillp(_parm=_f, _tilda=1);
%* ****************************************************************** *;
%* _fillp - macro which unpacks information about multipliers
%* and builds up the correctly multiplied values
%*
%* Note to macro readers - the three & specification is correct
%* If you change it to 2 or 4, it will screw it up
%* That is because we call _fillp with the NAME of a macro, not
%* a macro argument.
%*
%* Protocol
%* _parm  - name of the macro variable with possible values to
%*          be unpacked
%* _tilda - whether the tilda separator is to be included
%*          in the repeated arguments - 1 yes 0 no
%*
%* Logic - Use @ specifications to find the mult + string
%*         Assume that multiplier is first two columns
%*         Keep the info before the repeated section as _fa
%*         Keep the info after  the repeated section as _fr
%*         Build up repeats in _fm
%*         The final result is _parm again
%* ****************************************************************** *;
%if (&_tilda.) %then %do; %let _b=1;%let _sstr=~@; %end;
%else                %do; %let _b=0;%let _sstr=@;  %end;
%* put * ******************************************************** *;
%do %while(%index(&&&_parm.,&_sstr.));
	%put  _fillp: &_parm[&&&_parm.] (before);
	%let _fa=%substr(&&&_parm., 1, %eval(%index(&&&_parm.,&_sstr.)-1+&_b.));
	%let _fx=%substr(&&&_parm.,    %eval(%index(&&&_parm.,&_sstr.)+3+&_b.));
	%let _fcnt=%substr(&&&_parm.,  %eval(%index(&&&_parm.,&_sstr.)+1+&_b.), 2);
	%put  _fillp: _fcnt[&_fcnt.] (multiplier);
	%if (%length(&_fx.) > %index(&_fx.,@)) %then
	%let _fr=%substr(&_fx.,       %eval(%index(&_fx.,@)+1));
	%else %let _fr=;
	%let _fb=%substr(&_fx., 1,    %eval(%index(&_fx.,@)-1));
	%let _fm=;
	%do _i=1 %to &_fcnt.;
		%let _fm=&_fm.&_fb.;
		%if (&_tilda.) %then %if (&_i. < &_fcnt.) %then %let _fm=&_fm.~;
		%end;
	%let &_parm=&_fa.&_fm.&_fr.;
	%put  _fillp: &_parm[&&&_parm.] (after);
	%end;
%mend _fillp;

%macro _hlout;
%* ****************************************************************** *;
%* This specification creates an observation which outputs a line    *;
%* ****************************************************************** *;
     do;
	  nc=1;
	  _nm_+1; js=""; _op_="h"; _ty_=4; tx=""; output;
	  end;
%mend _hlout;

%macro _skout(_n=1);
%* ****************************************************************** *;
%* This specification creates an observation which skips _nc cols  *;
%* ****************************************************************** *;
     do;
	  nc=&_n.; _nm_+1; js=""; _op_="s"; tx=""; _ty_=4; output;
	  end;
%mend _skout;

%macro _txout(_n=1, _t=" ", _j=l, _jt=1);
%* ****************************************************************** *;
%* This specification creates an obs with the VALUE of _t            *;
%* That is, whatever _t is is just put into the variable TX        *;
%* ****************************************************************** *;
do; nc=&_n.; _ty_=4; _nm_+1;
	%if (&_jt. = 1) %then %do;
		js="&_j."; 
		%end;
	%else %do;
		js=&_j.;
		%end;
	_op_=""; tx=&_t.; output;
	end;
%mend _txout;

%macro _tlout(_n=1, _t=" ", _j=l);
%* ****************************************************************** *;
%* This specification creates an label specification                 *;
%* That is, whatever _t is is just put into the variable TX        *;
%* ****************************************************************** *;
do;
	nc=&_n.;
	_ty_=2; _nm_+1; js="&_j."; _op_=""; tx=&_t.; output;
	end;
%mend _tlout;

%macro _tzout(_n=1, _t=" ", _j=l);
%* ****************************************************************** *;
%* This specification creates an title specification                 *;
%* That is, whatever _t is is just put into the variable TX        *;
%* ****************************************************************** *;
	do;
		nc=&_n.; _ty_=1; _nm_+1; js="&_j."; _op_=""; tx=&_t.; output;
	end;
%mend _tzout;

%macro _clout(_j=l, _t=" ");
%* ****************************************************************** *;
%* This specification creates an obs which sets up a column, and     *;
%* which converts _t into tx in terms of VALUE, not MEANING        *;
%* ****************************************************************** *;
     do;
		  nc=1; _nm_+1; js="&_j."; _op_=""; _ty_=3; tx=&_t.; output;
	  end;
%mend _clout;

%macro _moout(_t=" ", _n=1);
%* ****************************************************************** *;
%* This specification creates an obs which sets up a column, and     *;
%* which converts _t into tx in terms of MEANING not VALUE         *;
%* ****************************************************************** *;
     do; nc=&_n.; _nm_+1; js=""; _op_="mo"; _ty_=4; tx=&_t.; output; end;
%mend _moout;
%macro _pcout(_t=" ");
%* ****************************************************************** *;
%* This specification creates an obs which sets up a column, and     *;
%* which converts _t into tx in terms of MEANING not VALUE         *;
%* ****************************************************************** *;
     do; nc=1; _nm_+1; js=""; _op_="pc"; _ty_=4; tx=&_t.; output; end;
%mend _pcout;

%macro _poout(_t=" ");
%* ****************************************************************** *;
%* This specification creates an obs which sets up a column, and     *;
%* which converts _t into tx in terms of MEANING not VALUE         *;
%* ****************************************************************** *;
     do; nc=1; _nm_+1; js=""; _op_="po"; tx=&_t.; _ty_=4; output; end;
%mend _poout;

%macro _meout(_t=" ");
%* ****************************************************************** *;
%* This specification creates an obs which sets up a column, and     *;
%* which converts _t into tx in terms of MEANING not VALUE         *;
%* ****************************************************************** *;
     do; nc=1; _nm_+1; js=""; _op_="me"; tx=&_t.; _ty_=4; output; end;
%mend _meout;

%macro _peout(_t=" ");
%* ****************************************************************** *;
%* This specification creates an obs which sets up a column, and     *;
%* which converts _t into tx in terms of MEANING not VALUE         *;
%* ****************************************************************** *;
     do; nc=1; _nm_+1; js=""; _op_="pe"; tx=&_t.; _ty_=4; output; end;
%mend _peout;
options source;

     /* Example
data chk;
do k=1 to 3;
	do i=1 to 5;
	subj="Sub"||put(i,2.);
	array vls v1-v3;
	do j=1 to 3;
		vls{j}=uniform(98232)*3;
		end;
	output;
	end;
end;
run;

 data chk2;set chk(in=ina) chk(in=inb);
 tabv=ina+inb*2;run;

 proc format ;value gfmt 1="Group A" 2="Group B" 3="Group C";
              value tabvfmt 1="File with main"
                            2="Additional information";run;
 %_maktab;
 %macro _runit;
 %do _zzi=1 %to 2;
 %textable(byvar=k,
          byhline=2,
 	%if (&_zzi. = 1) %then %do;
          caption=%str(Example table, random values),
          outfile=redoa,
          data=chk,
	%end;
	 %if (&_zzi. = 2) %then %do;
          caption=%str(Separate tables:),
          tabyfile=1,
          tabyvar=tabv,
          tabyfmt=tabvfmt,
          outfile=redob,
          data=chk2,
	 %end;
          ext=tex,
          formats=gfmt.~$~@036.3@,
          head1=#05Overall table header,
          head2=_S_~Subject~#03Random Quantities,
          head3=#02_S_~#03With values specified,
          head4=_S_~_S_~Value 1~Value 2~Value 3,
          locate=H,
          just=rrccc,
          miss=0,
          ncol=,
          dirsep=\,
          sideway=0,
          _strx=,
          outdir=,
          subdir=,
          tabenv=\small,
          tabid=textable example:full use,
          tabname=tab:exmp,
          tail1=Note: Information in this table defined,
          tail2=according to Freedom of Information Act,
          tail3=specifications,
          tlwidth=4in,
          var=k~subj~v1~v2~v3);
 %end;
 %mend  _runit;
 */
%*  %_runit;
  /*     */
