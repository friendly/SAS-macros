%macro dumpsas( data=_LAST_        ,   /* Sas Data Set name            */
                fileref=DUMP       ,   /* SAS Fileref/DDname           */
                linesize=, ls=80   ,   /* Linesize of external file    */
                width=9            ,   /* Default Field width          */
                compare=NO         ,   /* Add code to compare datasets */
                special=           ,   /* Vars needing wider fields    */
                specwid=24         );  /*   Field width for above      */
 /* This SAS macro creates a data file containing the data from a      */
 /* SAS Data set.  The SAS statements necessary to re-create the       */
 /* SAS Data set precede the data lines.                               */
 /*                                                                    */
 /* The macro parameters are:                                          */
 /*                                                                    */
 /* DATA=          Names the SAS Data set to dump from                 */
 /* FILEREF=       Name the SAS Fileref identifing the external file   */
 /*                to contain the data.  CMS users must include the    */
 /*                DISP MOD option on the FILEDEF command defining the */
 /*                enternal file.  On some operating systems, the      */
 /*                external file name may be used instead of the       */
 /*                fileref (CMS, PC DOS).                              */
 /*                If not specified, the macro creates the fileref.    */
 /* LINESIZE=      Specifies the linesize of the external data file.   */
 /*                This value MUST be greater than or equal to the     */
 /*                length of the longest character variable.           */
 /*                You may also need to include the linesize           */
 /*                information on the operating system command used    */
 /*                to allocate the external file.                      */
 /* WIDTH=         Specifies the default field width for numeric       */
 /*                variables.                                          */
 /* COMPARE=       If COMPARE=YES, additional SAS statements are       */
 /*                included in the file that allow the reconstructed   */
 /*                SAS Data set to be compared with the original.      */
 /*                Variables that do not compare are usually the       */
 /*                result of a field width too small to maintain       */
 /*                precision.                                          */
 /* SPECIAL=       Specifies a list of numeric variable names that     */
 /*                require a wider field in order to maintain precision*/
 /* SPECWID=       Specifies the field width used for the variables    */
 /*                listed in the SPECIAL= parameter.                   */
 /*                                                                    */
 /*  Written by:   Kevin Thompson   Agric. Stat. Lab.   Univ of Ark.   */
 /*                KT227032@UAFSYSB.BITNET              8Sep88         */
 
%let data    = %upcase( &data );
%let fileref = %upcase( &fileref );
%let compare = %upcase( &compare );
%let special = %upcase( &special );
%if %index( &data, _ALL_ ) > 0 %then %do;      /* DataSet _ALL_ not    */
    %put ERROR:  Dataset: _ALL_ not allowed.;  /* allowed              */
    %goto exit;
    %end;
%if &linesize = %then %let linesize = &ls;     /* Abbrev for LINESIZE= */
 
 /* Allow FileName instead of FileRef ******************************** */
%let fileid = &fileref;
%if XXXXXXXXX ^= %scan( &fileref XXXXXXXXX, 2) %then %do;
    %let fileref = DUMP;
    %if &sysscp=CMS %then %do;
        cms erase &fileid;
        cms filedef &fileref disk &fileid
            (disp mod block &linesize lrecl &linesize  recfm v);
        %end;
    %if &sysscp=PC DOS | &sysscp=WIN | &sysscp=OS2 %then %do;
        x "erase &fileid";
        filename &fileref "&fileid";
        %end;
    %end;
options dquote;
 /* Obtain Variable Information ************************************** */
proc contents data=&data out=work._conten_ noprint memtype=data;
run;
proc sort data=work._conten_;
     by npos;
proc print data=work._conten_;
     var name type length label format formatl formatd
              informat informl informd;
run;
 /* Print DATA Stmt,  ATTRIB Stmts *********************************** */
data work._conten_;
     file &fileref linesize=&linesize;
     length vtype $1  fmtl fmtd ifmtl ifmtd $4  datafmt fmt ifmt $15
            dsname $17 memlabel label $42;
     drop fmt fmtl fmtd ifmt ifmtl ifmtd len fmtflag vn vl vc;
     set work._conten_;
 
     /* Print DATA Stmt ********************************************** */
     if _N_ = 1 then do;
        dsname = compress( libname || "." || memname );
        call symput( "DATA", dsname );
        put @1 "DATA " @;
        %if &compare = YES %then %do;
            put " /*" @;
            %end;
        put dsname  @;
        if memtype ^= " " then typemem=memtype;/* Fix for CMS SAS 5.16 */
        if typemem ^= " " or memlabel ^= " " then do;
           put "( " @;
           if typemem  ^= " " then put "type=" typemem @;
           if memlabel ^= " " then do;
              memlabel = "'" || trim(memlabel) || "'";
              put  "label=" memlabel @;
              end;
           put ")" @;
           end;
        %if &compare = YES %then %do;
            put " */" @;
            %end;
        put ";";
        end;
 
     /* Calculate field width of variable ***************************/
     datafmt  = informat;
     datafmtl = informl;
     datafmtd = informd;
 
     if datafmt = ' ' then select ( format );
        when ( "DATE"     ) datafmt =   "DATE";
        when ( "DATETIME" ) datafmt =   "DATETIME";
        when ( "DDMMYY"   ) datafmt =   "DDMMYY";
        when ( "MMDDYY"   ) datafmt =   "MMDDYY";
        when ( "WEEKDATE" ) datafmt =   "MMDDYY";
        when ( "WORDDATE" ) datafmt =   "MMDDYY";
        when ( "MONYY"    ) datafmt =   "MONYY";
        when ( "TIME"     ) datafmt =   "TIME";
        when ( "HHMM"     ) datafmt =   "TIME";
        when ( "HOUR"     ) datafmt =   "TIME";
        when ( "MMSS"     ) datafmt =   "TIME";
        when ( "TOD"      ) datafmt =   "DATETIME";
        when ( "YYMMDD"   ) datafmt =   "YYMMDD";
        when ( "YYQ"      ) datafmt =   "YYQ";
        otherwise;
        end;
     if datafmt ^= informat then do
        datafmtl = 0;
        datafmtd = 0;
        end;
 
     if datafmtl = 0 then select ( datafmt );
        when ( "DATE"     ) datafmtl = 7;
        when ( "DATETIME" ) datafmtl =17;
        when ( "DDMMYY"   ) datafmtl = 8;
        when ( "MMDDYY"   ) datafmtl = 6;
        when ( "MONYY"    ) datafmtl = 5;
        when ( "TIME"     ) datafmtl = 8;
        when ( "YYMMDD"   ) datafmtl = 8;
        when ( "YYQ"      ) datafmtl = 4;
        otherwise;
        end;
 
     if type = 1
        then do;
             vtype = " ";
             if datafmt = " " then do;
                datafmt = "BEST";
                datafmtl = 0;
                end;
             if datafmtl > 0
                then width = datafmtl;
                else width = &width;
             if index( " &special ", " " || trim(name) || " " ) then do;
                datafmt = "BEST";
                datafmtl = 0;
                width = &specwid;
                end;
             end;
        else do;
             vtype = "$";
             if datafmt = " " then datafmt = "CHAR";
             width = length;
             datafmtl = length;
             end;
     if datafmtl = 0 then datafmtd = 0;
     datafmt = compress( vtype || datafmt || put(width,3.) || ".");
     if datafmtd > 0 then datafmt = compress( datafmt || put(datafmtd,3.) );
     if width > &linesize then do;
        file log;
        put "ERROR: The field width of the variable '" name "'> linesize.";
        put "ERROR:     Set LINESIZE= parameter larger than  " width ;
        file &fileref;
        put "if _N_ = 1 then error 'ERROR: The field width of the variable "
             name "> linesize.';" ;
        end;
 
     /* Calculate Line #, Column # of data **************************/
     retain line 0 col1 col2 &linesize;
     if (col2 + 1 + width) > &linesize
        then do;
             line = line + 1;
             col1 = 1;
             end;
        else col1 = col2 + 2;
     col2 = col1 + width - 1;
 
     /* Print ATTRIB Stmt *******************************************/
     put @6   "attrib " name $8. @;
 
     len = compress( vtype || put(length,3.) );
     put @22 "length=" len  $4. @;
 
     put @48 "/* line" line 3. "  col" col1 3. "-" col2 3. " */" @;
 
     if formatl = 0 then fmtl = "   ";
                    else fmtl = put(formatl,3.);
     if formatd = 0 then fmtd = "   ";
                    else fmtd = put(formatd,3.);
     fmt  = compress( format || fmtl );
     fmtflag = 0;
     if fmt ^= " " then do;
        fmt = compress( vtype || fmt || "." || fmtd );
        put / @13 "format=" fmt @;
        fmtflag = 1;
        end;
 
     if informl = 0 then ifmtl = "   ";
                    else ifmtl = put(informl,3.);
     if informd = 0 then ifmtd = "   ";
                    else ifmtd = put(informd,3.);
     ifmt = compress( informat || ifmtl );
     if ifmt ^= " " then do;
        ifmt  = compress( vtype || ifmt || "." || ifmtd );
        if fmtflag then put   @38 "informat=" ifmt @;
                   else put / @38 "informat=" ifmt @;
        end;
 
     if label ^= " " then do;
        label = "'" || trim(label) || "'";
        put / @13 "label=" label @;
        end;
     put ";" ;
 
     /* Post Variable information to MACRO Variables ****************/
     vn = compress( put( _N_,5.) );
     vl = compress( put(line,5.) );
     vc = compress( put(col1,5.) );
 
     call symput( "N", vn );
     call symput( "NAME" || vn , name );
     call symput( "LINE" || vn , vl );
     call symput( "COL"  || vn , vc );
     call symput( "FMT"  || vn , datafmt );
run;
  /* Print INPUT Stmt ***********************************************/
data _null_;
     retain col 12;
     file &fileref mod linesize=&linesize;
     set work._conten_ end=end;
     lastline = lag(line);
     atcol = compress( "@" || put(col1,4.) );
     if _N_ = 1
        then put @6 "input " @;
        else if line ^= lastline then do;
             put / @10 "/" @;
             col = 12;
             end;
     if col > 60 then do;
             put ;
             col = 12;
             end;
     put @col atcol $5. +1 name $8. +1 datafmt $15. @;
     col = col + 32;
     if end then put ";" / "cards;" ;
     drop col lastline atcol;
run;
 
  /* Print Data Lines ***********************************************/
data _null_;
     set &data;
     file &fileref mod linesize=&linesize n=&&line&n;
     put
     %do i = 1 %to &n;
         #&&line&i @&&col&i &&name&i &&fmt&i
     %end;
     ;
run;
%if &compare = YES %then %do;
data _null_;
     file &fileref mod linesize=&linesize;
     put "run;" ;
     put "PROC COMPARE DATA=&data COMPARE=_LAST_ NOLISTEQUAL "
         "METHOD=RELATIVE CRITERION=-10 ;" ;
     put "run;" ;
run;
%end;
%exit:
%mend dumpsas;
