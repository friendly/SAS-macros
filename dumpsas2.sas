 /* DUMPSAS SAS Macro  Version 2 ************************************* */
 /* This SAS macro creates a data file containing the data from a SAS  */
 /* Data set.  By default, the entire SAS data set is dumped in a form */
 /* sutiable for recreating the original SAS Data Set, including       */
 /* variable attribute information.  The SAS statements necessary for  */
 /* re-reading the data are also supplied.  The macro attempts to dump */
 /* date and time values in a readable format.                         */
 /* Optionally, you may use the VAR= option to select a subset of      */
 /* variables to be dumped, and the FORMAT= option to specify formats  */
 /* to use when writing the data.                                      */
 /*                                                                    */
 /*  Written by:   Kevin Thompson   Agric. Stat. Lab.   Univ of Ark.   */
 /*                KThompsn@Comp.UArk.Edu  KThompsn@UAFSysB.Bitnet     */
 
%macro dumpsas( data=_LAST_        ,   /* SAS Data Set name            */
                fileref=DUMP       ,   /* SAS Fileref/DDname for data  */
                SASref=            ,   /* SAS Fileref/DDname for stmts */
                linesize=80        ,   /* LS of external file (or MAX) */
                dfltfmt=BEST9.     ,   /* Default numeric format       */
                var=_ALL_          ,   /* Variable list                */
                format=            ,   /* Variable-Format list         */
                dlm=SPACE          ,   /* NONE, SPACE, DBLSPACE        */
                compare=NO         ,   /* Add code to compare datasets */
 /* The following options maintain some compatability with version 1   */
                ls=, width=, special= , specwid= );
 /*                                                                    */
 /* The macro parameters are:                                          */
 /*                                                                    */
 /* DATA=          Specifies the SAS Data set to dump.                 */
 /* FILEREF=       Specifies the SAS Fileref identifing the external   */
 /*                file to contain the data.  A host file name may be  */
 /*                used if it is embedded in single quote marks.       */
 /* SASREF=        Specifies the SAS Fileref identifing the external   */
 /*                file to contain the SAS statements.  If omitted, the*/
 /*                SAS statements precede the data in the data file.   */
 /* LINESIZE=      Specifies the linesize of the external data file.   */
 /*                This value MUST be greater than or equal to the     */
 /*                length of the longest character variable.           */
 /*                Specify the value  MAX  to cause the macro to put   */
 /*                all data values on a single line.                   */
 /*                You may also need to include the linesize           */
 /*                information on the operating system command used    */
 /*                to allocate the external file.                      */
 /* DFLTFMT=       Specifies the default numeric format for variables  */
 /*                not listed in the FORMAT= option.  BEST24. is       */
 /*                recommended to retain full precision.               */
 /* VAR=           Specifies the list of variables to be dumped.       */
 /*                If omitted, all variables will be used.  The macro  */
 /*                will attempt to dump the variables in the order     */
 /*                listed.                                             */
 /* FORMAT=        Lists selected variables followed by the format to  */
 /*                be used in dumping the data.  All formats MUST      */
 /*                include a format width value.  The format BEST24. is*/
 /*                recommended for variables which must retain full    */
 /*                precision.  If you use a format for which there is  */
 /*                no informat with the same name, you will need to    */
 /*                edit the generated SAS statements before you can    */
 /*                re-read the data.                                   */
 /* DLM=           NONE      places no spaces between data columns     */
 /*                SPACE     places one space between data values      */
 /*                DBLSPACE  places two spaces between data values     */
 /*                COMMA     places a comma between data values.  This */
 /*                          does not guarantee a valid comma delimited*/
 /*                          file.                                     */
 /* COMPARE=       If COMPARE=YES, additional SAS statements are       */
 /*                included in the file that allow the reconstructed   */
 /*                SAS Data set to be compared with the original.      */
 /*                Variables that do not compare are usually the       */
 /*                result of a field width too small to maintain       */
 /*                precision.                                          */
%let data    = %upcase( &data );
%let var     = %upcase( &var );
%let dlm     = %upcase( &dlm );
%let format  = %upcase( &format );
%let dfltfmt = %upcase( &dfltfmt );
%let compare = %upcase( &compare );
 /* Compatibility with Version 1 ************************************* */
%if &ls ^=
    %then %let linesize=&ls ;
%if &width ^=
    %then %let dfltfmt = BEST&width.. ;
%if &format = & &special ^= & &specwid ^=
    %then %let format= &special BEST&specwid.. ;
 /* Parameter Error checking ***************************************** */
%if &data=  %then %let data=_LAST_;
%if %index( &data, _ALL_ ) > 0 %then %do;
    %put ERROR:  Dataset: _ALL_ not allowed.;
    %goto exit;
    %end;
%if &fileref= %then %do;
    %put ERROR:  The FILEREF= option is required.;
    %goto exit;
    %end;
%if %index( &fileref, %str(%") ) > 0 %then %do;
    %put ERROR: Double Quote marks are invalid in the FILEREF= option.;
    %put        Use single quotes instead. ;
    %goto exit;
    %end;
%if %index( &fileref, %str(%') )=0  & %index( &fileref, %str(%") )=0
    %then %if %length( &fileref ) > 8 | %scan( &fileref, 1, %str(: /\.)) ^= &fileref
          %then %let fileref=%unquote(%str(%'&fileref%'));
%if &SASref ^= %then
%if %index( &SASref, %str(%') )=0  & %index( &SASref, %str(%") )=0
    %then %if %length( &SASref ) > 8 | %scan( &SASref, 1, %str(: /\.)) ^= &SASref
          %then %let SASref=%unquote(%str(%'&SASref%'));
%if &linesize= %then %let linesize=80 ;
 /* Obtain Variable Information ************************************** */
proc contents data=&data out=work._conten_ noprint memtype=data;
run;
proc sort data=work._conten_;
     by name;
quit;
 /* Save data set information **************************************** */
data _dsname_;
     length dsname $17;
     set work._conten_(obs=1);
     dsname = compress( libname || "." || memname );
     call symput( "DATA"    , dsname );
     call symput( "NOBS"    , put(nobs,f8.) );
     keep dsname libname memname typemem memlabel nobs ;
run;
 /* Error checking on the default numeric format ********************* */
data work._dfmt_;
     x=1;
     format x &dfltfmt;
run;
proc contents data=work._dfmt_ out=work._dfmt_ noprint memtype=data;
run;
data work._dfmt_;
        set work._dfmt_(keep= format formatl formatd
                        rename=(format=dfmt formatl=dfmtl formatd=dfmtd)
                         );
        if dfmtl=0 then do;
           put 'ERROR: Invalid default format. Using BEST12. instead';
           dfmt='BEST';
           dfmtl=12;
           dfmtd=0;
           call symput( "DFLTFMT" , 'BEST12.' );
           end;
run;
 /* Select variable subset and formats ******************************* */
data work._var_;
     set &data ;
     keep &var;
     format _numeric_ &dfltfmt _character_ ;
     format &format;
     stop;
run;
proc contents data=work._var_ out=work._var_ noprint memtype=data;
run;
proc sort data=work._var_;
     by name;
quit;
data work._var_;
     if _N_=1 then set work._dfmt_;
     set work._var_(keep=name format formatl formatd type length);
run;
 /* Error checking on specified formats ****************************** */
data work._var_;
     set work._var_;
     _emsg_ ="ERROR: The field width of the variable " ||
             trim(name) ||
             "is undefined. Using the default format instead.";
     drop _emsg_;
     if type=1 & format=' ' then format='F';
  /* Check for invalid numeric formats */
     if type=1 & formatl=0 then do;
        put _emsg_;
        format=dfmt;
        formatl=dfmtl;
        formatd=dfmtd;
        end;
  /* Check for invalid character formats */
     if type=2 then do;
        dfmt='$CHAR';
        dfmtl=max(1,length);
        dfmtd=0;
        if format='' then do;
           format = dfmt;
           formatl = dfmtl;
           formatd = dfmtd;
           end;
        if formatl=0 then do;
           put _emsg_;
           format = dfmt;
           formatl = dfmtl;
           formatd = dfmtd;
           end;
        if index(format,'$')=0 then format='$'||format;
        end;
run;
data work._conten_;
     merge work._conten_(keep=name varnum npos label
                              format formatl formatd informat informl informd
                         rename=(format=ofmt formatl=ofmtl formatd=ofmtd)
                         in=_indata_
                         )
           work._var_    (in=_invar_);
     by name;
     if _invar_ ;
     if not _indata_ then do;
        put 'ERROR: Variable ' name 'not found in dataset.';
        delete;
        end;
run;
 /* Reset formats for Date, Time, and DateTime variables ************* */
data work._conten_;
     set work._conten_;
     if type=1 & format=dfmt & formatl=dfmtl & formatd=dfmtd then do;
        if informat in ('DATE' 'DDMMYY' 'JULIAN' 'MMDDYY'
                        'MONYY' 'NENGO' 'YYMMDD' 'YYQ') then do;
                format='DATE';
                formatl=7;
                formatd=0;
                end;
        if informat in ('DATETIME') then do;
                format='DATETIME';
                formatl=16;
                formatd=0;
                end;
        if informat in ('TIME') then do;
                format='TIME';
                formatl=8;
                formatd=0;
                end;
        if ofmt in ('DATE' 'DDMMYY' 'JULIAN' 'MMDDYY'
                    'MONYY' 'NENGO' 'YYMMDD' 'YYQ'
                    'DAY' 'DOWNAME' 'JULDAY' 'MMYY' 'MONNAME'
                    'MONTH' 'MONYY' 'QTR' 'QTRR' 'WEEKDATE'
                    'WEEKDATX' 'WEEKDAY' 'WORDDATE' 'WORDDATX'
                    'YEAR' 'YYMM' 'YYMON'
                    'YYQC' 'YYQD' 'YYQN' 'YYQP' 'YYQS'
                    'YYQRC' 'YYQRD' 'YYQRN' 'YYQRP' 'YYQRS'
                    ) then do;
                format='DATE';
                formatl=7;
                formatd=0;
                end;
        if ofmt='DATETIME' then do;
                format='DATETIME';
                formatl=max(16,ofmtl);
                formatd=max( 0,ofmtd);
                end;
        if ofmt in ('TOD') then do;
                format='DATETIME';
                formatl=16;
                formatd= 0;
                end;
        if ofmt='TIME' then do;
                format='TIME';
                formatl=max(8,ofmtl);
                formatd=max(0,ofmtd);
                end;
        if ofmt in ('HHMM' 'HOUR' 'MMSS') then do;
                format='TIME';
                formatl=8;
                formatd=0;
                end;
        end;
run;
/* Try to reorder the variables ************************************** */
%if %index( &var, _ALL_ ) = 0 &
    %index( &var, --    ) = 0 %then %do;
    data work._sort_;
         retain &var 0;
         stop;
    run;
    proc contents data=work._sort_ out=work._sort_ noprint memtype=data;
    run;
    data work._conten_;
         merge work._conten_(rename=(varnum=ovarnum))
               work._sort_(keep=name varnum in=_invar_ );
         by name;
         if _invar_ ;
    run;
%end;
%else %do;
    data work._conten_;
         set work._conten_;
         ovarnum=varnum;
    run;
%end;
proc sort data=work._conten_;
     by varnum;
run;
 /* Determine Maximum LineSize *************************************** */
%if %upcase(&LINESIZE)=MAX %then %do;
    proc summary data=work._conten_;
         var formatl;
         output out=work._ls_ sum=formatl n=nvar;
    quit;
    data _null_;
         set work._ls_;
         select ("&DLM");
            when("NONE"    )  space=0;
            when("SPACE"   )  space=1;
            when("DBLSPACE")  space=2;
            otherwise         space=1;
            end;
         maxls= formatl + (nvar-1)*space ;
         call symput( "LINESIZE" , compress(put(maxls, best5.)) );
    run;
    %put WARNING:  LINESIZE reset from MAX to &LINESIZE ;
%end;
 /* Print DATA Stmt ************************************************** */
data work._null_;
     %if &SASref =
         %then %do;
               file &fileref linesize=&linesize ;
               %end;
         %else %do;
               file &SASref linesize=80 ;
               %end;
     set work._dsname_;
     put @1 "DATA " @;
     %if &compare = YES %then %do;
         put " /* " @;
         %end;
     put dsname  @;
     if typemem ^= " " or memlabel ^= " " then do;
        put "( " @;
        if typemem  ^= " " then put "TYPE=" typemem @;
        if memlabel ^= " " then do;
           memlabel = "'" || trim(memlabel) || "'";
           put  "LABEL=" memlabel @;
           end;
        put ")" @;
        end;
     %if &compare = YES %then %do;
         put " */" @;
         %end;
     put " ;";
     %if &SASref ^= %then %do;
         put @6 "INFILE &fileref linesize= &linesize ;"  ;
         %end;
run;
 /* Print ATTRIB Stmts *********************************************** */
data work._conten_;
     set work._conten_;
     %if &SASref =
         %then %do;
               file &fileref linesize=&linesize mod ;
               %end;
         %else %do;
               file &SASref linesize=80 mod ;
               %end;
 
     if formatl > &linesize then do;
        file log;
        put "ERROR: The field width of the variable " name "> linesize.";
        put "ERROR:     Set LINESIZE= parameter larger than  " formatl ;
        %if &SASref =
            %then %do;
                  file &fileref ;
                  %end;
            %else %do;
                  file &SASref ;
                  %end;
        file &fileref;
        put "if _N_ = 1 then error 'ERROR: The field width of the variable "
             name "> linesize.';" ;
        formatl=&linesize ;
        end;
 
     /* Format used to write data values ***************************** */
     length datafmt $17;
     datafmt = compress( format || put(formatl,3.) || ".");
     if formatd > 0 then datafmt = compress( datafmt || put(formatd,3.) );
 
     /* Calculate Line #, Column # of data *************************** */
     retain line 0 col1 col2 &linesize;
         select ("&DLM");
            when("NONE"    )  space=0;
            when("SPACE"   )  space=1;
            when("DBLSPACE")  space=2;
            otherwise         space=1;
            end;
     if (col2 + space + formatl) > &linesize
        then do;
             line = line + 1;
             col1 = 1;
             end;
        else col1 = col2 + 1 + space ;
     col2 = col1 + formatl - 1;
 
     length lengthc $4;
     if type=2 then lengthc = '$';
     lengthc = compress(lengthc || put(length,3.) );
 
     length fmtc $17;
     fmtc = ofmt ;
     if ofmtl > 0 then fmtc = compress( fmtc || put(ofmtl,3.) );
     fmtc = compress(fmtc || '.' );
     if fmtc = '.'
        then fmtc=' ';
        else do;
             if ofmtd > 0
                then fmtc = compress( fmtc || put(ofmtd,3.) );
             if type=2 and index(fmtc, '$') = 0
                then fmtc = '$' || fmtc ;
             end;
 
     length infmtc $17;
     infmtc = informat;
     if informl > 0 then infmtc = compress( infmtc || put(informl,3.) );
     infmtc = compress(infmtc || '.' );
     if infmtc = '.'
        then infmtc=' ';
        else do;
             if informd > 0
                then infmtc = compress( fmtc || put(informd,3.) );
             if type=2 and index(infmtc, '$') = 0
                then infmtc = '$' || infmtc ;
             end;
 
     /* Print ATTRIB Stmt *******************************************/
     put @6  "ATTRIB " name $8. @;
     put @22 "LENGTH=" lengthc  $4. @;
     put @35 "/* LINE " line 4. "   COL " col1 4. " - " col2 4. " */" @;
     if fmtc ^=" " | infmtc ^= " " then put;
     if fmtc   ^= " " then put  @13 "FORMAT=" fmtc @;
     if infmtc ^= " " then put  @38 "INFORMAT=" infmtc @;
     if label  ^= " " then put /@13 "LABEL='" label +(-1) "'" @;
     put @70 ";" ;
 
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
 /* Print INPUT Stmt ************************************************* */
data _null_;
     retain col 12;
     %if &SASref =
         %then %do;
               file &fileref linesize=&linesize mod ;
               %end;
         %else %do;
               file &SASref linesize=80 mod ;
               %end;
     set work._conten_ end=end;
     lastline = lag(line);
     atcol = compress( "@" || put(col1,4.) );
     if _N_ = 1
        then put @6 "INPUT " @;
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
     if end then do;
        put ";" ;
        %if &SASref = %then %do;
            put "CARDS;" ;
            %end;
        end;
run;
 /* Print Data Lines ************************************************* */
data _null_;
     set &data;
     %if &SASref =
         %then %do;
               file &fileref linesize=&linesize mod ;
               %end;
         %else %do;
               file &fileref linesize=&linesize     ;
               %end;
     file &fileref mod linesize=&linesize ;
     put
     %let lline = &line1 ;
     %do i = 1 %to &n;
         %if &&line&i ^= &lline
             %then  / ;
         %if &dlm=COMMA & %eval(&&col&i) > 1
             %then ',' ;
         @&&col&i &&name&i &&fmt&i
         %let lline = &&line&i ;
     %end;
     ;
run;
%if &compare = YES %then %do;
data _null_;
     %if &SASref =
         %then %do;
               file &fileref linesize=&linesize mod ;
               %end;
         %else %do;
               file &SASref linesize=80 mod ;
               %end;
     put "RUN;" ;
     put "PROC COMPARE DATA=&data COMPARE=_LAST_ NOLISTEQUAL "
         "METHOD=EXACT;" ;
     put "RUN;" ;
run;
%end;
%exit:
%mend dumpsas;

