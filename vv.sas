%*---------------------------------------
%*    Name:    vv.sas
%*   Title:    validate and verify macro;
%*  Author:    Karsten Self
%*    Date:    1/17/96
%*
;
/*
  Description:    validate and verify macro;
                
	Generate informational listing of contents of input dataset.  Resource 
	intensive for largeish datasets.

	Uses SQL and the SASHELP.VCOLUMN tables to check number of unique
	values each variable has, and generate additional information.

	Creates series of datasets in WORK library:
	- _vv<extension>
	- <variable> where 'variable' is a variable from the input dataset


  Copyright & Disclaimer:
	VV is copyright 1996 Karsten M. Self, all rights reserved.

	Permission is hereby granted for free reproduction and distribution
	if this program is distributed in its entirety.  This program may
	not be resold.

	No warrantee or guarantee of accuracy is made for this program or
	the consequences of its use.

	This is an unsupported program, though I will be glad to hear your
	suggestions and know of any bugs found:

		Karsten M. Self
		kmself@ix.netcom.com

				Thank you and have a VVery nice day :-)



  Invocation:        
		Required:

		libname = <valid SAS libname>
			data    = <valid SAS dataset>

		Optional:

		Title   = <text>                 Up to 200 characters.  Appears as second title
		FreqOrdr= freq | data | formatted | internal
													Display order of proc freq output (default = freq)
		Uniplot = false | true         Run PLOT option on proc univariate (default = false)
		ValCtOff= <number>             Cutoff number of data values for full FREQ (default= 25)
		Cleanup = true | false              Delete working datasets (default = true)
		

  Features:
                
		- Dataset Summary:
			- Observations and variables count
			- Count by:
				- char
				- numeric
					quantity values (identified by format - default)
					date/time values (identified by format - date/time/datetime formats)

			- Unevaluated variables (count)
			- Uniformly evaluated variables (count)
				A note on this.  These are variables which have one value and only one
				value, where evaluated.  There may or may not be missing values, these
				are handled and reported as seperate cases.

			- show all variables, number of unique values, properties

		- Unevaluated variables (list)
		- Uniformly evaluated variables (list)

		- Character variables: 
			- less than <n> values, show all values
			- more than <n> values, show most frequent, least frequent <m> 
				values
		- Numeric variables
			- non-date (determined by format -- as default)
				- proc univariate of all
				- proc freq if less then <n> distinct values

			- date/time/datetime format
			Frequency of values, formatted according to format.
				- dates    -- mmyys7 (mm/yyyy)
				- time     -- time5  (hh:mm)
				- datetime -- datetime7 (dd/mm/yyyy) (?)
				...see wishlist for possible changes.
					

		- Proc contents (extended format)

		- Proc print
			- all records if <= 60
			- first, last, and middle 20 if obs > 60

  Bugs and lapses
                
			- will not work on views and tape libraries (exits with message).  To analyze, 
				copy (or sample) to online storage.

			- sql runs painfully slow on large datasets.  We are testing this 
				right now, running all afternoon on 1.25 m records.  Just finished.
				4 hr 16 min elapsed, 43:47 cpu here, where single data step pass
			requires somewhere less than 10 minutes.

			- Any prior specified titles are overwritten.  Previously specified footnotes will print.

			- User-defined formats must be installed or made available prior to running VV.

			- Special missing values (.A - .Z) ARE included as non-missing values, and values are
				displayed either with univariate output or freq output (if number of values is below
			cutoff for running freq), IF the variable has at least one non-missing (. - .Z) value.

*/

%* --------------------
%*  Revised by:     KMS        
%*  Revision Date:  2/23/96
%*  Version:            0.3h
%*
%* --------------------
%*  Modification log:
%*
%*  date        pgmr        ver         notes
%* -------      ----        ----        -----
%*  1/15/96     kms                     Created program.  Copy to Norm.
%*
%*  1/19/96     kms         0.1c        Fixed proc freq.  'Productionalized' header, updated/added 
%*                                      'features', 'bugs', and 'additional features'.  
%*                                      Cleaned up early 'call execute' code.
%*
%*                          0.1d        Corrected MaxFreq from test value.  Added 'ver' to mod log head.
%*
%*  1/22/96     kms         0.1e        Added code to retain obs option value for restore on exit.  Obs is 
%*                                      set to max for execution of vv.  Fixed the 'cleanup' stuff (moved 
%*                                      to proc datasets at exit:) again!
%*
%*  1/23/96     kms        0.1f        Took care of most of the 1/19 and 1/22 wishlists:
%*                                      - first report is now data _null_ put.  Includes a summary of 
%*                                        variable by type (char, num (quantity, date/time)), and evaluation 
%*                                        (nulls and ones).
%*                                      - variables are sorted alpha
%*                                      - date/time variable handling code is more solid and splits data 
%*                                        into date, datetime, and time types according to format, does 
%*                                        frequency according to format (by internal value).
%*
%*  1/24/96     kms        0.1g        - Added format to 'Observations' and 'Variables' fields in 'Dataset 
%*                                       Summary' report.
%*                                     - Format was going to scientific notation for > 1m observations.  Put 
%*                                       location shifted from @20 to @30.
%*                                     - Revised 'Features' to reflect recently added features.
%*
%*  1/26/96     kms        0.1h        Fixed bug (missing call execute line) which was causing the dates 
%*                                     frequency problem.
%*
%*  2/6/96      kms        0.2         Some stuff that has been festering for a while, including
%*                                     - Linesize and pagesize are now forced.  Incoming defaults saved 
%*                                       and reset on exit
%*                                     - Changed 'obs' macrovariable to 'nobs'.  Changed obs macro to 
%*                                       conform.  Renamed it 'nobs' as well.
%*                                     - Fixed the missing, single value (some missing), single value 
%*                                       (none missing) logic
%*                                     - Reformatted first report
%*                                     - Found the 'null values' generating bug in the univariate stuff.
%*                                     - Added 'plot' option to univariate output.
%*                                     - Added output value or value counts to reports (need to label)
%*
%*  2/7/96      kms        0.2a        - Fixed SQL procedure which was printing blank output page (assigned 
%*                                       noprint option)
%*                                     - Found the source of the 'varaible TRUE is unititialized' message 
%*                                       (macrovariable being read as a variable...
%*
%*  2/8/96      kms        0.2b        Noted by Frank Poppe:
%*                                     - missing 'values' var option on 'Frequency tab..numeric var...'
%*                                     - changing logic to gt 1 value at his recommendation (predates the 
%*                                       value in summary code). 
%*
%*                                    Noted by KMSelf:
%*                                    - missing ampersand in MaxLen assignment SQL.
%*                                    - in singles vars processing, ' ' to . for numerics.
%*
%*  2/10/96     kms        0.2c       More patches and fixes:
%*                                    - changed the positional and formatting parmeters on the first report 
%*                                      to fix that nagging scientific notational display for obs in the 
%*                                      millions.
%*                                    - updated the 'uniformly evaluated variables' description and 'date' 
%*                                      report description under 'Features'.
%*
%*  2/13/96     kms        0.2d       Modified NOTE/WARNING/ERROR messages to refer to this macro.
%*
%*  2/15/96     kms        0.3        - Changed Date/Time/Datetime variables summary from frequency base to 
%*                                      univariate based.
%*                                    - Put a really ugly patch over the 'ones' (single evaluated variable) 
%*                                      processing.
%*
%*  2/15/96     kms        0.3a       Housecleaning and then some...
%*                                    - Fixing formatting of numeric 'ones' variables in 'Single Value' 
%*                                      report had been printing as '*'.
%*                                    - Implementing Phil Gallagher suggestions and/or adding to wish list 
%*                                      -- instructions on titles, footnotes, and user-defined formats.
%*                                    Cleaning up processing 
%*                                    - eliminating intermediate datasets where possible in the Master and 
%*                                      One series
%*                                    - ensuring labeling (and consistent labeling) of process datasets
%*                                    - added titles option (becomes second line of titles)
%*                                    - added frequency order option (for character and numeric, not extreme 
%*                                      values)
%*
%* 2/16/96      kms        0.3b       Cleaning up after the housecleaners...
%*                                    - filled in the MMYYxw and YYMMxw format values (had missed the 'x' 
%*                                      substitutions).
%*
%* 2/21/96      kms        0.3c       More general cleanup, some reformatting....
%*                                    - Finishing cleanup after eliminating intermediate Master/One dataset 
%*                                      stuff.
%*                                    - Changed One/Missing reports to proc report from proc print.
%*                                    - Simplified dataset summary report (#1).
%*
%* 2/22/96      kms        0.3d       Refining reports 
%*                                    - added 'titles' label in data _null_ generating 'missing', 'ones', 
%*                                      'extremes', 'univariate', 'numeric freq', and 'date/time' summary 
%*                                      reports -- enables titles to show if output goes to multiple pages.
%*                                    - added labels for '#' (count) on several reports, label for 'rank' on
%*                                      extremes report.
%*
%* 2/23/96      kms        0.3e       Refinements, cont.
%*                                    - indexed _vvNum1 and _vvNum2 by variable, added 'by' processing to data
%*                                      null generating univariate code.
%*
%* 2/28/96      kms        0.3f       Changed proc datasets to nolist when Cleanup is true (assume user does 
%*                                      not care or want to see datasets listing)
%*
%* 3/1/96       kms        0.3g       Changed width option in first summary display for obs/vars to be min 
%*                                      4 (accomodates short fat datasets)
%*
%* 3/18/96      kms        0.3h       Added name= and col= options to PROC TRANSPOSE of _vvM1, to specify
%*                                      default '_name_' and 'col<n>' variable names (had run into problems
%*                                      with SAS mistakingly ducking apparent name collisions).  Macro works
%*                                      under HPUX.  Removed all tabs, replaced with spaces.  May be some
%*                                      with this in formatting... 
%*
%*---------------------------------------
;


%macro VV( 
    libname  = ,            /* libname of input dataset */
    data     = ,            /* name of input dataset */

    title    = ,            /* title (displayed as second title) */

    FreqOrdr = freq,        /* display order of frequency output - see PROC FREQ for details */
    UniPlot  = false,       /* 'true' enables ('false' disables) plot option on univariate output */

    ValCtOff = 25,          /* Maximum number of levels a variable can have for frequency counts */
    Cleanup  = true,        /* true= enable deletion of temporary working datasets */
    );


    * Data step to generate SQL statements;
    * ..."vv" stands for "verify + validate";
    
    *--------------------
    * local macrovariables;
    
    %local 
        nobs
        nvar
        exit     
        pagesize
        linesize
        Cleanup  
        ValCtOff 
        ExtrmVal 
        MaxFreq  
        doOnes    /* Boolean, enables 'Ones' variable processing if necessary */
        MaxLen
        Width
        ;


    %* Initialize internal macrovariables; 
    %let ExtrmVal = 10;     * Number of high/low frequency values to show;    
    %let MaxFreq  = 32767;  * SAS system maximum number of freq levels in an dataset;    
    %let PageSize = 60;     * Optimal pagesize setting (may parameterize later);
    %let LineSize = 132;    * Optimal linesize setting (may parameterize later);
    %let exit     = 0;      * Exit flag value;

    
    * Date formats -- used for identifying date numeric variables later;
    %let dateFmt = 
        %str(
        'DATE', 
        'DAY',
        'DDMMYY', 
        'DOWNAME',
        'JULDAY',
        'JULIAN', 
        'MMDDYY',
        'MMYY',
        'MMYYC',
        'MMYYD',
        'MMYYN',
        'MMYYP',
        'MMYYS',
        'MONNAME',
        'MONTH',
        'MONYY',
        'NENGO', 
        'QTR',
        'QTRR',
        'WEEKDATE',
        'WEEKDATEX',
        'WEEDKAY',
        'WORDDATE',
        'WORDDATEX',
        'YEAR',
        'YYMM',
        'YYMMC',
        'YYMMD',
        'YYMMN',
        'YYMMP',
        'YYMMS',
        'YYMMDD', 
        'YYMON',
        'YYQ',
        'YYQR'
        );

    %let dttimFmt = 
        %str(
        'DATETIME',
        'TOD'
        );

    %let timeFmt =
        %str(
        'HHMM',
        'HOUR',
        'MMSS',
        'MSEC', 
        'PDTIME', 
        'RMFDUR', 
        'RMFSTAMP', 
        'SMFSTAMP', 
        'TIME', 
        'TODSTAMP', 
        'TU'
        );

    %let dtFmts= 
        %str( &dateFmt., &dttimFmt., &timeFmt. );
        

    %*---------------
    %* Internal macros; 

 
    *---------------
    * Get obs option setting for current session.  Save.  Reset to max for VV and restore on exit;
    proc sql noprint;
        create table work._vvRSOpt( label= "VV - SAS Options Reset Values" ) as
        select optname, setting
        from dictionary.options
        ;
    
        quit;

    options 
        obs= max 
        firstobs= 1
        compress= no
        pagesize= &PageSize.
        linesize= &LineSize.
        ;        


    *----------------
    * Records and variables on requested dataset;

    %let nobs = test;
    %nobs( Libname= &libname., Data= &data. );

    %let nvar = test;
    %nvar( Libname= &libname., Data= &data. );
    

    * No records? Quit;
    %if &nobs. lt 1 %then
            %do;
            %put ERROR: (VV macro) the dataset %upcase( &libname..&data. ) has zero records.;
            %put ERROR: (VV macro) Further processing halted.;
            %let exit = 1;
            %end;

    * No variables? Quit;
    %if &nvar. lt 1 %then
        %do;
        %put ERROR: (VV macro) the dataset %upcase( &libname..&data. ) has zero variables.;
	%put ERROR: (VV macro) Further processing halted.;
        %let exit = 1;
        %end;

    * Exit if flagged;
    %if &exit. gt 0 %then
        %goto exit;



    * ----------------
    * Parameter parsing;

    * ...FreqOrdr - change to default if illegal, with warning;

    %let FreqOrdr = %lowcase( &FreqOrdr. );

    %if &FreqOrdr. ne data       and
        &FreqOrdr. ne formatted  and
        &FreqOrdr. ne freq       and
        &FreqOrdr. ne internal      %then
        %do;
        %put WARNING: (VV macro)  invalid FreqOrdr &FreqOrdr. selected.  Using 'Freq' instead;
        %let FreqOrdr = freq;
        %end;


    * ...UniPlot - change to false if illegal, with warning;

    %let UniPlot = %lowcase( &UniPlot. );

    %if &UniPlot. ne true  and  &UniPlot. ne false %then
        %do;
        %put WARNING: (VV macro)  invalid UniPlot value &UniPlot. selected.  Using 'false' instead;
        %let UniPlot = false;
        %end;


    * ...Cleanup - change to false if illegal, with warning;

    %let Cleanup = %lowcase( &Cleanup. );

    %if &Cleanup. ne true  and  &Cleanup. ne false %then
        %do;
        %put WARNING: (VV macro)  invalid Cleanup value &Cleanup. selected.  Using 'false' instead;
        %let Cleanup = false;
        %end;


    * ...Title - trunc to 200 characters if too long;
    %if %length( &title. ) gt 200 %then
        %do;
        %let title = %substr( &title, 1, 200 );
        %end;



    * ----------------
    * Else, get serious;


    title;     * Clear titles;


    * Generate sql for query.  This is run immediately following data step;
    data _null_;
    
        length text $ 200;
    
        set sashelp.vcolumn(
            where= ( libname = "%upcase( &libname. )" and memname = "%upcase( &data. )" ))
            end = last
            ;

        * First -- open sql statment;
        if _n_ = 1 then
            do;

            text = "proc sql noprint; " ;
            call execute( text );

            text = "create table work._vv1M( label= 'VV - Master - distinct values') as" ;
            call execute( text );

            text = "   select" ;
            call execute( text );

            text = "   count ( * ) as records, " ;
            call execute( text );

            end;


        * First and all but last -- generate query statment for variable counts;
        if not last then
            do;

            text = "count ( distinct " || name || ") as " || name || " , ";
            call execute( text );

            end;


        * Last -- close sql statement;
        else if last then
            do;
            text = "count ( distinct " || name || ") as " || name ;
            call execute( text );

            text = "from &libname..&data. " ;
            call execute( text );

            text = "; " ;
            call execute( text );

            text = "quit; " ;
            call execute( text );

            end;

        run;

    
    title "DATA VERIFICATION + VALIDATION FOR %upcase( &libname. ).%upcase( &data. )";

    %if %length( &title. ) gt 0 %then
        %do;
        title2 "&title.";
        %end;


    * Generate a dataset with variable names, labels, and counts;

    * ...transpose what you have ;
    proc transpose 
	data= work._vv1M 
	out= work._vv1M(
	    label= "VV - SQL results - transposed"
	    )
        prefix= col
	name= _name_
	;
    run;
    

    * ...get more info.  Label, type, length, format;
    data _vv1D( 
        label= "VV - Master - Dictionary "
        rename= ( name = variable )
        )
        ;

        keep name label type length format;

        set sashelp.vcolumn (
            where= ( libname = "%upcase( &libname. )" and memname = "%upcase( &data. )" )
            );


        run;


    * join them in...;
    proc sql noprint;
        create table _vv1M(
            compress = no 
            label= 'VV - Master - merge '
            ) as
        select 
            M._name_ as variable label= "Variable Name",
            M.col1   as values   label= "# of Distinct Values" format= comma10. ,
            D.label  as label    label= "Label",
            D.type   as type     label= "Type",
            D.length as length   label= "Length",
            D.format as format   label= "Format"

        from
            work._vv1M as M,
            work._vv1D as D

        where
            M._name_ = D.variable 
        order by D.variable
        ;
        quit;
    
    

    * More stuff to do;
    * ...identify character and numeric types for further analysis;
    *    character...
    *          ...if less than &ValCtOff values, proc freq in frequency order;
    *         ...if more than &ValCtOff values, 10 most frequent, 10 least frequent values;
    *    numeric
    *    ...if not a date....
    *            ...all - proc univariate
    *            ...less than &ValCtOff values, proc freq
    *    ...if a date
    *            proc freq on mm/yy format or something similar
    *    ...by number of values
    *       - none (null)
    *       - one (n_one) -- these guys need more looking at to see if there is just 
    *         one value, or if sometimes there are missing values.  Keep them only if
    *         there is one and only one value, none missing
    ;
    

    * Get nobs count again, just to be sure;
    %nobs( libname= &libname., data= &data. );

    data
        _vv1M( label  = "VV - Master - classed" )
        
            _vvch1( 
            label = "VV - Char <= &ValCtOff. values"
            drop= 
                nNumA 
                nNum 
                nDate 
                nNul 
                nulls 
                n_One 
                n_One0 
                n_One1
                 )

            _vvch2( 
            label = "VV - Char  > &ValCtOff. values"  
            drop= 
                nNumA 
                nNum 
                nDate 
                nNul 
                nulls 
                n_One 
                n_One0 
                n_One1
                )

        _vvNumA(
            label = "VV - Num - All quantity + date/time" 
            drop= 
                nChar
                nDate 
                nNul 
                nulls 
                n_One 
                n_One0 
                n_One1
                )

            _vvNum1(
            label = "VV - Num <= &ValCtOff. values "  
            drop= 
                nChar
                nDate 
                nNul 
                nulls 
                n_One 
                n_One0 
                n_One1
                )
        
            _vvNum2(
            label = "VV - Num  > &ValCtOff. values "  
            drop= 
                nChar 
                nDate 
                nNul 
                nulls 
                n_One 
                n_One0 
                n_One1
                )

            _vvDt( 
            label  = "VV - Date/Time variables"  
            drop= 
                nChar 
                nNumA 
                nNum 
                nNul 
                nulls 
                n_One 
                n_One0 
                n_One1
                )


        _vvNul(
            label  = "VV - Not evaluated"  
            drop= 
                nChar
                nNumA 
                nNum 
                nDate 
                nulls 
                n_One 
                n_One0 
                n_One1
                )
        
        _vvOne(
            label  = "VV - Ones vars"  
            drop=
                nChar
                nNumA 
                nNum 
                nDate 
                nNul
                n_One0 
                n_One1
                )

        _vvErr(label  = "VV - Error output" )
        ;

        * Redundant and overkill, but what the hey;
        length
            nObs
            nVar
            nChar
            nNum
            nDate
            nNul
            n_One
                 8.
            ;


        retain

            /* Tabulation counters */

            nObs
            nVar
            nChar
            nNum
            nDate
            nNul
            n_One

            /* Null and One counts (values supplied in later data step) */
        
            nulls
            n_One0
            n_One1

            /* Initial value */

                0
            ;

        keep 
            /* Info fields */
            variable
            label
            values
            format
            type
            length

            /* Dataset counts */
            _n
            nObs
            nVar

            /* Var counts */
            nChar
            nNumA 
            nNum 
            nDate 
            nNul 
            n_One 

            /* Sepcial cases */

            nulls
            n_One0 
            n_One1

            ;



        * Initial pass through data to pick up summary tabulations;
        if _n_ eq 1 then
            do;

            do point = 1 to _vars;

                set _vv1M 
                    nobs= _vars 
                    point= point
                    ;

                if _error_ then abort;

                nObs = &nobs.;
                nVar = _vars;

                select( type );
                    when( "char" ) 
                        nChar + 1;

                    when( "num" ) 
                        do;

                        nNumA + 1;

                        if upcase( compress( format, '0123456789. ' )) in( &dtFmts. ) then 
                            nDate + 1;

                        else
                            nNum + 1;

                        end;

                    otherwise
                        put "ERROR: (VV macro) unexpected 'type' value:  " type= variable= _n_=;

                    end;   * select(type) processing;

                if values eq 0 then
                    nNul + 1;

                if values eq 1 then
                    n_One + 1;



                * While we are here, generate some width formatting variables;
                if point eq 1 then
                    do;
                    wnObs = max( 5, round( 1 + ( log10( nobs ) + floor( log10( nobs ) / 3 ))));
                    call symput( 'wnObs', put( wnObs, f. ) );
                    end;


                end;   * do point= processing;
            end;   * if _n_ eq 1 processing;

    
        set _vv1M;



        * Output, continue process;
        if values eq 1 then
            do;
            cvvOne + 1;
            _n = cvvOne;
            output _vvOne;
            end;


        * Output all;
        cvv1M + 1;
        _n = cvv1M;
        output _vv1M;

        
        * Output;
        if values eq 0 then
            do;
            cvvNul + 1;
            _n = cvvNul;
            output _vvNul;
            end;

    
        select( type );

            when( "char" ) 
                do;
                if values le &ValCtOff. then 
                    do;
                    cvvCh1 + 1;
                    _n = cvvCh1;
                    output _vvch1;
                    end;

                else
                    do; 
                    cvvCh2 + 1;
                    _n = cvvCh2;
                    output _vvch2;
                    end;
                end;  * 'char' processing;

            when( "num" ) 
                do;
                cvvNumA + 1;
                _n = cvvNumA;
                output _vvNumA;

                * Date type data;
                if upcase( compress( format, '0123456789. ' )) in( &dtFmts. ) then 
                    do;
                    cvvDt + 1;
                    _n = cvvDt;
                    output _vvDt;
                    end;

                else
                    do;
                    if values le &ValCtOff. then
                        do;
                        cvvNum1 + 1;
                        _n = cvvNum1;
                        output _vvnum1;
                        end;
                    else 
                        do;
                        cvvNum2 + 1;
                        _n = cvvNum2;
                        output _vvnum2;
                        end;
                    end;  * numerics;

                end;

            otherwise
                do;

                error;
                put "ERROR: (VV macro) Unexpected data type " type " in %upcase( &libname. ).%upcase( &data. )";
                cvvErr + 1;
                _n = cvvErr;
                output _vverr;

                end;
        
            end;   * select(type);

        run;


    * Index numeric datasets by variable (to allow sequential processing later);

    proc datasets lib= work nolist;

        modify _vvNum1;
        index create variable;

        modify _vvNum2;
        index create variable;

        run;
        quit;



*----------------------------------------
*        Beginning of 'Ones' variable processing
*----------------------------------------
*
*
* If there are any 'Ones' variables, find out their null counts and the single value, use to 
*  update Master and Ones datasets.
* 
* Because of CALL EXECUTE processing, this processing cannot be run if there are no 'Ones' 
*  variables -- much unhappiness results.
*
* I am breaking this up into several smaller chunks in order to avoid having a macro do loop
*   spanning pages of code
*
*  Process:
*  
*     - Find out if there are any 'Ones' records (by obs count)
*     - Set test variable (doOnes)
*     - Execute each step if true
;

    %nobs( libname= work, data= _vvOne );

    %let doOnes = false;

    %if &nObs. gt 0 %then
        %let doOnes= true;



    %* ----------------;
    %if &doOnes. eq true %then
        %do;

        * Generate SQL to find number of nulls for 'Ones' variables;

        data _null_;
            length text $ 200;
            
            set work._vvOne
                end = last
                ;

            * First -- open sql statment;
            if _n_ = 1 then
                do;

                text = "proc sql noprint; " ;
                call execute( text );

                text = "create table work._vvOneS1(label= 'VV - Ones vars - nulls') as" ;
                call execute( text );

                text = "   select" ;
                call execute( text );

                end;


            * First and all but last -- generate query statment for variable counts;
            if not last then
                do;

                text = "nmiss( " || variable || ") as " || variable || " , ";
                call execute( text );

                end;


            * Last -- close sql statement;
            else if last then
                do;
                text = "nmiss( " || variable || ") as " || variable ;
                call execute( text );

                text = "from &libname..&data. " ;
                call execute( text );

                text = "; " ;
                call execute( text );

                text = "quit; " ;
                call execute( text );

                end;

        run;

        %end;  * doOnes processing step;
        %* ----------------;




    %* ----------------;
    %if &doOnes. eq true %then
        %do;


        * Transpose to turn variables as variables to records with variable names;

        proc transpose 
            data= work._vvOneS1
            out= work._vvOneS1( label= 'VV - Ones - nulls Transposed' );
        run;

        proc datasets lib= work
            nolist
            ;

            modify _vvOneS1;
            rename
                _name_= variable
                col1  = nulls
                ;
        run;

        %end;  * doOnes processing step;
        %* ----------------;




    %* ----------------;
    %if &doOnes. eq true %then
        %do;


        * Get data value associated with each 'Ones' variable;
        * ...efficiently, even (maybe)

        * ...first want to get max width of 'value';
        proc sql noprint;
            select max( length )
                into :MaxLen
                from sashelp.vcolumn
                where libname= "%upcase( &libname. )" and memname= "%upcase( &data. )"
                ;

            quit;


        * ...code-generating datastep;
        data _null_;
            
            length   text   $ 200;

            array PutFmt{ &nVar. }   $ 16 _temporary_;   * Formatting for each variable;
            array Alignmnt{ &nVar. } $ 5  _temporary_;   * Alignment for each variable;
            
            set work._vvOne
                end  = last
                nobs = _nvar
                ;

            * First -- open data step;
            if _n_ eq 1 then
                do;

                * Initializaitons;
                MaxLen = min( 30, &MaxLen. );   * Maximum data length (restricted to 30);
                retain MaxLen;
                

                text= "    data work._vvOneV1(label= 'VV - Ones values') ;" ;
                call execute( text );

                text= "    keep variable value;" ;
                call execute( text );

                * Added put statement to solve numeric-to-char conversion problem (kms 2/7/96);
                text= "    length value $ " || put( MaxLen, f4. ) || "; " ;
                call execute( text );

                text= "    if _n_ eq 1 then do; " ;
                call execute( text );

                end;  * _n_ eq  processing;


            * Accumulate non-null value of each variable - using multiple set statements with 'where'
            *  processing to eliminate nulls
            ;

            * ...set up options for char/num processing -- format, alignment;
            select( type );

                    * Character processing;
                when( "char" )
                    do;

                    * Used to align formatted value for output;
                    Alignmnt{ _n_ } = 'left';

                    * Display format;
                    if format ne ' ' then 
                        PutFmt{ _n_ } = format;

                    else
                        PutFmt{ _n_ } = compress( '$F' || ( put( MaxLen, best. )) || '.' );

                    * Missing values test;
                    MissVal = "' '";

                    end;  * type(char) processing;


                * Numeric processing;
                when( "num" )
                    do;

                    * Several possibilities: date, time, datetime, or quantity.  Question is whether
                    * or not there is a format to use.  If there is, use it.
                    ;

                    * Used to align formatted value for output;
                    Alignmnt{ _n_ } = 'right';

                    * Display format;
                    if format ne ' ' then 
                        PutFmt{ _n_ } = format;

                    else
                        PutFmt{ _n_ } = compress( 'best' || ( put( MaxLen, best.)) || '.' );


                    * Missing values test;
                    MissVal = ".";

                    end;  * type(num) processing;


                otherwise
                    do;

                    * Bad type variable;
                    error "ERROR: (VV macro) bad variable TYPE value: %upcase( &libname. ).%upcase( &data. )" 
                        variable= type= ;
                    stop;

                    end;

                end;  * select(type) processing;


            text= "    do i = 1 to 2; ";
            call execute( text );

            text= "        set &libname..&data.(keep= " || variable || 
                " where=( " || variable || " gt " || MissVal || " )) ; "
                ;
            call execute( text );

            text= "        retain " || variable || "; " ;
            call execute( text );

            text= "        if " || variable || " gt " || MissVal || "  then leave; " ;
            call execute( text );

            text= "        end; ";
            call execute( text );



            * Now we have a bunch of variables in one record.  Put to multiple records for each
            *  of the 'singles' vars (sounds like a bad place to meet a desperate programmer);
            if last then
                do;

                text= "    end;  * if _n_ = 1 procesing;" ;
                call execute( text );

                do _i2 = 1 to _nvar;

                    set work._vvOne;

                    text= "    variable = '" || variable || "';" ;
                    call execute( text );
                    
                    text= "    value = " || Alignmnt{ _i2 } || 
                        " ( trim( put( " || variable || ", " || PutFmt{ _i2 } || " )));"
                        ;
                    call execute( text );

                    text= "    output;";
                    call execute( text );


                    if _i2 eq ( _nvar ) then
                        do;

                        text= "    stop; " ;
                        call execute( text );
                        
                        text= "    run; " ;
                        call execute( text );

                        end;  * if (_i2 ) processing;
                    end;  * do ( _i2 ) processing;
                end;  * if (last) processing;
            run;

        %end;  * doOnes processing step;
        %* ----------------;




    %* ----------------;
    %if &doOnes. eq true %then
        %do;


        *----------------
        * Update the Ones dataset
        ;

        * Sort.  Nodupkey is a 'just in case';

        proc sort data= work._vvOneS1;
            by variable;
        run;


        proc sort data= work._vvOneV1  nodupkey;
            by variable value;
        run;



        data work._vvOne( 
            compress= no
            label= 'VV - Ones master w/ Nulls'
            )
            ;

            merge
                work._vvOne (
                    drop= nulls
                    in= o
                )
                work._vvOneS1 (in = n)
                work._vvOneV1 (in = v)
                ;

            by variable;
            if n;

            * number 1 and 0 stuff;
            if nulls eq 0 then
                do;
                _n0 + 1;
                end;

            else
                do;
                _n1 + 1;
                end;
        run;


        * Add labels;
        proc datasets lib= work nolist;
            modify _vvOne;
            label 
                nulls = '# of Missing Values'
                value = 'Data Value'
                ;

        run;
        quit;

        %end;  * doOnes processing step;
        %* ----------------;




    %* ----------------;
    %if &doOnes. eq true %then
        %do;

        *---------------
        * Update the Master dataset.  This is a majorly backass way to do things.
        * ...populate:
        *    - nulls   -- null count (global) from _vvOneS1
        *    - n_One1  -- One w/o nulls count (global)
        *    - n_One0  -- One w/ nulls count (global)
        *
        ;

        * Merge with Ones SQL for nulls -- only need 'variable' (for merge), 
        * 'nulls', and 'values' (for n_One1 and n_One0 processing)
        ;

        data work._vv1M(label=  "VV - Master - %upcase( &libname. ).%upcase( &data. )" );

            merge
                work._vv1M(
                in= m
                )

                work._vvOneS1(
                in= s
                )
                ;

            by variable;
            if m;

            run;


        *--------
        * Generate totals for n_One1 and n_One0 to carry with all records
        ;
        data work._vv1M(
            label= "VV - Master - %upcase( &libname. ).%upcase( &data. )" 
            )
            ;

            set work._vv1M(
                drop=
                    n_One1
                    n_One0
                )
                ;

            retain 
                n_One1
                n_One0
                    0
                ;
                
            drop point;


            if values eq 1 then
                do;

                * Count single value guys with and without nulls;
                if nulls eq 0 then
                    do;

                    * with nulls;
                    n_One1 + 1;
                    end;

                else
                    do;

                    * without nulls;
                    n_One0 + 1;
                    end;
                end;  * if (values) processing;


            * Point processing to generate totals to carry with all records of dataset
            * ...all we need to keep is 'values' -- we just generated 'nulls'
            * ...on last, re re-read the input dataset, keeping, say, variable, and;
            * create the output dataset
            ;

            if _n_ eq _vars then 
                do;

                do point= 1 to _vars;

                    set _vv1M(
                        drop=
                            n_One1
                            n_One0
                        )
                        nobs= _vars
                        ;

                    output;
                    end;  * point= processing;
                end;  * last processing;


            label
                nulls = "# Missing obs for Var"
                n_One0= "# of Unique Vars w/missing"
                n_One1= "# of Unique non-Missing Vars"
                ;
        run;


        %end;  * doOnes processing;
        %* ----------------;

    *----------------------------------------
    *        End of 'Ones' variable processing
    *----------------------------------------
    ;

    


*--------------------------------------------------------------------------------
*  Start of reports
*--------------------------------------------------------------------------------
* Initial reports:
* ...all variables + dataset summary
*    breakout by
*       character
*       numeric
*          - numeric
*          - data + time
*
*       unevaluated
*       single (nonmissing) value
;


    data _null_;

        file print
            linesleft= remain
            ;
        
        set _vv1M;

        * column variables;
        y_n_  = 1;
        yVar  = y_n_  + 7;
        yVal  = yVar  + 10;
        yLabl = yVal  + 23;
        yType = yLabl + 41;
        yLen  = yType + 8;
        yFmt  = yLen  + 8;
        


        if _n_ eq 1 then
            do;

            
            put 
                "Dataset summary for %upcase( &libname. ).%upcase( &data. )"
                //
                @5  "Observations: " @%eval( 5 + 12 + 3 + &wnObs. ) nObs comma&wnObs..-r /
                @5  "Variables:    " @%eval( 5 + 12 + 3 + &wnObs. ) nVar comma&wnObs..-r /
                @5  40*'-' /
                /
                @5  "Variables by type:" /
                @5  19*'-' /
                @8  "Numeric:   " @20 nNumA comma5. /
                @10 "Quantity:  " @25 nNum comma5./
                @10 "Date/Time: " @25 nDate comma5./
                /
                @8  "Character: " @20 nChar comma5./
                @5  25*'=' /
                /
                /
                @5  "Missing or uniformly evaluated variables:" /
                @5  42*'-' /
                @7  "- missing for all observations: "    @40 nNul comma5. /
                @7  "- uniformly evaluated -- all: "      @40 n_One comma5. /
                @11  "with one or more missing values:"  @45 n_One0 comma5. /
                @11  "with no missing values:"           @45 n_One1 comma5. /
                @5  50*'=' /
                
                ;

            link title;
            
            end;

        if remain le 1 then
            do;
            put _page_;
            goto title;
            end;


        put
            @y_n_  _n_          3.0
            @yVar  variable     $8.
            @yVal  values  comma12.
            @yLabl label       $40.
            @yType type         $4.
            @yLen  length        3.
            @yFmt  format      $10.
            ;

        return;
        
        title:
            put /
            @y_n_  " #"
            @yVar  "Variable"
            @yVal  "Unique Values"
            @yLabl "Label"
            @yType "Type"
            @yLen  "Length"
            @yFmt  "Format"
            ;

        put
            @y_n_ "---"
            @yVar "--------"
            @yVal "-------------"
            @yLabl "-----"
            @yType "----"
            @yLen  "------"
            @yFmt  "------"
            /
            ;

        return;

        run;

    
    *----------------------------------------------------------------------------------------
    *    character...
    *          ...if less than &ValCtOff values, proc freq in frequency order
    ;

    * first get nvar again to do some formatting - width of 'obs' count in report.  Min 3;

    %nvar( libname= &libname, data= &data );

    data _null_;
        nvar= "&nvar";
        Width= length( compress( nvar ));
        Width= max( width, 3 );
        call symput( 'Width', put( Width, best. ));
        run;



    title3 "The following variables are missing or unevaluated for all occurances";

    proc report data= work._vvNul 
        nowindows
        headskip
        spacing= 4
        ;

        column    _n variable label nNul;

        define   _n       / analysis width= &width '#' '--';
        define   variable / display 'Variable Name' '--';
        define   label    / display 'Label' '--';
        define   nNul     / analysis noprint;

        run;



    title3 "The following variables are uniformly evaluated with NO missing values";
    title4 "a single non-missing value is present for all observations";

    proc report data= work._vvOne(
        where= ( nulls eq 0 )
        )

        nowindows
        headskip
        spacing= 4
        ;

        column  _n0 variable value label;

        define   _n0     / analysis width= &width '#' '--';
        define variable  / display 'Variable' '--';
        define value     / display 'Value' '--';
        define label     / display 'Label' '--';

        run;

        

    title3 "The following variables are uniformly evaluated with SOME missing values";

    proc report data= work._vvOne(
        where=( nulls gt 0 )
        )

        nowindows
        headskip
        spacing= 4
        ;

        column  _n1 variable value nulls label;

        define   _n1     / analysis width= &width '#' '--';
        define variable  / display 'Variable' '--';
        define value     / display 'Value' '--';
        define nulls     / display '# of Missing Values' '--';
        define label     / display 'Label' '--';

        run;



    *----------------------------------------------------------------------------------------
    * Character variables -- frequencies
    ;

    title3 "Frequency tabulations of Character variables with <= &ValCtOff. discrete values";
    title4 "non-evaluated variables excluded";

    data _null_;
        file print
            linesleft= remain
            ;

        set _vvch1(where= ( values gt 1 ));
        

        if _n_ eq 1 then
            link title;

        if remain le 1 then
            do;
            put _page_;
            goto title;
            end;


        put 
            @15 _n_ 3.0 
            @20 20*'.' 
            @19 variable  
            @40 label 
            @94 values comma6.
            ;
        
        file log;

        return;

        title:
            put /
                @15 " # "
                @20 "Variables listed:" 
                @40 "Label"
                @85 "# of Values"
                /        
                @15 3*'-'        
                @20 18*'-' 
                @40 40*'-'
                @85 15*'-'
                /
                ;
        return;


        run;


    data _null_;

        length text $ 200;
        set _vvch1(where= (values gt 1))
            end= last
            ;

        if _n_ eq 1 then
            do;

            text = "proc freq data= &libname..&data.";
            call execute( text );

            text = "order= &FreqOrdr.";
            call execute( text );

            text = ";";
            call execute( text );

            text = "tables ";
            call execute( text );

            end;

        call execute( variable );

        if last then 
            do;

            text = "/ missing nocum ;";
            call execute( text );

            text = "run;";
            call execute( text );

            end;

        run;
 

    *----------------------------------------------------------------------------------------
    *         ...if more than &ValCtOff values, 10 most frequent, 10 least frequent values;

    title3 "&ExtrmVal. most frequent + &ExtrmVal. least frequent values";
    title4 "For Character variables with > &ValCtOff. discrete values";


    data _null_;
        file print
            linesleft= remain
            ;

        set _vvch2(where= ( values gt 1 ));
        
        if _n_ eq 1 then
            link title;

        if remain le 1 then
            do;
            put _page_;
            goto title;
            end;

            
        put 
            @15 _n_ 3.0 
            @20 20*'.' 
            @19 variable  
            @40 label 
            @88 values comma12.
            ;
        
        file log;
        
        return;

        title:
            put /
                @15 " # "
                @20 "Variables listed:" 
                @40 "Label"
                @85 "# of Values"
                /        
                @15 3*'-'        
                @20 18*'-' 
                @40 40*'-'
                @85 15*'-'
                /
                ;

        return;
            
        run;


    * Generate frequency data;
    data _null_;

        length text $ 200;

        set _vvch2(where= ( values gt 1 ));

        * If fewer than &MaxFreq results then use proc freq, else, sql;
        * &MaxFreq is the SAS system maximum number of levels for a value;
        * allowed by Proc Freq, = 32,767 as of 1/18/96 (emperically determined :-);
        * ...Order here MUST be freq -- not parameterized
        ;

        if values le &MaxFreq. then
            do;

            text = "proc freq data= &libname..&data.";
            call execute( text );

            text = "order= freq;";
            call execute( text );

            text = "tables ";
            call execute( text );

            call execute( variable );

            text = "/ missing noprint out= " || variable || "( label= 'VV - Char Extremes Freq - " || variable || "' );";
            call execute( text );

            text = "run;";
            call execute( text );

            end;

        else
            do;

            text = "proc sql noprint; " ;
            call execute( text );

            text = "create table " || variable || " as" ;
                call execute( text );

            text = "select " || variable || " as " || variable || ", count( * ) as count" ;
            call execute( text );

            text = "from &libname..&data." ;
            call execute( text );

            text = "group by " || variable ;
            call execute( text );

            text = " ; " ;        
            call execute( text );

            call execute( text );
            text = "quit; " ;

            end;

        run;


    * Sort that stuff;
    data _null_;

        length text $ 200;

        set _vvch2(where= ( values gt 1 ));

        text = "proc sort data= " || variable || " nodupkey; by descending count " || variable || " ; run; ";
        call execute( text );

        run;



    * Print it;
    data _null_;

        length text $ 200;
        retain extreme;
        extreme = "&ExtrmVal.";

        set _vvch2(where= ( values gt 1 ));
        
        text = "data _null_ ; " ; 
        call execute( text );

            text = "set " || variable || " nobs= recs ; " ; 
            call execute( text );

            text = "file print;" ;
            call execute( text );

            * Report title and header;
            text = "if _n_ eq 1 then do; " ;
            call execute( text );

            text = "put // @10 ' " || extreme || " most frequent values of " || variable || 
                "   (" || trim( label ) || ") ' / ; " ;
            call execute( text );

            text = "put @20 recs comma9. ' distinct values in total' / ; " ;
            call execute ( text );

            text = "put @11 'Rank' @20 'Value' @ 33 'Frequency' ; ";
            call execute( text );

            text = "put @11 '----' @20 '-----' @ 33 '---------' //; ";
            call execute( text );

            text = "end;";
            call execute( text );

                
            * Report data;
            text = "if ( _n_ ge 1 and _n_ le &ExtrmVal. ) or ( _n_ le recs and _n_ ge ( recs - &ExtrmVal. )) then do; " ;
            call execute( text );

            text = "put @5 _n_ comma9. @18 " || variable ||  " @30 count comma9.; " ;
            call execute( text );

            text = "end; " ;
            call execute( text );


            * Low 10;
            text = "if _n_ = 11 then " ;
            call execute( text );  

            text = "    put // @10 ' " || extreme || "  least frequent values' / ;" ;
            call execute( text );

            text= "run; " ;

        * Run Cleanup;

        if "&Cleanup." eq "true" then
            do;
            text = "proc delete data= " || variable || " ; ";
            call execute( text );

            text = "run; " ;
            call execute( text );
            
            end;
        
        run;


    *----------------------------------------------------------------------------------------
    *    numeric
    *    ...if not a date....
    *            ...all - proc univariate;

    title3 "Univariate tabulations non-date/time/datetime Numeric variables";
    title4 "includes only variables with TWO or more values";


    * order by variable utilizing index;

    data _null_;
        file print
            linesleft= remain
            ;

        set 
            _vvnum1(where= ( values gt 1 ))
            _vvnum2(where= ( values gt 1 ))
            ;

        by variable;
        
        if _n_ eq 1 then
            link title;

        if remain le 1 then
            do;
            put _page_;
            goto title;
            end;


        put 
            @15 _n_ 3.0 
            @20 20*'.' 
            @19 variable  
            @40 label
            ;
        
        file log;

        return;

        title:
            put /
                @15 " # " 
                @20 "Variables listed:" 
                /
                @15 3*'-'
                @20 18*'-' 
                /
                ;

        return;
        
        run;



    

    data _null_;
        set 
            _vvnum1(where= ( values gt 1 )) 
            _vvnum2(where= ( values gt 1 ))
            end= last
            ;

        by variable;

        if _n_ eq 1 then
            do;

            text = "proc univariate data= &libname..&data.";
            call execute( text );

            if "&UniPlot." eq "true" then
                do;

                text= "plot";
                call execute ( text );

                end;  * UniPlot processing;


            text = "; ";
            call execute( text );

            text = "var ";
            call execute( text );

            end;

        call execute( variable );

        if last then 
            do;

            text = ";";
            call execute( text );

            text = "run;";
            call execute( text );

            end;

        run;

 
    *----------------------------------------------------------------------------------------
    *            ...less than &ValCtOff values, proc freq;
    title3 "Frequency tabulations of numeric variables with <= &ValCtOff. discrete values";
    title4 "non-evaluated variables excluded";


    data _null_;
        file print
            linesleft= remain
            ;

        set _vvnum1(where= ( values gt 1 ));
        
        if _n_ eq 1 then
            link title
            ;

        if remain le 1 then
            do;
            put _page_;
            goto title;
            end;

        put 
            @15 _n_ 3.0 
            @20 20*'.' 
            @19 variable  
            @40 label
            @94 values  comma6.
            ;
        
        file log;

        return;

        title:
            put /
                @15 " # "
                @20 "Variables listed:" 
                @40 "Label"
                @85 "# of Values"
                /        
                @15 3*'-'        
                @20 18*'-' 
                @40 40*'-'
                @85 15*'-'
                /
                ;

        return;
        
        run;


    data _null_;
        set _vvnum1(where= ( values gt 1 ))
            end= last;

        if _n_ eq 1 then
            do;

            text = "proc freq data= &libname..&data.";
            call execute( text );

            text = "order= &FreqOrdr.";
            call execute( text );

            text = ";";
            call execute( text );

            text = "tables ";
            call execute( text );

            end;


        call execute( variable );

        if last then 
            do;

            text = "/ missing nocum ;";
            call execute( text );

            text = "run;";
            call execute( text );

            end;

        run;





    *----------------------------------------------------------------------------------------
    *    numeric
    *    ...if not a date....
    *            ...all - proc univariate;



    title3 "Date/Time/Datetime Variables";
    title4 "non-evaluated variables excluded";

    data _null_;
        file print
            linesleft= remain
            ;

        set _vvDt(where= ( values gt 0 ));
        
        if _n_ eq 1 then
            link title;

        if remain le 1 then
            do;
            put _page_;
            goto title;
            end;

        put 
            @15 _n_ 3.0 
            @20 20*'.' 
            @19 variable  
            @40 label
            @94 values
            ;
        
        file log;

        return;

        title:
            put /
                @15 " # "
                @20 "Variables listed:" 
                @40 "Label"
                @85 "# of Values"
                /        
                @15 3*'-'        
                @20 18*'-' 
                @40 40*'-'
                @85 15*'-'
                /
                ;

        return;

        run;



    *----------------------------------------------------------------------------------------
    * 2/14/96 -- Date/Time/Datetime processing -- 
    *    Changing this to provide Univariate data and maybe a plot
    *
    * ...General structure:
    *    Get Univariate statistics for each date/time/datetime variable
    *    Format the output accordingly for date, time, or datetime
    *    Data _null_ to output Univariate statistics and percentile values.
    ;

    data _null_;

        length 
            text     $ 200
            DispFmt  $ 20
            _format  $ 20
            timetype $ 8
            ;

        set _vvDt(where= ( values gt 0 ));

        * Assign display format according to assigned format;
        *    - DispFmt --  display format 
        *    - DispUnt -- unit text (days, mins, secs)
        *    - DispAgr -- aggregate text (years, days, hours)
        *    - DispFact-- conversion factor from display unit to aggregate unit
        ;

            * ...temp value for testing;
        _format = upcase( compress( format, '0123456789. ' ));

        * dateFmt dttimFmt timeFmt;

        select( _format );

            when( &DateFmt. )
                do;

                TimeType= "date";
                DispFmt= "mmddyy10.";
                DispUnt= "days";
                DispAgr= "years";
                DispFact= 365.25;

                end;  * date formats processing;

            when( &dttimFmt. )
                do;

                TimeType= "datetime";
                DispFmt= "datetime7.";
                DispUnt= "secs";
                DispAgr= "days";
                DispFact= 60 * 60 * 24 ;  * Seconds by minutes by hours per day;

                end;  * datetime formats processing;

            when( &timeFmt. )
                do;

                TimeType= "time";
                DispFmt= "time5.";
                DispUnt= "secs";
                DispAgr= "hours";
                DispFact= 60 * 60 ;   * Seconds per minute per hour;

                end;  * time formats processing;

            otherwise
                do;

                error "ERROR: (VV macro) bad format value encountered, contact developer -- Karsten Self";
		error "ERROR: (VV macro) email to:  kmself@ix.netcom.com";
                error "ERROR: (VV macro) Date/Time/Datetime variable processing" variable= format= ;
                stop;

                end;  * Otherwise (format) processing;

        
            end; * Select processing;


        * Generate proc univariate for each variable (do this for all _n_);

        text= "proc univariate data= &libname..&data. noprint; " ;
        call execute( text );

        text= "   var " || variable || " ; " ;
        call execute( text );

        text= "    output " ;
        call execute( text );

        text= "        out= " || variable || "( label= 'VV - Date Univariate - " || variable || "' ) " ;
        call execute( text );

        text= "        n        = n ";
        call execute( text );

        text= "        nmiss        = nmiss ";
        call execute( text );

        text= "        mean        = mean ";
        call execute( text );

        text= "        std        = std ";
        call execute( text );

        text= "        max        = max ";
        call execute( text );

        text= "        min        = min ";
        call execute( text );

        text= "        range        = range ";
        call execute( text );

        text= "        qrange        = qrange ";
        call execute( text );

        text= "        p1        = p1 ";
        call execute( text );

        text= "        p5        = p5 ";
        call execute( text );

        text= "        p10        = p10 ";
        call execute( text );

        text= "        q1        = q1 ";
        call execute( text );

        text= "        median        = median ";
        call execute( text );

        text= "        q3        = q3 ";
        call execute( text );

        text= "        p90        = p90 ";
        call execute( text );

        text= "        p95        = p95 ";
        call execute( text );

        text= "        p99        = p99 ";
        call execute( text );

        text= "        ; " ;
        call execute( text );



        text= "   run; " ;
        call execute( text );



        * Generate proc datasets to modify formats -- this is done according to 
        *  date/time/datetime format;

        text= "proc datasets lib= work nolist; " ;
        call execute( text );

        text= "    modify " || variable || " ; " ;
        call execute( text );

        text= "    format " ;
        call execute( text );

        text= "        n nmiss     comma10. " ;
        call execute( text );



        * ...the next bit depends on the kind of data;

        text= "        mean min max median q1 q3 p99 p95 p90 p10 p5 p1 " || DispFmt ;
        call execute( text );

        text= "        range qrange comma12. " ;
        call execute( text );

        text= "        std comma12.2 " ;
        call execute( text );

        text= "        ; " ;
        call execute( text );


        text= "    run; " ;
        call execute( text );

        text= "    quit; " ;
        call execute( text );




        * Generate data _null_ for report;
        * ...setups and initalizations;
        *    specify n= pagesize to allow relocation over page;


        text= "title3 'Date/Time/Datetime Variables';" ;
        call execute( text );

        text= "title4 'non-evaluated variables excluded'; ";
        call execute( text );



        text= "data _null_; ";
        call execute( text );

        text= "    set " || variable || "; " ;
        call execute( text );

        text= "    file print  n= pagesize; " ;
        call execute( text );


        text= "    format dRange dQrange   comma12.2  dStd comma12.2; " ;
        call execute( text );


        text= "    dStd  = std  / " || put( DispFact, 8. ) || " ; " ;
        call execute( text );
        
        text= "    dRange  = range  / " || put( DispFact, 8. ) || " ; " ;
        call execute( text );

        text= "    dQRange = qrange / " || put( DispFact, 8. ) || " ; " ;
        call execute( text );


        * Define postional column and row variables -- as text, since we are just writing them out;
        yTitle=  '4';    * Vertical position of first row of titles;
        yData=   '9';   * Vertical position of first row data output;

        xTitle=  '5';   * Horizontal position titles;

        xTtl1=  '5';    * Position first title column;


        * Following is depending on date, time, or datetime;

        select( TimeType );

            when( 'date' )
                do;

                xVal1a=  '10';   * Position first values column 'a' (n, nmiss);
                xVal1b=  '29';   * Position first values column 'b' (  );
                xVal1c=  '26';   * Position first values column 'c' (mean);
                xVal1d=  '27';   * Position first values column 'd' (std, decimal range);
                xVal1e=  '24';   * Position first values column 'e' (range);

                xTtl2=  '50';   * Position second title column;
                xVal2=  '60';   * Position second values column;
    
                end;  * date;

            when( 'time' )
                do;

                xVal1a=  '10';   * Position first values column 'a' (n, nmiss);
                xVal1b=  '29';   * Position first values column 'b' (  );
                xVal1c=  '30';   * Position first values column 'c' (mean);
                xVal1d=  '26';   * Position first values column 'd' (std, decimal range);
                xVal1e=  '23';   * Position first values column 'e' (range);

                xTtl2=  '50';   * Position second title column;
                xVal2=  '60';   * Position second values column;

                end;  * time;

            when( 'datetime' )
                do;

                xVal1a=  '10';   * Position first values column 'a' (n, nmiss);
                xVal1b=  '26';   * Position first values column 'b' (  );
                xVal1c=  '28';   * Position first values column 'c' (mean);
                xVal1d=  '26';   * Position first values column 'd' (std, decimal range);
                xVal1e=  '23';   * Position first values column 'e' (range);

                xTtl2=  '50';   * Position second title column;
                xVal2=  '60';   * Position second values column;

                end;  * datetime;

            otherwise
                do;
                error "ERROR: (VV macro) %upcase( &libname. ).%upcase( &data. )  invalid time time " 
                    TimeType= format= variable= ;
                end;

            end;  * select( TimeType );


        * ...report put statements;

        *    titles;

        text= "    if _n_ eq 1 then";
        call execute( text );

        text= "        do; ";
        call execute( text );

        text= "        put ";
        call execute( text );

        text= "           #" || yTitle || " @" || xTitle ;
        call execute( text );

        text= "         'Univariate distribution of " || trim( variable ) || " ( " || trim( label ) || " ) values' /";
        call execute( text );

        text= "           @"|| xTitle || " 'Distinct values: " || trim( left( put( values, comma10.-l))) || "' /" ;
        call execute( text );

        text= "           @"|| xTitle || " 'Format in dataset:  " || trim( left( format ) ) || "' /";
        call execute( text );

        text= "           @"|| xTitle || " 65*'-' /";
        call execute( text );

        text= "           ;" ;
        call execute( text );

        text= "        end;" ;
        call execute( text );




        *    data;

        *    ...title 1;

        text= "    put // ";
        call execute( text );

        text= "    #" || yData || " @" || xTtl1 || " 'n'      / ";
        call execute( text );

        text= "                     @" || xTtl1 || " 'nmiss'  / ";
        call execute( text );

        text= "    / ";
        call execute( text );

        text= "                     @" || xTtl1 || " 'mean'   / ";
        call execute( text );

        text= "                     @" || xTtl1 || " 'std - " || DispUnt || "'    / ";
        call execute( text );

        text= "                     @" || xTtl1 || " 'std - " || DispAgr || "'    / ";
        call execute( text );

        text= "    / ";
        call execute( text );

        text= "                     @" || xTtl1 || " 'range - " || DispUnt || "' / " ;
        call execute( text );

        text= "                     @" || xTtl1 || " 'range - " || DispAgr || "' / " ;
        call execute( text );

        text= "    / ";
        call execute( text );

        text= "                     @" || xTtl1 || " 'Q1-Q3 - " || DispUnt || "' / ";
        call execute( text );

        text= "                     @" || xTtl1 || " 'Q1-Q3 - " || DispAgr || "' / ";
        call execute( text );



        *    ...value 1;

        text= "           #" || yData || " @" || xVal1a || " n comma10.-r     / " ;
        call execute( text );

        text= "                            @" || xVal1a || " nmiss comma10.-r / " ;
        call execute( text );

        text= "    / ";
        call execute( text );

        text= "                            @" || xVal1c || " mean " || DispFmt ||"-r  / " ;
        call execute( text );

        text= "                            @" || xVal1d || " std  comma12.2-r  / " ;
        call execute( text );

        text= "                            @" || xVal1d || " dStd  comma12.2-r  / " ;
        call execute( text );

        text= "    / ";
        call execute( text );

        text= "                            @" || xVal1e || " Range comma12.-r / " ;
        call execute( text );

        text= "                            @" || xVal1d || " dRange comma12.2-r / " ;
        call execute( text );

        text= "    / ";
        call execute( text );

        text= "                            @" || xVal1e || " QRange comma12.-r / " ;
        call execute( text );

        text= "                            @" || xVal1d || " dQRange comma12.2-r / " ;
        call execute( text );


        *    ...title 2;

        text= "           #" || yData || " @" || xTtl2 || " 'min'    / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'P1'     / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'P5'     / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'P10'    / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'Q1'     / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'median' / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'Q3'     / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'P90'    / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'P95'    / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'P99'    / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'max'    " ;
        call execute( text );



        *    ...value 2;

        text= "           #" || yData || " @" || xVal2 || " min    / " ;
        call execute( text );

        text= "                            @" || xVal2 || " P1     / " ;
        call execute( text );

        text= "                            @" || xVal2 || " P5     / " ;
        call execute( text );

        text= "                            @" || xVal2 || " P10    / " ;
        call execute( text );

        text= "                            @" || xVal2 || " Q1     / " ;
        call execute( text );

        text= "                            @" || xVal2 || " median / " ;
        call execute( text );

        text= "                            @" || xVal2 || " Q3     / " ;
        call execute( text );

        text= "                            @" || xVal2 || " P90    / " ;
        call execute( text );

        text= "                            @" || xVal2 || " P95    / " ;
        call execute( text );

        text= "                            @" || xVal2 || " P99    / " ;
        call execute( text );

        text= "                            @" || xVal2 || " max    " ;
        call execute( text );



        *    end of put statement;

        text= ";";
        call execute( text );



        * Close out data step;

        text= "run;";        
        call execute( text );




        * Cleanup, if selected;

        if "&Cleanup." eq "true" then
            do;
            
            text= "proc delete data= work." || variable || " ; " ;
            call execute( text );

            text= "run; " ;
            call execute( text );

            end;  * Cleanup processing;

        * We are done here;

        run;



    *----------------------------------
    * Contents and prints;

    * ...clear all secondary titles;
    title3;

    proc contents data= &libname..&data.
        position
        details
        ;
        run;

    title3 "Sample observations";

    * We need to get this again, polluted the value above;
    %nobs( libname= &libname., data= &data. );

    * If less than 60 records, print them all;
    %if &nobs. le 60 %then
        %do;

        title5 "The following is a complete listing of %upcase( &libname. ).%upcase( &data. )";
        proc print data= &libname..&data.
            rows= page
            label
            uniform
            n
            ;
           
            run;

        %end;

    %else
        %do;

        title3 "Sample observations  --  showing first, last, and middle 20 records";

        title5 "Records 1 - 20";
        proc print data= &libname..&data.( obs= 20 )
            rows= page
            label
            uniform
            n
            ;
           
            run;

        title5 "Last 20 Records  --  %eval( &nobs. - 19) - &nobs.";
        proc print data= &libname..&data.( 
            firstobs= %eval( &nobs. - 19) 
            )

            rows= page
            label
            uniform
            n
            ;
           
            run;


        %let Middle = %eval( &nobs. / 2 );

        title5 "Middle 20 Records  --  %eval( &Middle. - 9 ) - %eval( &Middle. + 10 )";
        proc print data= &libname..&data.( 
            firstobs= %eval( &Middle. - 9 )
            obs     = %eval( &Middle. + 10 ) 
            )

            rows= page
            label
            uniform
            n
            ;
           
            run;


        %end;


    *--------------------------------;
    %exit:
    
    * Clear titles;
    title;

    * Reset obs to its default;
    data _null_;
        set work._vvRSOpt;

        select( optname );
        when( 'OBS' )
            call execute( 'options obs= ' || trim( setting ) || ';' );
        when( 'FIRSTOBS' )
            call execute( 'options firstobs= ' || trim( setting ) || ';' );
        when( 'LINESIZE' )
            call execute( 'options linesize= ' || trim( setting ) || ';' );
        when( 'PAGESIZE' )
            call execute( 'options pagesize= ' || trim( setting ) || ';' );

        otherwise
            do;
            * nothing;
            end;

        end; * select;

        run;

    proc delete data= work._vvRSOpt;


    * Clear temp datasets if they exist;
    proc datasets lib= work

        %if &Cleanup eq true %then
            %do;

            nolist

            %end;
        ;

        %if &Cleanup. eq true %then
            %do;
            delete
                _vv1M
                _vv1D
                _vvCh1
                _vvCh2
                _vvNumA
                _vvDt
                _vvNum1
                _vvNum2
                _vvOne
                _vvOneS1
                _vvOneV1
                _vvErr
                _vvNul
                ;

            %end;
    run;
    quit;

    %mend VV;

%*----------------------------------------;
