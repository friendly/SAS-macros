 /****************************************************************/
 /*          S A S   S A M P L E   L I B R A R Y                 */
 /*                                                              */
 /*    name: distnew                                             */
 /*   title: Compute Distance or Similarity Matrix (New Version) */
 /* product: stat                                                */
 /*  system: all                                                 */
 /*    keys: distance and similarity, correlation, macros        */
 /*   procs:                                                     */
 /*    data:                                                     */
 /*                                                              */
 /* support: sasahk                      update:  05Jun97        */
 /*     ref:                                                     */
 /*    misc:                                                     */
 /*                                                              */
 /****************************************************************/

 /*------------------------------------------------------------------

 DISCLAIMER:

       THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE
 TO ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
 EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
 PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
 CONTAINED HEREIN.

 -------------------------------------------------------------------*/

 /************ change and uncomment the following statements to refer
               to the file containing XMACRO and the file containing
               STDIZE on your system ********/

*%inc '<location of SAS/STAT samples>xmacro.sas';
*%inc '<location of SAS/STAT samples>stdize.sas';
%include macros(xmacro);

 /********************************************************************

The XMACRO macros from the SAS/STAT sample library in 6.10 or later
are required. The first macro in this file, %xmacinc, checks to see
whether XMACRO has been included, and if not, attempts to include it.
It is advisable to modify the %inc statements within %xmacinc to read
the XMACRO file from the appropriate location on your system.

No products other than base SAS software are required for using the
%DISTANCE macro unless STD=AGK(p) or L(p) is specified, in which case
SAS/STAT software is required. Use of the STD= argument requires the
%STDIZE macro.

This macro will not work in 6.04 or earlier releases.

The %DISTANCE macro computes various measures of distance,
dissimilarity, or similarity between the observations (rows) of a SAS
data set.  These proximity measures are stored as a lower triangular
or a square matrix in an output data set, depending on the specification
of the SHAPE=, that can then be used as input to the
CLUSTER, MDS, or MODECLUS procedures.  The input data set may contain
numeric or character variables or both, depending on which proximity
measure is used.

The number of rows and columns in the output matrix equals the number of
observations in the input data set.  If there are BY groups, an output
matrix is computed for each BY group with the size determined by the
maximum number of observations in any BY group.

The following arguments may be listed within parentheses in any order,
separated by commas:

   DATA=        SAS data set to analyze. The default is _LAST_.
                Most data set options may be used. WHERE processing
                is not supported. Compressed data sets, tape data
                sets, and views are not supported.

   VAR=         List of variables from which distances are
                to be computed. The usual forms of abbreviated lists
                (e.g., X1-X100, ABC--XYZ, ABC:) may be used.
                The variables may be numeric or character or mixed
                depending on the METHOD= argument.  Variable names
                should not begin with an underscore.  VAR= should not
                be specified when any of the following arguments is
                specified: ANOMINAL=, NOMINAL=, ORDINAL=, INTERVAL=,
                and RATIO=.  The default list of VAR= is _NUMERIC_.

                Note: If no variable lists are specified, by default,
                      all the numerical variables in the DATA= data
                      set will be included in the analysis and are
                      treated as VAR=_NUMERIC_.

   ID=          A single character variable to be copied to the OUT=
                data set and used to generate names for the distance
                variables as in the TRANSPOSE procedure.

                If you specify both ID= and BY=, the ID variable must
                have the same values in the same order in each BY
                group. Also, the ID values must be valid SAS names.
                These restrictions may be removed in later versions.

   COPY=        List of additional variables to be copied to the OUT=
                data set. The usual forms of abbreviated
                lists (e.g., X1-X100, ABC--XYZ, ABC:) may be used.

   BY=          List of variables for BY groups. Abbreviated variable
                lists (e.g., X1-X100, ABC--XYZ, ABC:) may NOT be used.

   OUT=         The output data set containing the BY variables, ID
                variable, computed distance variables, COPY variables,
                and FREQ and WEIGHT variables. The default is _TEMP_
                if SHAPE=SQR (SQUARE) is specified; otherwise, the
                default is _DATA_.

                Data set options may NOT be used with the OUT= data set.

   PREFIX=      A prefix to be used to generate names for the distance
                variables in the OUT= data set as in the TRANSPOSE
                procedure. Do not use quotes. The default is DIST.

   METHOD=      The method for computing distance/dissimilarity or
                similarity measures.

                The value of the METHOD= argument is the name of a
                macro to compute the distance/dissimilarity or similarity
                measure between two vectors. The user can write additional
                macros to implement other distance/dissimilarity or
                similarity measures besides those listed above.

                For use in PROC CLUSTER, the distance/dissimilarity
                type of measures should be used; for example,
                METHOD=EUCLID or METHOD=DGOWER.

                Variable types:
                  R  Ratio--must be numeric
                  I  Interval--must be numeric
                  O  Ordinal--must be numeric
                  N  Nominal--may be numeric or character
                  A  Asymmetric nominal--may be numeric or character

                (1) type assumed for VAR= list --+
                (2) accepted list types --+      |
                                          |      |
                  +-----------------------|------+
                  |   +-------------------+
                  |   |
                  V   |
                types V
      method    (1)  (2)   description
      ------    ---------  -----------
      GOWER      I  RIONA  Gower's similarity
                            (see STD=, STDINTER=, and STDRATIO=
                             for more details)
      DGOWER     I  RIONA  1-Gower
                            (see STD=, STDINTER=, and STDRATIO=
                             for more details)
      EUCLID     I  RIO    Euclidean distance
      SQEUCLID   I  RIO    Squared Euclidean distance
      SIZE       I  RIO    Size distance
      SHAPE      I  RIO    Shape distance
      COV        I  RIO    Covariance
      CORR       I  RIO    Correlation
      DCORR      I  RIO    Correlation transformed to Euclidean distance
                           as sqrt(1-CORR)
      SQCORR     I  RIO    Squared correlation
      DSQCORR    I  RIO    One minus squared correlation
      L(p)       I  RIO    Minkowski L(p) distance,
                           where p is a positive numeric value
      CITY       I  RIO    L(1)
      CHEBYCHE   I  RIO    L(infinity)
      POWER(p,r) I  RIO    Generalized Euclidean distance,
                           where p is a positive numeric value and
                           r is a non-negative numeric value
                           The distance between two observations is the
                           rth root of sum of the absolute differences to
                           the pth power between the values for the
                           observations.

      Methods require ratio-level variables:

                 types
      method    (1) (2)    description
      ------    -------    -----------
      SIMRATIO   R  R      Similarity ratio (if variables are binary,
                           this is the Jaccard coefficient)
      DISRATIO   R  R      One minus similarity ratio
      NONMETRI   R  R      Lance-Williams nonmetric coefficient
      CANBERRA   R  R      Canberra metric coefficient
      COSINE     R  R      Cosine
      DOT        R  R      Dot (inner) product
      OVERLAP    R  R      sum(min(x(i),y(i))
      DOVERLAP   R  R      max(sum(x(i)),sum(y(i)))-overlap
      CHISQ      R  R      Chi-squared
      CHI        R  R      sqrt(Chi-squared)
      PHISQ      R  R      Phi-squared
      PHI        R  R      sqrt(Phi-squared)

      Methods require symmetric nominal variables:

        M = nonmissing matches
        X = nonmissing mismatches
        N = total nonmissing pairs

                 types
      method    (1) (2)    description
      ------    -------    -----------
      HAMMING    N  N      X: Hamming distance
      MATCH      N  N      M / N: Simple matching coefficient
      DMATCH     N  N      sqrt( X / N): Simple matching coefficient
                           transformed to Euclidean distance
      DSQMATCH   N  N      X / N: Simple matching coefficient transformed
                           to squared Euclidean distance
      HAMANN     N  N      (M-X) / N
      RT         N  N      M / (M+2X)
      SS1        N  N      2M / (2M+X)
      SS3        N  N      M / X


      Methods require asymmetric nominal variables:

        The following methods distinguish between the presence and
        absence of attributes. Use ABSENT= to designate the list of
        values to be considered indicating absence.

        X  =mismatches with at least one present
        PM =present matches
        PX =present mismatches
        PP =both present = PM+PX
        P  =at least one present = PM+X
        PAX=present-absent mismatches
        N  =total nonmissing pairs

                 types
      method    (1) (2)    description
      ------    -------    -----------
      JACCARD    A  RA     PM / P: Jaccard similarity coefficient

                           Note:
                            - The JACCARD coefficient is the SIMRATIO
                               coefficient if RATIO= is the only
                               variable list specified.
                            - The JACCARD coefficient is computed as
                               PM / P if ANOMIANL= or VAR= is the only
                               variable list specified,
                            - The JACCARD coefficient is the sum of the
                               result from RATIO= list and the result
                               from ANOMINAL= list, when both RATIO= and
                               ANOMINAL= are specified.

      DJACCARD   A  RA     X / P: Jaccard dissimilarity coefficient
                            Note:
                             - The DJACCARD coefficient is the DISRATIO
                                coefficient if RATIO= is the only variable
                                list specified.

                             - The DJACCARD coefficient is computed as
                                X / P if ANOMIANL= or VAR= is the only
                                variable list specified,

                             - The DJACCARD coefficient is the sum of the
                                result from RATIO= list and the result from
                                ANOMINAL= list when both RATIO= and
                                ANOMINAL= are specified,

      DICE       A  A      2PM / (P+PM)
      RR         A  A      PM / N: RR is equivalent to DOT except for
                                    division by the sum of weights.
      BLWNM      A  A      X / (PAX+2PP): L&W nonmetric, also is known as
                                          Bray and Curtis coefficient.
      K1         A  A      PM / X


   ABSENT=      List of values are to be used as absence values in an
                irrelevant absent-absent match for all of the variables
                specified in the ANOMINAL= list.

                An absence value for a variable consists of combinations
                of all legal SAS characters and is quoted by either single
                quote (') or double quote (").

                An empty list or lack of ABSENT= requests the default.
                The default of an absence value for a character
                variable is 'NONE' (note that a blank value is treated
                as a missing value), and the default of an absence value
                for a numeric type of variable is '0'.

   ANOMINAL=
   ANO=         List of variables are to be treated as asymmetric nominal
                variables.  The usual forms of abbreviated
                lists (e.g., X1-X100, ABC--XYZ, ABC:) may be used.
                The variables may be numeric or character or mixed.
                Variable names should not begin with an underscore.

                If both ANOMINAL= and ANO= are used, variables in the
                ANO= list will be concatenated to ANOMINAL= list.

                Do not use VAR= when ANOMINAL= is specified.

   NOMINAL=
   NOM=         List of variables list are to be treated as symmetric
                nominal variables. The usual forms of
                abbreviated lists (e.g., X1-X100, ABC--XYZ, ABC:)
                may be used.  The variables may be numeric, character
                or mixed.  Variable names should not begin with an
                underscore.

                If both NOMINAL= and NOM= are used, variables in the
                NOM= list will be concatenated to NOMINAL= list.

                Do not use VAR= when NOMINAL= is specified.

   ORDINAL=
   ORD=         List of variables are to be treated as
                ordinal variables. The usual forms of abbreviated lists
                (e.g., X1-X100, ABC--XYZ, ABC:) may be used.
                Only numeric variables are allowed.  Variable names
                should not begin with an underscore.

                If both ORDINAL= and ORD= are used, variables in the
                ORD= list will be concatenated to ORDINAL= list.

                Do not use VAR= when ORDINAL= is specified.

                The data in the ORDINAL= list will be replaced by
                their corresponding ranks before the standardization.
                Since PROC RANK used in %DISTANCE to compute ranks
                does not accept FREQ and WEIGHT statements, FREQ= and
                WEIGHT= are ignored in this case.
                After being replaced by ranks,
                ordinal-level variables are treated as interval-level
                variables.

   INTERVAL=
   INT=         List of variables are to be treated as interval variables.
                The usual forms of
                abbreviated lists (e.g., X1-X100, ABC--XYZ, ABC:)
                may be used.  Only numeric variables are allowed.
                Variable names should not begin with an underscore

                If both INTERVAL= and INT= are used, variables in the
                INT= list will be concatenated to INTERVAL= list.

                Do not use VAR= when INTERVAL= is specified.

   RATIO=
   RAT=         List of variables are to be treated as ratio variables.
                The usual forms of
                abbreviated lists (e.g., X1-X100, ABC--XYZ, ABC:)
                may be used.  Only numeric variables are allowed.
                Variable names should not begin with an underscore.

                If both RATIO= and RAT= are used, variables in the
                RAT= list will be concatenated to RATIO= list.

                Do not use VAR= when RATIO= is specified.

   STD=         Method for standardizing variables in the VAR= list.
                See the %STDIZE macro for more details.
                By default, variables are not standardized unless
                METHOD=GOWER|DGOWER.  Standardization is mandatory
                when METHOD= GOWER|DGOWER by the following rules:

            o   When METHOD=GOWER, variables are
                standardized by STD= RANGE and whatever is specified
                in STD= will be ignored.

            o   When method=DGOWER, by default, variables are
                standardized by STD= RANGE.

                Notice that STD= MAXABS is used for standardizing a
                ratio-level variable, a variable should be
                designated ratio only if it is nonnegative.

                The available methods of standardization are:

      method     scale                       location
      ------     -----                       --------
      MEAN       1                           mean
      MEDIAN     1                           median
      SUM        sum                         0
      EUCLEN     Euclidean length            0
      USTD       standard dev. about origin  0
      STD        standard deviation          mean
      RANGE      range                       minimum
      MIDRANGE   range/2                     midrange
      MAXABS     maximum abs value           0
      IQR        intervalquartile range      median
      MAD        median abs dev from median  median
      ABW(c)     biweight A-estimate         biweight 1-step M-estimate
      AHUBER(c)  Huber A-estimate            Huber 1-step M-estimate
      AGK(p)     AGK estimate (ACECLUS)      median
      SPACING(p) minimum spacing             mid minimum-spacing
      L(p)       L(p)                        L(p)
      IN(ds)     read from data set          read from data set


   STDINTER=    Method for standardizing interval-level
                variables for variables in the ORDINAL= and INTERVAL= lists.
                See the %STDIZE macro for details.
                By default, variables are not standardized unless
                METHOD=GOWER|DGOWER.  Standardization is mandatory
                when METHOD= GOWER|DGOWER by the following rules:

             o  When METHOD= GOWER, variables are standardized
                by STDINTER= RANGE, and whatever is specified
                in STDINTER= will be ignored.

             o  When METHOD= DGOWER, by default, variables are
                standardized by STDINTER= RANGE.

                Do not specify both STD= and STDINTER=.

   STDRATIO=    Method for standardizing ratio-level variables for variables
                in the RATIO= list. See the %STDIZE macro for details.
                By default, variables are not standardized unless
                METHOD=GOWER|DGOWER.  Standardization is mandatory
                when METHOD= GOWER|DGOWER by the following rules:

             o  When METHOD= GOWER, by default, variables are standardized
                by STDRATIO= MAXABS.  Using other method of
                standardization will not guarantee the GOWER
                coefficient > 0.

             o  When METHOD= DGOWER, by default, variables are
                standardized by STDRATIO= MAXABS.

                Notice that when STDRATIO= MAXABS is used for standardizing a
                ratio-level variable, a variable should be
                designated ratio only if it is nonnegative.

                Do not specify both STD= and STDRATIO=.

   FREQ=        A single numeric frequency variable used as in PROC
                UNIVARIATE.  Applies only when STD= is specified.
                FREQ= is not applicable when computing ranks for variables
                in the ORDIANL= list.

   WEIGHT=      A single numeric weight variable used as in PROC
                UNIVARIATE. Only works for STD=MEAN, SUM, EUCLEN, STD,
                AGK, or L(p), and when ORDINAL= is not specified.

   UNDEF=       Numeric constant or missing value with which to replace
                undefined distances, for example, when an observation
                has all missing values, or if a divisor is zero.

   MISSING=     Method or a numeric value for replacing missing values.

                Use MISSING= when you want to replace missing values by
                something other than the location measure associated
                with the STD=, STDINTER=, or STDRATIO= arguments, which
                is what the REPLACE option replaces them by.  The usual
                methods include MEAN, MEDIAN, and MIDRANGE.  Any of the
                values for the STD=, STDINTER=, or STDRATIO=
                arguments can also be specified for MISSING=, and the
                corresponding location measure will be used to replace
                missing values.  If a numeric value is given, the
                specified value replaces missing values.  If
                standardization is performed, the replacement is done
                after standardizing the data.

   VARDEF=      The divisor to be used in the calculation of similarity,
                dissimilarity/distance measures and for standardizing
                variables (see WEIGHT= ) whenever a variance or
                covariance is computed. The default value is VARDEF=DF.
                Other available values are N, WDF, and WEIGHT (or WGT).

   ANOWGT=      List of positive numbers used as weights for the variables
                in the ANOMINAL= list.  Weights in the list are separated
                by blanks.  The number of weights in the list must be
                same as the number of variables in the ANOMINAL= list.

                The default value is 1 for each variable in the ANOMINAL=
                list.

   NOMWGT=      List of positive numbers used as weights for the variables
                in the NOMINAL= list.  Weights in the list are separated
                by blanks.  The number of weights in the list must be
                same as the number of variables in the NOMINAL= list.

                The default value is 1 for each variable in the NOMINAL=
                list.

   ORDWGT=      List of positive numbers used as weights for the variables
                in the ORDINAL= list.  Weights in the list are separated
                by blanks.  The number of weights in the list must be
                same as the number of variables in the ORDWGT= list.

                The default value is 1 for each variable in the ORDINAL=
                list.


   INTWGT=      List of positive numbers used as weights for the variables
                in the INTERVAL= list.  Weights in the list are separated
                by blanks.  The number of weights in the list must be
                same as the number of variables in the INTERVAL= list.

                The default value is 1 for each variable in the INTERVAL=
                list.

   RATWGT=      List of positive numbers used as weights for the variables
                in the RATIO= list.  Weights in the list are separated
                by blanks.  The number of weights in the list must be
                same as the number of variables in the RATIO= list.

                The default value is 1 for each variable in the RATIO=
                list.

   VARWGT=      List of positive numbers used as weights for the variables
                in the VAR= list.  Weights in the list are separated
                by blanks.  The number of weights in the list must be
                same as the number of variables in the VAR= list.

                The default value is 1 for each variable in the VAR=
                list.

   SHAPE=       Shape of proximity matrix to be stored in the OUT=
                data set. The available shapes are:

                TRIANGLE (TRI) Stored as a lower triangular matrix.  This
                               is the default.

                SQUARE (SQR)   Stored as a squared matrix.

   OPTIONS=     List of additional options separated by blanks:

                PRINT          Print the distance matrices.

                NOMISS         Generate missing distances for
                               observations with missing values
                               for the selected variables through the
                               specifications of anominal=, nominal=,
                               ordinal=, interval=, and ratio= arguments.

                               In general, missing values are tolerated,
                               but some particular methods may get upset
                               by missing values.  This option may
                               increase efficiency considerably
                               depending on the method.

                               Note that PROC CLUSTER will not accept
                               distance matrices with missing values.


                REPLACE        Replace missing data by zero in the
                               standardized data (which corresponds
                               to the location measure before
                               standardizing). To replace missing data
                               by something else, see the MISSING=
                               argument.

                               OPTIONS= REPLACE implies standardization.
                               The following rules are used to standardize
                               variables:

                            o  When METHOD= GOWER,
                               STD(INTER)= RANGE is the mandatory method
                               of standardization for interval variables,
                               and the default method of standardization
                               for ratio variables is STDRATIO= MAXABS.

                            o  When METHOD= DGOWER,
                               STD(INTER)= RANGE and STDRATIO= MAXABS are
                               the default methods of standardization for
                               interval variables and ratio variables,
                               respectively.

                            o  When METHOD= anything other than
                               GOWER|DGOWER, STD(INTER)= MEAN is the
                               default method of standardization for both
                               interval and ratio variables.

                               You may not specify both REPLACE and
                               REPONLY.


                REPONLY        Replace missing data by the location
                               measure specified by the STD=, STDINTER=,
                               STDRATIO=, or MISSING= arguments, but do
                               _not_ standardize the data.  If MISSING= is
                               specified, missing values are replaced by the
                               location measure specified by the MISSING=.

                               If MISSING= is absent, the following rules
                               are used to replace missing values:

                            o  When METHOD= GOWER,
                               the location measure specified by
                               STD(INTER)= RANGE is used to replace
                               missing values for interval variables,
                               and the location measure specified by
                               STDRATIO= MEAN is used as a default
                               estimate to replace missing values for
                               ratio variables.

                            o  When METHOD= DGOWER,
                               the location measure specified by
                               STD(INTER)= RANGE and the location measure
                               specified by STDRATIO= MEAN are used by
                               default to replace missing values for
                               interval variables and for ratio variables,
                               respectively.

                            o  When METHOD= anything other than
                               GOWER|DGOWER, by STD(INTER)= MEAN and the
                               location measure specified by STDRATIO= MEAN
                               are used by default to replace missing values
                               for interval variables and for ratio
                               variables, respectively.

                               You may not specify both REPLACE and
                               REPONLY.

**********************************************************************


The following statements may be useful for diagnosing errors:

   %let _notes_=1;       %* Prints SAS notes for all steps;
   %let _echo_=1;        %* Prints the arguments to the DISTANCE macro;
   %let _echo_=2;        %* Prints the arguments to the DISTANCE macro
                            after defaults have been set;
   options mprint;       %* Prints SAS code generated by the macro
                            language;
   options mlogic symbolgen; %* Prints lots of macro debugging info;

This macro normally spends a lot of time checking the arguments you
specify for validity, in hopes of avoiding mysterious error messages
from the generated SAS code.  You can reduce the amount of time spent
checking arguments (and thereby speed up the macro at the risk of
getting inscrutable error messages if you make a mistake) by using one
of the following statements before invoking the macro:

   %let _check_=1; %* reduce argument checking;
   %let _check_=0; %* suppress argument checking--use at your own risk!;

No products other than base SAS software are required for using the
%DISTANCE macro unless STD=AGK(p) or L(p) is specified, in which case
SAS/STAT software is required.

Use of the STD= argument requires the %STDIZE macro.

This macro will not work in 6.04 or earlier releases.

Note: Due to the limitation on the length of a macro variable (8)
      the maximum number of variables will be restricted to 999.

 *********************************************************************/
%* checks whether 6.10 version of xmacro has been included;
%macro xmacinc;
   %global _xmacro_;
   %if %bquote(&_xmacro_)= %then
      %put ERROR: XMACRO has NOT been included.;
   %else %if %bquote(&_xmacro_)=done %then %do;
      %let _xmacro_=;
      %put %qcmpres(ERROR: The 6.09 version of XMACRO has been
           included but the 6.10 or later version is required.);
   %end;
   %if %bquote(&_xmacro_)= %then %do;
      %put NOTE: The DISTANCE macro will not be compiled.;
      %* comment out the rest of this file;
      %unquote(%str(/)%str(*))
   %end;
%mend xmacinc;

*xmacinc;

*===================== BEGIN DISTANCE MACRO ==========================;

%unquote(%xmain(
   distance(data=,var=,id=,copy=,by=,
            absent=,
            anominal=,ano=,
            nominal=,nom=,
            ordinal=,ord=,
            interval=,int=,
            ratio=,rat=,
            out=,prefix=,shape=,
            method=,undef=,
            std=,stdinter=,stdratio=,
            missing=,vardef=DF,
            anowgt=,nomwgt=,ordwgt=,intwgt=,ratwgt=,varwgt=,
            freq=,weight=,options=)
   ))

************ check arguments ************;
%xchkdata(data,_LAST_)
%xchkone(id,,)
%xchklist(copy)
%xchklist(by)
%xchklist(absent,'NONE' '0',Q)
%xchklist(anominal)
%xchklist(ano)
%xchklist(nominal)
%xchklist(nom)
%xchklist(ordinal)
%xchklist(ord)
%xchklist(interval)
%xchklist(int)
%xchklist(ratio)
%xchklist(rat)
%local s1;
%if %bquote(&var)=
    and %bquote(&anominal)= and %bquote(&ano)=
    and %bquote(&nominal)= and %bquote(&nom)=
    and %bquote(&ordinal)= and %bquote(&ord)=
    and %bquote(&interval)= and %bquote(&int)=
    and %bquote(&ratio)= and %bquote(&rat)= %then %do;
 %xchklist(var,_NUMERIC_);
 %if &_notes_ %then %do;
   %let s1=%bquote(NOTE: No variable lists are specified, all the numerical);
   %let s1=&s1 %bquote(variables in the DATA= data set will be included);
   %let s1=&s1 %bquote(in the analysis and are treated as VAR=_NUMERIC_.);
   %put &s1;
 %end;
%end;
%else %do;
   %if %bquote(&var)^= %then
      %xchklist(var);
%end;

%xchkone(prefix,,)
%xchkdef(method,EUCLID)
%xchkmiss(undef)
%xchkdef(std)
%xchkdef(stdinter)
%xchkdef(stdratio)
%xchkdef(missing)
%xchkkey(vardef,DF,N DF WDF WEIGHT WGT:WEIGHT)

%************************** xchkflst **********************;
%* Issue error message if an argument value does not contain a list
   of floating-point values, and return the values in a macro array;
%* _arg      name of argument to check;
%* _nval     (optionally returned) number of values in the list;
%* _pref     (optional) prefix for macro array;

%macro xchkflst(_arg,_nval,_pref);
   %xchkech(&_arg);
   %xchkeq(&_arg,,y)
   %local __i __tok __ok;
   %let __i=1;
   %let __ok=1;
%loop:
      %let __tok=%qscan(&&&_arg,&__i,%str( ));
      %if &__tok= %then %goto break;
      %if %datatyp(&__tok)=CHAR %then %do;
         %let __ok=0;
         %put %qcmpres(ERROR: the value "&__tok" of the
            %upcase(&_arg)= argument is not a numeric
            constant.);
      %end;
      %if &_pref^= %then %do;
         %global &_pref&__i;
         %let &_pref&__i=&__tok;
      %end;
      %let __i=%eval(&__i+1);
   %goto loop;
%break:
   %if ^&__ok %then %let _xrc_=%qcmpres(The value of the argument
      %qupcase(&_arg)= must be a list of numeric constants);
   %if &_nval^= %then %do;
      %global &_nval;
      %let &_nval=%eval(&__i-1);
   %end;
   %xxbug(XCHKFLST,&_arg)
%mend xchkflst;

%xchkflst(anowgt,nanowgt,)
%xchkflst(nomwgt,nnomwgt,)
%xchkflst(ordwgt,nordwgt,)
%xchkflst(intwgt,nintwgt,)
%xchkflst(ratwgt,nratwgt,)
%xchkflst(varwgt,nvarwgt,vwgt)

%xbug(,anowgt nanowgt)
%xbug(,nomwgt nnomwgt)
%xbug(,ordwgt nordwgt)
%xbug(,intwgt nintwgt)
%xbug(,ratwgt nratwgt)
%xbug(,varwgt nvarwgt)

%xchkname(freq)
%xchkname(weight)
%xchkeq(options)

%global vtype;  %* type of VAR= variables: 1=num 2=char 3=mixed;
%global atype;  %* type of ANOMINAL= variables: 1=num 2=char 3=mixed;
%global ntype;  %* type of NOMINAL= variables: 1=num 2=char 3=mixed;
%global otype;  %* type of ORDINAL= variables: 1=num;
%global itype;  %* type of INTERVAL= variables: 1=num;
%global rtype;  %* type of RATIO= variables: 1=num;

%let _mainerr=FALSE;

*********** initialize variables ********;
%let riowgflg=0;  %* flag is if any of ORDWGT=, INTWGT=,
                  %* RATWGT=, and VARWGT= has been specified.;

************ process aliases ************;
%let anominal=&anominal &ano;
%let nominal=&nominal &nom;
%let ordinal=&ordinal &ord;
%let interval=&interval &int;
%let ratio=&ratio &rat;

************ process ABSENT= ************;
%let absent=%upcase(&absent);
%xbug(, absent)

************ process METHOD= ************;
%let gowerflg=FALSE;
%let numflg=FALSE;

%let _lp=%bquote(%str(%());

%let ucmethod=%upcase(%bquote(&method));

%xbug(,ucmethod)

%if %bquote(&ucmethod)=DGOWER or %bquote(&ucmethod)=GOWER %then
  %let gowerflg=TRUE;
%let grp1=%str(EUCLID,SQEUCLID,SIZE,SHAPE,COV,CORR,DCORR,
               SQCORR,DSQCORR,CITY,CHEBYCHE);
%let grp2=%str(SIMRATIO DISRATIO NONMETRI COSINE DOT OVERLAP DOVERLAP
                    CHISQ CHI PHISQ PHI);
%if %index(&grp1,&ucmethod)^=0 or
    %index(&grp2,&ucmethod)^=0 or
    %index(&ucmethod,POWER&_lp)=^0 or
    %index(&ucmethod,L&_lp)^=0 or gowerflg=TRUE %then %do;
       %if %bquote(&ucmethod)^=RR %then %let numflg=TRUE;
    %end;

%xbug(,gowerflg)
%xbug(,numflg)

************ process Shape= ************;
%let square=0;
%xchkkey(shape,TRI,TRI:TRIANGLE TRIANGLE SQR:SQUARE SQUARE)
%xbug(,shape)
%if %xsubstr(&shape,1,6)=SQUARE or %xsubstr(&shape,1,3)=SQR %then
   %let square=1;

%if &square %then %xchkdsn(out,_TEMP_);
%else %xchkdsn(out,_DATA_);

%if &_xrc_^=OK %then %goto exit;

************ process Options= ************;
%* REPONLY is implemented for compatibility with the STDIZE macro;
%local distprt nomiss replace reponly;
%let distprt=0;
%let nomiss=0;
%let replace=0;
%let reponly=0;

%let n=1;
%let token=%scan(&options,&n,%str( ));
%do %while(%bquote(&token)^=);
   %let token=%upcase(&token);
   %if %xsubstr(&token,1,5)=PRINT %then %let distprt=1; %else
   %if %xsubstr(&token,1,6)=NOMISS %then %let nomiss=1; %else
   %if %xsubstr(&token,1,7)=REPLACE %then %let replace=1; %else
   %if %xsubstr(&token,1,7)=REPONLY %then %let reponly=1; %else
   %do;
      %let _xrc_=BAD;
      %put ERROR: Unrecognized option &token..;
   %end;
   %let n=%eval(&n+1);
   %let token=%scan(&options,&n,%str( ));
%end;

%xbug(options:,distprt nomiss replace reponly)

%if &replace and &reponly %then %do;
   %let _xrc_=%qcmpres(You may not specify both REPLACE and REPONLY
               options);
   %put ERROR: &_xrc_..;
%end;

%if &_xrc_^=OK %then %goto exit;

************ process BY variables ************;
%xbylist;
%xvlist(data=&data,_list=by,_name=by,valid=0123)

************ dummy data step to check for errors ***************;
%xchkvar(&data,&by,&var &id &copy &freq &weight)
%if &_xrc_^=OK %then %goto exit;

************** process ID= list ***************;
%xvlist(data=&data,_list=id,_name=id,_count=nid,valid=02)
%if &nid>1 %then %do;
   %let _xrc_=%qcmpres(ID= list may contain only one variable);
   %put ERROR: &_xrc_..;
%end;

************** process COPY= list ***************;
%xvlist(data=&data,_list=copy,_name=copy,_type=tcopy,_count=ncopy,
        valid=0123)

************** process FREQ= variable ***************;
%xvfreq(&data)

************** process WEIGHT= variable ***************;
%xvweight(&data)

************** print error message ***************;
%macro prnterr;
   %local s1;

   %let s1=%bquote(You may not specify both VAR= and any of the);
   %let s1=&s1 %bquote(following variable lists:);
   %let _xrc_=&s1 %bquote(ANOMINAL=,NOMINAL=, ORDINAL=, INTERVAL=, and RATIO=);
   %put ERROR: &_xrc_..;
%mend prnterr;

%macro prnterr2(list1,list2);
   %let _xrc_=%qcmpres(The number of weights in the &list1= list must equal
                       to the number of variables in the &list2= list);
   %put ERROR: &_xrc_..;
%mend prnterr2;

************** process ANOMINAL= list ***************;
%if %bquote(&anominal)^= and %bquote(&var)^= %then %do;
   %prnterr;
%end;
%else %if %bquote(&anominal)^= %then %do;
   %let anominal=%upcase(&anominal);
   %xbug(,anominal)

   %xvlist(data=&data,_list=anominal,_name=anom,_fmt=afmt,
           _type=tanom,_count=nanom,_ltype=atype,valid=0123,replace=1);

   %xbug(,nanom anominal)
   %do n=1 %to &nanom;
      %xbug(, afmt&n)
   %end;
   %if %bquote(&anowgt)^= %then %do;
      %if &nanowgt^=&nanom %then %prnterr2(ANOWGT,ANOMINAL);
   %end;

%end;

%if &_xrc_^=OK %then %goto exit;

************** process NOMINAL= list ***************;
%if %bquote(&nominal)^= and %bquote(&var)^= %then %do;
   %prnterr;
%end;
%else %if %bquote(&nominal)^= %then %do;
   %let nominal=%upcase(&nominal);
   %xbug(,nominal)

   %xvlist(data=&data,_list=nominal,_name=nom,
           _type=tnom,_count=nnom,_ltype=ntype,valid=0123,replace=1);

   %xbug(,nnom nominal ntype)
   %if %bquote(&nomwgt)^= %then %do;
      %if &nnomwgt^=&nnom %then %prnterr2(NOMWGT,NOMINAL);
   %end;
%end;

%if &_xrc_^=OK %then %goto exit;

************** process ORDINAL= list ***************;
%if %bquote(&ordinal)^= and %bquote(&var)^= %then %do;
   %prnterr;
%end;
%else %if %bquote(&ordinal)^= %then %do;
   %if %bquote(&freq)^= %then
      %put %qcmpres(WARNING: FREQ= is ignored when computing ranks for
           variables in the ORDINAL= list);

   %if %bquote(&weight)^= %then
      %put %qcmpres(WARNING: WEIGHT= is ignored when computing ranks for
           variables in the ORDINAL= list);

   %local rankout;
   %let rankout=_RANK_;

   %let ordinal=%upcase(&ordinal);
   %xbug(,ordinal)
   %xvlist(data=&data,_list=ordinal,_name=ord,
           _count=nord,_ltype=otype,valid=01,replace=1);

   %xbug(,nord ordinal otype)

   %if &otype^=1 and &otype^=0 %then %do;
      %put ERROR: ORDINAL= variables must be numeric.;
      %let _xrc_=BAD;
   %end;

   %if %bquote(&ordwgt)= %then %do;
      %do n=1 %to &nord;
         %let ordwgt=&ordwgt 1;
      %end;
      %let nordwgt=&nord;
   %end;
   %else %do;
      %if &nordwgt^=&nord %then %prnterr2(ORDWGT,ORDINAL);
      %let riowgflg=1;
   %end;
%end;

%if &_xrc_^=OK %then %goto exit;

************** process INTERVAL= list ***************;
%if %bquote(&interval)^= and %bquote(&var)^= %then %do;
   %prnterr;
%end;
%else %if %bquote(&interval)^= %then %do;
   %let interval=%upcase(&interval);

   %xbug(,interval)

   %xvlist(data=&data,_list=interval,_name=int,
           _count=nint,_ltype=itype,valid=01,replace=1);

   %xbug(,nint interval)

   %if &itype^=1 and &itype^=0 %then %do;
      %put ERROR: INTERVAL= variables must be numeric.;
      %let _xrc_=BAD;
   %end;

   %if %bquote(&intwgt)= %then %do;
      %do n=1 %to &nint;
         %let intwgt=&intwgt 1;
      %end;
      %let nintwgt=&nint;
   %end;
   %else %do;
      %if &nintwgt^=&nint %then %prnterr2(INTWGT,INTERVAL);
      %let riowgflg=1;
   %end;
%end;

%if &_xrc_^=OK %then %goto exit;

************** process RATIO= list ***************;
%if %bquote(&ratio)^= and %bquote(&var)^= %then %do;
   %prnterr;
%end;
%else %if %bquote(&ratio)^= %then %do;
   %let ratio=%upcase(&ratio);
   %xbug(,ratio)
   %xvlist(data=&data,_list=ratio,_name=rat,_fmt=rfmt,
          _count=nrat,_ltype=rtype,valid=01,replace=1);

   %xbug(,nrat ratio rtype)

   %if &rtype^=1 and &rtype^=0 %then %do;
      %put ERROR: RATIO= variables must be numeric.;
      %let _xrc_=BAD;
   %end;

   %if %bquote(&ratwgt)= %then %do;
      %do n=1 %to &nrat;
         %let ratwgt=&ratwgt 1;
      %end;
      %let nratwgt=&nrat;
   %end;
   %else %do;
      %if &nratwgt^=&nrat %then %prnterr2(RATWGT,RATIO);
      %let riowgflg=1;
   %end;
%end;

%if &_xrc_^=OK %then %goto exit;

************** process VAR= list ***************;
%let remove=;
%let var=%upcase(&var);
%xbug(,var)
%if %bquote(&var)^= %then %do;
   %if %bquote(&id)^= %then %let remove=&remove id;
   %if %bquote(&by)^= %then %let remove=&remove by;
   %if %bquote(&copy)^= %then %let remove=&remove copy;
   %if %bquote(&freq)^= %then %let remove=&remove freq;
   %if %bquote(&weight)^= %then %let remove=&remove weight;
   %if %bquote(&anominal)^= %then %let remove=&remove anom;
   %if %bquote(&nominal)^= %then %let remove=&remove nom;
   %if %bquote(&ordinal)^= %then %let remove=&remove ordinal;
   %if %bquote(&interval)^= %then %let remove=&remove interval;
   %if %bquote(&ratio)^= %then %let remove=&remove ratio;
%end;
%xbug(,remove)

%xvlist(data=&data,_list=var,_name=var,_fmt=vfmt,
        _type=type,_count=nvar,_ltype=vtype,
        remove=&remove,valid=0123,replace=1)
%xbug(,nvar var)
%xbug(,vtype)

%if %bquote(&var)^= %then %do;

   %if %bquote(&varwgt)^= %then %do;
      %if &nvarwgt^=&nvar %then %prnterr2(VARWGT,VAR);
   %end;

%end;

%let numvar=;
%let numno=0;

%do n=1 %to &nvar;
   %if &&type&n=1 %then %do;
      %let numno=%eval(&numno+1);
      %let numvar=&numvar &&var&n;
   %end;
%end;

%xbug(,numvar)
%xbug(,numno)

%if %bquote(&numvar)^= %then %do;
   %let nuvarwgt=;
   %if &varwgt^= %then %do;
      %do n=1 %to &nvar;
         %if &&type&n=1 %then
            %let nuvarwgt=&nuvarwgt &&vwgt&n;
      %end;
      %let riowgflg=1;
   %end;

%xbug(,nuvarwgt)
%end;

%if &_xrc_^=OK %then %goto exit;

%xchkend(&data);
%if &_xrc_^=OK %then %goto exit;


%****** utility for counting number of missing variables  ****;
%macro countmx(var,type,nvar);
    %local _numlst;
    %local _chrlst;
    %local _tmp1 _tmp2;

    %let _numlst=;
    %let _nchr=0;
    %let _chrmiss=0;

    %do n=1 %to &nvar;
       %let _tmp1=&type&n;
       %let _tmp1=%unquote(%nrstr(&)&_tmp1);
       %let _tmp2=&var&n;
       %let _tmp2=%unquote(%nrstr(&)&_tmp2);
       %if &_tmp1=1 %then
           %let _numlst=&_numlst &_tmp2;
       %else %if &_tmp1=2 %then %do;
           %let _nchr=%eval(&_nchr+1);
           %let _chr&_nchr=&_tmp2;
       %end;
    %end;
    %if %bquote(&_numlst)^= and &_nchr^=0 %then %do;

        _mcount+nmiss(of &_numlst);

        %do n=1 %to &_nchr;
           %let _tmp2=&&_chr&n;

           _mcount+(%bquote(&_tmp2)=' ');

        %end;
    %end;
    %else %if %bquote(&_numlst)^= and &_nchr=0 %then %do;

           _mcount+nmiss(of &_numlst);

    %end;
    %else %if %bquote(&_numlst)= and &_nchr^=0 %then %do;
        %do n=1 %to &_nchr;
           %let _tmp2=&&_chr&n;

           _mcount+(%bquote(&_tmp2)=' ');

        %end;
    %end;
%mend countmx;

%macro assgnmx(var,type,nvar);
    %local _tmp1 _tmp2;

    %do n=1 %to &nvar;
       %let _tmp1=&type&n;
       %let _tmp1=%unquote(%nrstr(&)&_tmp1);
       %let _tmp2=&var&n;
       %let _tmp2=%unquote(%nrstr(&)&_tmp2);
       %if &_tmp1=1 %then %do;
          %str(&_tmp2=.;);
       %end;
       %else %if &_tmp1=2 %then %do;
          &_tmp2=' ';
       %end;
    %end;
%mend assgnmx;

%if &nomiss and &replace=0 and &reponly=0 and %bquote(&missing)= %then %do;
    /*-------------------------------------------------*/
    /*-- assign missing values to those observations --*/
    /*-- containing at least one missing values for  --*/
    /*-- selected variables                          --*/
    /*-------------------------------------------------*/

    /*-- check if contains missing value --*/
   data _NOMISS; drop _mcount; set &data;

   %if %bquote(&anominal)^= %then %do;
      _mcount=0;
      %countmx(anom,tanom,&nanom)
      if _mcount>0 then do;
         %assgnmx(anom,tanom,&nanom);
      end;
   %end;
   %if %bquote(&nominal)^= %then %do;
      _mcount=0;
      %countmx(nom,tnom,&nnom)
      if _mcount>0 then do;
         %assgnmx(nom,tnom,&nnom)
      end;
   %end;
   %if %bquote(&ordinal)^= %then %do;
      _mcount=nmiss(of &ordinal);
      if _mcount>0 then do;
         %do n=1 %to &nord;
             %str(&&ord&n=.;);
         %end;
      end;
   %end;
   %if %bquote(&interval)^= %then %do;
      _mcount=nmiss(of &interval);
      if _mcount>0 then do;
         %do n=1 %to &nint;
            %str(&&int&n=.;);
         %end;
      end;
   %end;
   %if %bquote(&ratio)^= %then %do;
      _mcount=nmiss(of &ratio);
      if _mcount>0 then do;
         %do n=1 %to &nrat;
            %str(&&rat&n=.;);
         %end;
      end;
   %end;

   %if &numflg=TRUE %then %do;
      %if %bquote(&numvar)^= %then %do;
         _mcount=nmiss(of &numvar);
         if _mcount>0 then do;
            %do n=1 %to &nvar;
              %if &&type&n=1 %then
                 %str(&&var&n=.;);
            %end;
         end;
      %end;
   %end;
   %else %do;
      %if %bquote(&var)^= %then %do;
         _mcount=0;
         %countmx(var,type,&nvar)
         if _mcount>0 then do;
             %assgnmx(var,type,&nvar)
         end;
      %end;
   %end;

   run;

   %let data=_NOMISS;

   %xbugdo(
      title2 'debug: data=_NOMISS';
      proc print data=_NOMISS; run;
      title2 ' ';
   )


%end;

****** Generate ranks for variables in the ORDINAL= list ****;
****** Note that the FREQ stmt is _not_ used here        ****;

%if %bquote(&ordinal)^= %then %do;
   %if &otype^=0 %then %do;
       proc rank data=&data out=&rankout ties=mean;
         var &ordinal;
         by &by;
       run;
     %let data=&rankout;
     %xbugdo(
        title2 'debug: data=_RANK_';
        proc print data=&data; run;
        title2 ' ';
     )
   %end;
%end;

************** get names for distance variables **************;
%if %bquote(&prefix)= & %bquote(&id)= %then %let prefix=DIST;

   data _idby;   * get rid of unneeded variables;
      set &data; * cannot use data set option on original data set;
      %if %bquote(&id &by)= %then %str(drop _all_;);
                            %else %str(keep &id &_byvars;);
   run;
%if &syserr>4 %then %goto exit;

   proc transpose data=_idby(firstobs=1 obs=2000000000)
      %if %bquote(&prefix)^= %then prefix=&prefix;
      out=_name;
      %if %bquote(&id)^= %then %str(id &id;);
      by &by;
   run;
%if &syserr>4 %then %goto exit;

   proc transpose data=_name(obs=0) out=_name2;
      by &by;
   run;
%if &syserr>4 %then %goto exit;

   data _null_;
      set _name2(firstobs=1 obs=2000000000) end=_end;
      call symput("dist"||trim(left(put(_n_,5.))),_name_);
      if _end then call symput("ndist",trim(left(put(_n_,5.))));
   run;
%if &syserr>4 %then %goto exit;

%xbug(,ndist)
%let distvar=%xconcat(dist,&ndist);
%xbug(,distvar)

********************* standardize variables *************************;
%global _stdize_;
%local stdout stdmtd stdvar;
%let stdout=_STD_;
%let stdmtd=;

%if %bquote(&numvar)^= %then %do; *** standardize VAR= list ***;
   %if %bquote(&std)= %then %do;
      %if &replace or &reponly %then %do;
         %if %bquote(&missing)= %then
            %let stdmtd=MEAN;
      %end;
   %end;
   %else %do;
      %let stdmtd=&std;
   %end;

   %if %bquote(&ucmethod)=GOWER %then %do;
      %let stdmtd=RANGE;
   %end;
   %else %if %bquote(&ucmethod)=DGOWER %then %do;
      %if %bquote(&stdmtd)= %then %let stdmtd=RANGE;
   %end;

   %if %bquote(&stdmtd &missing)^= %then %do;
      %if %bquote(&_stdize_)= %then %do;
          %xerrset(The STDIZE macro has not been included)
         %goto exit;
      %end;
      %xstdize(data=&data,method=&stdmtd,var=&numvar,
            replace=&replace,reponly=&reponly,missing=&missing,
            vardef=&vardef,nomiss=&nomiss,_out=stdout)
      %let data=&stdout;
   %end;
%end;
%else %do;

   %let stdvar=;
   %if %bquote(&ordinal)^= or %bquote(&interval)^= %then %do;
      *** standardize ORDINAL= list and INTERVAL= list ***;
      %if %bquote(&stdinter)= %then %do;
         %if &replace or &reponly %then %do;
            %if %bquote(&missing)= %then
               %let stdmtd=MEAN;
         %end;
      %end;
      %else %let stdmtd=&stdinter;

      %if %bquote(&ucmethod)=GOWER %then %do;
         %let stdmtd=RANGE;
      %end;
      %else %if %bquote(&ucmethod)=DGOWER %then %do;
         %if %bquote(&stdmtd)= %then %let stdmtd=RANGE;
      %end;

      %let stdvar=&ordinal &interval;

      %if %bquote(&stdmtd &missing)^= %then %do;
         %if %bquote(&_stdize_)= %then %do;
             %xerrset(The STDIZE macro has not been included)
            %goto exit;
         %end;
         %xstdize(data=&data,method=&stdmtd,var=&stdvar,
               replace=&replace,reponly=&reponly,missing=&missing,
               vardef=&vardef,nomiss=0,_out=stdout)
         %let data=&stdout;
      %end;
   %end;

   %if %bquote(&ratio)^= %then %do; *** standardize RATIO= list ***;
      %let stdmtd=;
      %if %bquote(&stdratio)= %then %do;
         %if &replace or &reponly %then %do;
            %if %bquote(&missing)= %then
               %let stdmtd=MEAN;
         %end;
      %end;
      %else %let stdmtd=&stdratio;

      %if &gowerflg=TRUE %then %do;
         %if %bquote(&stdmtd)= %then %let stdmtd=MAXABS;
      %end;

      %if %bquote(&stdmtd &missing)^= %then %do;
         %let stdvar=&ratio;
         %if %bquote(&_stdize_)= %then %do;
             %xerrset(The STDIZE macro has not been included)
            %goto exit;
         %end;
         %xstdize(data=&data,method=&stdmtd,var=&stdvar,
               replace=&replace,reponly=&reponly,missing=&missing,
               vardef=&vardef,nomiss=0,_out=stdout)
         %let data=&stdout;
      %end;
   %end;

%end;

%if &stdmtd^= %then %do;
   %xbug(,stdmtd stdvar)
   %xbugdo(
      title2 'debug: data=_STD_';
      proc print data=&data; run;
      title2 ' ';
    )
%end;

************* utility for nonmissing test on mixed variables ********;
%macro mxnmiss(var,type,nvar);
    %local _numlst;
    %local _chrlst;
    %local _tmp1 _tmp2;

    %let _numlst=;
    %let _nchr=0;
    %let _chrmiss=0;

    %do n=1 %to &nvar;
       %let _tmp1=&type&n;
       %let _tmp1=%unquote(%nrstr(&)&_tmp1);
       %let _tmp2=&var&n;
       %let _tmp2=%unquote(%nrstr(&)&_tmp2);
       %if &_tmp1=1 %then
           %let _numlst=&_numlst &_tmp2;
       %else %if &_tmp1=2 %then %do;
           %let _nchr=%eval(&_nchr+1);
           %let _chr&_nchr=&_tmp2;
       %end;
    %end;
    %if %bquote(&_numlst)^= and &_nchr^=0 %then %do;

        or nmiss(of &_numlst)

        %do n=1 %to &_nchr;
           %let _tmp2=&&_chr&n;

           or %bquote(&_tmp2)=' '

        %end;
    %end;
    %else %if %bquote(&_numlst)^= and &_nchr=0 %then %do;

        or nmiss(of &_numlst)

    %end;
    %else %if %bquote(&_numlst)= and &_nchr^=0 %then %do;
        %do n=1 %to &_nchr;
           %let _tmp2=&&_chr&n;

           or %bquote(&_tmp2)=' '

        %end;
    %end;
%mend mxnmiss;

************************* MAIN DATA STEP ****************************;
%xnotes(1)
   data &out(type=distance keep=&_byvars &id &distvar &copy
                                &freq &weight);
         _errflg=0; /* error flag in data step */
         set &data;

%if %bquote(&anominal)^= %then %do;
    %do n=1 %to &nanom;    *** to define ANOMINAL variables correctly;
         _ay&n=&&anom&n;   *** to set type and length correctly;
    %end;
    %if &atype<3 %then %do;  *** arrays cannot be of mixed type;
         array _ax[&nanom] &anominal;
         array _ay[&nanom] _ay1-_ay&nanom;
    %end;

   %if %bquote(&anowgt)^= %then %do;
         array _awgt[&nanom] _temporary_ (&anowgt);
   %end;
         retain _an &nanom;
%end;
%else %do;
         retain _an 0;
%end;

%if %bquote(&nominal)^= %then %do;
    %do n=1 %to &nnom;
         _ny&n=&&nom&n;     *** to define NOMINAL variables correctly;
    %end;                    *** to set type and length correctly;
    %if &ntype<3 %then %do;  *** arrays cannot be of mixed type;
         array _nx[&nnom] &nominal;
         array _ny[&nnom] _ny1-_ny&nnom;
    %end;
   %if %bquote(&nomwgt)^= %then %do;
         array _nwgt[&nnom] _temporary_ (&nomwgt);
   %end;
         retain _nn &nnom;
%end;
%else %do;
         retain _nn 0;
%end;

%let riotot=0;
%let xriolst=;
%let yriolst=;
%let riowgt=;

%if %bquote(&ordinal)^= %then %do;
    %do n=1 %to &nord;     *** to define ORDINAL variables correctly;
         _oy&n=&&ord&n;    *** to set type and length correctly;
    %end;

    %let riotot=%eval(&riotot+&nord);
    %let xriolst=&xriolst &ordinal;
    %let yriolst=&yriolst _oy1-_oy&nord;
    %let riowgt=&ordwgt;

         retain _on &nord;
%end;
%else %do;
         retain _on 0;
%end;

%if %bquote(&interval)^= %then %do;
    %do n=1 %to &nint;     *** to define INTERVAL variables correctly;
         _iy&n=&&int&n;    *** to set type and length correctly;
    %end;

    %let riotot=%eval(&riotot+&nint);
    %let xriolst=&xriolst &interval;
    %let yriolst=&yriolst _iy1-_iy&nint;
    %let riowgt=&riowgt &intwgt;
         retain _in &nint;
%end;
%else %do;
         retain _in 0;
%end;

%if %bquote(&ratio)^= %then %do;
    %do n=1 %to &nrat;     *** to define RATIO variables correctly;
          _ry&n=&&rat&n;   *** to set type and length correctly;
    %end;

          retain _rn &nrat;

    %let riotot=%eval(&riotot+&nrat);
    %let xriolst=&xriolst &ratio;
    %let yriolst=&yriolst _ry1-_ry&nrat;
    %let riowgt=&riowgt &ratwgt;
%end;
%else %do;
          retain _rn 0;
%end;

%if %bquote(&var)^= %then %do;
   %do n=1 %to &nvar;       *** to define VAR variables correctly;
         _y&n=&&var&n;      *** to set type and length correctly;
   %end;

   %if &vtype<3 %then %do;   *** arrays cannot be of mixed type;
         array _x[&nvar] &var;
         array _y[&nvar] _y1-_y&nvar;
   %end;

   %if &numno %then %do;
      %let riotot=%eval(&numno);
      %let xriolst=&xriolst &numvar;
      %let yriolst=&yriolst _y1-_y&numno;
      %let riowgt=&nuvarwgt;
   %end;
   %if %bquote(&varwgt)^= %then %do;
         array _varwgt[&nvarwgt] _temporary_ (&varwgt);
   %end;

%end;

         retain _n &nvar;
         array _dist[&ndist] &distvar;

%if &riotot %then %do;
         array _xrio[&riotot] &xriolst;
         array _yrio[&riotot] &yriolst;
   %if &riowgt^= %then %do;
         array _riowgt[&riotot] _temporary_ (&riowgt);
   %end;
%end;

         _rowmiss= 0; %* number of row missing records for debugging;

************************* loop over BY groups ***********************;
         %xdo_by
%xbugdo(put _nby=;)
%xbugdo(put 'Initialize BY group';);
            _row=0;

************************* loop over rows ****************************;
            %xdo_obs(&data)
                 %*-- number of column missing records for debugging;
               _colmiss=0;
               if _row=0 then _first=_obs;
               _row+1;
               _obs+1;
               if _row>&ndist then do;
                  put 'ERROR: Too many rows?';
                  abort;
               end;

%xbugdo(put 'debug: Process row' _row;);
%if %bquote(&id)^= & %bquote(&by)^= %then %do; %*** check ID values;
               length _vname $ 8;
               length _idname $ 8;

               call vname(_dist[_row],_vname);
   %xbugdo(put 'debug: value of _vname:' _vname;);

               _idname=left(&id);

   %if %bquote(&prefix)= %then %do;
               if  _idname >=:'1' and _idname <=:'9' then
                  _idname=upcase("_"||_idname);
               else
   %end;

                  _idname=upcase(trimn("&prefix")||_idname);

   %xbugdo(put 'debug: value of _idname:' _idname;);

               if _vname^=_idname then do;
                  put 'ERROR: The ID variable must have the same values'
                      ' in the same order in each BY group.';
                  abort;
               end;
%end;

               do _col=1 to &ndist;
                  _dist[_col]=.;
               end;

%if &nomiss %then %do; %*** skip obs with missing values;

               if (1^=1) %*** pseudo expression for syntax purpose;
    %if %bquote(&anominal)^= %then %do;
                  %mxnmiss(anom,tanom,&nanom)
    %end;
    %if %bquote(&nominal)^= %then %do;
                  %mxnmiss(nom,tnom,&nnom)
    %end;
    %if %bquote(&ordinal)^= %then %do;
                  or nmiss(of &ordinal)
    %end;
    %if %bquote(&interval)^= %then %do;
                  or nmiss(of &interval)
    %end;
    %if %bquote(&ratio)^= %then %do;
                  or nmiss(of &ratio)
    %end;
    %if &numflg=TRUE %then %do;
       %if %bquote(&numvar)^= %then %do;
                  or nmiss(of &numvar)
       %end;
    %end;
    %else %do;
       %if %bquote(&var)^= %then %do;
                  %mxnmiss(var,type,&nvar)
       %end;
    %end;

               then do;
                  _rowmiss+1;
                  %xbugdo(put 'current missing row record:' _row+1;);
                  goto nextrow;
               end;
%end;

%if %bquote(&anominal)^= %then %do;
   %if &atype<3 %then %do;  *** save data for current obs.
                            *** from ANOMINAL=list;
               do _i=1 to &nanom;
                  _ay[_i]=_ax[_i];
               end;
   %end;
   %else %do;
      %do n=1 %to &nanom;
               _ay&n=&&anom&n;
      %end;
   %end;
%end;

%if %bquote(&nominal)^= %then %do;
   %if &ntype<3 %then %do;   *** save data for current obs.
                             *** from NOMINAL=list;
               do _i=1 to &nnom;
                  _ny[_i]=_nx[_i];
               end;
   %end;
   %else %do n=1 %to &nnom;
               _ny&n=&&nom&n;
   %end;
%end;

               _ntot=_on+_in+_rn+&numno;

%if &riotot %then %do;
               do _i=1 to _ntot;  *** save data for current obs.
                                  *** from ORDINAL=, INTERVAL=,
                                  *** and RATIO= or VAR= list;
                  _yrio[_i]=_xrio[_i];
               end;
%end;

%if %bquote(&var)^= %then %do;
   %if &vtype<3 %then %do; %*** save data for current obs;
               do _i=1 to &nvar;
                  _y[_i]=_x[_i];
               end;
   %end;

   %do n=1 %to &nvar;
               _y&n=&&var&n;
   %end;
%end;

************************* loop over columns *************************;
               do _col=1 to _row;
                  _point=_first+_col;
                  set &data point=_point;

%xbugdo(put 'debug: Process column' _point;);

%if &nomiss %then %do; %*** skip obs with missing values;

               if (1^=1) %*** pseudo expression for syntax purpose;

   %if %bquote(&anominal)^= %then %do;
                  %mxnmiss(anom,tanom,&nanom)
   %end;
   %if %bquote(&nominal)^= %then %do;
                  %mxnmiss(nom,tnom,&nnom)
   %end;
   %if %bquote(&ordinal)^= %then %do;
                  or nmiss(of &ordinal)
   %end;
   %if %bquote(&interval)^= %then %do;
                  or nmiss(of &interval)
   %end;
   %if %bquote(&ratio)^= %then %do;
                  or nmiss(of &ratio)
   %end;
   %if &numflg=TRUE %then %do;
       %if %bquote(&numvar)^= %then %do;
                  or nmiss(of &numvar)
       %end;
   %end;
   %else %do;
      %if %bquote(&var)^= %then %do;
                  %mxnmiss(var,type,&nvar)
      %end;
   %end;
               then do;
                  _colmiss+1;
                  %xbugdo(put 'missing column record:' _point;);
                  goto nextcol;
               end;
%end;

************************* compute distance **************************;
%let _xrc_=%qcmpres(No macro found to implement METHOD=&ucmethod);
%let method=%unquote(&method);
%xbug(,method)

               length _outstr $ 82;  %* for error message in some methods;


                  %&method
%if &_xrc_^=DIST_OK %then %do;
   %let _mainerr=TRUE;
   %put ERROR: &_xrc_..;
   %goto exit;
%end;
%let _xrc_=OK;

                  if _errflg then do;
                     call symput("_xrc_",_outstr);
                     goto errout;
                  end;
%if &square %then %do;
                  if (_col eq _row) & (nmiss(_d)=0) then
                     _d = _d / 2.;
%end;
                  _dist[_col]=_d;

%if &nomiss %then %do;
nextcol:;
%end;
               end;
************************* end loop over columns *********************;

%if &nomiss %then %do;
nextrow:;
%end;
%if &_putall_ %then %do;
               put _all_;
%end;
               output;
            %xend_obs
************************* end loop over rows ************************;

            if _row<&ndist then do; *** add extra rows to form square;
               do _col=1 to &ndist;
                  _dist[_col]=.;
               end;
%if %bquote(&copy)^= & %bquote(&by)^= %then %do n=1 %to &ncopy;
               &&copy&n= %if &&tcopy&n=1 %then .; %else ' '; ;
%end;
               do _i=_row+1 to &ndist;
%if %bquote(&id)^= %then %do;
                  call vname(_dist[_i],&id);
%end;
                  output;
               end;
            end;
%xbugdo(put 'total missing record:' _rowmiss;);
%xbugdo(put 'Terminate BY group';);

         %xend_by
************************* end loop over BY groups ******************;
errout:
         ;
         stop;
   run;

%if &syserr>4 %then %do;
   %let _mainerr=TRUE;
   %goto exit;
%end;

************************* END MAIN DATA STEP ************************;
%if &square %then %do;

   proc transpose data=&out out=_TRAN_ prefix=v;
      var &distvar;
%if %bquote(&_byvars)^= %then %do;
      by &_byvars;
%end;
   run;

   data &out(type=distance);
      merge &out _TRAN_;
      array _dist &distvar;
      array _tran v1-v&ndist;
      drop v1-v&ndist

%if %upcase(%bquote(&id))^=_NAME_ %then %do;
           _NAME_
%end;
      ;
   do _i=1 to &ndist;
      if nmiss(_tran[_i])=0 then
         _dist[_i] + _tran[_i];
   end;
%end;

%if &distprt %then %do;
   proc print uniform;
      var &distvar &copy;
      %if %bquote(&id)^= %then %str(id &id;);
      %if %bquote(&by)^= %then %str(by &by;);
%end;

************************* terminate ********************************;

%exit:

%if &_mainerr=TRUE %then %do;
   end; end; end; %xnotes(0) run cancel; %xnotes(1)
   %let _mainerr=FALSE;
%end;

%xterm;

%mend distance;


%***************** DISTANCE & SIMILARITY FUNCTIONS *******************;
%macro rio_chk; %* utility to check on ORDINAL=, INTERVAL=, RATIO= lists;
   %local s1;
   %let _semerr=0;

   %if &ratio= and &interval= and &ordinal= and &numvar= %then %do;
       %let _xrc_=%qcmpres(You must specify at least one numeric variable
                  from ORDINAL=, INTERVAL=, RATIO=, or VAR= lists);
       %let _semerr=1;
   %end;

   %if &anominal^= or &nominal^= %then %do;
       %let s1=%bquote(ANOMINAL= or NOMINAL= are illegitimate when);
       %let _xrc_=&s1 %bquote(METHOD= &ucmethod is specified);
       %let _semerr=1;
   %end;

   %if &numno ^= &nvar %then
      %put %qcmpres(WARNING: Character variable(s) in the VAR= list is(are)
            excluded from the analysis.);
%mend rio_chk;

%macro legit2;  %* legitmate check with 2 variable lists;
   %if &anominal^= or &nominal^= %then %do;
       %let s1=%bquote(ANOMINAL= or NOMINAL= are illegitimate when);
       %let _xrc_=&s1 %bquote(METHOD= &ucmethod is specified);
       %let _semerr=1;
   %end;
%mend legit2;

%macro euclid; %* Euclidean distance;
   %let _xrc_=DIST_OK;
   %local _semerr;

   %rio_chk

   %legit2

   %if &_semerr=0 %then %do;
      %if %bquote(&undef)= %then %let undef=.;

      _d=0;
      %if &riowgflg %then %do;
         %if &nomiss %then %do;
            do _i=1 to _ntot;
               _d+_riowgt[_i]*(_yrio[_i]-_xrio[_i])**2;
            end;
            _d=sqrt(_d);
         %end;
         %else %do;
            _sumwgt=0; _sriowgt=0;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                  _sumwgt+_riowgt[_i];
                  _d+_riowgt[_i]*(_yrio[_i]-_xrio[_i])**2;
               end;
               _sriowgt+_riowgt[_i];
            end;
            if _sumwgt then _d=sqrt(_d*_sriowgt/_sumwgt);
            else _d=&undef;
         %end;
      %end;
      %else %do;
         %if &nomiss %then %do;
            do _i=1 to _ntot;
               _d+(_yrio[_i]-_xrio[_i])**2;
            end;
            _d=sqrt(_d);
         %end;
         %else %do;
            _m=0;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                  _m+1;
                  _d+(_yrio[_i]-_xrio[_i])**2;
               end;
            end;
            if _m then _d=sqrt(_d*_ntot/_m);
            else _d=&undef;
         %end;
      %end;
   %end;
%mend euclid;

%macro sqeuclid; %* Squared Euclidean distance;
   %let _xrc_=DIST_OK;
   %local _semerr;

   %rio_chk;

   %if &_semerr=0 %then %do;
      %if %bquote(&undef)= %then %let undef=.;

         _d=0;
      %if &riowgflg %then %do;
         %if &nomiss %then %do;
            do _i=1 to _ntot;
               _d+_riowgt[_i]*(_yrio[_i]-_xrio[_i])**2;
            end;
         %end;
         %else %do;
            _sumwgt=0; _sriowgt=0;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                  _d+_riowgt[_i]*(_yrio[_i]-_xrio[_i])**2;
               end;
               _sriowgt+_riowgt[_i];
            end;
            if _sumwgt then _d=_d*_sriowgt/_sumwgt;
            else _d=&undef;
         %end;
      %end;
      %else %do;
         %if &nomiss %then %do;
            do _i=1 to _ntot;
               _d+(_yrio[_i]-_xrio[_i])**2;
            end;
         %end;
         %else %do;
            _m=0;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                  _m+1;
                  _d+(_yrio[_i]-_xrio[_i])**2;
               end;
            end;
            if _m then _d=_d*_ntot/_m;
            else _d=&undef;
         %end;
      %end;
   %end;
%mend sqeuclid;

%macro sums; %* utility to compute sums;
         _sx=0;
         _sy=0;
         _sumwgt=0;

   %if &riowgflg %then %do;
      %if &nomiss %then %do;
            do _i=1 to _ntot;
               _sx+_xrio[_i]*_riowgt[_i];
               _sy+_yrio[_i]*_riowgt[_i];
               _sumwgt+_riowgt[_i];
            end;
      %end;
      %else %do;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                  _sx+_xrio[_i]*_riowgt[_i];
                  _sy+_yrio[_i]*_riowgt[_i];
                  _sumwgt+_riowgt[_i];
               end;
            end;
      %end;
   %end;
   %else %do;
      %if &nomiss %then %do;
            do _i=1 to _ntot;
               _sx+_xrio[_i];
               _sy+_yrio[_i];
            end;
            _sumwgt=_ntot;
      %end;
      %else %do;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                  _sumwgt+1;
                  _sx+_xrio[_i];
                  _sy+_yrio[_i];
               end;
            end;
      %end;
   %end;

%mend sums;

%macro size; %* Size distance;
   %let _xrc_=DIST_OK;
   %local _semerr;

   %rio_chk;

   %if &_semerr=0 %then %do;
      %if %bquote(&undef)= %then %let undef=.;
      %sums
         if _sumwgt then do;
            _d=abs(_sx-_sy)/sqrt(_sumwgt)
      %if &riowgflg %then %do;
               *_sriowgt/_sumwgt;
      %end;
      %else %do;
               *_ntot/_sumwgt;
      %end;
         end;
         else _d=&undef;
   %end;
%mend size;

%macro shape; %* Shape distance;
   %let _xrc_=DIST_OK;
   %local _semerr;

   %rio_chk;

   %if &_semerr=0 %then %do;
      %if %bquote(&undef)= %then %let undef=.;
      %sums

         if _sumwgt then do;
            _sx=_sx/_sumwgt;
            _sy=_sy/_sumwgt;
         end;
         _d=0;

      %if &riowgflg %then %do;
         %if &nomiss %then %do;
            do _i=1 to _ntot;
               _xm=_xrio[_i]-_sx;
               _ym=_yrio[_i]-_sy;
               _d+_riowgt[_i]*(_xm-_ym)**2;
            end;
            _d=sqrt(_d);
         %end;
         %else %do;
            _sriowgt=0;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                  _xm=_xrio[_i]-_sx;
                  _ym=_yrio[_i]-_sy;
                  _d+_riowgt[_i]*(_xm-_ym)**2;
               end;
               _sriowgt+_riowgt[_i];
            end;
            _d=sqrt(_d*_sriowgt/_sumwgt);
         %end;
      %end;
      %else %do;
         %if &nomiss %then %do;
            do _i=1 to _ntot;
               _xm=_xrio[_i]-_sx;
               _ym=_yrio[_i]-_sy;
               _d+(_xm-_ym)**2;
            end;
            _d=sqrt(_d);
         %end;
         %else %do;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                  _xm=_xrio[_i]-_sx;
                  _ym=_yrio[_i]-_sy;
                  _d+(_xm-_ym)**2;
               end;
            end;
            _d=sqrt(_d*_ntot/_sumwgt);
         %end;
      %end;

   %end;
%mend shape;

%macro getvdiv;
            %let vardef=%bquote(%upcase(&vardef));
            %if &vardef=DF %then %do;
               _vardiv=_ntot-1;
            %end;
            %else %if &vardef=N %then %do;
               _vardiv=_ntot;
            %end;
            %else %if &vardef=WDF %then %do;
               _vardiv=_sumwgt-1;
            %end;
            %else %do;
               _vardiv=_sumwgt;
            %end;
%mend getvdiv;

%macro getvdivm;
            %let vardef=%bquote(%upcase(&vardef));
            %if &vardef=DF %then %do;
               _vardiv=_m-1;
            %end;
            %else %if &vardef=N %then %do;
               _vardiv=_m;
            %end;
            %else %if &vardef=WDF %then %do;
               _vardiv=_sumwgt-1;
            %end;
            %else %do;
               _vardiv=_sumwgt;
            %end;

%mend getvdivm;

%macro cov; %* Covariance;
   %let _xrc_=DIST_OK;
   %local _semerr;

   %rio_chk;

   %if &_semerr=0 %then %do;
      %if %bquote(&undef)= %then %let undef=.;

      %sums
         if _sumwgt then do;
            _d=0; _m=0;
            _sx=_sx/_sumwgt;
            _sy=_sy/_sumwgt;

      %if &nomiss %then %do;
            do _i=1 to _ntot;
         %if &riowgflg %then %do;
               _d+_riowgt[_i]*((_xrio[_i]-_sx)*(_yrio[_i]-_sy));
         %end;
         %else %do;
               _d+(_xrio[_i]-_sx)*(_yrio[_i]-_sy);
         %end;
            end;
            _m=_ntot;
            %getvdiv
            _d=_d/_vardiv;
      %end;
      %else %do;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
         %if &riowgflg %then %do;
                  _d+_riowgt[_i]*((_xrio[_i]-_sx)*(_yrio[_i]-_sy));
         %end;
         %else %do;
                  _d+(_xrio[_i]-_sx)*(_yrio[_i]-_sy);
         %end;
                  _m+1;
               end;
            end;
            %getvdivm
            if _vardiv then _d=_d/_vardiv;
            else _d=&undef;
      %end;
         end;
         else _d=&undef;
   %end;
%mend cov;

%macro sscp; %* utility to compute sums of squares and cross-products;
   %sums
         _sx2=0; _sy2=0; _sxy=0;


         if _sumwgt then do;
            _sx=_sx/_sumwgt;
            _sy=_sy/_sumwgt;

   %if &nomiss %then %do;
            do _i=1 to _ntot;
               _xm=_xrio[_i]-_sx;
               _ym=_yrio[_i]-_sy;

      %if &riowgflg %then %do;
               _sx2+_riowgt[_i]*_xm**2;
               _sy2+_riowgt[_i]*_ym**2;
               _sxy+_riowgt[_i]*(_xm*_ym);
      %end;
      %else %do;
               _sx2+_xm**2;
               _sy2+_ym**2;
               _sxy+_xm*_ym;
      %end;
            end;
   %end;
   %else %do;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                  _xm=_xrio[_i]-_sx;
                  _ym=_yrio[_i]-_sy;
      %if &riowgflg %then %do;
                  _sx2+_riowgt[_i]*_xm**2;
                  _sy2+_riowgt[_i]*_ym**2;
                  _sxy+_riowgt[_i]*_xm*_ym;
      %end;
      %else %do;
                  _sx2+_xm**2;
                  _sy2+_ym**2;
                  _sxy+_xm*_ym;
      %end;
               end;
            end;
   %end;
         end;
%mend sscp;

%macro corr; %* Correlation;
   %let _xrc_=DIST_OK;
   %local _semerr;

   %rio_chk;

   %if &_semerr=0 %then %do;
      %if %bquote(&undef)= %then %let undef=.;
      %sscp
      if _sx2*_sy2 then _d=_sxy/sqrt(_sx2*_sy2);
      else _d=&undef;
   %end;
%mend corr;

%macro dcorr; %* Correlation transformed to Euclidean distance;
   %let _xrc_=DIST_OK;
   %local _semerr;

   %rio_chk;

   %if &_semerr=0 %then %do;
      %if %bquote(&undef)= %then %let undef=.;
      %local save; %let save=&undef;
      %let undef=.;
      %corr
      %let undef=&save;
      if _d=. then _d=&undef;
      else _d=sqrt(max(0,1-_d));
   %end;
%mend dcorr;

%macro sqcorr; %* Squared correlation;
   %let _xrc_=DIST_OK;
   %local _semerr;

   %rio_chk;

   %if &_semerr=0 %then %do;
      %if %bquote(&undef)= %then %let undef=.;
      %sscp
      if _sx2*_sy2 then _d=_sxy**2/(_sx2*_sy2);
      else _d=&undef;
   %end;
%mend sqcorr;

%macro dsqcorr; %* Squared correlation transformed to dissimilarity;
   %let _xrc_=DIST_OK;
   %local _semerr;

   %rio_chk;

   %if &_semerr=0 %then %do;
      %if %bquote(&undef)= %then %let undef=.;
      %local save; %let save=&undef;
      %let undef=.;
      %sqcorr
      %let undef=&save;
      if _d=. then _d=&undef;
      else _d=max(0,1-_d);
   %end;
%mend dsqcorr;

%macro l(p); %* Minkowski L(p) distance, requires p>0;
   %let _xrc_=DIST_OK;
   %local _semerr;

   %rio_chk;

   %if &_semerr=0 %then %do;
      _p = &p;

      if _p <= 0 then do;
         _outstr='ERROR:` The parameter of the L(p) coefficent '||
                 'needs to be positive.';
         put _outstr $65.;
         _errflg=1;
         goto lout;
      end;

      %if %bquote(&undef)= %then %let undef=.;

            _d=0;

      %if &riowgflg %then %do;
         %if &nomiss %then %do;
               do _i=1 to _ntot;
                  _d+_riowgt[_i]*(abs(_yrio[_i]-_xrio[_i]))**_p;
               end;
               _d=_d**(1/_p);
         %end;
         %else %do;
               _sumwgt=0; _sriowgt=0;
               do _i=1 to _ntot;
                  if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                     _sumwgt+_riowgt[_i];
                     _d+_riowgt[_i]*(abs(_yrio[_i]-_xrio[_i]))**_p;
                  end;
                  _sriowgt+_riowgt[_i];
               end;
               if _sumwgt then _d=(_d*_sriowgt/_sumwgt)**(1/_p);
               else _d=&undef;
         %end;
      %end;
      %else %do;
         %if &nomiss %then %do;
               do _i=1 to _ntot;
                  _d+abs(_yrio[_i]-_xrio[_i])**_p;
               end;
               _d=_d**(1/_p);
         %end;
         %else %do;
               _sumwgt=0; _sriowgt=0;
               do _i=1 to _ntot;
                  if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                     _sumwgt+1;
                     _d+abs(_yrio[_i]-_xrio[_i])**_p;
                  end;
                  _sriowgt+1;
               end;
               if _sumwgt then _d=(_d*_sriowgt/_sumwgt)**(1/_p);
               else _d=&undef;
         %end;
      %end;
   %end;
lout:;
%mend l;

%macro city; %* Cityblock distance, equivalent to L(1);
   %let _xrc_=DIST_OK;
   %local _semerr;

   %rio_chk;

   %if &_semerr=0 %then %do;
      %if %bquote(&undef)= %then %let undef=.;
            _d=0;

      %if &riowgflg %then %do;
         %if &nomiss %then %do;
               do _i=1 to _ntot;
                  _d+_riowgt[_i]*abs(_yrio[_i]-_xrio[_i]);
               end;
         %end;
         %else %do;
               _sumwgt=0; _sriowgt=0;
               do _i=1 to _ntot;
                  if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                     _sumwgt+_riowgt[_i];
                     _d+_riowgt[_i]*abs(_yrio[_i]-_xrio[_i]);
                  end;
                  _sriowgt+_riowgt[_i];
               end;
               if _sumwgt then _d=(_d*_sriowgt)/_sumwgt;
               else _d=&undef;
         %end;
      %end;
      %else %do;
         %if &nomiss %then %do;
               do _i=1 to _ntot;
                  _d+abs(_yrio[_i]-_xrio[_i]);
               end;
         %end;
         %else %do;
               _sumwgt=0; _sriowgt=0;
               do _i=1 to _ntot;
                  if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                     _sumwgt+1;
                     _d+abs(_yrio[_i]-_xrio[_i]);
                  end;
                  _sriowgt+1;
               end;
               if _sumwgt then _d=(_d*_sriowgt)/_sumwgt;
               else _d=&undef;
         %end;
      %end;
   %end;
%mend city;

%macro chebyche;  %* Chebychev distance mertic;
   %let _xrc_=DIST_OK;
   %local _semerr;

   %rio_chk;

   %if &_semerr=0 %then %do;
      %if %bquote(&undef)= %then %let undef=.;

            _max=0;
      %if &riowgflg %then %do;
         %if &nomiss %then %do;
               do _i=1 to _ntot;
                  _max=max(_max,_riowgt[_i]*abs(_xrio[_i]-_yrio[_i]));
               end;
               _d= _max;
         %end;
         %else %do;
               _sumwgt=0;
               do _i=1 to _ntot;
                  if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                     _max=_riowgt[_i]*max(_max,abs(_xrio[_i]-_yrio[_i]));
                     _sumwgt+_riowgt[_i];
                  end;
               end;
               if _sumwgt then _d= _max;
               else _d=&undef;
         %end;
      %end;
      %else %do;
         %if &nomiss %then %do;
               do _i=1 to _ntot;
                  _max=max(_max,abs(_xrio[_i]-_yrio[_i]));
               end;
               _d= _max;
         %end;
         %else %do;
               _sumwgt=0;
               do _i=1 to _ntot;
                  if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                     _max=max(_max,abs(_xrio[_i]-_yrio[_i]));
                     _sumwgt+1;
                  end;
               end;
               if _sumwgt then _d= _max;
               else _d=&undef;
         %end;
      %end;
   %end;
%mend chebyche;

%macro power(p,r); %* Power(p,r) distance, requires p>=0 and r>0;
   %let _xrc_=DIST_OK;
   %local _semerr;

   %rio_chk;

   %if &_semerr=0 %then %do;
      %xchkflst(p,,);

      _p = &p;
      _r = &r;

      if _p < 0 then do;
         _outstr='ERROR: The first parameter of the POWER(p,r) coefficent '||
              'needs to be non-negative.';
         put _outstr $82.;
         _errflg=1;
         goto pout;
      end;

      if _r <= 0 then do;
        _outstr='ERROR: The second parameter of the POWER(p,r) coefficent '||
            'needs to be positive.';
         put _outstr $77.;
         _errflg=1;
         goto pout;
      end;

      %if %bquote(&undef)= %then %let undef=.;
            _d=0;

      %if &riowgflg %then %do;
         %if &nomiss %then %do;
               do _i=1 to _ntot;
                  _d+_riowgt[_i]*(abs(_yrio[_i]-_xrio[_i]))**_p;
               end;
               _d=_d**(1/_r);
         %end;
         %else %do;
               _sumwgt=0; _sriowgt=0;
               do _i=1 to _ntot;
                  if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                     _sumwgt+_riowgt[_i];
                     _d+_riowgt[_i]*(abs(_yrio[_i]-_xrio[_i]))**_p;
                  end;
                  _sriowgt+_riowgt[_i];
               end;
               if _sumwgt then _d=(_d*_sriowgt/_sumwgt)**(1/_r);
               else _d=&undef;
         %end;
      %end;
      %else %do;
         %if &nomiss %then %do;
               do _i=1 to _ntot;
                  _d+abs(_yrio[_i]-_xrio[_i])**_p;
               end;
               _d=_d**(1/_r);
         %end;
         %else %do;
               _sumwgt=0; _sriowgt=0;
               do _i=1 to _ntot;
                  if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                     _sumwgt+1;
                     _d+abs(_yrio[_i]-_xrio[_i])**_p;
                  end;
                  _sriowgt+1;
               end;
               if _sumwgt then _d=(_d*_sriowgt/_sumwgt)**(1/_r);
               else _d=&undef;
         %end;
      %end;

   %end;
pout: ;
%mend power;

%macro ratiocom; %* utility for simratio and disratio;

         _sxy=0; _sd2=0;

   %if &nomiss %then %do;
         do _i=1 to _ntot;
%if &riowgflg %then %do;
            _sd2+_riowgt[_i]*(_xrio[_i]-_yrio[_i])**2;
            _sxy+_riowgt[_i]*_xrio[_i]*_yrio[_i];
%end;
%else %do;
            _sd2+(_xrio[_i]-_yrio[_i])**2;
            _sxy+_xrio[_i]*_yrio[_i];
%end;
         end;
   %end;
   %else %do;
         _sumwgt=0;
         do _i=1 to _ntot;
            if nmiss(_yrio[_i],_xrio[_i])=0 then do;
%if &riowgflg %then %do;
               _sumwgt+_riowgt[_i];
               _sd2+_riowgt[_i]*(_xrio[_i]-_yrio[_i])**2;
               _sxy+_riowgt[_i]*_xrio[_i]*_yrio[_i];
%end;
%else %do;
               _sumwgt+1;
               _sd2+(_xrio[_i]-_yrio[_i])**2;
               _sxy+_xrio[_i]*_yrio[_i];
%end;

            end;
         end;
   %end;
         _d=_sd2+_sxy;
%mend ratiocom;

%macro errmsg2(var1,var2); %* Print error messages with 2 arguments;
   %local s1;
   %let s1=%bquote(You must specify at least one variable either from);
   %let _xrc_=&s1 %bquote(&var1= or &var2= lists);
%mend errmsg2;

%macro errmsg2a(var1,var2); %* Print error messages with 2 arguments;
   %local s1;
   %let s1=%bquote(You must specify at least one numeric variable from);
   %let _xrc_=&s1 %bquote(either &var1= or &var2= lists);
%mend errmsg2a;

%macro chknum(var1,var2); %* Check if all numeric in VAR= lists;
   %if &numno ^= &nvar %then
      %put %qcmpres(WARNING: Character variable(s) in the VAR= list
      is(are) excluded from the analysis.);
%mend chknum;

%macro legit4;
                    %* legitmate check with 4 variable lists;
   %if %bquote(&anominal)^= or %bquote(&nominal)^= or %bquote(&ordinal)^=
      or %bquote(&interval)^= %then %do;
      %let s1=%bquote(ANOMINAL=, NOMINAL=, ORDINAL=, or INTERVAL= are);
      %let _xrc_=&s1 %bquote(illegitimate when METHOD= &ucmethod is specified);
      %let _semerr=1;
   %end;
%mend legit4;

%macro legit4a;
                    %* legitmate check with 4 variable lists;
   %if %bquote(&nominal)^= or %bquote(&ordinal)^= or %bquote(&interval)^=
      or %bquote(&ratio)^= %then %do;
      %let s1=%bquote(NOMINAL=, ORDINAL=, INTERVAL=, or RATIO= are);
      %let _xrc_=&s1 %bquote(illegitimate when METHOD= &ucmethod is specified);
      %let _semerr=1;
   %end;
%mend legit4a;

%macro legit4b;
                    %* legitmate check with 4 variable lists;
   %if %bquote(&anominal)^= or %bquote(&ordinal)^= or %bquote(&interval)^=
      or %bquote(&ratio)^= %then %do;
      %let s1=%bquote(ANOMINAL=, ORDINAL=, INTERVAL=, or RATIO= are);
      %let _xrc_=&s1 %bquote(illegitimate when METHOD= &ucmethod is specified);
      %let _semerr=1;
   %end;
%mend legit4b;

%macro simratio; %* Similarity ratio;
   %let _xrc_=DIST_OK;

   %if &ratio= and &numvar= %then %do;
      %errmsg2a(RATIO,VAR)
   %end;
   %else %do;
      %legit4
      %chknum;
      %if %bquote(&undef)= %then %let undef=.;
      %ratiocom
      if _d>0 then _d=_sxy/_d;
              else _d=&undef;
   %end;
%mend simratio;

%macro disratio; %* Dissimilarity ratio;
   %let _xrc_=DIST_OK;

   %if &ratio= and &var= %then %do;
      %errmsg2a(RATIO,VAR)
   %end;
   %else %do;
      %legit4
      %chknum;
      %if %bquote(&undef)= %then %let undef=0;
      %ratiocom
      if _d>0 then _d=_sd2/_d;
              else _d=&undef;
   %end;
%mend disratio;

%macro canberra; %* Canberra metric distance coefficient;
   %let _xrc_=DIST_OK;

   %if &ratio= and &var= %then %do;
      %errmsg2a(RATIO,VAR)
   %end;
   %else %do;
      %legit4
      %chknum;
      %if %bquote(&undef)= %then %let undef=.;
         _divide=0; _d=0;

      %if &riowgflg %then %do;
         %if &nomiss %then %do;
            do _i=1 to _ntot;
               _divide = _riowgt[_i]*(_xrio[_i]+_yrio[_i]);
               if _divide ne 0 then do;
                  _d+(_riowgt[_i]*abs(_xrio[_i]-_yrio[_i]))/_divide;
               end;
               else do;
                  _outstr=
       'ERROR: Divided by zero when method=CANBERRA.';
                  _d= &undef;
                  put _outstr $62.;
                  _errflg=1;
                  goto canout;
               end;
            end;
         %end;
         %else %do;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                  _divide = _riowgt[_i]*(_xrio[_i]+_yrio[_i]);
                  if _divide ne 0 then do;
                     _d+(_riowgt[_i]*abs(_xrio[_i]-_yrio[_i]))/_divide;
                  end;
                  else do;
                     _outstr=
       'ERROR: Divided by zero when method=CANBERRA.';
                     _d= &undef;
                     put _outstr $62.;
                     _errflg=1;
                     goto canout;
                  end;
               end;
            end;
         %end;
      %end;
      %else %do;
         %if &nomiss %then %do;
            do _i=1 to _ntot;
               _divide = _xrio[_i]+_yrio[_i];
               if _divide ne 0 then do;
                  _d+(abs(_xrio[_i]-_yrio[_i]))/_divide;
               end;
               else do;
                  _outstr=
       'ERROR: Divided by zero when method=CANBERRA.';
                  _d= &undef;
                  put _outstr $62.;
                  _errflg=1;
                  goto canout;
               end;
            end;
         %end;
         %else %do;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                  _divide = _xrio[_i]+_yrio[_i];
                  if _divide ne 0 then do;
                     _d+(abs(_xrio[_i]-_yrio[_i]))/_divide;
                  end;
                  else do;
                     _outstr=
       'ERROR: Divided by zero when method=CANBERRA.';
                     _d= &undef;
                     put _outstr $62.;
                     _errflg=1;
                     goto canout;
                  end;
               end;
            end;
         %end;
      %end;
   %end;
canout: ;

%mend canberra;

%macro nonmetri; %* Lance-Williams nonmetric coefficient;
   %let _xrc_=DIST_OK;

   %if &ratio= and &var= %then %do;
      %errmsg2a(RATIO,VAR)
   %end;
   %else %do;
      %legit4
      %chknum;
      %if %bquote(&undef)= %then %let undef=.;
         _sabs=0; _d=0;

      %if &riowgflg %then %do;
         %if &nomiss %then %do;
            do _i=1 to _ntot;
               _sabs+_riowgt[_i]*abs(_xrio[_i]-_yrio[_i]);
               _d+_riowgt[_i]*(_xrio[_i]+_yrio[_i]);
            end;
         %end;
         %else %do;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                  _sabs+_riowgt[_i]*abs(_xrio[_i]-_yrio[_i]);
                  _d+_riowgt[_i]*(_xrio[_i]+_yrio[_i]);
               end;
            end;
         %end;
      %end;
      %else %do;
         %if &nomiss %then %do;
            do _i=1 to _ntot;
               _sabs+abs(_xrio[_i]-_yrio[_i]);
               _d+_xrio[_i]+_yrio[_i];
            end;
         %end;
         %else %do;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
                  _sabs+abs(_xrio[_i]-_yrio[_i]);
                  _d+_xrio[_i]+_yrio[_i];
               end;
            end;
         %end;
      %end;

      if _d>0 then _d=_sabs/_d;
              else _d=&undef;
   %end;
%mend nonmetri;

%macro sum_prod; %* utility computes uncentered sum of product
                    and sum of squares;

         _sxy=0; _sx2=0; _sy2=0;
         _sumwgt=0;

         do _i=1 to _ntot;
   %if &nomiss=0 %then %do;
            if nmiss(_yrio[_i],_xrio[_i])=0 then do;
   %end;
   %if &riowgflg %then %do;
               _sx2+_riowgt[_i]*_xrio[_i]**2;
               _sy2+_riowgt[_i]*_yrio[_i]**2;
               _sxy+_riowgt[_i]*_xrio[_i]*_yrio[_i];
               _sumwgt+_riowgt[_i];
   %end;
   %else %do;
               _sx2+_xrio[_i]**2;
               _sy2+_yrio[_i]**2;
               _sxy+_xrio[_i]*_yrio[_i];
               _sumwgt+1;
   %end;

   %if &nomiss=0 %then %do;
            end;
   %end;
         end;
%mend sum_prod;

%macro dot; %* Dot (inner) product;
   %let _xrc_=DIST_OK;

   %if &ratio= and &var= %then %do;
      %errmsg2a(RATIO,VAR)
   %end;
   %else %do;
      %legit4
      %chknum;
      %if %bquote(&undef)= %then %let undef=.;
      %sum_prod;
         _d=_sxy;
   %end;
%mend dot;

%macro cosine; %* cosine;
   %let _xrc_=DIST_OK;

   %if &ratio= and &var= %then %do;
      %errmsg2a(RATIO,VAR)
   %end;
   %else %do;
      %legit4
      %chknum;
      %if %bquote(&undef)= %then %let undef=.;
      %sum_prod;
         if _sx2*_sy2 then _d=_sxy/sqrt(_sx2*_sy2);
         else _d=&undef;
   %end;
%mend cosine;

%macro overlap; %* Overlap similarity;
   %let _xrc_=DIST_OK;

   %if &ratio= and &var= %then %do;
      %errmsg2a(RATIO,VAR)
   %end;
   %else %do;
      %legit4
      %chknum;
      %if %bquote(&undef)= %then %let undef=.;
            _smin=0; _smax=0;
      %if &nomiss %then %do;
            do _i=1 to _ntot;
         %if &riowgflg %then %do;
               _smin+_riowgt[_i]*(min(_xrio[_i],_yrio[_i]));
         %end;
         %else %do;
               _smin+min(_xrio[_i],_yrio[_i]);
         %end;
            end;
      %end;
      %else %do;
            do _i=1 to _ntot;
               if nmiss(_yrio[_i],_xrio[_i])=0 then do;
          %if &riowgflg %then %do;
                  _smin+_riowgt[_i]*(min(_xrio[_i],_yrio[_i]));
          %end;
          %else %do;
                  _smin+min(_xrio[_i],_yrio[_i]);
          %end;
               end;
            end;
      %end;
            _d=_smin;
   %end;
%mend overlap;

%macro doverlap; %* Overlap dissimilarity;
   %let _xrc_=DIST_OK;

   %if &ratio= and &var= %then %do;
      %errmsg2a(RATIO,VAR)
   %end;
   %else %do;
      %legit4
      %chknum;
      %sums
      %overlap
            _d=max(_sx,_sy)-_smin;
   %end;
%mend doverlap;

%macro chisq; %* Chi-squared similarity;
   %let _xrc_=DIST_OK;

   %if &ratio= and &numvar= %then %do;
      %errmsg2a(RATIO,VAR)
   %end;
   %else %do;
      %legit4
      %chknum;
      %if %bquote(&undef)= %then %let undef=.;
      _schix=0; _schiy=0;
      _p=1;

      %sums

      _sxplusy=_sx+_sy;
      if _sxplusy=0 then do;
        _outstr=
            'ERROR: Divided by zero when method=CHI, CHISQ, PHI, or PHISQ.';
         _d= &undef;
         put _outstr $62.;
         _errflg=1;
         goto cout;
      end;

      %if &nomiss=0 %then %do;
         _sriowgt=0;
      %end;

      do _i=1 to &riotot;

      %if &nomiss=0 %then %do;
         %if &riowgflg %then %do;
            _sriowgt+_riowgt[_i];
         %end;
         %else %do;
            _sriowgt+1;
         %end;

         if nmiss(_yrio[_i],_xrio[_i])=0 then do;
      %end;

         %if &riowgflg %then %do;
               _csum=_riowgt[_i]*(_xrio[_i]+_yrio[_i]);
               _cellmx=_sx*_csum/_sxplusy;
               _cellmy=_sy*_csum/_sxplusy;

               if _cellmx and _cellmy then do;
                  _schix+(_riowgt[_i]*_xrio[_i]-_cellmx)**2/_cellmx;
                  _schiy+(_riowgt[_i]*_yrio[_i]-_cellmy)**2/_cellmy;
         %end;
         %else %do;
               _csum=_xrio[_i]+_yrio[_i];
               _cellmx=_sx*_csum/_sxplusy;
               _cellmy=_sy*_csum/_sxplusy;
               if _cellmx and _cellmy then do;
               _schix+(_xrio[_i]-_cellmx)**2/_cellmx;
               _schiy+(_yrio[_i]-_cellmy)**2/_cellmy;
         %end;
            end;
            else do;
               _outstr=
            'ERROR: Divided by zero when method=CHI, CHISQ, PHI, or PHISQ.';
               _d= &undef;
               put _outstr $62.;
               _errflg=1;
               goto cout;
            end;
      %if &nomiss=0 %then %do;
         end;
      %end;

      end;

      if _sumwgt then _d=(_schix+_schiy)
      %if &nomiss=0 %then %do;
                          *_sriowgt/_sumwgt
      %end;
         ;
      else _d=&undef;

      %if &syserr>4 %then %let _xrc_=%qcmpres(DATA step failed);
   %end;

cout: ;

%mend chisq;

%macro chi; %* Squared root of Chi-squared similarity;
   %chisq;
   if _d ^= &undef then _d=sqrt(_d);
%mend chi;

%macro phisq; %* Phi-squared similarity;
   %chisq;
   if _d ^= &undef then _d=_d/_sumwgt;
%mend phisq;

%macro phi; %* Squared root of Phi-squared similarity;
   %phisq;
   if _sumwgt then _d=sqrt(_d);
   else _d=&undef;
%mend phi;

%macro count;  %* utility to count number of match/mismatch pairs
                  and total number of non-missing pairs;

            _d1=0; _d2=0; _sumwgt=0;

%if &nomiss and (&var^= and &vtype<3)  %then %do;
            do _i=1 to _n;
   %if &varwgt^= %then %do;
              _d1+_varwgt[_i]*(_y[_i]=_x[_i]);
              _d2+_varwgt[_i]*(^(_y[_i]=_x[_i]));
              _sumwgt+_varwgt[_i];
   %end;
   %else %do;
              _d1+_y[_i]=_x[_i];
              _d2+(^(_y[_i]=_x[_i]));
              _sumwgt+1;
   %end;
            end;
%end;
%else %if &nomiss and (&nominal^= and &ntype<3) %then %do;
            do _i=1 to _nn;
   %if &nomwgt^= %then %do;
              _d1+_nwgt[_i]*(_ny[_i]=_nx[_i]);
              _d2+_nwgt[_i]*(^(_ny[_i]=_nx[_i]));
              _sumwgt+_nwgt[_i];
   %end;
   %else %do;
              _d1+_ny[_i]=_nx[_i];
              _d2+(^(_ny[_i]=_nx[_i]));
              _sumwgt+1;
   %end;
            end;
%end;
%else %do;
   %if &var^= %then %do;
      %do n=1 %to &nvar;
         %if &&type&n=1 %then %do;
            if nmiss(_y&n,&&var&n)=0 then do;
         %end;
         %else %do;
            if _y&n^=' ' & &&var&n^=' ' then do;
         %end;
         %if &varwgt^= %then %do;
              _d1+_varwgt[&n]*(_y&n=&&var&n);
              _d2+_varwgt[&n]*(^(_y&n=&&var&n));
              _sumwgt+_varwgt[&n];
         %end;
         %else %do;
              _d1+(_y&n=&&var&n);
              _d2+(^(_y&n=&&var&n));
              _sumwgt+1;
         %end;
            end;
      %end;
   %end;
   %else %do;
      %do n=1 %to &nnom;
         %if &&tnom&n=1 %then %do;
            if nmiss(_ny&n,&&nom&n)=0 then do;
         %end;
         %else %do;
            if _ny&n^=' ' & &&nom&n^=' ' then do;
         %end;
         %if &nomwgt^= %then %do;
              _d1+_nwgt[&n]*(_ny&n=&&nom&n);
              _d2+_nwgt[&n]*(^(_ny&n=&&nom&n));
              _sumwgt+_nwgt[&n];
         %end;
         %else %do;
              _d1+(_ny&n=&&nom&n);
              _d2+(^(_ny&n=&&nom&n));
              _sumwgt+1;
         %end;
            end;
      %end;
   %end;
%end;
%mend count;

%macro hamming; %* Hamming distance, allows mixed variables;
   %let _xrc_=DIST_OK;
   %if &nominal= and &var= %then %do;
      %errmsg2(NOMINAL,VAR)
   %end;
   %else %do;
      %legit4b

      %count;
            _d=_d2;
   %end;
%mend hamming;

%macro match; %* Simple matching coefficient, allows mixed vars;
   %let _xrc_=DIST_OK;
   %if &nominal= and &var= %then %do;
      %errmsg2(NOMINAL,VAR)
   %end;
   %else %do;
      %legit4b

      %if %bquote(&undef)= %then %let undef=.;
      %count
            if _sumwgt then _d=_d1/_sumwgt;
            else _d=&undef;
   %end;
%mend match;

%macro dmatch; %* Simple matching coefficient transformed to
                  Euclidean distance, allows mixed vars;
   %let _xrc_=DIST_OK;
   %if &nominal= and &var= %then %do;
      %errmsg2(NOMINAL,VAR)
   %end;
   %else %do;
      %legit4b

      %if %bquote(&undef)= %then %let undef=.;
      %local save; %let save=&undef;
      %let undef=.;
      %count
      %let undef=&save;
            if _sumwgt then _d=sqrt(max(0,1-_d1/_sumwgt));
            else _d=&undef;
    %end;
%mend dmatch;

%macro dsqmatch; %* Simple matching coefficient transformed to
                    Squared Euclidean distance, allows mixed vars;
   %let _xrc_=DIST_OK;
   %if &nominal= and &var= %then %do;
      %errmsg2(NOMINAL,VAR)
   %end;
   %else %do;
      %legit4b

      %if %bquote(&undef)= %then %let undef=.;
      %local save; %let save=&undef;
      %let undef=.;
      %count
      %let undef=&save;
            if _sumwgt then _d=max(0,1-_d1/_sumwgt);
            else _d=&undef;
   %end;
%mend dsqmatch;

%macro hamann;  %* Hamann similarity coefficient;
   %let _xrc_=DIST_OK;
   %if &nominal= and &var= %then %do;
      %errmsg2(NOMINAL,VAR)
   %end;
   %else %do;
      %legit4b

      %if %bquote(&undef)= %then %let undef=.;
      %count;
            if _sumwgt then _d=(_d1-_d2)/_sumwgt;
            else _d=&undef;
   %end;
%mend hamann;

%macro rt; %* Roger-Tanimoto similarity coefficient;
   %let _xrc_=DIST_OK;
   %if &nominal= and &var= %then %do;
   %end;
   %else %do;
      %legit4b

      %if %bquote(&undef)= %then %let undef=.;
      %count;
            if (_d1+2*_d2) then _d=_d1/(_d1+2*_d2);
            else _d=&undef;
   %end;
%mend rt;

%macro ss1;  %* Sokal-Sneath 1 similarity coefficient;
   %let _xrc_=DIST_OK;
   %if &nominal= and &var= %then %do;
      %errmsg2(NOMINAL,VAR)
   %end;
   %else %do;
      %legit4b

      %if %bquote(&undef)= %then %let undef=.;
      %count;
            if (2*_d1+_d2) then _d=2*_d1/(2*_d1+_d2);
            else _d=&undef;
   %end;
%mend ss1;

%macro ss3; %* Sokal-Sneath 2 similarity coefficient;
   %let _xrc_=DIST_OK;
   %if &nominal= and &var= %then %do;
      %errmsg2(NOMINAL,VAR)
   %end;
   %else %do;
      %legit4b

      %if %bquote(&undef)= %then %let undef=.;
      %count;
            if _d2 then _d=_d1/_d2;
            else _d=&undef;
   %end;
%mend ss3;

%macro asycount; %* utility to count number of match/mismatch pairs
                    and total number of non-missing pairs for
                    aymmetric nomial variables;

            _mx=0;     %* sum of weights of mismatches with at least
                          one present;
            _pm=0;     %* sum of weights of presen matches;
            _px=0;     %* sum of weights of present mismatches;
            _pp=0;     %* sum of weights of both present = _pm+ _px;
            _p=0;      %* sum of weights of at least one prsent = _pm+ _mx;
            _pax=0;    %* sum of weights of present-absent mismatches;
            _sumwgt=0; %* total nommissing sum of weights;

%if &var^= %then %do;
   %do n=1 %to &nvar;
      %if &&type&n=1 %then %do;
         if nmiss(_y&n,&&var&n)=0 then do;
      %end;
      %else %do;
         if _y&n^=' ' & &&var&n^=' ' then do;
      %end;
      %if &varwgt^= %then %do;
           _sumwgt+_varwgt[&n];
      %end;
      %else %do;
           _sumwgt+1;
      %end;
           fvar1=trim(left(put(&&var&n,&&vfmt&n.)));
           fvar2=trim(left(put(_y&n,&&vfmt&n.)));
           if ^(upcase(fvar1) in (&absent)) and
              ^(upcase(fvar2) in (&absent)) then do;
      %if &varwgt^= %then %do;
              _pm+_varwgt[&n]*(_y&n=&&var&n);
              _px+_varwgt[&n]*(^(_y&n=&&var&n));
              _mx+_varwgt[&n]*(^(_y&n=&&var&n));
              _pp+_varwgt[&n];
      %end;
      %else %do;
              _pm+(_y&n=&&var&n);
              _px+(^(_y&n=&&var&n));
              _mx+(^(_y&n=&&var&n));
              _pp+1;
      %end;
           end;
           else if ^(upcase(fvar1) in (&absent) and
                     upcase(fvar2) in (&absent)) then do;
      %if &varwgt^= %then %do;
              _pax+_varwgt[&n]*(^(_y&n=&&var&n));
              _mx+_varwgt[&n]*(^(_y&n=&&var&n));
      %end;
      %else %do;
              _pax+(^(_y&n=&&var&n));
              _mx+(^(_y&n=&&var&n));
      %end;
           end;
         end;
   %end;
         _p=_pm+_mx;
%end;
%else %if &anominal^= %then %do;
   %do n=1 %to &nanom;
      %if &&tanom&n=1 %then %do;
         if nmiss(_ay&n,&&anom&n)=0 then do;
      %end;
      %else %do;
         if _ay&n^=' ' & &&anom&n^=' ' then do;
      %end;
      %if &anowgt^= %then %do;
           _sumwgt+_awgt[&n];
      %end;
      %else %do;
           _sumwgt+1;
      %end;
           fvar1=trim(left(put(&&anom&n,&&afmt&n.)));
           fvar2=trim(left(put(_ay&n,&&afmt&n.)));
           if ^(upcase(fvar1) in (&absent)) and
              ^(upcase(fvar2) in (&absent)) then do;
         %if &anowgt^= %then %do;
             _pm+_awgt[&n]*(_ay&n=&&anom&n);
             _px+_awgt[&n]*(^(_ay&n=&&anom&n));
             _mx+_awgt[&n]*(^(_ay&n=&&anom&n));
             _pp+_awgt[&n];
         %end;
         %else %do;
             _pm+(_ay&n=&&anom&n);
             _px+(^(_ay&n=&&anom&n));
             _mx+(^(_ay&n=&&anom&n));
             _pp+1;
         %end;
           end;
           else if ^(upcase(fvar1) in (&absent) and
                     upcase(fvar2) in (&absent)) then do;
         %if &anowgt^= %then %do;
              _pax+_awgt[&n]*(^(_ay&n=&&anom&n));
              _mx+_awgt[&n]*(^(_ay&n=&&anom&n));
         %end;
         %else %do;
              _pax+(^(_ay&n=&&anom&n));
              _mx+(^(_ay&n=&&anom&n));
         %end;
           end;
         end;
   %end;
         _p=_pm+_mx;
%end;

%if &ratio^= %then %do;

         _sxy=0; _sd2=0;

   %if &nomiss %then %do;
         do _i=1 to _rn;
      %if &riowgflg %then %do;
            _sd2+_riowgt[_i]*(_xrio[_i]-_yrio[_i])**2;
            _sxy+_riowgt[_i]*_xrio[_i]*_yrio[_i];
      %end;
      %else %do;
            _sd2+(_xrio[_i]-_yrio[_i])**2;
            _sxy+_xrio[_i]*_yrio[_i];
      %end;
         end;
   %end;
   %else %do;
         do _i=1 to _rn;
            if nmiss(_yrio[_i],_xrio[_i])=0 then do;
      %if &riowgflg %then %do;
               _sumwgt+_riowgt[_i];
               _sd2+_riowgt[_i]*(_xrio[_i]-_yrio[_i])**2;
               _sxy+_riowgt[_i]*_xrio[_i]*_yrio[_i];
      %end;
      %else %do;
               _sumwgt+1;
               _sd2+(_xrio[_i]-_yrio[_i])**2;
               _sxy+_xrio[_i]*_yrio[_i];
      %end;
            end;
         end;
   %end;
%end;
%mend asycount;

%macro errmsg3(var1,var2,var3); %* Print error messages with 3 arguments;
   %local s1;
   %let s1=%bquote(You must specify at least one variable either from);
   %let _xrc_=&s1 %bquote(&var1= and &var2= lists or from &var3= list);
%mend errmsg3;

%macro legit3;
                    %* legitmate check with 3 variable lists;
   %if &NOMINAL^= or &ORDINAL^= or &INTERVAL^= %then %do;
      %let s1=%bquote(NOMINAL=, ORDINAL=, or INTERVAL= are illegitimate );
       %let _xrc_=&s1 %bquote(when METHOD= &ucmethod is specified);
       %let _semerr=1;
   %end;
%mend legit3;

%macro jaccard; %* Jaccard similarity coefficient;
   %let _xrc_=DIST_OK;
   %if &anominal= and &ratio= and &var= %then %do;
      %errmsg3(ANOMINAL,RATIO,VAR)
   %end;
   %else %do;
      %legit3

      %asycount
      %if %bquote(&undef)= %then %let undef=.;
      %if &ratio^= and &anominal^= %then %do;
            if (_sd2+_sxy) and _p then
               _d= _sxy/(_sd2+_sxy) + _pm/_p;
      %end;
      %else %if &ratio^= and &anominal= %then %do;
            if (_sd2+_sxy) then
               _d= _sxy/(_sd2+_sxy);
      %end;
      %else %do;
            if _p then
               _d= _pm/_p;
      %end;
            else _d=&undef;
   %end;
%mend jaccard;

%macro djaccard; %* Jaccard dissimilarity coefficient;
   %let _xrc_=DIST_OK;
   %if &anominal= and &ratio= and &var= %then %do;
      %errmsg3(ANOMINAL,RATIO,VAR)
   %end;
   %else %do;
      %legit3

      %asycount
      %if %bquote(&undef)= %then %let undef=.;
      %if &ratio^= and &anominal^= %then %do;
            if (_sd2+_sxy) and _p then
               _d= _sd2/(_sd2+_sxy) + _mx/_p;
      %end;
      %else %if &ratio^= and &anominal= %then %do;
            if (_sd2+_sxy) then
               _d= _sd2/(_sd2+_sxy);
      %end;
      %else %do;
            if _p then
               _d= _mx/_p;
      %end;
            else _d=&undef;
   %end;
%mend djaccard;

%macro dice; %* Czekanowski/Sorensen similarity coefficient;
   %let _xrc_=DIST_OK;
   %if &anominal= and &var= %then %do;
      %errmsg2(ANOMINAL,VAR)
   %end;
   %else %do;
      %legit4a

      %asycount
      %if %bquote(&undef)= %then %let undef=.;
            if _p+_pm then _d=2*_pm/(_p+_pm);
            else _d=&undef;
   %end;
%mend dice;

%macro rr; %* Russell & Rao similarity coefficient;
   %let _xrc_=DIST_OK;
   %if &anominal= and &var= %then %do;
      %errmsg2(ANOMINAL,VAR)
   %end;
   %else %do;
      %legit4a
      %asycount
      %if %bquote(&undef)= %then %let undef=.;
            if _sumwgt then _d=_pm/_sumwgt;
            else _d=&undef;
   %end;
%mend rr;

%macro blwnm; %* Lance & Williams nonmetric dissimilarity coefficient;
   %let _xrc_=DIST_OK;
   %if &anominal= and &var= %then %do;
      %errmsg2(ANOMINAL,VAR)
   %end;
   %else %do;
      %legit4a

      %asycount
      %if %bquote(&undef)= %then %let undef=.;
            if _pax+2*_pp then _d=_mx/(_pax+2*_pp);
            else _d=&undef;
   %end;
%mend blwnm;

%macro k1; %* Kulczynski similarity coefficient;
   %let _xrc_=DIST_OK;
   %if &anominal= and &var= %then %do;
      %errmsg2(ANOMINAL,VAR)
   %end;
   %else %do;
      %legit4a

      %asycount
      %if %bquote(&undef)= %then %let undef=.;
            if _mx then _d=_pm/_mx;
            else _d=&undef;
   %end;
%mend k1;

%macro dgower; %* Generalized Gower dissimilarity coefficient;
   %let _xrc_=DIST_OK;

   %if %bquote(&undef)= %then %let undef=.;

   _d=0; _d1=0; _d2=0; _d3=0; _sumwgt=0;
   _nabs=0;  ** for degugging purpose;

   *** ANOMINAL= var;
   %if %bquote(&anominal)^= %then %do;
      %do n=1 %to &nanom;
         %if &&tanom&n=1 %then %do;
            if nmiss(_ay&n,&&anom&n)=0 then do;     /* check if missing
                                                     numeric type vars */
         %end;
         %else %do;
            if _ay&n^=' ' & &&anom&n^=' ' then do;  /* or if missing
                                                   character type vars */
         %end;
               fvar1=trim(left(put(&&anom&n,&&afmt&n.)));
               fvar2=trim(left(put(_ay&n,&&afmt&n.)));
               if upcase(fvar1) in (&absent) and
                  upcase(fvar2) in (&absent) then
                   _nabs+1;
               else do;
         %if &anowgt^= %then %do;
                  _sumwgt+_awgt[&n];
                  _d1+_awgt[&n]*(^(_ay&n=&&anom&n));
         %end;
         %else %do;
                  _sumwgt+1;
                  _d1+^(_ay&n=&&anom&n);
         %end;
               end;
            end;
      %end;
%xbugdo(put 'after ANOMINAL: _sumwgt=' _sumwgt;);
%xbugdo(put 'after ANOMINAL: _nabs=' _nabs;);

   %end;

   *** NOMINAL= var;
   %if %bquote(&nominal)^= %then %do;
      %if &nomiss & &ntype<3 %then %do;
         do _i=1 to _nn;
         %if &nomwgt^= %then %do;
            _sumwgt+_nwgt[_i];
            _d2+_nwgt[_i]*(^(_ny[_i]=_nx[_i]));
         %end;
         %else %do;
            _sumwgt+1;
            _d2+^(_ny[_i]=_nx[_i]);
         %end;
         end;
      %end;
      %else %do;
         %do n=1 %to &nnom;
            %if &&tnom&n=1 %then %do;   ** missing numeric type of vars;
               if nmiss(_ny&n,&&nom&n)=0 then do;
            %end;
            %else %do;                    ** missing character type vars;
               if _ny&n^=' ' & &&nom&n^=' ' then do;
            %end;
            %if &nomwgt^= %then %do;
                  _sumwgt+_nwgt[&n];
                  _d2+_nwgt[&n]*(^(_ny&n=&&nom&n));
            %end;
            %else %do;
                  _sumwgt+1;
                  _d2+^(_ny&n=&&nom&n);
            %end;
               end;
         %end;
      %end;
   %end;

   *** ORDINAL, INTERVAL, RATIO= or VAR= list;
   %if &riotot %then %do;
       %if %bquote(&var)^= %then
          %if &numno ^= &nvar %then
              %put %qcmpres(WARNING: Character variable(s) in the VAR= list
               is(are) excluded from the analysis.);
       %if &nomiss %then %do;
          do _i=1 to _ntot;
          %if &riowgflg %then %do;
             _sumwgt+_riowgt[_i];
             _d3+_riowgt[_i]*abs(_yrio[_i]-_xrio[_i]);
          %end;
          %else %do;
             _sumwgt+1;
             _d3+abs(_yrio[_i]-_xrio[_i]);
          %end;
          end;
       %end;
       %else %do;
          do _i=1 to _ntot;
             if nmiss(_yrio[_i],_xrio[_i])=0 then do;
          %if &riowgflg %then %do;
               _sumwgt+_riowgt[_i];
               _d3+_riowgt[_i]*abs(_yrio[_i]-_xrio[_i]);
          %end;
          %else %do;
               _sumwgt+1;
               _d3+abs(_yrio[_i]-_xrio[_i]);
          %end;
             end;
          end;
       %end;
   %end;

%xbugdo(
   put '_d1=' _d1 '_d2=' _d2 '_d3=' _d3);
   if _sumwgt then _d=(_d1+_d2+_d3)/_sumwgt;
   else _d=&undef;
%xbugdo(put '_d=' _d '_sumwgt=' _sumwgt;);
%mend dgower;

%macro gower;  %* Generalized Gower similarity coefficient;
   %let _xrc_=DIST_OK;
   %if %bquote(&undef)= %then %let undef=.;
   %local save; %let save=&undef;
   %let undef=.;
   %dgower
   %let undef=&save;
   if _d=. then _d=&undef;
   else _d=1-_d;
%mend gower;


%global _distan_;
%let _distan_=611;

%* close comment possibly generated by xmacro */;

*===================== END DISTANCE MACRO ==========================;
