 /****************************************************************/
 /*          S A S   S A M P L E   L I B R A R Y                 */
 /*                                                              */
 /*    NAME: ITEM                                                */
 /*   TITLE: Item analysis for multiple choice tests             */
 /* PRODUCT: STAT                                                */
 /*  SYSTEM: ALL                                                 */
 /*    KEYS: ITEM RELIABILITY KR20 KR21 BISERIAL                 */
 /*   PROCS: SORT CONTENTS SUMMARY PRINT CORR                    */
 /*    DATA:                                                     */
 /*                                                              */
 /* SUPPORT: saswss                      UPDATE:  26Jun92        */
 /*     REF:                                                     */
 /*    MISC:                                                     */
 /*                                                              */
 /****************************************************************/
 
 /*--------------------------------------------------------------------
 
The %ITEM macro computes descriptive statistics for analysis of data
from a multiple-choice test. Each observation contains the answers
from one subject to a set of questions ("items"). The data are
compared to an answer key to determine which answers are correct.
The score for each subject is computed as the number of correct answers.
The output is very similar to that from the ITEM procedure in the SUGI
Supplemental library, but several incorrect statistics have been fixed.
 
To use the %ITEM macro, use the %INCLUDE statement referencing this
separate file before invoking the %ITEM macro. Type "%ITEM(arguments)"
instead of "PROC ITEM options".
 
The examples following the %ITEM macro include the CORR procedure to
validate the summary statistics for the test score and the item-total
correlations. The ALPHA option is also used to validate KR20 (which
is equivalent to Cronbach's alpha) and the item-remainder correlations.
 
The following arguments may be listed within parentheses in any order,
separated by commas:
 
   DATA=        SAS data set to analyze. The default is _LAST_.
                Data set options may be used.
 
   VAR=         List of variables representing items. Only the first
                character of the formatted value of each variable is
                used to compare with the answer key. The variables may
                be numeric or character or a mixture of both. The usual
                forms of abbreviated lists (e.g., X1-X100, ABC--XYZ,
                ABC:) may be used.
 
                The default is _ALL_; BY variables and ID variables
                are not automatically removed from the default list.
 
   FORMAT=      An optional SAS format to be applied to all the VAR
                variables.
 
                If FORMAT= is not specified and if formats have been
                assigned to the VAR variables in a previous DATA step,
                then those formats are used.
 
   ID=          List of variables to be copied to the OUT= and OUTBIN=
                data sets and printed in the grade report. These
                variables are not used in the analysis. The usual forms
                of abbreviated lists (e.g., X1-X100, ABC--XYZ, ABC:)
                may be used.
 
                If you specify ID=, you must also specify VAR=.
 
   COPY=        List of additional variables to be copied to the OUT=
                and OUTBIN= data sets. The usual forms of abbreviated
                lists (e.g., X1-X100, ABC--XYZ, ABC:) may be used.
 
                If you specify COPY=, you must also specify VAR=.
 
   BY=          List of variables for BY groups. Abbreviated variable
                lists (e.g., X1-X100, ABC--XYZ, ABC:) may NOT be used.
 
                If you specify BY=, you must also specify VAR=.
 
   SUBTEST=     Series of one or more subtest specifications separated
                by one of the characters given in the SUBSEP= argument.
 
                Each subtest specification contains either a variable
                list that is a subset of the variables in the VAR= list,
                or the keyword _ALL_ indicating all of the variables
                in the VAR= list. A separate analysis is performed for
                each of these variable lists.
 
                Each subtest specification may also contain a quoted
                string of up to 40 characters providing a title to
                identify the subtest on the printout. This title is
                also written to the output data sets in a variable
                named by the SUBNAME= argument. This string may not
                contain any of the characters in the SUBSEP= argument,
                nor may it contain two consecutive quotation marks.
 
                If no subtests are specified, only one analysis is
                performed using all the variables in the VAR= list.
 
   SUBSEP=      One or more characters used to separate subtest
                specifications in the SUBTEST= argument. The default
                is a /. Do not use blanks or commas.
 
   SUBNAME=     Name of the variable in the output data sets containing
                the subtest title. The default is _SUBTEST.
 
   RESPONSE=    Quoted string containing all the valid single-character
                responses to all the items. Any answer not in this list
                is declared invalid. There cannot be more than 200
                valid responses because of the limit on the length of
                quoted strings in the SAS system.
 
                The default is RESPONSE='12345'
 
   KEY=         Quoted string specifying the correct single-character
                answers for each item in the same order as specified
                in the VAR= argument. If you specify a quoted string
                for the KEY= argument, then there cannot be more than
                200 items because of the limit on the length of
                character strings in the SAS system. You may specify
                several quoted strings separated by the concatenation
                operator || as long as the total length does not
                exceed 200.
 
                If you do not specify KEY=, or if you specify
                KEY=_FIRST_, then the first observation read from
                the data set is assumed to contain the answer key,
                and this observation is omitted from other
                computations. This method of specifying the answer key
                must be used when there are more than 200 items. If
                there are BY groups, they are assumed all to have the
                same key, so only the first BY group should have a
                record giving the answer key.
 
   OUT=         SAS data set containing the ID variables, the score for
                each subject, the number of missing and invalid answers,
                and the score as a percentage.
                Data set options may NOT be used.
 
   OUTBIN=      SAS data set containing the ID variables, the score for
                each subject, and one variable for each item coded as
                1 for a correct answer or 0 for an incorrect answer.
                The variable names are BIN1, BIN2, ..., BINn, where n
                is the number of items.
                Data set options may NOT be used.
 
   OUTITEM=     SAS data set containing item statistics.
                Data set options may NOT be used.
 
   SCORE=       Name of the score variable in the OUT= and OUTBIN=
                data sets. The default is SCORE=SCORE.
                The name should not begin with an underscore.
 
   MISSING=     Name of the variable giving the number of missing values
                in the OUT= data set. The default is MISSING=MISSING.
                The name should not begin with an underscore.
 
   INVALID=     Name of the variable giving the number of invalid values
                in the OUT= data set. The default is INVALID=INVALID.
                The name should not begin with an underscore.
 
   PERCENT=     Name of the variable giving the percentage score
                in the OUT= data set. The default is PERCENT=PERCENT.
                The name should not begin with an underscore.
 
   PROPHECY=    The desired reliability level to be used in the
                Spearman-Brown prophecy formula. The default is
                PROPHECY=.9.
 
   UPLO=        The proportion of subjects to be included in each of
                the two subsets of subjects when comparing the
                percentage of occurrence of each response in the
                highest-scoring subset and the lowest-scoring subset.
                The default is UPLO=.3334.
 
   OPTIONS=     List of additional options separated by blanks:
 
                NOGRADE        Suppress printing the grade report.
                NOG
                NG
 
                NOHISTOGRAM    Suppress printing the bar chart of
                NOHIST         score frequencies.
 
                NOITEM         Suppress printing statistics for
                               each item.
 
                NOTES          Do not suppress notes in the SAS log
                               for various preliminary procedure
                               and data steps. This option can be
                               useful for diagnosing mysterious error
                               messages.
 
                NOTOTAL        Suppress computation of item-total
                               correlations.
 
                NOUPLO         Suppresses computation of percentage
                               of responses by subjects in the
                               highest-scoring and lowest-scoring
                               subsets.
 
                NOX            Suppress printing extra columns for
                               omitted and invalid responses for
                               each item.
 
Restrictions:
   Number of responses <= 200.
   Variable names should not begin with an underscore.
   Under 6.03 or 6.04, expanded memory may be required except
      for very small tests. MS DOS 5 or DR DOS 5 or 6 provide
      more memory for SAS than earlier DOS versions.
 
Bugs:
   Spurious warning from CONTENTS when OPTIONS OBS= is specified.
 
Things that ought to be done for a later release:
   Remove BY, ID, COPY variables from default VAR list.
   Parse SUBTEST= more intelligently.
   Full format list.
   Support OPTIONS NOCENTER.
 
----------------------------------------------------------------------*/
 
*======================= BEGIN ITEM MACRO ============================;
 
%macro item(data=_LAST_,var=_ALL_,format=,id=,copy=,by=,
            subtest=,subsep=/,subname=_subtest,
            response='12345',key=_FIRST_,
            out=_NULL_,outbin=_NULL_,outitem=_NULL_,
            score=score,missing=missing,invalid=invalid,percent=percent,
            prophecy=.9,uplo=.3334,options=,
            debug=0,compat=0);
 
%if &debug %then %do;
   %put data=&data!;
   %put var=&var!;
   %put format=&format!;
   %put id=&id!;
   %put copy=&copy!;
   %put by=&by!;
   %put subtest=&subtest!;
   %put subsep=&subsep!;
   %put subname=&subname!;
   %put response=&response!;
   %put key=&key!;
   %put out=&out!;
   %put outbin=&outbin!;
   %put outitem=&outitem!;
   %put score=&score!;
   %put missing=&missing!;
   %put invalid=&invalid!;
   %put percent=&percent!;
   %put prophecy=&prophecy!;
   %put uplo=&uplo!;
   %put options=&options!;
   %put debug=&debug!;
   %put ;
%end;
 
%if &compat %then %put WARNING: Some statistics will deliberately be
computed incorrectly in an effort to reproduce the results of the
ITEM procedure.;
 
%global itemrc;
%let itemrc=OK;
 
%global itemver;
%let itemver=&sysver;
%if &itemver=6.04 %then %let itemver=6.03;
 
%let sq=%bquote(%str(%'));
%let dq=%bquote(%str(%"));
 
************ process options ************;
%let gradeprt=1;
%let histprt=1;
%let itemprt=1;
%let itemtotl=1;
%let nonotes=1;
%let itemx=2;
 
%let n=1;
%let token=%scan(&options,&n,%str( )&sq&dq);
%do %while(%bquote(&token)^=);
   %let token=%upcase(&token);
   %if %xsubstr(&token,1,7)=NOGRADE %then %let gradeprt=0; %else
   %if %xsubstr(&token,1,3)=NOG %then %let gradeprt=0; %else
   %if %xsubstr(&token,1,2)=NG %then %let gradeprt=0; %else
   %if %xsubstr(&token,1,6)=NOHIST %then %let histprt=0; %else
   %if %xsubstr(&token,1,6)=NOITEM %then %let itemprt=0; %else
   %if %xsubstr(&token,1,7)=NOTOTAL %then %let itemtotl=0; %else
   %if %xsubstr(&token,1,6)=NOUPLO %then %let uplo=0; %else
   %if %xsubstr(&token,1,5)=NOTES %then %let nonotes=0; %else
   %if %xsubstr(&token,1,3)=NOX %then %let itemx=0;
   %else %do;
      %let itemrc=BAD;
      %put ERROR: Unrecognized option &token..;
   %end;
   %let n=%eval(&n+1);
   %let token=%scan(&options,&n,%str( ));
%end;
 
%if &debug %then %do;
   %put gradeprt=&gradeprt!;
   %put histprt=&histprt!;
   %put itemprt=&itemprt!;
   %put itemtotl=&itemtotl!;
   %put nonotes=&nonotes!;
   %put itemx=&itemx!;
   %put uplo=&uplo!;
   %put ;
%end;
 
%if &nonotes %then %do;
   options nonotes;
%end;
 
************ data sets ************;
%let lastds=&syslast;
%let data=%upcase(&data);
%if %bquote(&data)=_LAST_ %then %do;
   %let data=&syslast;
   %if &debug %then %put data=&data!;
   %if &data=_NULL_ %then %do;
      %let itemrc=BAD;
      %put ERROR: No input data set.;
   %end;
%end;
 
%let out=%upcase(&out);
%let outbin=%upcase(&outbin);
%let outitem=%upcase(&outitem);
 
************ do some error checking *************;
%if %bquote(&var)=_ALL_ %then %do;
   %if %bquote(&id)^= %then %do;
      %put ERROR: If ID= is used, VAR= must also be specified.;
      %let itemrc=BAD;
   %end;
   %if %bquote(&by)^= %then %do;
      %put ERROR: If BY= is used, VAR= must also be specified.;
      %let itemrc=BAD;
   %end;
   %if %bquote(&copy)^= %then %do;
      %put ERROR: If COPY= is used, VAR= must also be specified.;
      %let itemrc=BAD;
   %end;
%end;
 
%if &itemrc=BAD %then %goto fail;
 
 
************ process by variables ************;
%let byline=;
%let byvars=;
%let lastby=;
%let n=1;
%let token=%upcase(%scan(&by,&n,%str( )));
%do %while(%bquote(&token)^=);
   %if %bquote(&token)=DESCENDING |
       %bquote(&token)=NOTSORTED %then %do;
      %let byline=&byline &token;
      %let byvars=&byvars &token;
   %end;
   %else %do;
      %let byline=&byline &token=;
      %let byvars=&byvars GROUPFORMAT &token;
      %let lastby=&token;
   %end;
   %let n=%eval(&n+1);
   %let token=%upcase(%scan(&by,&n,%str( )));
%end;
 
%if &debug %then %do;
   %put byline=&byline!;
   %put byvars=&byvars!;
   %put lastby=&lastby!;
   %put ;
%end;
 
*** check for errors ***;
%if %bquote(&by)^= %then %do;
      data _null_;
         set &data;
         by &byvars;
         stop;
      run;
   %if &syserr>4 %then %goto fail;
%end;
 
************ get item names and formats ************;
%if &format= %then
   %xlist(&data,&var,var,'var',fvar,'fvar',nitem,'nitem');
%else
   %xlist(&data,&var,var,'var', ,' ',nitem,'nitem');
%if &debug %then %do;
   %put nitem=&nitem!;
   %do n=1 %to &nitem;
      %put var&n=&&var&n!;
   %end;
   %put ...;
%end;
%if &nitem<2 %then %do;
   %put ERROR: There must be at least 2 items.;
   %let itemrc=BAD;
%end;
%if &itemrc=BAD %then %goto fail;
 
************ subtests ************;
%if %bquote(&subtest)= %then %do;
   %let ntest=1;
   %let subname=;
%end;
%else %do;
         proc datasets nolist nowarn; delete _ITEMA_/memtype=data; run;
   %if &syserr>4 %then %goto fail;
         proc sort
   %if &itemver=6.06 %then force;
            data=_ITEM2_(firstobs=1 obs=32767) out=_ITEM3_;
            by name;
         run;
   %if &syserr>4 %then %goto fail;
   %let ntest=0;
   %let n=1;
   %let token=%bquote(%scan(&subtest,&n,%bquote(&subsep)));
   %do %while(&token^=);
      %if &debug %then %put subtest &n=&token!;
 
      %* get subtitle;
      %let q=&sq;
      %let i=%index(&token,&q);
      %if &i=0 %then %do;
         %let q=&dq;
         %let i=%index(&token,&q);
      %end;
      %if &debug %then %put i=&i!;
      %if &i=0 %then %do;
         %let subtitle="Subtest %eval(&ntest+1)";
         %if &debug %then %put token=&token!;
         %if &debug %then %put subtitle=&subtitle!;
      %end;
      %else %do;
         %let subtoken=%bquote(%xsubstr(&token,1,&i-1));
         %let subtitle=%bquote(%xsubstr(&token,&i+1));
         %if &debug %then %put subtoken=&subtoken!;
         %if &debug %then %put subtitle=&subtitle!;
         %let i=%index(&subtitle,&q);
         %if &debug %then %put i=&i!;
         %if &i=0 %then %do;
            %put ERROR: Unmatched quote in subtest <&token>;
            %goto fail;
         %end;
         %let subtoken=&subtoken %bquote(%xsubstr(&subtitle,&i+1));
         %let subtitle=&q.%bquote(%xsubstr(&subtitle,1,&i));
         %if &debug %then %put subtoken=&subtoken!;
         %if &debug %then %put subtitle=&subtitle!;
         %if &subtoken= %then %do;
            %put ERROR: No items specified in subtest <&token>;
            %goto fail;
         %end;
         %let token=&subtoken;
         %let subtitle=%unquote(&subtitle);
      %end;
 
      %* process variable list;
      %let ntest=%eval(&ntest+1);
      %if %bquote(%upcase(&token))=_ALL_ %then %do;
         data _ITEM4_;
            array _act(&nitem) _act1-_act&nitem; retain _act 1;
            length &subname $ 40; retain &subname &subtitle;
            output;
         run;
      %if &syserr>4 %then %goto fail;
      %end;
      %else %do;
         %xlist(&data,%unquote(&token), ,' ', ,' ',nst,'nst');
         %if &itemrc=BAD %then %goto fail;
         %if &debug %then %put nst=&nst!;
         proc sort
         %if &itemver=6.06 %then force;
            data=_ITEM2_(firstobs=1 obs=32767);
            by name;
         run;
         %if &syserr>4 %then %goto fail;
         data _ITEM4_;
            merge _ITEM2_(firstobs=1 obs=32767 in=in2 drop=npos)
                  _ITEM3_(firstobs=1 obs=32767 in=in3);
            by name;
            keep npos act;
            if ^in2 then act=0; else
            if ^in3 then do;
               put 'ERROR: variable ' name
                   ' in subtest is not in the VAR list';
               delete;
            end;
            else act=1;
         run;
         %if &syserr>4 %then %goto fail;
         proc sort
         %if &itemver=6.06 %then force;
            data=_ITEM4_(firstobs=1 obs=32767); by npos; run;
         %if &syserr>4 %then %goto fail;
         proc transpose data=_ITEM4_(firstobs=1 obs=32767)
                        out=_ITEM4_(drop=_name_) prefix=_act;
            var act;
         run;
         %if &syserr>4 %then %goto fail;
         data _ITEM4_;
            set _ITEM4_(firstobs=1 obs=1);
            length &subname $ 40; retain &subname &subtitle;
         run;
         %if &syserr>4 %then %goto fail;
      %end;
         proc append data=_ITEM4_(firstobs=1 obs=1) base=_ITEMA_; run;
         run;
      %if &syserr>4 %then %goto fail;
      %let n=%eval(&n+1);
      %let token=%bquote(%scan(&subtest,&n,%bquote(&subsep)));
   %end;
   %if &ntest<=0 %then %do;
      %put ERROR: No items in subtest.;
      %goto fail;
   %end;
   %if &debug %then %do;
         proc print data=_ITEMA_(firstobs=1 obs=32767); run;
   %end;
%end;
 
%if &debug %then %do;
   %put ntest=&ntest!;
   %put ;
%end;
 
 
************ process response and key ************;
      data _ITEMK_;
         array _key{&nitem} $ 1 _key1-_key&nitem; retain _key;
         keep _key1-_key&nitem;
         retain _l;
 
%if %index(&response,&sq)=0 & %index(&response,&dq)=0 %then %do;
   %put WARNING: RESPONSE= should be a quoted string.;
   %let response=%unquote(&dq&response&dq);
%end;
         retain _resp &response;
         call symput('nresp',trim(left(put(length(_resp),5.))));
 
%if &key=_FIRST_ %then %do;
   %let skip1=1;
         _l=&nitem;
         set &data;
         %fmtv(&format,_key);
%end;
%else %do;
   %let skip1=0;
   %if %index(&key,&sq)=0 & %index(&key,&dq)=0 %then %do;
      %put WARNING: KEY= should be a quoted string or _FIRST_.;
      %let key=%unquote(&dq&key&dq);
   %end;
         _l=length(&key);
         if _l^=&nitem then do;
            put "ERROR: Key length " _l
                "does not equal number of items &nitem..";
            call symput('itemrc','BAD');
            _l=min(_l,&nitem);
         end;
 
         retain _item_;
         do _item_=1 to _l;
            _key{_item_}=substr(&key,_item_,1);
         end;
%end;
 
         do _item_=1 to _l;
            if index(_resp,_key{_item_})=0 then do;
               length _name_ $8;
               _name_=symget("var"||left(put(_item_,5.)));
               put "ERROR: Key for item " _item_ _name_
                   "contains invalid response '"
                   _key{_item_} $char1. "'.";
               call symput('itemrc','BAD');
            end;
         end;
         output;
         stop;
      run;
%if &syserr>4 %then %goto fail;
 
%if &debug %then %do;
      proc print data=_ITEMK_; run;
%end;
 
%let nrespx=%eval(&nresp+&itemx);
%if &debug %then %do;
   %put nresp=&nresp! nrespx=&nrespx!;
   %put ;
%end;
 
%if &itemrc=BAD %then %goto fail;
 
 
************ process id variables ************;
%if &gradeprt %then %do;
   %if %bquote(&id)= %then %do;
      %let nid=0;
      %let idwid=8;
   %end;
   %else %do;
      %xlist(&data,&id, ,' ', ,' ',nid,'nid');
      %if &itemrc=BAD %then %goto fail;
            data _null_;
               set _ITEM2_(firstobs=1 obs=32767) end=end;
               retain idwid 0;
               length idhdr $60 idval $60;
               if format=' ' then do;
                  if type=1 then format='BEST';
                            else format='CHAR';
               end;
               if formatl=0 then do;
                  if type=1 then formatl=8;
                            else formatl=length;
               end;
               width=max(length(name),formatl);
               idwid+width+2;
               idhdr="'";
               if just
                  then substr(idhdr,2+width-length(name),length(name))=
                          name;
                  else substr(idhdr,2,length(name))=
                          name;
               substr(idhdr,3+width)="' +1";
               idval=name||' '||trim(format)||
                     trim(left(put(formatl,3.)))||
                     '.'||trim(left(put(formatd,3.)))||' +2';
               n=left(put(_n_,3.));
               call symput('idhdr'||n,idhdr);
               call symput('idval'||n,idval);
               if end then do;
                  call symput('idwid',left(put(idwid,5.)));
               end;
            run;
      %if &syserr>4 %then %goto fail;
   %end;
%end;
%else %do;
   %let nid=0;
   %let idwid=0;
%end;
 
%if &debug %then %do;
   %put nid=&nid!;
   %put idwid=&idwid!;
   %do n=1 %to &nid;
      %put idhdr&n=&&idhdr&n!;
      %put idval&n=&&idval&n!;
   %end;
   %put ...;
%end;
 
*** reset _last_ so it will not be one of the temporary data sets ***;
options _last_=&lastds;
 
*** turn on notes only for data step that does printing ***;
%if &nonotes %then %do;
   options notes;
%end;
 
 
************************ MAIN DATA STEP **************************;
 
data &out(keep=&by &subname &id &score &missing &invalid &percent &copy)
     &outbin(keep=&by &subname &id &score bin1-bin&nitem &copy)
     &outitem(keep=&by &subname _item_ _name_ _respon_ _keyed_ _propor_
%if %bquote(&uplo)^=0 %then %do;
        _upper_ _lower_
%end;
%if &itemtotl %then %do;
        _it_pb_ _it_bis_
%end;
        _ir_pb_ _ir_bis_
        _t_test_ _prob_t_);
 
         set &data;   * to define var array correctly;
 
         set _ITEMK_(firstobs=1 obs=1); * answer key;
         array _key{&nitem} $ 1 _key1-_key&nitem; retain _key;
 
%if %bquote(&by)^= %then %do;
         length _bydash $200; retain _bydash;
         retain _byl -1;
%end;
 
         length _name_ $8 _v $1;
         length _char $60;
 
         retain _name_ _char ' ';
         retain _resp &response;
         retain _nby _nsub _nmiss _ninval 0;
         retain _ls 0; * linesize;
 
         retain _div _in _in2 _in2 _item_ _kr20 _kr21
                _l _max _mean _min
                _sbp _ss _std _v _varianc _x;
 
%if &skip1 %then %do;
         retain _skip 0;
%end;
 
%if &gradeprt %then %do;
         retain _nobs 0;
%end;
 
%if &histprt %then %do;
         length _xxx $200; retain _xxx;
         retain _cum _pcent _ptile _xl _xw _z;
%end;
 
%if &itemprt | &outitem^=_NULL_%then %do;
         retain _a _nrl _r _r1 _r2 _r2x _rem _y _z;
   %if %bquote(&uplo)^=0 %then %do;
         retain _lon _loq _upn _upq 0;
   %end;
%end;
 
%if &outitem^=_NULL_ %then %do;
         retain _respon_ _keyed_ _propor_
   %if %bquote(&uplo)^=0 %then %do;
                _upper_ _lower_
   %end;
   %if &itemtotl %then %do;
                _it_pb_ _it_bis_
   %end;
                _ir_pb_ _ir_bis_ _t_test_ _prob_t_;
%end;
 
%if &outbin^=_NULL_ %then %do;
         array _b{*} bin1-bin&nitem; retain _b;
%end;
 
%if %bquote(&subtest)^= %then %do;
      %let nact=_nact;
         retain _nact;
         array _act{&nitem} _act1-_act&nitem;
%end;
%else %let nact=&nitem;
 
         * formatted items;
         array _fmt{&nitem} $ 1 _temporary_; retain _fmt;
 
         * frequency of each score;
         array _freq{0:&nitem} _temporary_;  retain _freq 0;
 
         * proportion correct for each item;
         array _p{&nitem} _temporary_;  retain _p 0;
 
         * sum for total;
         retain _t 0;
 
         * sum of squares for total;
         retain _t2 0;
 
%if &itemprt | &outitem^=_NULL_%then %do;
 
         * proportion of each response for each item;
         array _pr{&nitem,&nrespx} _temporary_;  retain _pr 0;
 
   %if %bquote(&uplo)^=0 %then %do;
         * proportion of each response in upper subset;
         array _up{&nitem,&nrespx} _temporary_;  retain _up 0;
 
         * proportion of each response in lower subset;
         array _lo{&nitem,&nrespx} _temporary_;  retain _lo 0;
   %end;
 
         * sums for each item response;
         array _i{&nitem,&nresp} _temporary_;  retain _i 0;
 
         * sums of squares for each item response;
         array _i2{&nitem,&nresp} _temporary_; retain _i2 0;
 
         * sums of crossproducts with total for each item response;
         array _it{&nitem,&nresp} _temporary_; retain _it 0;
 
         * sums of crossproducts with remainder for each item response;
         array _iq{&nitem,&nresp} _temporary_; retain _iq 0;
 
         * sums for remainder for each item;
         array _q{&nitem} _temporary_;  retain _q 0;
 
         * sum of squares for remainder for each item;
         array _q2{&nitem} _temporary_; retain _q2 0;
 
         * biserial for each response with item-total
           for a given item;
         array _bist{&nresp} _temporary_; retain _bist 0;
 
         * biserial for each response with item-remainder
           for a given item;
         array _bisr{&nresp} _temporary_; retain _bisr 0;
 
         * point biserial for each response with item-total
           for a given item;
         array _pbist{&nresp} _temporary_; retain _pbist 0;
 
         * point biserial for each response with item-remainder
           for a given item;
         array _pbisr{&nresp} _temporary_; retain _pbisr 0;
 
         * t statistics for each response for a given item;
         array _tstat{&nresp} _temporary_; retain _tstat 0;
 
         * p values for each response for a given item;
         array _pval{&nresp} _temporary_; retain _pval 0;
 
%end;
 
 
         file print ll=_ll column=_col header=header;
 
************************* loop over BY groups ************************;
         _end=0;
         do until(_end);
 
            ****** initialize BY group ******;
            _nby+1;
%if &debug %then %str(put _nby=;);
%if %bquote(&by)^= %then %do;
            _byl=-1; * need new byline;
%end;
 
******************** loop over subtests ************************;
%do test=1 %to &ntest;
            _test=&test;
 
%if %bquote(&subtest)^= %then %do;
            set _ITEMA_(firstobs=1 obs=32767) point=_test;
            _nact=sum(of _act1-_act&nitem);
%if &debug %then %do;
            put 'subtest ' _test ' with ' _nact ' active items';
%end;
            if _nact<=0 then do;
               put 'BUG: no items in subtest ' _test;
               stop;
            end;
%end;
 
%if %bquote(&uplo)^=0 & (&itemprt | &outitem^=_NULL_) %then %do;
            ************** first pass ***************;
%if &debug %then %str(put 'First pass';);
            link init1;
   %if %bquote(&by)^= %then %do;
            last.&lastby=0;
            do until(last.&lastby);
               set &data;
               by &byvars;
   %end;
   %else %do;
            _end=0;
            do until(_end);
               set &data end=_end;
   %end;
               link pass1;
            end;
            link doquant;
            ************** end first pass ***************;
%end;
            ************** second pass ***************;
%if &debug %then %str(put 'Second pass';);
            link init2;
            _end=0;
%if %bquote(&by)^= %then %do;
            last.&lastby=0;
            do until(last.&lastby);
               set &data end=_end;
               by &byvars;
%end;
%else %do;
            do until(_end);
               set &data end=_end;
%end;
               link pass2;
            end;
            link summary;
%if &histprt %then %do;
            link histog;
%end;
%if &itemprt | &outitem^=_NULL_%then %do;
            if _nsub>1 then link itemstat;
%end;
            ************** end second pass ***************;
%end;
******************** end loop over subtests ***********************;
 
         end;
********************** end loop over BY groups **********************;
 
         stop;
 
 
header:  ************ page header ************;
         retain _h;
         if _ls then put;
         else do;
            _h=_col;
            do while(_h=_col);
               put ' ' @;
               _h+1;
            end;
            _ls=_h-2;
         end;
 
%if %bquote(&by)^= %then %do;
         if _byl<0 then do;
            put @1 &byline @;
            _byl=max(0,(_ls-_col-1)/2);
            _bydash=' ';
            put @1 _bydash $varying200. _ls @;
            if _byl then _bydash=repeat('-',_byl-1);
         end;
         put @1 _bydash &byline _bydash /;
%end;
 
%if %bquote(&subtest)^= %then %do;
         retain _hin;
         _hin=1+(_ls-length(&subname))/2;
         put @_hin &subname /;
%end;
 
         return;
 
 
 
title:   ************ print title ************;
         put 'XYZZY' @;
         _in=1+(_ls-length(_char))/2;
         put @1 '       ' @_in _char /;
         return;
 
 
 
%if %bquote(&uplo)^=0 & (&itemprt | &outitem^=_NULL_) %then %do;
 
init1:   ************ initialize for pass 1 ************;
%if &debug %then %str(put 'Init pass 1';);
            _nsub=0;
            do _item_=0 to &nitem; _freq{_item_}=0; end;
 
%if &skip1 %then %do;
            if _nby=1 then _skip=1;
%end;
            return;
 
pass1:   ************ inside loop for pass 1 ************;
%if &skip1 %then %do;
%if &debug %then %str(put 'Loop pass 1';);
               if _skip then do;
                  _skip=0;
                  goto nextobs1;
               end;
%end;
               *** format items ***;
               %fmtv(&format,_fmt);
 
               *** compute scores ***;
               _nsub+1;
%if &debug %then %str(put _nsub=;);
               &score=0;
               do _item_=1 to &nitem;
%if %bquote(&subtest)^= %then %do;
                  if _act{_item_} then do;
%end;
                  %getv(&format)
                  if _v=_key{_item_} then do;
                     &score+1;
                  end;
%if %bquote(&subtest)^= %then %do;
               end;
%end;
               end;
               _freq{&score}+1;
%if &skip1 %then %do;
nextobs1:
%end;
               return;
 
 
 
doquant:    ************ get upper and lower quantiles
                         from frequency distribution ************;
%if &debug %then %str(put 'Quantiles';);
            _div=max(1,_nsub);
%if &debug %then %str(put _nsub= _div=;);
 
            *** find minimum and maximum scores;
            do _max=&nact to 0 by -1 while(_freq{_max}=0); end;
            do _min=0 to &nact by  1 while(_freq{_min}=0); end;
 
            *** find upper and lower quantiles;
            _x=&uplo*_div-1e-12;
 
            _l=0;
            do _upq=_max to 0 by -1;
               _l+_freq{_upq};
               if _l>=_x then goto breakup;
            end;
breakup:;
            if _upq<_max then _upq+1;
%if &debug %then %str(put _upq=;);
 
            _l=0;
            do _loq=_min to &nact;
               _l+_freq{_loq};
               if _l>=_x then goto breaklo;
            end;
breaklo:;
            if _loq>_min then _loq+-1;
%if &debug %then %str(put _loq=;);
            return;
%end;
 
 
 
init2:      ************ initialize for pass 2 ************;
%if &debug %then %str(put 'Init pass 2';);
            _nsub=0;
            _nmiss=0;
            _ninval=0;
            _t=0;
            _t2=0;
%if %bquote(&uplo)^=0 & (&itemprt | &outitem^=_NULL_) %then %do;
            _upn=0; * #obs above upper quantile;
            _lon=0; * #obs below lower quantile;
%end;
            _freq{0}=0;
            do _item_=1 to &nitem;
               _freq{_item_}=0;
               _p{_item_}=0;
%if &itemprt | &outitem^=_NULL_%then %do;
               _q{_item_}=0;
               _q2{_item_}=0;
               do _r=1 to &nresp;
                  _pr{_item_,_r}=0;
                  _i{_item_,_r}=0;
                  _i2{_item_,_r}=0;
                  _it{_item_,_r}=0;
                  _iq{_item_,_r}=0;
   %if %bquote(&uplo)^=0 %then %do;
                  _up{_item_,_r}=0;
                  _lo{_item_,_r}=0;
   %end;
               end;
   %if &itemx %then %do;
               do _r=%eval(&nresp+1) to &nrespx;
                  _pr{_item_,_r}=0;
      %if %bquote(&uplo)^=0 %then %do;
                  _up{_item_,_r}=0;
                  _lo{_item_,_r}=0;
      %end;
               end;
   %end;
%end;
            end;
 
%if &skip1 %then %do;
            if _nby=1 then _skip=1;
%end;
            return;
 
 
 
pass2:   ************ inside loop for pass 2 ************;
%if &debug %then %str(put 'Loop pass 2';);
%if &skip1 %then %do;
               if _skip then do;
                  _skip=0;
                  goto nextobs2;
               end;
%end;
               *** format items ***;
               %fmtv(&format,_fmt);
 
               *** accumulate statistics ***;
               _nobs+1;
               _nsub+1;
%if &debug %then %str(put _nsub=;);
               &score=0;
               &missing=0;
               &invalid=0;
               do _item_=1 to &nitem;
%if %bquote(&subtest)^= %then %do;
                  if _act{_item_} then do;
%end;
 
%if &outbin^=_NULL_ %then %do;
                  _b{_item_}=0;
%end;
                  * get first character of formatted value;
                  %getv(&format)
 
                  if _v=_key{_item_} then do;
                     &score+1;
                     _p{_item_}+1;
%if &outbin^=_NULL_ %then %do;
                     _b{_item_}=1;
%end;
                  end;
                  else if _v=' ' | _v='.' then &missing+1;
                  else if index(_resp,_v)=0 then &invalid+1;
%if %bquote(&subtest)^= %then %do;
               end;
   %if &outbin^=_NULL_ %then %do;
                  else _b{_item_}=.;
   %end;
%end;
               end;
%if &debug %then %str(put &score=;);
 
               &percent=100*&score/&nact;
               _freq{&score}+1;
               _nmiss+&missing;
               _ninval+&invalid;
               _t+&score;
               _t2+&score**2;
%if %bquote(&uplo)^=0 & (&itemprt | &outitem^=_NULL_) %then %do;
               if &score>=_upq then _upn+1;
               if &score<=_loq then _lon+1;
%end;
 
%if &itemprt | &outitem^=_NULL_%then %do;
               do _item_=1 to &nitem;
   %if %bquote(&subtest)^= %then %do;
                  if _act{_item_} then do;
   %end;
                  * remainder is score for all items except
                    the current one;
                  _rem=&score;
 
                  * get first character of formatted value;
                  %getv(&format)
 
                  if _v=_key{_item_} then _rem+-1;
                  _q{_item_}+_rem;
                  _q2{_item_}+_rem**2;
                  _a=index(_resp,_v);
                  if _a then do;
                     _pr{_item_,_a}+1;
                     _i{_item_,_a}+1;
                     _i2{_item_,_a}+1;
                     _it{_item_,_a}+&score;
                     _iq{_item_,_a}+_rem;
   %if %bquote(&uplo)^=0 %then %do;
                     if &score>=_upq then _up{_item_,_a}+1;
                     if &score<=_loq then _lo{_item_,_a}+1;
   %end;
                  end;
   %if &itemx %then %do;
                  else if _v=' ' | _v='.' then do;
                     _a=%eval(&nresp+1);
                     _pr{_item_,_a}+1;
      %if %bquote(&uplo)^=0 %then %do;
                     if &score>=_upq then _up{_item_,_a}+1;
                     if &score<=_loq then _lo{_item_,_a}+1;
      %end;
                  end;
                  else do;
                     _a=%eval(&nresp+2);
                     _pr{_item_,_a}+1;
      %if %bquote(&uplo)^=0 %then %do;
                     if &score>=_upq then _up{_item_,_a}+1;
                     if &score<=_loq then _lo{_item_,_a}+1;
      %end;
                  end;
   %end;
 
   %if %bquote(&subtest)^= %then %do;
               end;
   %end;
               end;
%end;
 
            *** new page at start of each BY group or subtest
                after reading at least one observation
                to get values of BY variables for BY line ***;
            if (_nby>1 | _test>1) & _nsub=1 then put _page_;
 
%if &gradeprt %then %do;
               *** print grade ***;
               if _nsub=1 | _ll<2 then do;
                  if _ll<2 then put _page_;
                  _char='Grade Report';
                  link title;
                  _in=1+max(0,_ls-35-input(symget('idwid'),5.))/2;
                  put @_in
%if &debug %then %put nid=&nid!;
%if &nid %then %do;
   %* DO loop bug in 6.03 forces use of GOTO;
   %let n=1; %idloop1:
                  &&idhdr&n
   %let n=%eval(&n+1); %if &n<=&nid %then %goto idloop1;
%end;
%else             '   Obs  ';
                  'Score' +2 '#Missing' +2 '#Invalid' +2 ' Percent' /;
               end;
%if &debug %then %str(put 'Grade' @;);
               put @_in
%if &nid %then %do n=1 %to &nid;
   %* DO loop bug in 6.03 forces use of GOTO;
   %let n=1; %idloop2:
               &&idval&n
   %let n=%eval(&n+1); %if &n<=&nid %then %goto idloop2;
%end;
%else          _nobs 6. +2;
               &score 5. +2 &missing 6. +2
               &invalid 8. +2 &percent 8. '%';
%end;
 
%if &out^=_NULL_ %then %do;
               output &out;
%end;
 
%if &outbin^=_NULL_ %then %do;
               output &outbin;
%end;
 
%if &skip1 %then %do;
nextobs2:
%end;
               return;
 
 
 
summary:    ************ print summary statistics ************;
%if &debug %then %str(put 'Test statistics';);
%if &gradeprt %then %do;
            put _page_;
%end;
 
            _div=max(1,_nsub);
%if &debug %then %str(put _nsub= _div=;);
 
            *** find minimum and maximum scores;
            do _max=&nact to 0 by -1 while(_freq{_max}=0); end;
            do _min=0 to &nact by  1 while(_freq{_min}=0); end;
%if &debug %then %str(put _min= _max=;);
 
            *** compute statistics for test;
            _ss=max(0,_t2-_t*_t/_div);
            _mean=_t/_div;
            if _div>1 then do;
               _varianc=_ss/(_div-1);
               _std=sqrt(_varianc);
            end;
            else do;
               _varianc=.;
               _std=.;
            end;
 
            if _ss>0 then do;
               _kr20=0;
               do _item_=1 to &nitem;
%if %bquote(&subtest)^= %then %do;
                  if _act{_item_} then do;
%end;
                  _x=_p{_item_}/_div;
                  _kr20+_x*(1-_x);
%if %bquote(&subtest)^= %then %do;
               end;
%end;
               end;
               _x=&nact/(&nact-1);
%if &compat %then %do;
               _kr20=_x*(1-_kr20/(_ss/(_div-1)));
               _kr21=_x*(1-(_mean-_mean**2/&nact)/(_ss/(_div-1)));
%end;
%else %do;
               _kr20=_x*(1-_kr20/(_ss/_div));
               _kr21=_x*(1-(_mean-_mean**2/&nact)/(_ss/_div));
%end;
               _se20=_std*sqrt(max(0,1-_kr20));
               _se21=_std*sqrt(max(0,1-_kr21));
            end;
            else do;
               _kr20=.;
               _kr21=.;
               _se20=.;
               _se21=.;
            end;
 
            _char='Summary of Test Statistics';
            link title;
 
            _in=1+floor(max(0,_ls-40)/2);
            _in2=_in+29;
            _item_=&nact;
            _l=_nsub*&nact;
            put @_in 'Number of Items'            @_in2 _item_     6.
              / @_in 'Number of Subjects'         @_in2 _nsub      6.
              /
              / @_in 'Mean Score'                 @_in2 _mean     10.3;
            if _ll<3 then put _page_; else put;
            put @_in 'Variance of Scores'         @_in2 _varianc  10.3
              / @_in 'Standard Deviation'         @_in2 _std      10.3;
            if _ll<3 then put _page_; else put;
            put @_in 'Kuder-Richardson 20'        @_in2 _kr20     10.3
              / @_in 'Kuder-Richardson 21'        @_in2 _kr21     10.3;
            if _ll<3 then put _page_; else put;
            put @_in 'Standard Error (from KR20)' @_in2 _se20     10.3
              / @_in 'Standard Error (from KR21)' @_in2 _se21     10.3;
            if _ll<3 then put _page_; else put;
            put @_in 'Minimum Score'              @_in2 _min       6.
              / @_in 'Maximum Score'              @_in2 _max       6.;
            if _ll<4 then put _page_; else put;
            put @_in 'Total Number of Answers'    @_in2 _l         6.
              / @_in '      Number Missing'       @_in2 _nmiss     6.
              / @_in '      Number Invalid'       @_in2 _ninval    6.;
 
            if _ss>0 then if _kr21>0 then if _kr21<&prophecy then do;
%if &compat %then %do;
               _sbp=floor(&nact*.9)*(1-_kr21)/(_kr21*.1);
               _sbp=floor(_sbp);
%end;
%else %do;
               _sbp=&nact*(&prophecy/(1-&prophecy))*((1-_kr21)/_kr21);
               _sbp=ceil(_sbp);
%end;
               _x=&prophecy;
            if _ll<4 then put _page_; else put;
            put @_in 'Spearman-Brown Prophecy (from KR21):'
              / @_in '      To obtain a reliability of ' _x 4.2
              / @_in '      the test should contain ' _sbp 'items' ;
            end;
            return;
 
 
 
%if &histprt %then %do;
histog:     ************ print frequency distribution ************;
%if &debug %then %str(put 'Histogram';);
            put _page_;
 
            _xxx=repeat('X',199);
            _l=1;
            do _x=_min to _max;
               _l=max(_l,_freq{_x});
            end;
            _xl=ceil((_l-.5)/min(_ls-50,length(_xxx)));
            _xw=ceil(_l/_xl);
 
            _in=1+floor((_ls-27)/2);
            put @_in 'Test Frequency Distribution' /;
 
            if _xl=1 then _char='   Each X represents 1 score';
                     else _char='   Each X represents 1 to '||
                               trim(left(put(_xl,5.)))||' scores';
            _l=length(_char);
            _in=1+floor((_ls-47-max(_xw+3,_l))/2);
            if _in<1 then do;
               if _xl=1
                  then _char='   X=1 score';
                  else _char=' X=1-'||trim(left(put(_xl,5.)))||'scores';
               _l=length(_char);
               _in=1+floor((_ls-47-max(_xw+3,_l))/2);
            end;
 
            put @_in '  Raw    Standard    Per-                   Cum'
              / @_in ' Score     Score   Centile  Percent  Freq  Freq'
                _char $varying. _l /;
 
            if _std<=0 then _std=1;
            _cum=_div;
            do _x=_max to _min by -1;
               _z=((_x-_mean)/_std)*100+500;
               _pcent=100*_freq{_x}/_div;
               _ptile=floor(100*(_cum-_freq{_x}/2)/_div);
               _l=ceil((_freq{_x}-.5)/_xl);
%if &debug %then %str(put _l @;);
   %if &itemver=6.03 %then %do;
               * in 6.03 $varying always prints at least one char;
               if _l=0 then
               put @_in _x 5. +5 _z 5. +4 _ptile 5. +4 _pcent 5.1 '%'
                   +2 _freq{_x} 5. +1 _cum 5.;
               else
   %end;
               put @_in _x 5. +5 _z 5. +4 _ptile 5. +4 _pcent 5.1 '%'
                   +2 _freq{_x} 5. +1 _cum 5. +3 _xxx $varying200. _l;
               _cum+-_freq{_x};
            end;
            return;
%end;
 
 
 
%if &itemprt | &outitem^=_NULL_%then %do;
itemstat:   ************ print item statistics ************;
%if &debug %then %str(put 'Item statistics';);
 
            _nrl=&nrespx;
%if &itemprt %then %do;
            put _page_;
            _nrl=min(_nrl,floor((_ls-22)/8));
            _in=floor(max(0,_ls-22-8*_nrl)/2);
            _in2=_in+14;
            _in3=_in+23;
%end;
%if &debug %then %str(put _nrl=;);
%if %bquote(&uplo)^=0 %then %do;
            _upn=max(1,_upn);
            _lon=max(1,_lon);
   %if &debug %then %str(put _upn= _lon=;);
%end;
 
            do _item_=1 to &nitem;
%if %bquote(&subtest)^= %then %do;
               if _act{_item_} then do;
%end;
 
               _name_=symget("var"||left(put(_item_,5.)));
 
%if &debug %then %str(put _item_= _name_=;);
               _q2{_item_}+-_q{_item_}**2/_div;
               _q2{_item_}=max(0,_q2{_item_});
 
               do _r1=1 to &nrespx by _nrl;
                  _r2x=min(_r1+_nrl-1,&nrespx);
                  _r2=min(_r2x,&nresp);
%if &debug %then %str(put _r1= _r2= _r2x;);
 
%if &itemprt %then %do;
                  if _item_>1|_r1>1 then do;
                     if _r1>&nresp then do;
                        _l=3
   %if %bquote(&uplo)^=0 %then %do;
                          +2
   %end;
                          ;
                     end;
                     else do;
                        _l=9
   %if &itemtotl %then %do;
                          +4
   %end;
   %if %bquote(&uplo)^=0 %then %do;
                          +2
   %end;
                          ;
                     end;
                     if _ll<_l then put _page_; else put;
                  end;
 
                  if _r1=1 then put @_in 'Item' _item_ 5.
                                    ': * is keyed ' @;
                           else put @_in +22 @;
                  do _r=_r1 to _r2;
                     _v=substr(_resp,_r,1);
                     put +6 _v $1. @;
                     if _v=_key{_item_} then put '*' @;
                                        else put ' ' @;
                  end;
%if &itemx %then %do;
                  if _r2x>&nresp then do;
                     if _r1<=%eval(&nresp+1) then put '     Omit ' @;
                     if _r2x>=&nrespx then put 'Invalid' @;
                  end;
%end;
                  put;
%end;
 
%if &itemprt %then %do;
                  if _r1=1 then put @_in _name_ @;
                  _lab='Responses';
                  put @_in2 _lab @_in3 @;
%end;
                  do _r=_r1 to _r2x;
                     _pr{_item_,_r}=_pr{_item_,_r}/_div;
%if &itemprt %then %do;
                     _x=_pr{_item_,_r}*100;
                     put +2 _x 5.1 '%' @;
%end;
                  end;
%if &itemprt %then %do;
                  put;
%end;
 
%if %bquote(&uplo)^=0 %then %do;
   %if &itemprt %then %do;
                  _lab='Upper '||put(100*&uplo,2.)||'%';
                  put @_in2 _lab @_in3 @;
   %end;
                  do _r=_r1 to _r2x;
                     _up{_item_,_r}=_up{_item_,_r}/_upn;
   %if &itemprt %then %do;
                     _x=_up{_item_,_r}*100;
                     put +2 _x 5.1 '%' @;
   %end;
                  end;
   %if &itemprt %then %do;
                  put;
   %end;
 
   %if &itemprt %then %do;
                  _lab='Lower '||put(100*&uplo,2.)||'%';
                  put @_in2 _lab @_in3 @;
   %end;
                  do _r=_r1 to _r2x;
                     _lo{_item_,_r}=_lo{_item_,_r}/_lon;
   %if &itemprt %then %do;
                     _x=_lo{_item_,_r}*100;
                     put +2 _x 5.1 '%' @;
   %end;
                  end;
   %if &itemprt %then %do;
                  put;
   %end;
%end;
 
%if &itemx %then %do;
                  if _r1<=&nresp then do;
%end;
 
                  do _r=_r1 to _r2;
                     _i2{_item_,_r}+-_i{_item_,_r}**2/_div;
                     _i2{_item_,_r}=max(0,_i2{_item_,_r});
                  end;
 
%if &itemtotl %then %do;
   %if &itemprt %then %do;
                  _l=_in2-5;
                  put / @_l 'Item-Total:';
 
                  _lab='Point Bis';
                  put @_in2 _lab @_in3 @;
   %end;
                  do _r=_r1 to _r2;
                     _x=sqrt(_i2{_item_,_r}*_ss);
                     if _x>0 then do;
                        _it{_item_,_r}+-_i{_item_,_r}*_t/_div;
                        _pbist{_r}=_it{_item_,_r}/_x;
   %if &compat %then %do;
                        _pbist{_r}=_pbist{_r}/sqrt(_div/(_div-1));
   %end;
                        _z=_pr{_item_,_r};
                        _y=probit(_z);
                        _y=exp(-_y**2/2)/sqrt(2*3.1415926536);
                        _bist{_r}=_pbist{_r}*sqrt(_z*(1-_z))/_y;
                     end;
                     else do;
                        _pbist{_r}=0;
                        _bist{_r}=0;
                     end;
   %if &itemprt %then %do;
                     put +2 _pbist{_r} 6.3 @;
   %end;
                  end;
   %if &itemprt %then %do;
                  put;
   %end;
 
   %if &itemprt %then %do;
                  _lab='Biserial ';
                  put @_in2 _lab @_in3 @;
                  do _r=_r1 to _r2;
                     put +2 _bist{_r} 6.3 @;
                  end;
                  put;
   %end;
%end;
 
%if &itemprt %then %do;
                  _l=_in2-5;
                  put / @_l 'Item-Remainder:';
 
                  _lab='Point Bis';
                  put @_in2 _lab @_in3 @;
%end;
                  do _r=_r1 to _r2;
                     _x=sqrt(_i2{_item_,_r}*_q2{_item_});
                     if _x>0 then do;
                        _iq{_item_,_r}+-_i{_item_,_r}*_q{_item_}/_div;
                        _pbisr{_r}=_iq{_item_,_r}/_x;
                        _z=_pr{_item_,_r};
                        _y=probit(_z);
                        _y=exp(-_y**2/2)/sqrt(2*3.1415926536);
                        _bisr{_r}=_pbisr{_r}*sqrt(_z*(1-_z))/_y;
                     end;
                     else do;
                        _pbisr{_r}=0;
                        _bisr{_r}=0;
                     end;
                     if abs(_pbisr{_r})<1 & _div>=2 then do;
%if &compat %then %do;
                        _tstat{_r}=_pbist{_r}*sqrt(_div-2)/
                                      sqrt(1-_pbist{_r}**2);
%end;
%else %do;
                        _tstat{_r}=_pbisr{_r}*sqrt(_div-2)/
                                      sqrt(1-_pbisr{_r}**2);
%end;
                        if _div>2 then do;
                           _pval{_r}=(1-probt(abs(_tstat{_r}),
                                             _div-2))*2;
                           if _pval{_r}<.0001 then _pval{_r}=.0001;
                        end;
                        else _pval{_r}=.;
                     end;
                     else do;
                        _tstat{_r}=.;
                        _pval{_r}=0;
                     end;
%if &itemprt %then %do;
                     put +2 _pbisr{_r} 6.3 @;
%end;
                  end;
%if &itemprt %then %do;
                  put;
%end;
 
%if &itemprt %then %do;
                  _lab='Biserial ';
                  put @_in2 _lab @_in3 @;
                  do _r=_r1 to _r2;
                     put +2 _bisr{_r} 6.3 @;
                  end;
                  put;
 
                  _lab='t test   ';
                  put @_in2 _lab @_in3 @;
                  do _r=_r1 to _r2;
                     put +2 _tstat{_r} 6.3 @;
                  end;
                  put;
 
                  _lab='Prob>|t| ';
                  put @_in2 _lab @_in3 @;
                  do _r=_r1 to _r2;
                     put +2 _pval{_r} 6.4 @;
                  end;
                  put;
%end;
 
%if &itemx %then %do;
                  end;
%end;
 
               end; /* loop over panels */
 
%if &outitem^=_NULL_ %then %do;
%if &debug %then %str(put 'Outitem';);
               do _r=1 to &nresp;
                  _respon_=substr(_resp,_r,1);
                  if _respon_=_key{_item_} then _keyed_='*';
                                           else _keyed_=' ';
                  _propor_=_pr{_item_,_r};
   %if %bquote(&uplo)^=0 %then %do;
                  _upper_=_up{_item_,_r};
                  _lower_=_lo{_item_,_r};
   %end;
   %if &itemtotl %then %do;
                  _it_pb_=_pbist{_r};
                  _it_bis_=_bist{_r};
   %end;
                  _ir_pb_=_pbisr{_r};
                  _ir_bis_=_bisr{_r};
                  _t_test_=_tstat{_r};
                  _prob_t_=_pval{_r};
                  output &outitem;
               end;
%end;
 
%if %bquote(&subtest)^= %then %do;
            end;
%end;
            end; /* loop over items */
            ****** end print item statistics ******;
 
            return;
%end;
 
%goto exit;
 
 
%fail:;
   %put NOTE: ITEM macro terminated due to error(s).;
   options _last_=&lastds;
 
 
%exit:;
run;
 
%if &nonotes %then %do;
   options notes;
%end;
 
%mend item;
 
 
 
 ******************************************************************;
 
%* store name and/or format in macro variables
   for each variable in a sas variable list.
   both quoted and unquoted names are needed because of
   a macro bug in 6.04;
%macro xlist(data,list,name,qname,fmt,qfmt,count,qcount);
%if &debug %then %do;
   %put xlist: data=&data!;
   %put xlist: list=&list!;
   %put xlist: name=&name!;
   %put xlist: qname=&qname!;
   %put xlist: fmt=&fmt!;
   %put xlist: qfmt=&qfmt!;
   %put xlist: count=&count!;
   %put xlist: qcount=&qcount!;
%end;
   %local n;
   * make a small data set so summary wont take a lot of time--
     cant use obs= data set option on the real data set;
         data _ITEM0_;
            set &data;
            output; * avoid warning from summary in 6.03;
            stop;
         run;
   %if &syserr>4 %then %goto fail;
   * use summary to reorder the variables;
         proc summary data=_ITEM0_(firstobs=1 obs=1);
            output out=_ITEM1_(drop=_TYPE_ _FREQ_);
            id &list;
         run;
   %if &syserr>4 %then %goto fail;
   * get data set with variable names;
         proc contents data=_ITEM1_(firstobs=1)
            noprint out=_ITEM2_;
         run;
   %if &syserr>4 %then %goto fail;
%if &debug %then %do;
         proc print data=_ITEM2_; run;
%end;
   * order by position in data set, not alphabetically;
         proc sort
   %if &itemver=6.06 %then force;
            data=_ITEM2_(firstobs=1 obs=32767); by npos; run;
   %if &syserr>4 %then %goto fail;
   * find out how many variables;
   %global &count;
         data _null_;
            set _ITEM2_(firstobs=1 obs=32767) nobs=count;
%if &debug %then %do;
            put 'xlist: count=' count;
%end;
            call symput(&qcount,trim(left(put(count,5.))));
            stop;
         run;
   %if &syserr>4 %then %goto fail;
   * make macro variables global before defining them;
   %if &name^= %then %do n=1 %to &&&count;
      %global &name&n;
      %end;
   %if &fmt^=  %then %do n=1 %to &&&count;
      %global &fmt&n;
      %end;
   * assign variable names to macro variables;
   %if &name^= | &fmt^= %then %do;
         data _null_;
            set _ITEM2_(firstobs=1 obs=32767);
      %if &name^= %then %do;
            call symput(&qname||left(put(_n_,5.)),trim(name));
      %end;
      %if &fmt^= %then %do;
            length fmt $20;
            if format=' ' then do;
               if type=1 then format='BEST';
                         else format='CHAR';
            end;
            if formatl=0 then do;
               if type=1 then formatl=8;
                         else formatl=length;
            end;
            fmt=trim(format)||
                trim(left(put(formatl,3.)))||'.'||
                trim(left(put(formatd,3.)));
            call symput(&qfmt||left(put(_n_,5.)),trim(fmt));
      %end;
         run;
         %if &syserr>4 %then %goto fail;
   %end;
   %goto finish;
 
%fail:
   %let itemrc=BAD;
%finish:
%mend xlist;
 
 
%macro getv(format);
            _v=_fmt{_item_};
%mend getv;
 
 
%macro fmtv(format,var);
   %local n;
   %if &format= %then %do n=1 %to &nitem;
            &var{&n}=left(put(&&var&n,&&fvar&n));
   %end;
   %else %do n=1 %to &nitem;
            &var{&n}=left(put(&&var&n,&format));
   %end;
%mend fmtv;
 
%macro xsubstr(str,pos,len);
   %local r;
   %let r=%eval(%length(&str)-%eval(&pos)+1);
   %if &len= %then %let len=&r;
   %else %if &len>&r %then %let len=&r;
   %if &len<=0 %then %str();
   %else %bquote(%substr(&str,&pos,&len));
%mend xsubstr;
 
*======================== END ITEM MACRO =============================;
 
 
