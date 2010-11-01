/*
 * Richard A. DeVenezia
 * November 18, 2003
 * http://www.devenezia.com
 *
 * mod:
 * 11/19/03 rad force one empty row if data has no rows
 * 11/25/03 rad add translate= to xmlappend
 */

/*-----
 * group: Data out
 * purpose: Export several SAS tables in one XML file
 * notes: Used by <a href="?m=sas2xls">sas2xls<a>
 */

%macro xmlib (store=sasuser.templat);

   /*
    * Note: Tagset names xmlib<0|1><0|1> correspond to 0|1 state values
    * of start and finish macro variables in xmlappend
    */

   ods path &store (update)
            sashelp.tmplmst (read);

   proc template;

      define tagset Tagsets.xmlib__ /store=&store;
         parent = tagsets.sasioXML;
         notes "multiple tables SAS-XML generic";

         define event MLEVDAT;
            put VALUE;
            break;
         end;
         define event doc;
            start: break;
            finish:break;
         end;
         define event SASRow;
            start:
               put "<" NAME ">" nl;
               break;
            finish:
               put "</" NAME ">" nl;
               break;
         end;
         define event SASColumn;
            start:
               ndent;
               put "<" NAME  ">";
               break;
            finish:
               put "</" NAME ">" nl;
               xdent;
               break;
         end;
      end;

      define tagset Tagsets.xmlib10 /store=&store;
         parent = tagsets.xmlib__;
         notes "first of multiple tables SAS-XML generic";
         define event doc;
            start:
              trigger XMLversion;
              put "<library>" nl;
         end;
      end;

      define tagset Tagsets.xmlib00 /store=&store;
         parent = tagsets.xmlib__;
         notes "middle of multiple tables SAS-XML generic";
      end;

      define tagset Tagsets.xmlib01 /store=&store;
         parent = tagsets.xmlib__;
         notes "last of multiple tables SAS-XML generic";
         define event doc;
            finish:
              put "</library>" nl;
         end;
      end;

      define tagset Tagsets.xmlib11 /store=&store;
         parent = tagsets.xmlib__;
         notes "only of multiple tables SAS-XML generic";
         define event doc;
            start:
              trigger XMLversion;
              put "<library>" nl;
            finish:
              put "</library>" nl;
         end;
      end;
   run;

%mend;


%macro xmlappend (

    file=
  , data = &syslast
  , out=
  , start  = 0
  , finish = 0
  , nobs = 0
  , zerorows = forceone
  , translate =

);

  %if &start ne 0 and &start ne 1 %then %do;
    %put ERROR: Start must be 0 or 1;
    %goto EndMacro;
  %end;

  %if &finish ne 0 and &finish ne 1 %then %do;
    %put ERROR: Finish must be 0 or 1;
    %goto EndMacro;
  %end;

  %if (%superq(file) eq ) %then %do;
    %put ERROR: File should be supplied;
    %goto EndMacro;
  %end;

  /*
   * Note: tagset specified corresponds directly with <start><finish> state
   * Note: mod option allows only appending, if not present (asis in start=1 state)
   *       each datastep would overwrite destination file
   * Note: if out= is not specified the member name of the data set is used
   */

  %local xmlib mod dsid out ;

  %let xmlib = _%substr (%sysfunc (ranuni(0), 9.7), 3);

  %if &start=0 %then %let mod=mod;

  filename &xmlib "&file" &mod;
  libname  &xmlib xml tagset=tagsets.xmlib&start&finish;

   %if %length(&out)=0 %then %do;
     %let dsid = %sysfunc (open (&data));
     %let out  = %sysfunc (attrc (&dsid, MEM));
     %let dsid = %sysfunc (close (&dsid));
   %end;

   %local any;
   %let any = 0;

   data &xmlib..&out;
     %if &nobs>0 %then %do;
       if _n_ > &nobs then do;
        call symput ('any', '1');
        stop;;
       end;
     %end;
     set &data end=__end__;

     %if (%length(&translate) gt 0) %then %do;
       array &xmlib _character_;
       do _i_ = 1 to hbound(&xmlib);
         &xmlib[_i_] = translate (&xmlib[_i_], " ", "&translate"X);
       end;
     %end;

     if __end__ then call symput ('any', '1');
   run;

   %if (not &any and &zerorows=forceone) %then %do;
   data &xmlib..&out;
     output;
     set &data;
   run;
   %end;

   libname  &xmlib;
   filename &xmlib;

%EndMacro:

%mend;

/**html
 * <p>Sample code</p>
 */

 /*;

%xmlib; * only needed once, best in an automacro library called from autoexec;

%let xmlOut=%sysfunc(pathname(work))/multitable.xml;

%xmlappend(file=&xmlOut, data=sashelp.class(obs=3), start=1);
%xmlappend(file=&xmlOut, data=sashelp.company(obs=2));
%xmlappend(file=&xmlOut, data=sashelp.tourism(obs=5), finish=1);

libname check xml "&xmlOut";

proc contents data=check._all_;
run;

proc compare base=sashelp.class(obs=3) compare=check.class;run;
proc compare base=sashelp.company(obs=2) compare=check.company;run;
proc compare base=sashelp.tourism(obs=5) compare=check.tourism;run;


%let xmlOut=%sysfunc(pathname(work))/singletable.xml;

%xmlappend(file=&xmlOut, data=sashelp.class, start=1, finish=1);

libname check xml "&xmlOut";

proc contents data=check._all_;
run;

proc compare base=sashelp.class compare=check.class;run;

*/;


/*;
 * points of study;
proc template;
  source tagsets.sasioXML / file='c:\temp\sasioxml-tagset.txt';
  source tagsets.sasXML   / file='c:\temp\sasxml-tagset.txt';
run;
*/;
