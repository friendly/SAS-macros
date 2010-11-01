 /*-------------------------------------------------------------------*
  *    Name: mosmat.sas                                               *
  *   Title: Macro interface for mosaic matrices                      *
        Doc: http://www.datavis.ca/sasmac/mosmat.html              
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:   3 Nov 1998 09:27:20                                    *
  * Revised:  10 Nov 2000 10:27:37                                    *
  * Version: 1.1                                                      *
  * 1.1  Fixed bug in call to panels when mosmat was called more      *
  *      than once in a job or session.                               *
  *                                                                   *
  * Requires: %gdispla, %panels                                       *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The MOSMAT macro provides an easily used macro interface to the
 MOSAICS and MOSMAT SAS/IML programs, to create a scatterplot
 matrix of mosaic displays for all pairs of categorical variables.

 Each pairwise plot shows the marginal frequencies to the order specified
 by the PLOTS= parameter.  When PLOTS=2, these are the bivariate margins,
 and the residuals from marginal independence are shown by shading.
 When PLOTS>2, the observed frequencies in a higher-order marginal
 table are displayed, and the model fit to that marginal table is
 determined by the FITTYPE= parameter.
 
=Usage:

 The parameters for the mosaic macro are like those of the SAS/IML
 programs, except:
 
* DATA=    Specifies the name of the input dataset.  Should contain
           one observation per cell, the variables listed in VAR=
           and COUNT=.  [Default: DATA=_LAST_]

* VAR=     Specifies the names of the factor variables for the 
           contingency table. Abbreviated variable lists (e.g., V1-V3) 
           are not allowed. The levels of the factor variables may be
           character or numeric, but are used as is in the input data.
           Upper/lower case in the variable names is respected in the
           diagonal label panels.
           You may omit the VAR= variables if variable names are used in
           the VORDER= parameter.

* COUNT=   Specifies the name of the frequency variable in the dataset.
           The COUNT= variable must be specified.

* PLOTS=   The PLOTS= parameter determines the number of table variables
           displayed in each pairwise mosaic. [Default: PLOTS=2]

* CONFIG=  For a user-specified model, config= gives the terms in the
           model, separated by '/'.  For example, to fit the model of
           no-three-way association, specify CONFIG=1 2 / 1 3 / 2 3,
           or (using variable names) CONFIG = A B / A C / B C.
           Note that the numbers refer to the variables after they
           have been reordered, either sorting the data set, or by the
           VORDER= parameter.

* VORDER=  Specifies either the names of the variables or their indices
           in the desired order in the mosaic.  Note that the using the
           VORDER= parameter keeps the factor levels in their order in
           the data.

* SORT=    Specifies whether and how the input data set is to be sorted
           to produce the desired order of variables in the mosaic.
           SORT=YES sorts the data in the reverse order that they are
           listed in the VAR= paraemter, so that the variables are 
           entered in the order given in the VAR= parameter.  Otherwise,
           SORT= lists the variable names, possibly with the DESENDING
           or NOTSORTED options in the reverse of the desired order.
           e.g., SORT=C DESCENDING B DESCENDING A.
=*/

%macro mosmat(
   data=_last_,     /* Name of input dataset              */
   var=,            /* Names of factor variables          */
   count=count,     /* Name of the frequency variable     */
   fittype=joint,   /* Type of models to fit              */
   config=,         /* User model for fittype='USER'      */
   devtype=gf,      /* Residual type                      */
   shade=,          /* shading levels for residuals       */
   plots=2,         /* which plots to produce             */
   colors=blue red, /* colors for + and - residuals       */
   fill=HLS HLS,    /* fill type for + and - residuals    */
   split=V H,       /* split directions                   */
   vorder=,         /* order of variables in mosaic       */
   htext=,          /* height of text labels              */
   font=,           /* font for text labels               */
   title=,          /* title for plot(s)                  */
   space=,          /* room for spacing the tiles         */
   fuzz=,           /* smallest abs resid treated as zero */
	abbrev=,         /* abbreviate variable names in model */
   sort=YES,        /* Sort variables first?              */
   );

%if %length(&var)=0 & %length(&vorder)>0 %then %do;
	%if %verify(&vorder, %str(0123456789 ))>0 %then %let var=&vorder;
	%end;

%if %length(&var)=0 %then %do;
   %put ERROR: You must specify the VAR= classification variables.;
   %goto done;
   %end;

%if %length(&count)=0 %then %do;
   %put ERROR: You must specify the COUNT= frequency variable.;
   %goto done;
   %end;

%if %upcase(&data)=_LAST_ %then %let data = &syslast;
%let sort=%upcase(&sort);
%if &sort^=NO %then %do;
   %if &sort=YES %then %let sort=%reverse(&var);
   proc sort data=&data out=_sorted_;
      by &sort;
	%let data=_sorted_;
%end;
   
%if %upcase(&fittype)=USER and %length(&config)=0 %then %do;
   %put ERROR: You must specify the USER model with the CONFIG= argument;
   %goto done;
   %end;
   
%if %length(&config) %then %do;
   %*-- Translate / in config to , for iml;
   data _null_;
      length config $ 200;
      config = "&config";
      config = translate(config, ',', '/');
      call symput('config', trim(config));
   run;
   %put config: &config;
%end;

%gdispla(OFF);

   %*--Becuase of the large number of modules loaded, it may be
       necessary to adjust the symsize value;
proc iml /* worksize=10000 */ symsize=256;
   reset storage=mosaic.mosaic;
   load module=_all_;

   %include mosaics(mosmat); 

/* - now included in mosaics.sas
start str2vec(string,dlm);
	*-- String to character vector;
   free out;
   i=1;
   sub = scan(string,i,dlm);
   do while(sub ^=' ');
      out = out || sub;
      i = i+1;
      sub = scan(string,i,dlm);
   end;
	return(out);
	finish;
*/

start symput(name, val);
   *-- Create a macro variable from a char/numeric scalar;
   if type(val) ='N'
      then value = trim(char(val));
      else value = val;
   call execute('%let ', name, '=', value, ';');
   finish;

   vnames = str2vec("&var",' ');    *-- Preserve case of var names;

   %*-- Read and reorder counts;
   run readtab("&data","&count", vnames, table, levels, lnames);
   nv = ncol(levels);
   run symput('nv', nv);
   %if %length(&vorder) %then %do;
      vorder  = { &vorder };
      run reorder(levels, table, vnames, lnames, vorder);
      %end;

   %*-- These variables have their defaults set in mosaics(globals);
	%*   (Dont set them here unless passed as parameters.);
   %if %length(&space)>0 %then %do;
      space={&space};
      %end;
   %if %length(&shade)>0 %then %do;
      shade={&shade};
      %end;

   %*-- These variables have their defaults set in mosmat module;
   %if %length(&htext)>0 %then %do;
      htext=&htext;
      %end;
   %if %length(&font)>0 %then %do;
      font = "&font";
      %end;

   colors={&colors};
   filltype={&fill};
   split={&split};
   title = "&title";
   fittype = "&fittype";
   devtype = "&devtype";
   %if %length(&config)>0 %then %do; config=t({&config}); %end;
   %if %length(&fuzz)>0   %then %do; fuzz=&fuzz;  %end;
   %if %length(&abbrev)>0 %then %do; abbrev=&abbrev;  %end;

   plots = &plots;
   run mosmat(levels, table, vnames, lnames, plots, title);
quit;
%gdispla(ON);
%if &nv>0 %then %do;
	%let first = %eval(1 - &nv*&nv);
   %panels(rows=&nv, cols=&nv, order=down, first=&first, last=0);
	%end;

%done:

%mend;

%macro reverse(list);
   %local result i v;
   %let result =;
   %let i = 1;
   %let v = %scan(&list,&i,%str( ));
   %do %while (%length(&v) > 0);
      %let result = &v &result;
      %let i = %eval(&i + 1);
      %let v = %scan(&list,&i,%str( ));
      %end;
   &result
%mend;
