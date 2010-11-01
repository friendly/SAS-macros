 /*-------------------------------------------------------------------*
  *    Name: mosaic.sas                                               *
  *   Title: Macro interface for mosaic displays                      *
        Doc: http://www.datavis.ca/sasmac/mosaic.html              
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:   9 Sep 1997 17:04:26                                    *
  * Revised:  31 Mar 2009 08:48:02                                    *
  * Version: 1.5-1                                                    *
  *  - added BY= variables (calling MOSPART)                          *
  *  - fixed bug with multiple BY= variables                          *
  *  - added check for non-existant vars                              *
  * 1.4 Added ZEROS= variable to specify structural zeros             *
  * 1.5 Added LORDER= parameter to specify order of levels            *
  *  - added GOUT= to specify output graphics catalog                 *
  *  - added NAME= to specify graphics catalog entries                *
  *  - added LEGEND= to control legend, and OUTSTAT=                  *
  *  - fixed buglet with case of variable names                       *
  *  - adjusted SYMSIZE and WORKSIZE values                           *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The MOSAIC macro provides an easily used macro interface to the
 MOSAICS, MOSAICD and MOSPART SAS/IML programs.  Using the
 SAS/IML programs directly means that you must compose a PROC IML
 step, create the frequency table and associated variables properly
 as IML matrices, and invoke the mosaic module (or mospart, for partial
 mosaics).

 The MOSAIC macro may be used with any SAS data set in frequency
 form (e.g., the output from PROC FREQ).  The macro simply creates
 the PROC IML step, reads the input data set, and runs the either
 the mosaic module, the mosaicd module, or the mospart module,
 depending on the options specified.  
 
 If your data is in case form, or you wish to collapse over some
 table variables, you must use PROC FREQ first to construct the
 contingency table to be analyzed.  The TABLE macro may be used
 for this purpose.  It has the advantage of allowing formatted
 values of the table factors to be used by the mosaics program.

 Ordinarily, the program fits a model (specified by the FITTYPE=
 parameter) and displays residuals from this model in the mosaic
 for each marginal subtable specified by the PLOTS= parameter.
 However, if you have already fit a model and calculated residuals
 some other way (e.g., using PROC CATMOD or PROC GENMOD), specify
 a RESID= variable in the macro call.  The macro will then call
 the mosaicd module.
 
 If a BY= variable is specified, the macro produces one (partial)
 mosaic plot for each level of the BY variable(s).

=Usage:

 The parameters for the mosaic macro are like those of the SAS/IML
 program, except:
 
* DATA=    Specifies the name of the input dataset.  The data set
           should contain one observation per cell, the variables
           listed in VAR= and COUNT=, and possibly RESID= and BY=.

* VAR=     Specifies the names of the factor variables for the 
           contingency table. Abbreviated variable lists (e.g., V1-V3) 
           are not allowed. The levels of the factor variables may be
           character or numeric, but are used `as is' in the input data.
           You may omit the VAR= variables if variable names are used in
           the VORDER= parameter.

* BY=      Specifies the names of one (or more) By variables.  Partial
           mosaic plots are produced for each combination of the levels
           of the BY= variables.  The BY= variable(s) *must* be listed among
           the VAR= variables.

* COUNT=   Specifies the names of the frequency variable in the dataset

* CONFIG=  For a user-specified model, CONFIG= gives the terms in the
           model, separated by '/'.  For example, to fit the model of
           no-three-way association, specify config=1 2 / 1 3 / 2 3,
           or (using variable names) config = A B / A C / B C.
			  CONFIG must be a matrix, so use 0s as place-holders if the
			  number of variables in each term are not all the same.
           Note that the numbers refer to the variables after they
           have been reordered, either sorting the data set, or by the
           VORDER= parameter.

* VORDER=  Specifies either the names of the variables or their indices
           in the desired order in the mosaic.  Note that the using the
           VORDER parameter keeps the factor levels in their order in
           the input data set.

* LORDER=  Specifies a reordering of the levels of one or more variables,
           of the form 'A: a2 a1 a3 / B: b2 b3 b4 b1', where '/' separates
           different variables and ':' separates the name of a variable
			  from the desired order of the levels.

* SORT=    Specifies whether and how the input data set is to be sorted
           to produce the desired order of variables in the mosaic.
           SORT=YES sorts the data in the reverse order that they are
           listed in the VAR= parameter, so that the variables are 
           entered in the order given in the VAR= parameter.  Otherwise,
           SORT= lists the variable names, possibly with the DESENDING
           or NOTSORTED options in the reverse of the desired order.
           e.g., SORT=C DESCENDING B DESCENDING A.  The default is
           SORT=YES, unless VORDER= has been specified.

* RESID=   Specifies that a model has already been fit and that externally
           calculated residuals are contained in the variable named 
           by the RESID= parameter.
 =*/
 
%macro mosaic(
   data=_last_,     /* Name of input dataset                        */
   var=,            /* Names of all factor variable                 */
   count=count,     /* Name of the frequency variable               */
   by=,             /* Name(s) of BY variables                      */
   fittype=joint,   /* Type of models to fit                        */
   config=,         /* User model for fittype='USER'                */
   devtype=gf,      /* Residual type                                */
   shade=2 4,       /* shading levels for residuals                 */
   plots=,          /* which plots to produce                       */
   colors=blue red, /* colors for + and - residuals                 */
   fill=HLS HLS,    /* fill type for + and - residuals              */
   split=V H,       /* split directions                             */
   vorder=,         /* order of variables in mosaic                 */
   htext=1.5,       /* height of text labels                        */
   font=,           /* font for text labels                         */
   title=,          /* title for plot(s)                            */
   space=,          /* room for spacing the tiles                   */
   cellfill=,       /* write residual in the cell?                  */
   vlabels=,        /* Number of variable names used as plot labels */
   sort=,           /* Pre-sort variables?                          */
   resid=,          /* Name of residual variable                    */
   fuzz=,           /* Fuzz value for residuals near 0              */
   order=,          /* Do CA on marginal tables?                    */
   lorder=,         /* Reorder levels of one or more variables      */
   legend=,         /* Legend for shading levels: H, V or NONE      */
   outstat=,        /* Name of an output data set of fit statistics */
   zeros=,          /* 0/1 variable, where 0 indicates structural 0 */
   name=mosaic,     /* base name of graphic catalog entries         */
   gout=            /* name of graphic catalog                      */
   );


%if %length(&var)=0 & %length(&vorder)>0 %then %do;
	%if %verify(&vorder, %str(0123456789 ))>0 %then %let var=&vorder;
	%end;

%if %length(&var)=0 %then %do;
   %put ERROR: You must specify the VAR= classification variables;
   %goto done;
   %end;

%if %upcase(&data)=_LAST_ %then %let data = &syslast;

%if %length(&sort)=0 %then %do;
	%if %length(&vorder)>0
		%then %let sort=NO;
		%else %let sort=YES;
	%end;

%let sort=%upcase(&sort);
%if &sort^=NO %then %do;
   %if &sort=YES %then %let sort=%reverse(&var);
   proc sort data=&data;
      by &sort;
%end;
   
%if %upcase(&fittype)=USER and %length(&config)=0 %then %do;
   %put ERROR: You must specify the USER model with the CONFIG= argument;
   %goto done;
   %end;
   
%if %upcase(&fittype) ^=USER and %length(&config)>0 %then %do;
   %put WARNING: You specified a USER model with the CONFIG= argument...;
	%put WARNING: resetting FITTYPE=USER;
   %let fittype=USER;
   %end;
   
%if %length(&config) %then %do;
   data _null_;
      length config $ 200;
      config = "&config";
      config = translate(config, ',', '/');
      call symput('config', trim(config));
   run;
   %*put config: &config;
%end;

%*-- Get variable labels for use as vnames;
	%*** use summary to reorder the variables in order of var list;
/*
proc summary data=&data(firstobs=1 obs=1);
	id &var;
	output out=_tmp_(drop=_TYPE_ _FREQ_);
proc contents data=_tmp_ out=_work_(keep=name type label npos) noprint;
proc sort data=_work_;
	by npos;

data _null_;
	set _work_;
	call symput("name"||left(put(_n_,5.)),trim(name));
	call symput("type"||left(put(_n_,5.)),put(type,1.));
	call symput("lab"||left(put(_n_,5.)),trim(label));
*/

	%*--Becuase of the large number of modules loaded, it may be
	    necessary to adjust the symsize value;
proc iml  symsize=10000  worksize=10000;
   reset storage=mosaic.mosaic;
   load module=_all_;
   *include mosaics(mosaics);    *-- use if you havent compiled mosaics.sas;
   %if %length(&resid)>0 %then %do;
      %include mosaics(mosaicd);
      %end;
	%if %length(&by)>0 %then %do;
		%include mosaics(mospart);
		%end;
	%if %length(&lorder)>0 %then %do;
		%include mosaics(dimorder);
		%include mosaics(lorder);
		lorder = "&lorder";
		%end;

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

   vnames = str2vec("&var", ' ');          *-- Preserve case of var names;
	vars = t(contents("&data"));
	ok=1; 
	vn=upcase(vnames) || upcase("&count");
	%if %length(&by)>0 %then %do;
		vn = vn || upcase(str2vec("&by", ' '));
		%end;
	if ncol(union(upcase(vars), vn)) > ncol(vars) then ok=0;

	do;
	if ok=0 then do;
		print 'One or more variables are not contained in the input data set';
		print "Data set &data contains", vars;
		print "You asked for" vn;
		goto done; 
		end;

   %*-- Read and reorder residuals if specified;
   %if %length(&resid)>0 %then %do;
      vn = vnames;
      run readtab("&data","&resid", vn, dev,   lev, ln);
		if any(dev = .) then dev[loc(dev=.)] = 0;
      %if %length(&vorder) %then %do;
         vorder  = { &vorder };
         *-- marg bug workaround: subtract min value, then add back in;
         mdev = min(dev);
         dev = dev - mdev;
         run transpos(lev, dev, vn, ln, vorder);
         dev = dev + mdev;
         %end;
		%if %length(&lorder)>0 %then %do;
			run lorder(lev, ln, vn, dev);
			%end;
   %end;

   %*-- Read and reorder zeros if specified;
   %if %length(&zeros)>0 %then %do;
      vn = vnames;
      run readtab("&data","&zeros", vn, zeros,   lev, ln);
		if all(zeros)>0 then do;
			print "Zeros variable &zeros is all positive. Ignoring";
			free zeros;
			end;
		else do;
      %if %length(&vorder) %then %do;
         vorder  = { &vorder };
			zeros = zeros+1;
         run transpos(lev, zeros, vn, ln, vorder);
			zeros = zeros-1;
			%end;
		%if %length(&lorder)>0 %then %do;
			run lorder(lev, ln, vn, zeros);
			%end;
			end;
	%end;

   %*-- Read and reorder counts;
   run readtab("&data","&count", vnames, table, levels, lnames);

   %if %length(&vorder) %then %do;
      vorder  = { &vorder };
      run transpos(levels, table, vnames, lnames, vorder);
      %end;
	%if %length(&lorder)>0 %then %do;
		run lorder(levels, lnames, vnames, table);
		%end;

   shade={&shade};
   colors={&colors};
   filltype={&fill};
   split={&split};

   title = "&title";
   
   %if %length(&htext)>0    %then %do; htext=&htext;         %end;
   %if %length(&space)>0    %then %do; space={&space};       %end;
   %if %length(&font)>0     %then %do; font = "&font";       %end;
   %if %length(&fuzz)>0     %then %do; fuzz = "&fuzz";       %end;
   %if %length(&vlabels)>0  %then %do; vlabels=&vlabels;     %end;
   %if %length(&cellfill)>0 %then %do; cellfill="&cellfill"; %end;
   %if %length(&outstat)>0  %then %do; outstat="&outstat";   %end;
   %if %length(&order)>0    %then %do; order={&order};       %end;
   %if %length(&legend)>0   %then %do; legend={&legend};     %end;
   %if %length(&gout)>0     %then %do; gout={&gout};         %end;
   %if %length(&name)>0     %then %do; name="&name";         %end;


   %if %length(&resid)>0 %then %do;
      run mosaicd(levels, table, vnames, lnames, dev, title); 
      %end;

   %else %do;
      fittype = "&fittype";
      devtype = "&devtype";
      %if %length(&config)>0 %then %do;
         config=t({&config});
         %end;

		%if %length(&by)>0 %then %do;
			%if %verify(&by, %str(0123456789 ))>0 %then %let by={&by};
			%*put verify: %verify(&by, %str(0123456789 ));
			*show levels vnames lnames;
			run mospart(levels, table, vnames, lnames, title, &by); 
			%end;

		%else %do;
			%if %length(&plots)=0 %then %do;
				plots = 1:nrow(vnames);
				%end;
			%else %do;
				%if %index(&plots,:)
					%then %str(plots = &plots;);
					%else %str(plots = {&plots};);
				%end;
			run mosaic(levels, table, vnames, lnames, plots, title);
			%end;
		%end;
done:
	end;
   quit;
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
