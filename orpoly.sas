 /*--------------------------------------------------------------*
  *    Name: orpoly.sas                                          *
  *   Title: Orthogonal polyomial contrasts (unequal spacing | N)*
        Doc: http://www.datavis.ca/sasmac/orpoly.html      
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 30 Mar 2001 10:47:28                                *
  * Revised: 10 Jun 2003 14:50:05                                *
  * Version: 1.1                                                 *
  * 1.1 Added DEG=, TYPE=BOTH (CONTRAST &ESTIMATE)               *
  *     Use the %tempfile macro to generate the temp file        *
  * 1.2 Fixed bug with default degree                            *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 For ANOVA models with quantitative factor variables, it is most useful
 to describe and analyse the factor effects using tests for linear,
 quadratic, etc. effects.  These tests could be carried out with a
 regression model, but orthogonal polynomial contrasts provide a
 way to do the same tests, in an ANOVA framework with PROC GLM (or PROC
 MIXED).  But, you need to find the appropriate contrast coefficients.

 The ORPOLY macro finds contrast coefficients for orthogonal polynomials
 for testing a quantitative factor variable, and constructs CONTRAST
 (or ESTIMATE) statements (for use with PROC GLM or PROC MIXED) using 
 these values.

 This is most useful when either (a) the factor levels are unequally
 spaced (Trials=1 2 4 10), or (b) the sample sizes at the different levels
 are unequal.  In these cases, the 'standard' orthogonal polynomial
 coefficients cannot be used.  The ORPOLY macro uses the SAS/IML
 orpoly() function to find the correct values, and to construct the
 required CONTRAST (or ESTIMATE) statements.
 
 When the factor levels are equally spaced, *and* sample sizes are
 equal, the POLY macro provides a simpler way to generate the contrast
 coefficients, and the associated INTER macro generates contrasts for
 interactions among the polynomial contrasts.
 
=Usage:

 The ORPOLY macro is defined with keyword parameters.  The VAR= parameter
 must be specified.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%orpoly(var=A, file=temp);
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* VAR=        The name(s) of quantitative factor variable(s) for orthogonal 
              polynomial contrasts.

* DEG=        Maximum degree of orthogonal polynomial contrasts
              [Default:  number of levels of the VAR= variable]

* FILE=       Fileref for contrast statements.  The default,
              FILE=PRINT, simply prints the generated statements
				  in the listing file.  
				  
              To use the generated contrast statements directly following
              a PROC GLM step, use the FILE= parameter to assign a fileref
              and create temporary file, which may be used in a GLM step.

* TYPE=       Type of statement generated: CONTRAST, ESTIMATE, or BOTH
              [Default: TYPE=CONTRAST]                

=Example:

 Generate some data, with linear & quad A effects, linear B.
 Levels of B are unequally spaced;

   data testit;
      do a=1 to 5;
         do b=1, 5, 9, 13;
            do obs=1 to 2;
               y = a + a*a + b + 5*normal(124241);
               output;
               end;
            end;
         end;
   run;

 Generate the CONTRAST statements with ORPOLY, and %INCLUDE in the GLM step:

   %orpoly(data=testit, var=a b, file=poly);
   
   proc glm data=testit;
      class a b;
      model y=a b a*b;
      %include poly;

 The ORPOLY macro generates the following lines, which are used in the
 PROC GLM step as a result of the %include statement:

   contrast "A-lin" A -0.22361 -0.11180 -0.00000  0.11180  0.22361;
   contrast "A-quad" A  0.18898 -0.09449 -0.18898 -0.09449  0.18898;
   contrast "A-3rd" A -0.11180  0.22361  0.00000 -0.22361  0.11180;
   
   contrast "B-lin" B -0.21213 -0.07071  0.07071  0.21213;
   contrast "B-quad" B  0.15811 -0.15811 -0.15811  0.15811;

=Bugs:

 For some strange reason, PROC GLM considers the contrasts non-estimable.
 It does the proper tests when the sample sizes are equal, but not when
 they differ.

 =*/

%macro orpoly(
	data=_last_,
	var=,         /* quantitative factor variable for orthog. poly */
	deg=,         /* maximum degree of contrasts */
	file=print,   /* fileref for contrast statements */
	type=contrast /* type of statement */
	);

%let abort=0;
%if (%length(&var) = 0) %then %do;
	%put ERROR: ORPOLY: VAR= must be specified;
	%let abort=1;
	%goto done;
	%end;

%if %upcase(&data)=_LAST_ %then %let data = &syslast;
%let var=%upcase(&var);
%let type=%upcase(&type);

%if %upcase(&file) ^= PRINT %then %do;
	%tempfile(&file,,mod);
	%end;

%*-- get the current variable;
%let i=1;
%let vari = %scan(&var, &i, %str( ));

%do %while(&vari ^= );

	%*-- Determine levels and sample sizes;
	proc freq data=&data;
		tables &vari / out=_levels_ noprint;
	
	*proc print;

	proc iml;
		use _levels_;
		read all var {&vari} into &vari;
		read all var {count} into N;

		degree = nrow(&vari);
		%if %length(&deg) %then %do;
			degree = min(&deg +1, degree) ;
			%end;

		C = orpol(&vari, degree, N);
*		C = orpol(&vari, degree);     *-- works w/ GLM, but are not additive;
		C = c[,2:degree];
	
		cl = 'C1':('C'+char(degree,1));
		cl = "&vari"+'-' + ({'lin' 'quad' '3rd' '4th' '5th' '6th' '7th' '8th' '9th'})[1:degree];
		print "Contrast coefficients for variable &vari",
			&vari N[f=4.0] ' ' C[c=cl f=9.6];
	
		check = t(C) * diag(N) * C;
		print 'Check of orthogonality of contrasts (should be diagonal)',
		       check[f=8.5 r=cl c=cl];
	
		print "Contrast statements written to file &file";
		file &file;
		typ = {&type};
		if typ[1]="BOTH" then typ = {CONTRAST ESTIMATE};
		do j=1 to ncol(typ);
			type = typ[j];
			do i=1 to ncol(C);
				contrast = type + ' "' + trim(cl[i]) + '" ' + "&vari ";
				contrast = contrast + rowcat(t(char(C[,i], 10,6))) +';';
				put contrast;
				end;
		put;
		end;
		quit;

	%let i = %eval(&i+1);
	%let vari =  %scan(&var, &i, %str( ));
%end; /* %do %while() */

%done:
%if &abort %then %put ERROR: The ORPOLY macro ended abnormally.;	
%mend;

/* ---------------------------------------------------------------
 Macro to handle use of temporary files in a system- and version-
 independent way.
 --------------------------------------------------------------- */

%macro tempfile(fileref,ls,options);
   %global tempfn;
   %if %length(&ls)=0 %then %let ls=80;

   %if %sysevalf(&sysver  > 6.10) %then %do;
		filename &fileref temp &options
			%if %length(&ls)>0 %then lrecl=&ls;
			;		
		%end;
	%else %do;
		%if &sysscp = CMS
			%then %let tempfn=&fileref output a;
		%else %if &sysscp = WIN 
			%then %let tempfn=&fileref..tmp;
		%else /* assume unix */ 
			%let tempfn=/tmp/&fileref..tmp;
		filename &fileref "&tempfn" lrecl=&ls &options;
		%end;
%mend;

%macro tempdel(fileref);
   %global tempfn;
	%if length(&tempfn)=0 %then %goto done;
    *-- Avoid annoying flash with X commands;
    %if %sysevalf(&sysver  > 6.10) %then %do;
        %let rc=%sysfunc(fdelete(&fileref));
        %let rc=%sysfunc(filename(&fileref,''));
    %end;

    %else %do;
		%if &sysscp = CMS
			%then cms erase &tempfn;
		%else %if &sysscp = WIN
			%then %do;
				options noxsync noxwait; run;
				%sysexec(erase &tempfn); run;
				options   xsync   xwait; run;
			%end;
		%else /* assume flavor of UNIX */
				%sysexec(rm -f &tempfn);
    %end;
	 filename &fileref clear;
	 %let tempfn=;
%done:
%mend;

/*
*-- generate some data, with linear & quad A effects, linear B.
    Levels of B are unequally spaced, and sample sizes are unequal;
data testit;
	do a=1 to 5;
		do b=1, 5, 9, 13;
			do obs=1 to 1+int(5*uniform(0));
				y = a + a*a + b + 5*normal(0);
				output;
				end;
			end;
		end;
run;

* x 'rm orpoly.tst';
* filename poly 'orpoly.tst' mod;
options mprint;
%orpoly(data=testit, var=a b, file=poly);

proc glm data=testit;
	class a b;
	model y=a b a*b;
	%include poly / source2;
*/
