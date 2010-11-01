 /*--------------------------------------------------------------*
  *    Name: brewerpal.sas                                       *
  *   Title: Generate Brewer color palette(s)                    *
        Doc: http://www.datavis.ca/sasmac/brewerpal.html   
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 30 Aug 2004 10:05:08                                *
  * Revised: 22 Nov 2005 11:17:46                                *
  * Version: 1.1                                                 *
  *  - Added ORDER=REV to reverse the order of colors            *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The BREWERPAL macro generates a set of SAS/Graph colors for one of the
 color palettes developed by Cynthia Brewer.  These include color scales
 for sequential (quantitative) variables, discrete (qualitative) variables,
 and bipolar (diverging) variables, with varying numbers of levels.  The
 intended use is for constructing sets of color values with SAS/Graph, such
 as in SYMBOL and PATTERN statements.

 See: http://www.personal.psu.edu/faculty/c/a/cab38/ColorBrewerBeta2.html
 for an online application demonstrating these color scales.

 The result is returned in an output data set with a COLOR variable (RGB
 color code).  Optionally, the list of all colors may be saved to a macro
 variable as a blank-separated list.

==Method:

 The BREWERPAL macro is modeled on the RColorBrewer R package.  
 See: http://cran.us.r-project.org/src/contrib/Descriptions/RColorBrewer.html

 It requires that the data set colors.brewer has already been created (by the file 
 brewer.sas).
 You may have to modify the libname statement in the macro, or specify the LIB=
 parameter for something other than 'colors'.

 As implemented here, it simply selects observations from the &lib..brewer data set.


=Usage:

 The BREWERPAL macro is defined with keyword parameters.  Typically, the N=
 and PALETTE= arguments are specified.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%brewerpal(n=7, palette=Accent);
 
==Parameters:

* N=          Number of different colors included in the palette [Default: N=3]

* PALETTE=    Palette name.  Must be one of the values created in the palnames lists:

	Diverging:   BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
	Qualitative: Accent Dark2 Paired Pastel1 Pastel2 Set1 Set2 Set3;
	Sequential:  Blues BuGn BuPu GnBu Greens Greys Oranges OrRd
   	             PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd

              If PALETTE= is not specified, all palettes are selected.

* LIB=        Library name for the brewer data set  [Default: LIB=COLORS]

* CATEGORY=   Palette category: DIV, QUAL or SEQ.  If not specified, all palette
              categories are selected.

* OUT=        Name of the output data set. Contains the variable COLOR, as well as
              the Red, Green, Blue (decimal) components of each color [Default: OUT=PALETTE]

* ORDER=      You can specify ORDER=REV to reverse the order of the colors in the OUT=
              data set and in the RESULT= macro variable.

* RESULT=     Name of output macro variable containing color list.  If specified,
              a macro variable of that name is created with a list of all distinct
			  colors in the OUT= data set.
                
=Examples:

   *-- Typical usage, for a given palette;
   %brewerpal(n=6, palette=Blues, result=blues6);
   %put blues6 = &blues6;
   title 'Blues palette, n=6 colors';
   proc print;
   *-- Generate pattern statements with the blues6 list;
   %genpat(n=6, colors=&blues6);

   %brewerpal(n=9, palette=YlOrRd);
   title 'Yellow-Orange-Red palette, n=9 colors';
   proc print;

   *-- All n=4 palettes for diverging colors;
   %brewerpal(n=4, category=div);
   title 'Diverging palettes, n=4 colors';
   proc print;


 =*/
*-- Set this to the libname where the brewer.sas file has saved the output brewers data set;
libname colors '~/sasuser/colors';

%macro brewerpal(
	n=3,          /* number of different colors included in the palette */
	palette=,     /* Palette name */
	lib=colors,   /* Library name for the brewer data set */ 
	category=,    /* Palette category: DIV, QUAL or SEQ */
	order=,       /* How to sort the colors? */
	out=palette,  /* name of output data set */
	result=       /* name of output macro variable containing color list */
	);


	*-- List of palette categories from colors.brewer.  Should replace with a proc sql step.;
	%let catnames = div qual seq;
/*
	proc sql noprint;
		select distinct category into :catnames separated by ' '
		from &lib..brewer;
*/

	%*-- Lists of palettes from colors.brewer.  Should replace with a proc sql step.;
	%let divlist = BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral;
	%let quallist = Accent Dark2 Paired Pastel1 Pastel2 Set1 Set2 Set3;
	%let seqlist = Blues BuGn BuPu GnBu Greens Greys Oranges OrRd
   	PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd;
	%let palnames = &divlist &quallist &seqlist ;
/*
	proc sql noprint;
		select distinct palette into :palnames separated by ' '
		from &lib..brewer;
*/
	

%let abort=0;
%if %length(&palette) %then %do;
	%if %index(%upcase(&palnames), %upcase(&palette))=0 %then %do;
		%put ERROR: &palette is not a valid palette name for BREWERPAL;
		%put ERROR: Palette names are &palnames;
		%let abort=1;
		%goto done;
		%end;
	%end;
	
%if &n < 3 %then %do;
	%put WARN: Minimal value for n is 3. Returning requested palette for 3 levels;
	%let n=3;
	%end;

%if &n > 11 %then %do;
	%put WARN: Maximal value for n is 11. Returning requested palette for maximal levels;
	%let n=11;
	%end;
/* TODO:
	Check if &n <= maxc for the specified palette
*/

data &out;
	set &lib..brewer;
	%if %length(&category) %then %do;
		if upcase(category) = "%upcase(&category)";
		%end;
	%if %length(&palette) %then %do;
		if upcase(palette) = "%upcase(&palette)";
		%end;
	if nc = min(&n,maxc);
	run;
%if %length(&order) %then %do;
	%if %substr(%upcase(&order),1,3)=REV %then %do;
	proc sort data=&out;
		by descending colnum;
		run;
		%end;
	%end;
	
%if %length(&result) %then %do;

	%global &result;
	proc sql noprint;
		select  color into :&result separated by ' '
		from &out;
		quit;
	%let &result = &&&result;
	%end;

%done:
%if &abort %then %put The BREWERPAL macro ended abnormally;	
%mend;

/*
*-- Typical usage, for a given palette;
%brewerpal(n=6, palette=Blues, result=blues6);
%put blues6 = &blues6;
title 'Blues palette, n=6 colors';
proc print;

%brewerpal(n=9, palette=YlOrRd);
title 'Yellow-Orange-Red palette, n=9 colors';
proc print;

*-- All n=4 palettes for diverging colors;
%brewerpal(n=4, category=div);
title 'Diverging palettes, n=4 colors';
proc print;
*/

