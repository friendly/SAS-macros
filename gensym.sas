 /*-------------------------------------------------------------------*
  *    Name: gensym.sas                                               *
  *   Title: Macro to generate SYMBOL statement for each GROUP        *
        Doc: http://www.datavis.ca/sasmac/gensym.html              
  *-------------------------------------------------------------------*
     Author:  Michael Friendly            <friendly@yorku.ca>          
    Created: 05 Jan 1999 12:55                                         
    Revised: 05 Apr 2009 12:47:50                                      
    Version: 1.5-0                                                     
    1.1 - Added FONT= for those special symbol fonts (only one)        
          Added START= for first SYMBOL stmt.                          
    1.2 - Added WIDTH= and REPEAT=                                     
          Fixed bug with large N=                                      
    1.3 - Added LABEL= to set a pointlabel option (suggested by Eric   
          Eisenstein)                                                  
     - Fixed buglet with START=                                        
     - Added CI= for color of interpolation                            
     - Default CI= changed to empty                                    
    1.4
     - CI made to recycle
     - WIDTH made to recycle
     - Modified default SYMBOLS=
    1.5
     - H allowed to be vectorized and recycle
                                                                       
    From ``Visualizing Categorical Data'', Michael Friendly (2000)              
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The GENSYM macro generates a series of SYMBOL statements for multiple
 group plots of the form
 
	proc gplot;
		plot y * x = group;

 Separate plot symbols, colors, line styles and interpolation options
 may be generated for each group.  

=Usage:

 The GENSYM macro is called with keyword parameters.  All parameters
 have default values, but the N= parameter must usually be specified.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%gensym(n=4, colors=red blue, line=1 2 3 4);
 
 The H=, INTERP=, LINE=, SYMBOLS=, and COLORS= parameters are each lists
 of one or more values. If fewer than N (blank delimited) values are
 given,  the available values are reused cyclically as needed.

==Parameters:

* N=           The number of symbol statements constructed,
               named SYMBOL&start, ..., SYMBOL&N+&start.

* START=       Number of the first symbol statement. [Default: START=1]

* H=           List of one or more heights of the plotting symbol(s).  [Default: H=1.5]

* INTERP=      List of one or more interpolation options. 
               [Default: INTERP=NONE]

* LINE=        List of one or more numbers in the range 1..46 giving
               SAS/GRAPH line styles [Default: LINE=1]

* SYMBOLS=     A list of one or more names of SAS/GRAPH plotting symbols. 
               [Default: SYMBOLS=%STR(SQUARE TRIANGLE : $ = X _ Y)]

* COLORS=      A list of one or more names of SAS/GRAPH colors.
               [Default: COLORS=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE]

* CI=          Color for interpolated lines/curves.
                
* FONT=        Font used for the symbols (same for all SYMBOL statements)

* WIDTH=       Width for interpolated lines or curves. [Default: WIDTH=1]

* REPEAT=      Number of times each symbol statement should be re-used.
               [Default: REPEAT=1]

* LABEL=       The name of a label variable to be used as a point label in
               scatterplots.

=Example:

 To plot the four combinations of age group (old, young) and sex, with
 separate plotting symbols (circle, dot) for old vs. young, and
 separate colors (red, blue) for females vs. males, use the macro as
 follows:
 
	proc gplot;
		plot y * x = agesex;
		%gensym(n=4, symbols=circle circle dot dot, colors=red blue,
			interp=rl);
		run;

 This generates the following symbol statements:
 
 	symbol1 v=circle h=1.5 i=rl c=red;
 	symbol2 v=circle h=1.5 i=rl c=blue;
 	symbol3 v=dot    h=1.5 i=rl c=red;
 	symbol4 v=dot    h=1.5 i=rl c=blue;
 =*/

%macro gensym(
   n=1,
   start=1, 
   h=1.5,
   interp=none,
   line=1,
   symbols=%str(dot circle square triangle  $  X _ Y),
   colors=BLACK RED GREEN BLUE BROWN ORANGE PURPLE YELLOW,
   ci=,
   font=,
   width=1,
   repeat=1,
   label=
   );

*options mprint symbolgen;
    %*--  symbols, colors, line styles, and interp are recycled as needed;
  %local chr col int lin k cic ht;
  %do k=&start %to %eval(&n + &start -1) ;
     %if %length(%scan(&symbols, &k, %str( ))) = 0 
	      %then %let symbols = &symbols &symbols;
     %if %length(%scan(&colors, &k, %str( ))) = 0 
	      %then %let colors = &colors &colors;
     %if %length(%scan(&interp, &k, %str( ))) = 0 
	      %then %let interp = &interp &interp;
     %if %length(%scan(&line,   &k, %str( ))) = 0 
	      %then %let line = &line &line;
     %if %length(%scan(&width,   &k, %str( ))) = 0 
	      %then %let width = &width &width;
     %if %length(%scan(&h,   &k, %str( ))) = 0 
	      %then %let h = &h &h;
     %if %length(&ci) 	%then %do;
         %if %length(%scan(&ci,  &k, %str( ))) = 0 
	           %then %let ci = &ci &ci;
	     %end;

	 %let chr =%scan(&symbols,&k, %str( ));
     %let col =%scan(&colors, &k, %str( ));
     %let int =%scan(&interp, &k, %str( ));
     %let lin =%scan(&line,   &k, %str( ));
     %let wid =%scan(&width,  &k, %str( ));
     %let lin =%scan(&line,   &k, %str( ));
     %let ht  =%scan(&h,      &k, %str( ));

	 %if &k=99 %then %let repeat=999;
/*	 
 	 %let symstr = %nstr(
      %if %length(&font) 	  %then font=&font;
	  %if %length(&label) 	%then pointlabel=%str( (h=1 "#&label") );
	  height=&ht value=&chr color=&col i=&int l=&lin w=&wid r=&repeat
	  %if %length(&cic) 	%then %str(ci=&cic);
	  )
	  ;
*/
  
     symbol&k
      %if %length(&font) 	  %then font=&font;
	  %if %length(&label) 	%then pointlabel=%str( (h=1 "#&label") );
	  height=&ht value=&chr color=&col i=&int l=&lin w=&wid r=&repeat
	  %if %length(&cic) 	%then %str(ci=&cic);
	  ;

	%if &k=99 %then %goto done;
  %end;
*options nomprint nosymbolgen;
%done:
%mend;
