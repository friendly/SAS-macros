 /*--------------------------------------------------------------*
  *    Name: bpycolors.sas                                       *
  *   Title: Create n colors from Blue - Pink - Yellow palette   *
        Doc: http://www.datavis.ca/sasmac/bpycolors.html   
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 02 Mar 2005 10:12:39                                *
  * Revised: 02 Mar 2005 10:12:39                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The BPYCOLORS macro generates a data set containing n colors ranging
 from blue through pink to yellow, typically used to color code a
 unidimensional variable.  It has the advantage of looking nice when
 reproduced in black and white.

=Usage:

 The BPYCOLORS macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%bpycolors(n=15);
    %bpycolors(n=20, colorvar=col, result=pal);
    %put pal = &pal;
 
==Parameters:

* N=          Number of different colors included in the palette [Default: N=10]

* TRIM=       Tail fraction to be cut off. A value of 0 gives a palette
              ranging from black to white.  The default, 0.1, gives a
			  color range from blue to yellow. [Default: TRIM=0.1]

* OUT=        Name of output data set [Default: OUT=PALETTE]

* COLORVAR=   Name of the color variable in the data set [Default: COLORVAR=COLOR]

* RESULT=     Name of output macro variable containing color list
                

 =*/

%macro bpycolors(
	n=10,           /* number of different colors included in the palette  */
	trim=0.1,       /* tail fraction to be cut off                         */
	out=palette,    /* name of output data set                             */
	colorvar=color, /* name of the color variable in the data set          */
	result=         /* name of output macro variable containing color list */
	);

/*
After the function bpy.colors in the R gstat package

based on Gnuplot code from
http://www.ihe.uni-karlsruhe.de/mitarbeiter/vonhagen/palette.en.html

fr(i,imax) = (i <= 0.25*imax ? 0 : ( i <= 0.57*imax ? i/0.32/imax - 0.78125 : 1))
fg(i,imax) = ( i <= 0.42*imax ? 0 : (i <= 0.92*imax ? 2.*i/imax-0.84 : 1))
fb(i,imax)  = (i <= 0.25*imax ? 4.*i/imax : (i <= 0.42*imax ? 1 : (i <= 0.92*imax ? -2.*i/imax+1.84 : i/0.075/imax - 184./15.)))
*/

data &out;
	length &colorvar $8;
	drop i ;
	do i=1 to &n;
		ind = &trim/2 +  ((i-1)/(&n-1)) * (1-&trim);
		if ind <= 0.25
			then r = 0;
			else if ind <= 0.57
				then r = ind/0.32 -  0.78125;
				else r = 1;

		if ind <= 0.42
			then g = 0;
			else if ind <= 0.92
				then g = 2*ind -  0.84;
				else g = 1;

		if ind <= 0.25
			then b = 4*ind;
			else if ind <= 0.42
				then b = 1;
				else if ind <= 0.92
					then b = -2 * ind + 1.84;
					else b = ind/0.08 - 11.5;
		*-- Translate to hex;
		r = round(255 *r);
		g = round(255 *g);		
		b = round(255 *b);
		&colorvar = 'CX' || put(r, hex2.) || put(g, hex2.) || put(b, hex2.);
		output;
		end;

%if %length(&result) %then %do;

	%global &result;
	proc sql noprint;
		select  &colorvar into :&result separated by ' '
		from &out;
	%let &result = &&&result;
	%end;
%mend;


/* Test steam:
%bpycolors(n=15);
proc print;

%bpycolors(n=20, colorvar=col, result=pal);
%put pal = &pal;
proc print;

*/


