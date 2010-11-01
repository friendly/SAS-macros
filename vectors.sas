 /*--------------------------------------------------------------*
  *    Name: vectors.sas                                         *
  *   Title: Create an Annotate dataset to draw vectors in a plot*
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/vectors.html    *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 10 Feb 2006 12:42:03                                *
  * Revised: 10 Nov 2006 09:41:43                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The VECTORS macro creates an Annotate dataset to draw vectors on
 a plot, with optional text labels. 

=Usage:

 The VECTORS macro is defined with keyword parameters. The X= and Y=
 parameters are required.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%vectors(x=dim1, y=dim2);
 
==Parameters:

* DATA=       The name of the input data set.  No input data set is used
              unless DATA= is specified.

* X=          Name of abscissa variable

* Y=          Name of ordinate variable

* Z=          Name of Z variable (for G3D)

* X0=         Origin for x variable [Default: X0=0]

* Y0=         Origin for y variable [Default: Y0=0]

* Z0=         Origin for z variable [Default: Z0=0]

* SYS=        XSYS, YSYS (ZSYS) value(s) [Default: SYS=2]

* SCALE=      Scale factor(s) for vectors, either a numeric constant
              or the name of a dataset variable. [Default: SCALE=1]

* LINE=       Line style for vectors [Default: LINE=1]

* COLOR=      Line color for vectors.  This should be either a color
              name in quotes (e.g., 'BLACK') or the name of a dataset
			  variable. [Default: COLOR='BLACK']

* WIDTH=      Line width for vectors [Default: WIDTH=1]

* LABEL=      Label at end of vector, the name of a dataset variable
              or a character string in ''.

* POS=        Position of text label relative to X, Y (,Z)

* IN=         Input annotate data set

* OUT=        The name of the output data set [Default: OUT=_VECTORS_]

 =*/
%macro vectors(
	data=,           /* name of input data set  	  */
	x=,              /* name of abscissa variable	  */
	y=,              /* name of ordinate variable	  */
	z=,              /* name of Z variable (for G3D)  */
	x0=0,            /* origin for x variable		  */
	y0=0,            /* origin for y variable		  */
	z0=0,            /* origin for z variable		  */
	sys=2,           /* XSYS, YSYS (ZSYS) value(s)    */
	scale=1,         /* scale factor(s) for vectors	  */
	line=1,          /* line style for vectors	      */
	color='black',   /* line color for vectors	      */
	width=1,         /* line width for vectors	      */
	label=,          /* label at end of vector        */
	pos=,            /* position of text label        */
	in=,             /* input annotate data set       */
	out=_vectors_    /* output annotate data set      */
);

%*-- Check for required parameters;
%local abort;
%let abort=0;
%local me; %let me=VECTORS;
%if %length(&x)=0 or %length(&y)=0 %then %do;
	%put ERROR: (&me) The X= and Y= variables must be specified;
	%let abort=1;
	%goto done;
	%end;

%local xsys ysys zsys;
%let xsys = %scan(&sys &sys &sys,1,%str( ));
%let ysys = %scan(&sys &sys &sys,2,%str( ));
%let zsys = %scan(&sys &sys &sys,3,%str( ));

%let sx = %scan(&scale &scale &scale,1,%str( ));
%let sy = %scan(&scale &scale &scale,2,%str( ));
%let sz = %scan(&scale &scale &scale,3,%str( ));

data &out;
	%if %length(&data) %then %do;
	set &data;
		%end;
	xsys = "&xsys"; ysys="&ysys"; 
	%if %length(&z) %then %str(zsys="&zsys";); 
	length function color $8;

	x = &x0; y = &y0; 
	%if %length(&z) %then %str(z=&z0;); 
		function='move    '; output;

	line=&line;
	size=&width;
	color=&color;
	
	x = &x0 + &sx * &x;
	y = &y0 + &sy * &y;
	%if %length(&z) %then %str(z=&z0 + &sy * &z;); 
		function='draw    '; output;

	%if %length(&label) %then %do;
		length text $16;
		text = &label;
		function='label';
		%if %length(&pos) %then %do;
			position = &pos;
			%end; 
		output;
		%end;
	run;

%if %length(&in) %then %do;
data &out;
	set &out &in;
	%end;

%done:
	
%mend;

