 /*-------------------------------------------------------------------*         
  *    Name: biplot.sas                                               *
  *   Title: Generalized biplot of observations and variables         *
  *          Uses IML.                                                *         
        Doc: http://www.datavis.ca/sasmac/biplot.html              
  *-------------------------------------------------------------------*         
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *         
  * Created:   1 Mar 1989 13:16:36                                    *         
  * Revised:  01 Aug 2008 15:56:00                                    *         
  * Version:  2.3-0                                                   *         
  * 1.5  Added dimension labels, fixed problem with dim=3,            *
  *      Added colors option, Fixed problem with var=_NUM_            *
  * 1.6  Added power transformation (for log(freq))                   *
  *      Added point symbols, marker styles (interp=)                 *
  *      Made ID optional, can be char or numeric                     *
  *      Fixed bug introduced with ID                                 *
  * 1.7  Added code to equate axes if HAXIS= and VAXIS= are omitted   *
  *      Added code to preserve case of variable names                *
  *      Fixed positioning of variable names                          *
  * 1.8  Allow abbreviated variable lists (X1-X5, etc.)               *
  *      Allow glm-style input (var=A B, response=Y, id=)             *
  *      Added VTOH for PPLOT printer plots                           *
  *      Added FACTYPE=COV and VARDEF=N-1 (Tokuhisa SUZUKI)           *
  * 1.9  Aded POWER= for analysis of log freq & other generalizations *
  *      Added HTEXT= to control size of obs/var labels               *
  *      Removed &GOUT global variable use                            *
  * 2.0  Added PRINT=                                                 *
  *      Allow COLOR=(var) to reference a color variable              *
  *      Added COPY= to copy input variables to output data sets      *
  *      Allow htext=0 to suppress point labels                       *
  *      Handle long abbreviated variable lists (X1-X5)               *
  * 2.1  Handle missing data (by removing them)                       *
  *      Allow ID variable to be numeric (again)                      *
  *      Changed default to STD=STD                                   *
  *      Fixed problems when dim>9                                    *
  * 2.2  Added INANNO= to allow additional annotations                *
  * 2.3  Added ADJVAR= to allow adjustment of variable labels         *
  *      Added ADJOBS= to allow adjustment of observation labels      *
  *                                                                   *         
  *      From ``SAS System for Statistical Graphics, First Edition''  *         
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *         
  *      ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/        

 /*=
=Description:
 
 The BIPLOT macro produces generalized biplot displays for multivariate
 data, and for two-way and multi-way tables of either quantitative
 or frequency data.  It also produces labeled plots of the row and
 column points in 2 dimensions, with a variety of graphic options,
 and the facility to equate the axes automatically.


==Input data:

 The macro takes input in one of two forms:

 (a) A data set in table form, where the columns are separate
 variables and the rows are separate observations (identified by
 a row ID variable).    In this arrangment, use the VAR= argument
 to specify this list of variables and the ID= variable to specify
 an additional variable whose values are labels for the rows.
 
 Assume a dataset of reaction times to 4 topics in 3 experimental tasks,
 in a SAS dataset like this:

     TASK   TOPIC1   TOPIC2   TOPIC3   TOPIC4
	  Easy     2.43     3.12     3.68     4.04
	  Medium   3.41     3.91     4.07     5.10
	  Hard     4.21     4.65     5.87     5.69
	     
 For this arrangment, the macro would be invoked as follows:
   %biplot(var=topic1-topic4, id=task);

 (b) A contingency table in frequency form (e.g., the output from
 PROC FREQ), or multi-way data in the univariate format used as
 input to PROC GLM.  In this case, there will be two or more factor
 (class) variables, and one response variable, with one observation
 per cell.  For this form, you must use the VAR= argument to
 specify the two (or more) factor (class) variables, and specify
 the name of response variable as the RESPONSE= parameter. Do not specify
 an ID= variable for this form.

 For contingency table data, the response will be the cell frequency, and
 you will usually use the POWER=0 parameter to perform an analysis of
 the log frequency.
  
 The same data in this format would have 12 observations, and look like:

 		TASK  TOPIC    RT
		Easy    1     2.43
		Easy    2     3.12
		Easy    3     3.68
		...
		Hard    4     5.69
		
 For this arrangment, the macro would be invoked as follows:
   %biplot(var=topic task, response=RT);

 In this arrangement, the order of the VAR= variables does not matter.
 The columns of the two-way table are determined by the variable which
 varies most rapidly in the input dataset (topic, in the example).

=Usage:

 The BIPLOT macro is defined with keyword parameters.  The VAR=
 parameter must be specified, together with either one ID= variable
 or one RESPONSE= variable.

 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%biplot();
 
 The plot may be re-drawn or customized using the output OUT=
 data set of coordinates and the ANNO= Annotate data set.

 The graphical representation of biplots requires that the axes in the
 plot are equated, so that equal distances on the ordinate and abscissa
 represent equal data units (to perserve distances and angles in the plot).
 A '+', whose vertical and horizontal lengths should be equal,
 is drawn at the origin to indicate whether this has been achieved.

 If you do not specifiy the HAXIS= and YAXIS= parameters, the EQUATE
 macro is called to generate the AXIS statements to equate the
 axes.  In this case the INC=, XEXTRA=, and YEXTRA=, parameters
 may be used to control the details of the generated AXIS statements.

 By default, the macro produces and plots a two-dimensional solution.

==Parameters:

* DATA=		 Specifies the name of the input data set to be analyzed.
             [Default: DATA=_LAST_]

* VAR=		 Specifies the names of the column variables 
             when the data are in table form, or the names of the
			 factor variables when the data are in frequency form
			 or GLM form. [Default: VAR=_NUM_]

* ID=        Observation ID variable when the data are in table form.

* RESPONSE=  Name of the response variable (for GLM form)    

* DIM=       Specifies the number of dimensions of the biplot solution.
             Only two dimensions are plotted by the PPLOT and GPLOT options,
				 however. [Default: DIM=2]

* FACTYPE=   Biplot factor type: GH, SYM, JK or COV [Default: FACTYPE=SYM]

* VARDEF=    Variance def for FACTYPE=COV: DF | N [Default: VARDEF=DF]

* SCALE=     Scale factor for variable vectors. If SCALE=AUTO or SCALE=0
             the program calculates a scale factor based on the ratio of
			 the maximum distance from the origin of the observation points
			 relative to the variable vectors. [Default: SCALE=AUTO]

* POWER=     Power transform of response [Default: POWER=1]

* OUT=		 Specifies the name of the output data set of coordinates.
             [Default: OUT=BIPLOT]

* ANNO=		 Specifies the name of the annotate data set of labels
             produced by the macro.  [Default: ANNO=BIANNO]

* INANNO=  Specifies the name of an input annotate data set, which
           is concatenated to the ANNO= data set and plotted.

* STD=       How to standardize columns: NONE|MEAN|STD. STD=NONE removes only
             the grand mean before factoring the data matrix (rarely used).
			 STD=MEAN removes column means [Default: STD=STD]

* COLORS=    Colors for OBS and VARS. You can assign distinct colors
             to different observations by specifying the name of a
			 color variable in parentheses, e.g., COLORS=(col) BLACK.
			 You must then also use COPY=col to copy the color variable
			 to the output data sets.
			 [Default: COLORS=BLUE RED]

* SYMBOLS=   Symbols for OBS and VARS [Default: SYMBOLS=NONE NONE]

* INTERP=    Markers/interpolation for OBS and VARS. [Default: INTERP=NONE VEC]

* LINES=     Lines for OBS and VARS interpolation [Default: LINES=33 20]

* PPLOT=     Produce a printer plot? [Default: PPLOT=NO]

* VTOH=      The vertical to horizontal aspect ratio (height of one
             character divided by the width of one character) of the
				 printer device, used to equate axes for a printer plot,
				 when PPLOT=YES.  [Default: VTOH=2]

* GPLOT=     Produce a graphics plot? [Default: GPLOT=YES]

* PLOTREQ=   The dimensions to be plotted [Default: PLOTREQ=DIM2*DIM1]

* HAXIS=     AXIS statement for horizontal axis.  If both HAXIS= and
             VAXIS= are omitted, the program calls the EQUATE macro to
				 define suitable axis statements.  This creates the axis
				 statements AXIS98 and AXIS99, whether or not a graph
				 is produced.

* VAXIS=     The name of an AXIS statement for the vertical axis.

* INC=       The length of X and Y axis tick increments, in data units
             (for the EQUATE macro).  Ignored if HAXIS= and VAXIS= are
             specified. [Default: INC=0.5 0.5]

* XEXTRA=    # of extra X axis tick marks at the left and right.  Use to
             allow extra space for labels. [Default: XEXTRA=0 0]

* YEXTRA=    # of extra Y axis tick marks at the bottom and top.
             [Default: YEXTRA=0 0]

* M0=        Length of origin marker, in data units. [Default: M0=0.5]

* DIMLAB=    Prefix for dimension labels [Default: DIMLAB=Dimension]

* HTEXT=     Height of labels for OBS and VAR

* ADJVAR=    data step stmt(s) to adjust var labels

* NAME=      Name of the graphics catalog entry [Default: NAME=BIPLOT]        

==Dependencies:

 Requires: IML equate.sas
 
 =*/
                                                                                 
%macro biplot(                                                                  
	data=_LAST_,       /* Data set for biplot                       */         
	var=_NUM_,         /* Variables for biplot                      */         
	id=,               /* Observation ID variable (obs x var input) */
	response=,         /* Name of response variable (glm input)     */    
	dim=2,             /* Number of biplot dimensions               */         
   factype=SYM,       /* Biplot factor type: GH, SYM, JK or COV    */
	vardef=DF,         /* Variance def for factype=COV: DF | N      */
	scale=AUTO,           /* Scale factor for variable vectors         */
	power=1,           /* Power transform of response               */
	out=BIPLOT,        /* Output dataset: biplot coordinates        */         
	anno=BIANNO,       /* Output dataset: annotate labels           */
	copy=,             /* Variables to copy to the output dataset   */
	inanno=,         
	std=STD,           /* How to standardize columns: NONE|MEAN|STD */
	colors=BLUE RED,   /* Colors for OBS and VARS                   */
	symbols=none none, /* Symbols for OBS and VARS                  */
	interp=none vec,   /* Markers/interpolation for OBS and VARS    */          
	lines=33 20,       /* Lines for OBS and VARS interpolation      */
	pplot=NO,          /* Produce printer plot?                     */
	vtoh=2,            /* PPLOT cell aspect ratio                   */
	gplot=YES,         /* Produce hi-res plot?                      */
	plotreq=,          /* dimensions to be plotted                  */
	haxis=,            /* AXIS statement for horizontal axis        */
	vaxis=,            /* and for vertical axis- use to equate axes */
	inc=0.5 0.5,       /* x, y axis tick increments                 */
	xextra=0 0,        /* # of extra x axis tick marks              */
	yextra=0 0,        /* # of extra y axis tick marks              */
	m0=0.5,            /* Length of origin marker                   */
	dimlab=,           /* Dimension label                           */
	print=,
	htext=1.5 1.5,     /* Height of labels for OBS and VAR          */
	adjvar=,           /* data step stmt(s) to adjust var labels    */
	adjobs=,           /* data step stmt(s) to adjust obs labels    */
	name=biplot        /* Name for graphics catalog entry           */
	);        
                                                                                
%let abort=0;
%let std=%upcase(&std);
%let pplot=%upcase(&pplot);
%let gplot=%upcase(&gplot);

%if %upcase(&scale)=AUTO %then %let scale=0;
                                              
%if %length(&vardef) = 0 %then %let vardef=N;
%if %upcase(&vardef) = DF %then %let vardef = %str( N - 1 ) ;

%let factype=%upcase(&factype);                                                 
      %if &factype=GH  %then %let p=0;                                          
%else %if &factype=SYM %then %let p=.5;                                         
%else %if &factype=JK  %then %let p=1;                                          
%else %if &factype=COV %then %let p=0 ;
%else %do;                                                                      
   %put BIPLOT: FACTYPE must be GH, SYM, JK, or COV "&factype" is not valid.;
	%let abort=1;       
   %goto done;                                                                  
   %end;                                                                        
%if &data=_LAST_ %then %let data=&syslast;

%* --- Transform variable lists (X1-X10) into expanded form for IML ---;
%if %index(&var,-) >0 or 
 	"%upcase(&var)"="_NUM_" or 
 	"%upcase(&var)"="_NUMERIC_"  %then
 %do;
 %let ovar = &var;
 data _null_;
    set &data (obs=1);
       %*-- convert shorthand variable list to long form;
     length _vname_ $ 8 
		%if &sysver>=7	%then  _vlist_ $ 1000;
                        %else  _vlist_ $ 200;
						;
     array _xx_ &var;
     _vname_ = ' ';
	  i=0;
     do over _xx_;
        call vname(_xx_,_vname_);
        _vlist_ = trim(_vlist_)|| ' ' || trim(_vname_);
		  i+1;
     end;
     call symput( 'VAR', trim(_vlist_) );
     call symput( 'NV', trim(put(i,2.0)) );
	  put "NOTE:  Variable list (&ovar) expanded to VAR=" _vlist_;
  run;
  %if &nv=0 %then %do;
    %put ERROR: No variables were found in the VAR=&ovar list;
    %goto DONE;
  		%end;
  %end;

%if %length(&id) = 0 %then %do;
    %if %bquote(%scan(&var,2,%str( ))) = %str() or
	     %length(&response)=0 %then %do;
    %put ERROR: When no ID= variable is specified, you must supply
	      two+ VAR= variable names, and the name of the RESPONSE=
			variable.;
    %goto DONE;
	 %end;
%end;

                                                                                
%*-- Set defaults which depend on other options;
%if %length(&plotreq)=0 %then %do;
	%if &dim=2 %then %let plotreq =  dim2 * dim1;
	%if &dim=3 %then %let plotreq =  dim2 * dim1 = dim3;
	%else %let plotreq =  dim2 * dim1;
	%end;

%*-- Find dimensions to be ploted;
%let ya = %scan(&plotreq,1,%str(* ));
%let xa = %scan(&plotreq,2,%str(* ));
%let za = %scan(&plotreq,3,%str(=* ));

%if %length(&dimlab)=0 %then %do;
	%if %length(&za) %then %let dimlab = Dim;
	%else %let dimlab = Dimension;
/*
	%if &dim=2 %then %let dimlab = Dimension;
	%if &dim>2 %then %let dimlab = Dim;
*/
	%end;

proc iml;                                                                       
start biplot(y,id,vars,out, g, scale);
   N = nrow(Y);                                                                 
   P = ncol(Y);                                                                 
   %if &std = NONE                                                              
       %then Y = Y - Y[:] %str(;);             /* remove grand mean */          
       %else Y = Y - J(N,1,1)*Y[:,] %str(;);   /* remove column means */        
   %if &std = STD %then %do;                                                    
      S = sqrt(Y[##,] / ( &vardef ) );
      Y = Y * diag (1 / S );                                                    
   %end;                                                                        
   print "Standardization Type: &std  (VARDEF = &vardef) " ;
                                                                                
   *-- Singular value decomposition:                                            
        Y is expressed as U diag(Q) V prime                                     
        Q contains singular values, in descending order;                        
   call svd(u,q,v,y);                                                           
                                                                                
   reset fw=8 noname;                                                           
   percent = 100*q##2 / q[##];                                                  
   cum = cusum(percent);                                                           
   c1={'Singular Values'};                                                      
   c2={'Percent'};                                                              
   c3={'Cum % '};                                                               
	ls = 40;
	do i=1 to nrow(q);
		row = cshape('*', 1, 1, round(ls#percent[i]/max(percent)));
		hist = hist // cshape(row,1,1,ls,' ');
		end;
   print "Singular values and variance accounted for",,                         
         q       [colname=c1 format=9.4 ]                                       
         percent [colname=c2 format=8.2 ]                                       
         cum     [colname=c3 format=8.2 ]
		 hist    [colname={'Histogram of %'}];                                     
                                                                                
   d = &dim ;
	*-- Assign macro variables for dimension labels;
	lab = '%let p' + left(char(t(1:d),2)) + '=' + left(char(percent[t(1:d)],8,2)) + ';';
	call execute(lab);

   *-- Extract first  d  columns of U & V, and first  d  elements of Q;         
   U = U[,1:d];                                                                 
   V = V[,1:d];                                                                 
   Q = Q[1:d];                                                                  
                                                                                
   *-- Scale the vectors by QL, QR;                                             
   * Scale factor 'scale' allows expanding or contracting the variable          
     vectors to plot in the same space as the observations;                     
   QL= diag(Q ## g );                                                       
   QR= diag(Q ## (1-g));                                                    
   A = U * QL;                                                                  
   B = V * QR;
	*-- scale=AUTO ;
	ratio = max(sqrt(A[,##])) /  max(sqrt(B[,##]));
	if scale=0 then scale=ratio;                                                         
	print 'OBS / VARS ratio:' ratio 'Scale:' scale;
   B = B # scale;                                                          

  %if %upcase( &factype ) = COV %then
    %do ;
      A = sqrt( &vardef       ) # A ;
      B = ( 1 / sqrt(&vardef) ) # B ;
    %end ;
   OUT=A // B;                                                                  
                                                                                
   *-- Create observation labels;                                               
   id = shape(id,n,1) // shape(vars,p,1);                                                            
   type = repeat({"OBS "},n,1) // repeat({"VAR "},p,1);                         
   id  = concat(type, id);                                                      
                                                                                
	if upcase("&factype")='COV'
		then factype='COV';
   	else factype = {"GH" "Symmetric" "JK"}[1 + 2#g];                              
   print "Biplot Factor Type", factype;                                         
                                                                                
   cvar = concat(shape({"DIM"},1,d), trim(left(char(1:d,2.))));                             

   %if %index(&print, COORD) %then %do;
   print "Biplot coordinates",                                                  
         out[rowname=id colname=cvar f=9.4];
   %end;
   create &out  from out[rowname=id colname=cvar];                              
   append from out[rowname=id];                                                 
finish;                                                                         

start power(x, pow);
		if pow=1 then return(x);
		if any(x <= 0) then x = x + ceil(min(x)+.5);
		if abs(pow)<.001 then xt =  log(x);
			else xt = ((x##pow)-1) / pow;
	return (xt);
	finish;

start str2vec(string);
	*-- String to character vector;
   free out;
   i=1;
   sub = scan(string,i,' ');
   do while(sub ^=' ');
      out = out || sub;
      i = i+1;
      sub = scan(string,i,' ');
   end;
	return(out);
	finish;

/* --------------------------------------------------------------------
 Routine to read frequency and index/label variables from a SAS dataset
 and construct the appropriate levels, and lnames variables

 Input:  dataset - name of SAS dataset (e.g., 'mydata' or 'lib.nydata')
         variable - name of variable containing the response
		   vnames - character vector of names of index variables
 Output: dim (numeric levels vector)
         lnames (K x max(dim)) 
  --------------------------------------------------------------------*/
 
start readtab(dataset, variable, vnames, table, dim, lnames);
	if type(vnames)^='C'
		then do;
			print 'VNAMES argument must be a character vector';
			show vnames;
			return;
		end;		
   if nrow(vnames)=1 then vnames=vnames`;

	call execute('use ', dataset, ';');
	read all var variable into table;
	run readlab(dim, lnames, vnames);
	call execute('close ', dataset, ';');
	reset noname;
	print 'Variable' variable 'read from dataset' dataset,
		'Factors ordered:' vnames lnames;
	reset name;
	finish;

/* Read variable index labels from an open dataset, construct a dim 
   vector and lnames matrix so that variables are ordered correctly
   for mosaics and ipf (first varying most rapidly).

	The data set is assumed to be sorted by all index variables.  If
	the observations were sorted by A B C, the output will place
	C first, then B, then A.
   Input:    vnames (character K-vector)
 */

start readlab(  dim, lnames, vnames);
	free span lnames dim;
	nv = nrow(vnames);

	spc = '        ';
	do i=1 to nv;
		vi = vnames[i,];
		read all var vi into cli;
		if type(cli) = 'N'
			then do;
				tmp = trim(left(char(cli,8)));
				tmp = substr(tmp,1,max(length(tmp)));
				cli = tmp;
				end; 
		cli = trim(cli); 
		span = span || loc(0=(cli[1,] = cli))[1];
		d=design( cli );
		dim = dim || ncol(d);
		free row1;
		*-- find position of each first distinct value;
		do j=1 to ncol(d);
			row1 = row1 || loc(d[,j]=1)[1];
			end;
		*-- sort elements in row1 so that var labels are in data order;
		order = rank(row1);
		tmp = row1;
		row1[,order]=tmp;	

		li = t(cli[row1]);
		if i=1 then lnames = li;
			else do;
				if ncol(lnames) < ncol(row1) 
					then lnames=lnames || repeat(spc, i-1, ncol(row1)-ncol(lnames));
				if ncol(lnames) > ncol(row1)
					then li = li || repeat(spc, 1, ncol(lnames)-ncol(li));
				lnames = lnames // li;
			end;
		end;

	*-- sort index variables by span so that last varies most slowly;
	order = rank(span);
	tmp = span;    span[,order] = tmp;
	tmp = dim;     dim[,order] = tmp;
	tmp = lnames;  lnames[order,] = tmp;
	tmp = vnames;  vnames[order,] = tmp;
	finish;
 
start cellname(dim,lnames);
   cn = '';
	d = dim;
   if nrow(dim)=1 then d = dim`;
   do f=nrow(d) to 1 by -1;
      r  = nrow(cn);
      ol = repeat( cn, 1, d[f]);
      ol = shape(ol, r#d[f], 1);
      nl = repeat( (lnames[f,(1:d[f])])`, r,1);
		if f=nrow(d)
			then cn = trim(nl);
      	else cn = trim(nl)+':'+trim(ol);
      end;
   return(cn);
   finish;
 
start nomiss(matrix, obsnames, keep);
   *-- Remove rows with any missing data from matrix and obsnames;
   *   (pass ' ' for obsnames if no row labels are present);
   miss = loc(matrix <=.Z);
   if type(miss)='U'
      then do;
		   keep=1:nrow(matrix); 
			return;           /* no missing data */
			end;
      else do;
         nr = nrow(matrix);
         nc = ncol(matrix);
         rows= 1+floor((miss-1)/nc);
         rows= unique(rows);
         keep=remove(1:nrow(matrix),rows);
         deleted = nr - ncol(keep);
         matrix = matrix[keep,];
         reset noname;
         print 'Warning:' deleted 'row(s) with missing data have been removed.';
         reset name;
         if obsnames ^={' '}
            then do;
               obs = obsnames[rows];
               obs = shape(obs,1,nrow(obs)#ncol(obs));
               if type(obs)='N' then obs=char(obs,3,0);
               obsnames=obsnames[keep];
            *  print rows[c=obs], obs;
            end;
         end;
finish;


	/*--- Main routine */

	%if %length(&id) = 0 %then %do;
		run readtab("&data", "&response", {&var}, y, dim, lnames);
		cvar = 1;
		rvar = (cvar+1):ncol(dim);
		y = shape(y, (dim[rvar])[#], dim[1:cvar]);
		vars = lnames[1,1:dim[1]];
		if ncol(dim)=2 
			then id = t(lnames[2,1:dim[2]]);
			else id = cellname( dim[rvar], lnames[rvar,]);
		%end;
   %else %do; 
	  use &data;
     read all var{&var} into y;
     read all var{&id} into id;
     vars = str2vec("&var");          *-- Preserve case of var names;
	  %end;
	if type(id)='N' then id = trim(left(char(id,8,0)));
	run nomiss(y, id, kept);

	%if &power ^= 1 %then %do;
		y = power(y, &power);
		%end;

   scale = &scale;                                                              
   run biplot(y, id, vars, out, &p, scale );                                  
   quit;                                                                        
                                                                                
 /*----------------------------------*                                          
  |  Split ID into _TYPE_ and _NAME_ |                                          
  *----------------------------------*/                                         

data &out;                                                                      
   set &out;                                                                    
   drop id;                                                                     
   length _type_ $3 _name_ $16;                                                 
   _type_ = substr(id,1,3);                                                     
   _name_ = substr(id,5);
   %if %length(&copy)>0 and %length(&id)>0 %then %do;
    if _type_='OBS' then set &data(keep=&copy);
    %end;                                                       
   label                                                                        
   %do i=1 %to &dim;                                                            
       dim&i = "&dimlab &i (&&p&i%str(%%))"                                                   
       %end;                                                                    
   ;                                                                            

proc summary data=&out;
	var &xa &ya &za;
	output out=_range_ range=;

 /*--------------------------------------------------*                          
  | Annotate observation labels and variable vectors |                          
  *--------------------------------------------------*/                         
	%local c1 c2 v1 v2 i1 i2 h1 h2;
	%*-- Assign colors and symbols;
	%let c1= %scan(&colors,1,%str( ));                                                      
	%let c2= %scan(&colors,2,%str( ));
	%if &c2=%str() %then %let c2=&c1;                                                   

	%let v1= %upcase(%scan(&symbols,1));                                                      
	%let v2= %upcase(%scan(&symbols,2));
	%if &v2=%str() %then %let v2=&v1;                                                   

	%let i1= %upcase(%scan(&interp,1));                                                      
	%let i2= %upcase(%scan(&interp,2));
	%if &i2=%str() %then %let i2=&i1;                                                   

	%let l1= %upcase(%scan(&lines,1));                                                      
	%let l2= %upcase(%scan(&lines,2));
	%if &l2=%str() %then %let l2=&l1;                                                   

	%if %length(&htext) %then %do;
		%let h1= %upcase(%scan(&htext,1,%str( )));                                                      
		%let h2= %upcase(%scan(&htext,2,%str( )));
		%if &h2=%str() %then %let h2=&h1;                                                   
		%end;

	%*-- Plot increments;
	%let n1= %scan(&inc,1,%str( ));                                                      
	%let n2= %scan(&inc,2,%str( ));
	%if &n2=%str() %then %let n2=&n1;

%if &pplot = YES %then %do;
	%put WARNING: Printer plots may not equate axes (using VTOH=&vtoh);                                                     
   %if &sysver < 6.08
       %then %do;
          %put WARNING: BIPLOT cannot label points adequately using
               PROC PLOT in SAS &sysver - use SAS 6.08 or later;
          %let symbol = %str( = _name_ );
          %let place =;
			 %let axes=;
       %end;
       %else %do;
           %let symbol = $ _name_ = '*';
           %let place = placement=((h=2 -2 : s=right left)
                                   (v=1 -1 * h=0 -1 to -3 by alt)) ;
			  %let axes = haxis = by &n1 vaxis = by &n2 ;
       %end;
 
	proc plot data=&out vtoh=&vtoh;
		plot &ya * &xa &symbol
			/ &axes &place box;
%end;


%let _lp=%bquote(%str(%());
%let _rp=%bquote(%str(%)));
*options symbolgen;
data &anno;                                                                     
   set &out; 
   drop dim1-dim&dim;                                                                   
   length function color $8 text $16;                                                 
   xsys='2'; ysys='2'; %if %length(&za) %then %str(zsys='2';);                                               
   text = _name_;                                                               
                                                                                
   if _type_ = 'OBS' then do;         /* Label observations (row points) */
    	%if %index(&c1,&_lp)^=0 %then %do;  %* );
			%let c1=%substr(&c1,2,%eval(%length(&c1)-2) );
			color = &c1;
		%end;
		%else %do;             
		    color="&c1";
		%end;                                                            
%*		if "&i1" = 'NIL' then return;                                                            
		if "&i1" = 'VEC' then link vec;                                                            
      x = &xa; y = &ya;                                                       
      %if %length(&za) %then %str(z = &za;);
		%if &v1=NONE %then                                     
      	%str(position='5';);
		%else %do;
      if &xa >=0                                                               
         then position='>';             /* rt justify         */                
         else position='<';             /* lt justify         */                
      if &ya >=0                                                               
         then position='2';             /* up justify         */                
         else position='E';             /* down justify       */
			%end;
	  size = &h1;                
      &adjobs
      if size>0 then  do; function='LABEL   '; output;  end;                                            
      end;                                                                      
                                                                                
   if _type_ = 'VAR' then do;           /* Label variables (col points) */                
      color="&c2";
		if "&i2" = 'VEC' then link vec;                                                            
      x = &xa; y = &ya;                
      %if %length(&za) %then %str(z = &za;);

      if &ya >=0                                                               
         then position='2';             /* up justify         */                
         else position='E';             /* down justify       */

		size = &h2;                
      &adjvar
      function='LABEL   '; output;      /* variable name      */                
      end;                                                                      
		return;

vec:                    /* Draw line from the origin to point */
      x = 0; y = 0;                
      %if %length(&za) %then %str(z = 0;);                                          
      function='MOVE'    ; output;                                              
      x = &xa; y = &ya;                
      %if %length(&za) %then %str(z = &za;);                                       
      function='DRAW'    ; output;                                              
		return;

%if %length(&inanno) %then %do;
	data &anno;
		set &anno &inanno;
	%end;

*options nosymbolgen;
 /*--------------------------------------------------*
  | Mark the origin                                  |
  *--------------------------------------------------*/
%if &m0 > 0 %then %do;
data _zero_;
	xsys='2';  ysys='2';
	%if &dim=3 %then %do; zsys='2'; z=0; %end;             
	x = -&m0;  y=0;   function='move'; output;
	x =  &m0;         function='draw'; output;
	x = 0;  y = -&m0; function='move'; output;
	        y =  &m0; function='draw'; output;

data &anno;                                                                     
   set &anno _zero_;                                                                    
%end;

%if %length(&vaxis)=0 and %length(&haxis)=0 %then %do;
	%let x1= %scan(&xextra,1);                                                      
	%let x2= %scan(&xextra,2);
	%if &x2=%str() %then %let x2=&x1;
	%let y1= %scan(&yextra,1);                                                      
	%let y2= %scan(&yextra,2);
	%if &y2=%str() %then %let y2=&y1;

	%equate(data=&out, x=&xa, y=&ya, plot=no,
		vaxis=axis98, haxis=axis99, xinc=&n1, yinc=&n2,
		xmextra=&x1, xpextra=&x2, ymextra=&y1, ypextra=&y2);
	%let vaxis=axis98;
	%let haxis=axis99;
	*options nonotes;
	%end;
%else %do;
%if %length(&vaxis)=0 %then %do;
	%let vaxis=axis98;
	%put WARNING:  You should use an AXISn statement and specify VAXIS=AXISn to equate axis units and length;
   axis98 label=(a=90);
	%end;
%if %length(&haxis)=0 %then %do;
	%let haxis=axis99;
	%put WARNING:  You should use an AXISm statement and specify HAXIS=AXISm to equate axis units and length;
   axis99 offset=(2);
	%end;
%end;

	%if &i1=VEC %then %let i1=NONE;
	%if &i2=VEC %then %let i2=NONE;
symbol1 v=&v1 c=&c1 i=&i1 l=&l1;
symbol2 v=&v2 c=&c2 i=&i2 l=&l2;

%if &gplot = YES %then %do;
	%let legend=nolegend;

	proc gplot data=&out ;
		plot &ya * &xa = _type_/ 
			anno=&anno frame &legend
			%if &m0=0 %then %do;
			href=0 vref=0 lvref=3 lhref=3
			%end;
			vaxis=&vaxis haxis=&haxis
			vminor=1 hminor=1
			name="&name" des="Biplot of &data";
	run; quit;
*	goptions reset=symbol;
	%end;  /* %if &gplot=YES */

 /*------------------------------------*
  | Clean up datasets no longer needed |
  *------------------------------------*/
proc datasets nofs nolist library=work memtype=(data);
    delete _range_ _zero_;
     run; quit;

%done:                                                                          
%if &abort %then %put ERROR: The BIPLOT macro ended abnormally.;
%mend BIPLOT;                                                                   
