*------------------ LINEARLY OPTIMAL PROFILES --------------------*
   Name:  linpro.sas
  Title:  Linearly optimal profiles plot
  
        A scaling procedure for profiles data which represents
the profiles of  N  cases on   P variables by a set of linear
profiles.
 
Reference: Hartigan, Clustering Algorithms, Wiley, 1975, 34-38.
*------------------------------------------------------------------;
%macro linpro(data=_last_, 
	var=_num_,      /* list of variable names (no abbreviations) */
	id=,            /* case identifier variable                  */
	std=STD,        /* how to standardize (NOT IMPLEMENTED)      */
	plot=YES, 
	symbols=%str(triangle plus square circle  $ X _ Y),
	colors=black red green blue brown yellow orange purple,
	lines=1 20 41 21 7 14 33 12,     /* line styles for obs */
	out=linplot,
	fit=linfit,
	anno=vlabel,   /* name of output annotate data set */
	name=linpro    /* name of graphic catalog entry */
	);

%let std=%upcase(&std);                                                 
proc iml;
  start linpro;
   *--- Get input matrix and dimensions;
   use &data;
	%let vars=&var;
	%if %upcase("&var") ^= "_NUM_" %then %let vars={&var};
   read all var &vars into x;
   vars = str2vec("&var");          *-- Preserve case of var names;
   n = nrow(x);
   p = ncol(x);

   %if &id = %str() %then %do;
      id=compress(char(1:nrow(x),4))`;
   %end;
   %else %do;
      read all var{&id} into id;
		if type(id)='N' then id = compress(char(id));
   %end;
	&id = id;
 
   *--- Compute covariance matrix of x;
   m = x[ : ,];                   *- means;
   d = x - j( n , 1) * m;         *- deviations from means;
   c = d` * d /  n;               *- variance-covariance matrix;
   sd = sqrt( vecdiag( c))`;      *- standard deviations;
 
   *--- Prestandardize if necessary;
   ratio = sd[<>] / sd[><];
   if ratio > 3 then do;
      s = sd;
      c = diag( 1 / sd) * c * diag( 1 / sd);
      print "Analyzing the correlation matrix";
   end;
   else do;
      s = j( 1 , p);
      print "Analyzing the covariance matrix";
   end;
   print c [ colname=vars rowname=vars f=7.3];
 
   *--- Eigenvalues & vectors of C;
   call eigen ( val , vec , c );
   tr = val[+];
   pc = val / tr ;
	cum = cusum(pc);
   val = val || (100 * pc) || (100 * cum ) ;
   cl = { 'EigValue' '%_Trace' 'Cum_%'};
   print "Eigenvalues and Variance Accounted for"
         , val [ colname=cl f=8.3];
 
   *--- Scale values;
   e1 = vec[ , 1] # sign( vec[<> , 1]);
   e2 = vec[ , 2];
	ds = d;
   d = d / ( j( n , 1) * ( s # e1` ) );
   pos = e2 / e1;
 
   *--- For case i, fitted line is  Y = F1(I) + F2(I) * X   ;
   f1 = ( d * e1 ) / e1[## ,];
   f2 = ( d * e2 ) / e2[## ,];
   f = f1 || f2;
 
   *--- Output the results;
   scfac = round( ( sd` # e1 ) , .1);
   table = ( m` ) || ( sd` ) || e1 || e2 || scfac || pos;
   ct = { 'Mean' 'Std_Dev' 'Eigv1' 'Eigv2' 'Scale' 'Position'};
   print , table [ rowname=vars colname=ct f=8.2];
 
   *--- Rearrange columns;
   s = rank( pos);
   zz = table;  table [ s , ] = zz;
   zz = vars;   vars [ , s ] = zz;
   zz = pos;    pos [ s , ] = zz;
   zz = scfac;  scfac [ s , ] = zz;
   zz = d;      d [ , s ] = zz;
   print "TABLE, with variables reordered by position"
         , table [ rowname=vars colname=ct f=8.2];
   lt = { 'INTERCPT' 'SLOPE'};
   print "Case lines"    , f [ rowname=&id colname=lt f=7.2];
 
   *--- Fitted values, residuals;
   fit = f1 * j( 1 , p) + f2 * pos`;
   resid = d - fit;
   print "Fitted values" , fit [ rowname=&id colname=vars format=7.3 ];
   print "Residuals"     , resid [ rowname=&id colname=vars format=7.3 ];

	sse = resid[##];
	ssf = fit[##];
	sst = d[##];	
	vaf = ssf / (sse+ssf);
	print sse ssf sst vaf;

	sse = (ds-fit)[##];
	sst = ds[##];
	vaf = ssf / (sse+ssf);
	print sse ssf sst vaf;
		 
   *--- Construct output array -
          residuals bordered by fitted scale values;
   v1 = val[ 1 ] || {0};
   v2 = {0} || val[ 2 ];
   xout = ( resid || f ) //
           ( pos` || v1 ) //
         ( scfac` || v2 );
   rl = { 'VARIABLE','SCALE'};
   rl = shape( &id ,0 , 1) // rl;
   cl = { 'INTERCPT','SLOPE'};
   cl = shape( vars , p) // cl;
   create &fit from xout[ rowname=rl colname=cl ];
   append from xout[ rowname= rl ];
   free rl cl xout;
 
   *--- Output the array to be plotted;
   do col = 1 to p;
       rows = j(n,1,pos[col,]) ||  fit[,col] || resid[,col];
       pout = pout // rows;
       rl = rl // shape(&id,1);
       end;
   cl = { 'POSITION' 'FIT' 'RESIDUAL'};
   create &out from pout[ rowname=rl colname=cl ];
   append from pout[ rowname=rl ];
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

 run linpro;

quit;
data &out;
  set &out(rename=rl=&id);

proc print data=&fit;
   id rl;
   title3 'R E S I D U A L S, bordered by fitted coefficients';
*   title4 '(Variables sorted by position)';
   format _numeric_ 8.2;

*-- find max X position;
data _null_;
	set &fit nobs=n;
	where(rl='VARIABLE');
	last = max(of &var);
	call symput('xmax', put(last,best6.));
	call symput('nobs', trim(left(put(n-2,best6.))));
	run;
*put xmax=&xmax nobs=&nobs;

%gensym(n=&nobs, interp=join, symbols=&symbols, colors=&colors, line=&lines, h=1);
%let nc = %words(&colors);

data vlabel;
   set &fit;
   length text $12 function color $8;
   array vars &var;
	drop &var intercpt slope i rl;
   Select;
      when (RL='VARIABLE') do;
         xsys='2'; ysys='1';
         position='6';
         do over vars;
            x = vars;
            call vname(vars,text);
				text = trim(substr(text,1,1)) || lowcase(substr(text,2));
            y =  0; function='move';  output;
            y =  1; function='draw';  output;
            y = 99; function='move';  output;
            y = 97; function='draw';  output;
         	angle = 30;
            y =  2; function='label'; output;
         	angle =-30;
            y = 96; function='label'; output;
            end;
      end;
      when (RL='SCALE') ;        /* nothing */
      otherwise do;              /* observations */
			i+1;
         xsys='2'; ysys='2';
         angle = 0;
         position='C';
         function='LABEL';
         text = RL;              /* name of obs */
         x = &xmax;              /* data dependent: largest position */
         y = INTERCPT + SLOPE * x;
         x = x + .1;
*			color = scan("&colors &colors &colors &colors",i);
			color = scan("&colors",1+mod(i-1, &nc));
         output;
      end;
   end; /* select */
title3;
proc gplot data=&out &GOUT ;
   plot fit * position = &id
        / vaxis=axis1 haxis=axis2
          anno=vlabel nolegend
          name="&name" ;
/*
   symbol1 i=join v=- h=.5 l=1 c=black r=4;
   symbol2 i=join v=- h=.5 l=3 c=red   r=4;
   symbol3 i=join v=- h=.5 l=5 c=blue  r=4;
   symbol4 i=join v=- h=.5 l=9 c=green r=4;
   symbol5 i=join v=- h=.5 l=11 c=orange r=4;
*/
   axis1 label=(a=90 r=0);
   axis2 minor=none style=0 offset=(,5);
   label fit ='Fitted Value' position='Scaled Variable Position';
run;
quit;
goptions reset=symbol;
%mend;

%macro gensym(
	n=1, 
	h=1.5,
	interp=none,
	line=1,
	symbols=%str(triangle plus square circle  $ X _ Y),
	colors=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE
	 		);

    %*--  if more than 8 groups symbols and colors are recycled;
  %local chr col int lin k;
  %do k=1 %to &n ;
     %if %scan(&symbols, &k, %str( )) = %str() 
	      %then %let symbols = &symbols &symbols;
     %if %scan(&colors, &k, %str( )) = %str() 
	      %then %let colors = &colors &colors;
     %if %scan(&interp, &k, %str( )) = %str() 
	      %then %let interp = &interp &interp;
     %if %scan(&line,   &k, %str( )) = %str() 
	      %then %let line = &line &line;
     %let chr =%scan(&symbols, &k,%str( ));
     %let col =%scan(&colors, &k, %str( ));
     %let int =%scan(&interp, &k, %str( ));
     %let lin =%scan(&line,   &k, %str( ));
     symbol&k h=&h v=&chr c=&col i=&int l=&lin;
%*put symbol&k h=&h v=&chr c=&col i=&int l=&lin;

  %end;
%mend gensym;

%macro words(string);
%*--------------------------------------------------;
%* Return number of words in string. If root ^' ',  ;
%* then create global variables starting with root. ;
%*--------------------------------------------------;
   %local count word;
   %let count=1;
   %let word = %scan(&string,&count,%str( ));
   %do %while(&word^= );
       %let count = %eval(&count+1);
       %let word = %scan(&string,&count,%str( ));
   %end;
   %eval(&count-1)
%mend words;
