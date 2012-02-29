 /*-------------------------------------------------------------------*
  *    Name: twoway.sas                                               *
  *   Title: Analysis of two way tables                               *
        Doc: http://www.datavis.ca/sasmac/twoway.html             
  *                                                                   *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@YorkU.ca>         *
  * Created:  11 Dec 1989 10:51:22                                    *
  * Revised:  17 Nov 2003 11:07:11                                    *
  * Version:  1.8                                                     *
  *   - Changed to allow glm-style input (var=A B, response=Y, id=)   *
  *   - Fixed problem with scale labels on vertical axis              *
  *   - Added afactor, rfactor options, arrow heads for resids        *
  *   - Added power transformation parameter                          *
  * 1.7 Expand abbreviated variable lists (X1-X5, etc)                *
  *   - Fixed bug with setting hsize/vsize                            *
  *   - Fixed bug with readtab; error when no resids to draw          *
  *   - Use variable label with glm-style input                       *
  * 1.8 Generalize fitting to include median polish                   *
  *   - Added OUT= for output data set, FTEXT= to set font            *
  *   - Cleaned up printed output                                     *
  *   - Default RFACTOR=1 for METHOD=MEAN, =1.5 for METHOD=MEDIAN     *
  *   - Added PRINT= option                                           *
  *   - Suppressed default printing of output dataset                 *
  *                                                                   *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *-------------------------------------------------------------------*/

 /*=
=Description:
 
 The twoway macro carries out analyses of two way tables, including Tukey's           *
 1df test for row*column non-additivity, and graphical display of the 
 additive fit, together with a diagnostic plot for removable non-additivity
 via a power transformation.

 Up to Version 1.7, the macro fit row and column means only. The present
 version now includes fitting by median polish (METHOD=MEDIAN), as 
 described in Tukey (1977, Ch.10-11). [The Tukey 1df test is not reported
 in this case.]

 The input data can be pre-transformed to some power (POWER= option).
 This is sometimes useful for analyzing frequency data, where a
 loglinear model for log(count) implies an additive model under independence.
 In this case, the twoway display of the additive fit may be less
 useful than other displays (e.g., mosaic plots) because the rows and
 columns are ordered by marginal row and column frequencies, rather than
 according to the association structure of the table.
							 

=Usage:
 
 The original version of this macro required that the columns of the
 data table be stored as a set of variables in the input dataset.  In
 this arrangment, use the VAR= argument to specify this list of variables
 and the ID= variable to specify an additional variable whose values
 are labels for the rows.  In this case, the RESPONSE= option is simply
 a character label for the response variable in the two-way plot.
 
 Assume a dataset of reaction times to 4 topics in 3 experimental tasks,
 in a SAS dataset like this:
 
     TASK   TOPIC1   TOPIC2   TOPIC3   TOPIC4
	  Easy     2.43     3.12     3.68     4.04
	  Medium   3.41     3.91     4.07     5.10
	  Hard     4.21     4.65     5.87     5.69
	     
 For this arrangment, the macro would be invoked as follows:
 
   %twoway(var=topic1-topic4, id=task, response=Reaction Time);

 The present version also allows the dataset to contain all response values
 in a single variable, with two additional variables to specify the row
 and column class variables, as is done with PROC GLM in the univariate
 (non-repeated measures) format. In this case, DO NOT specify an ID=
 variable, use the VAR= argument to specify the two row and column class 
 variables, and specify the name of response variable as RESPONSE=.
 
 The same data in this format would have 12 observations, and look like:
 
 		TASK  TOPIC    RT
		Easy    1     2.43
		Easy    2     3.12
		Easy    3     3.68
		...
		Hard    4     5.69
		
 For this arrangment, the macro would be invoked as follows:
 
   %twoway(var=topic task, response=RT);

 In this arrangement, the order of the VAR= variables does not matter.
 The columns of the two-way table are determined by the variable which
 varies most rapidly in the input dataset (topic, in the example).

==Missing data:

 There can be no missing values in the response variable.
  
=*/
  
%macro twoway(
	data=_LAST_,        /* Data set to be analyzed            */
	var=,               /* list of variables: cols of table   */
	id=,                /* row identifier: char variable      */
	response=Response,  /* Label for response on 2way plot    */
	method=MEAN,        /* MEAN | MEDIAN                      */
	power=1,            /* Power transform of response        */
	plot=FIT DIAGNOSE,  /* What plots to do?                  */
	htext=1.5,          /* text height for FIT plot           */
	ftext=,             /* text font for FIT plot             */
	afactor=1,          /* angle factor for FIT plot labels   */
	rfactor=,           /* plot |resids| > rfactor*sqrt(MSPE) */
	rcolor=BLUE RED,    /* colors for + and - residuals       */
	rline=3,            /* line style for residuals           */
	arrow =0.03,        /* length of arrow heads              */
	out=compare,        /* Name of output data set            */
	print=,
	name=twoway,        /* Name for graphic catalog plots     */
	gout=GSEG);         /* Name for graphic catalog           */
 
%if %length(&var) = 0 %then %do;
    %put ERROR: You must supply a VAR= variable list for columns;
    %goto DONE;
%end;

%let method=%upcase(&method);
%if %upcase(&data)=_LAST_ %then %let data=&syslast;

%if %index(&var,-) >0 or 
 	"%upcase(&var)"="_NUM_" or 
 	"%upcase(&var)"="_NUMERIC_"  %then
 %do;
 data _null_;
    set &data (obs=1);
       %*-- convert shorthand variable list to long form;
     length _vname_ $ 8 _vlist_ $ 200;
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
	  put 'NOTE:  Variable list expanded to VAR=' _vlist_;
  run;
  %if &nv=0 %then %do;
    %put ERROR: No variables were found in the VAR= list;
    %goto DONE;
  		%end;
  %end;

%let rlabel=&response;
%if %length(&id) = 0 %then %do;
    %if %bquote(%scan(&var,2,%str( ))) = %str() %then %do;
    %put ERROR: When no ID= variable is specified, you must supply
	      two VAR= variable names, and the name of the RESPONSE=
			variable.;
    %goto DONE;
	 %end;

	%*-- Get variable label for response;
	data _null_;
		set &data (obs=1);
		length label $40;
		call label(&response, label);
		if upcase("&response")^=upcase(label)
			then call symput( 'rlabel', label);
%end;

%if %sysevalf(&sysver >6.10) %then %do;
	%*-- Find horizontal/vertical size: x.xx IN;
   %let hso=%sysfunc(getoption(hsize));
   %let vso=%sysfunc(getoption(vsize));
   
	%let hs = %scan(&hso,1,%str( ));
	%let vs = %scan(&vso,1,%str( ));

	%*-- getoption(hsize) adds a . at the end of the unit. Workaround;
	%let hu = %scan(&hso,2,%str( )); %let hu = %scan(&hu,1,%str(.));
	%let vu = %scan(&vso,2,%str( )); %let vu = %scan(&vu,1,%str(.));
	%let ftext=%sysfunc(getoption(ftext));
	%put hso=&hso  vso=&vso hs=&hs hu=&hu vs=&vs vu=&vu;
	
	%*-- Make the plot square;
	%if &hs > &vs %then  %let hs = &vs;
		%else %let vs = &hs;
	goptions hsize=&hs &hu vsize=&vs &vu;
   %end;

%*-- Set default for FTEXT= depending on &SYSDEVICE;
%if %length(&ftext)=0 %then %do;
	%if %index(%upcase(&sysdevic),PS)>0
		%then %let font=hwpsl009;
		%else %let font=swiss;
	%end;

%*-- Set default for RFACTOR= depending on METHOD;
%if %length(&rfactor)=0 %then %do;
	%if &method=MEAN
		%then %let rfactor = 1; 	%*-- x sqrt(MSPE);
		%else %let rfactor = 1.5;   %*-- x IQR;
	%end;

%let plot = %upcase(&plot);

proc iml;
   reset;

start twoway;
/*
   twoway: Module to plot the twoway display
   inputs: (roweff, coleff, all, rl, cl, fit, e, ...) 
*/

 /*-------------------------------------------------------------*
  | Calculate points for lines in two-way display of fitted     |
  | value. Each point is (COLFIT+ROWEFF, COLFIT-ROWEFF).        |
  *-------------------------------------------------------------*/
    do i=1 to r;
       clo  = coleff[><]+all;
       from = from // (clo-roweff[i] || clo+roweff[i]);
       chi  = coleff[<>]+all;
       to   = to   // (chi-roweff[i] || chi+roweff[i]);
       labl = labl || rlabel[i];
       end;
    do j=1 to c;
       rlo  = roweff[><];
       to   = to   // (coleff[j]+all-rlo || coleff[j]+all+rlo);
       rhi  = roweff[<>];
       from = from // (coleff[j]+all-rhi || coleff[j]+all+rhi);
       labl = labl || clabel[j];
       end;
 
    /*-----------------------------------*
     | Draw vectors for large residuals  |
     *-----------------------------------*/
    ahead = &arrow # ( max(fit)-min(fit) );
	 ahead = ahead # {-.25 -1, .25 -1};
    do i=1 to r;
    do j=1 to c;
*       if abs(e[i, j]) > &rfactor # sigma then do;
       if (showres[i, j])  then do;
          bot = ((cf[i,j]-re[i,j])||(cf[i,j]+re[i,j]));
		  top = bot + (0 || e[i,j]);
			*-- not drawn, but keep for scaling;
			 from = from // bot;
			 to   = to   // top;
			 ah   = sign(e[i,j])#ahead;
			 if e[i,j]>0 then do;
					frpos = frpos // bot // top // top;
					topos = topos // top // (top+ah[1,]) // (top+ah[2,]);
			 end;
			 else do;
					frneg = frneg // bot // top // top;
					toneg = toneg // top // (top+ah[1,]) // (top+ah[2,]);
			 end;
*			 from = from // top // top;
*			 to   = to // (top+ah[1,]) // (top+ah[2,]);
          end;
       end; end;
 
    /*----------------------------------*
     | Start IML graphics               |
     *---------------------------------**/
    %if %sysevalf(&sysver  < 6) %then %do;
       %let lib=%scan(&gout,1,.);
       %let cat=%scan(&gout,2,.);
       %if &cat=%str() %then %do;
           %let cat=&lib;
           %let lib=work;
           %end;
       call gstart gout={&lib &cat}
             name="&name" descript="Two-way plot for dataset &data";
    %end;
    %else %do;     /* Version 6 */
       call gstart("&gout");
       call gopen("&name",1,"Two-way plot of &response in &data");
    %end;
 
    /**--------------------------------**
     | Find scales for the two-way plot |
     **--------------------------------**/
/*
    call gport({10 10, 90 90});
    call gyaxis( {10 10}, 80, 5, 0,,'5.0') ;
    call gscale( scale2,from[,2]//to[,2], 5);
    call gscript(3, 40,"&Response",90,,3);
 
    call gscale( scale1, from[,1]//to[,1], 5);
    window = scale1[1:2] || scale2[1:2];
    call gwindow(window);
*/
	xx = from[,1]//to[,1];
	yy = from[,2]//to[,2];
	call gxyscale( xx, yy, {0 1}, { ' ' "&rlabel"}, ' ');
    /*----------------------------------*
     | Draw lines for fit and residuals |
     *----------------------------------*/
    l = nrow(from);
    call gdrawl( from[1:r+c,],   to[1:r+c,],1,"black");
	 c1 = scan("&rcolor &rcolor",1);
	 c2 = scan("&rcolor &rcolor",2);
    if nrow(frpos)>0 then call gdrawl( frpos, topos,&rline, c1);
    if nrow(frneg)>0 then call gdrawl( frneg, toneg,&rline, c2);
 
    /*----------------------------------------*
     | Plot row and column labels at margins; |
     *----------------------------------------*/
    xoffset=.04 * (to[<>,1]-to[><,1]);
    yoffset=0;
	 
    do i=1 to r+c;
	    angle = 45 * &afactor;
       if i>r then do;
          yoffset=-.04 * (to[<>,2]-to[><,2]);
			 angle = -45 * &afactor;
          end;
       call gscript(xoffset+to[i,1],yoffset+to[i,2],labl[i],angle,,&htext);
    end;
    call gshow;
    call gstop;
	finish;

 /*-------------------------------------------------------------*/
 /*-- Sets the range of data allowing for p1% of space below, --*/
 /*-- and p2% of space above. ----------------------------------*/
 /*-------------------------------------------------------------*/
start rngset(w, s, p1, p2);
   call gscale(s,s[1:2], s[3]);
   w[1] = s[2] - s[1];         /*-- rescaled range ------------*/
   s[3] = w[1]/s[3];           /*-- # of interval -------------*/
   w[2] = w[1]/(1-p1-p2);      /*-- expand range --------------*/
   w[1] = s[1] - p1*w[2];      /*-- move lower bound ----------*/
   w[2] = s[2] + p2*w[2];      /*-- move upper edge -----------*/
finish;
 
start gxyscale(x,y, axes, label, title)
		global(xticks, yticks, caxis, ctext, ftext, htext, xscale, yscale);

	if type(xticks)='U' then xticks=5;		
	if type(yticks)='U' then yticks=5;		
	if type(caxis)='U' then caxis='BLACK';		
	if type(ctext)='U' then ctext='BLACK';		
	if type(ftext)='U' then ftext="&ftext";		
	if type(htext)='U' then htext=&htext;
	
   /*-- set window --------------------------------------------*/
   w = {0 0, 100 100};  origin = {0,0};
   xscale = x[><] // x[<>] // xticks;
   run rngset( origin, xscale, .1, .05);
   w[,1] = origin;
   yscale = y[><] // y[<>] // yticks;
   run rngset( origin, yscale, .1, .05);
   w[,2] = origin;   origin = xscale[1] || yscale[1];
   call gwindow(w);
	* print 'Xscale, Yscale' xscale yscale;
	* print 'window' w;

	if all(mod(xscale,1)=0) then xfmt='6.0';
	   else if all(mod(10 # xscale,1)=0) then xfmt='7.1';
	   else xfmt='7.2';
	if all(mod(yscale,1)=0) then yfmt='6.0';
	   else if all(mod(10 # yscale,1)=0) then yfmt='7.1';
	   else yfmt='7.2';
 
   /*-- draw axes and axis labels -----------------------------*/

	if axes[1] ^=0 then do;
		call gxaxis(origin,xscale[2]-xscale[1],xscale[3],,,xfmt,,,caxis,'N');
		call gstrlen(len, label[1]);
		cx = (xscale[2]+xscale[1]-len)/2;
		call gscript(cx, 8, label[1],,,htext,,,w[,1] || {0,100});
		end;
	
	if axes[2] ^=0 then do;
		call gyaxis(origin,yscale[2]-yscale[1],yscale[3],,,yfmt,,,caxis,'N');
		call gstrlen(len, label[2], htext+.75);
		cy = (yscale[2]+yscale[1]-len)/2;
		call gscript(4, cy, label[2],90,,htext+.5,,,{0,100} || w[,2]);
		end;

   /*-- do title ----------------------------------------------*/
   if (substr(title,1,1) ^= ' ') then do;
      call gstrlen(len, title, htext, ftext);
		cx = (xscale[2]+xscale[1]-len)/2;
      call gscript(cx,(w[2,2]+yscale[2])/2,
                  title,0,0,htext,ftext,ctext);
      end;
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
 
start power(x, pow);
	if pow=1 then return(x);
	if any(x <= 0) then x = x + ceil(min(x)+.5);
	if abs(pow)<.001 then xt =  log(x);
		else xt = ((x##pow)-1) / pow;
	return (xt);
	finish;

start meanfit(y, rlabel, clabel, roweff, coleff, all, resid);
    r = nrow( y);
    c = ncol( y);
    rowmean = y[ , :];
    colmean = y[ : ,];
    all = y[ :  ];                        * grand mean ;
    roweff = rowmean - all;               * row effects;
    coleff = colmean - all;               * col effects;

    jc = j( r , 1);
    jr = j( 1 , c);
    resid = y - (rowmean * jr) - (jc * colmean) + all;

	reset noname;
    table = ( y   || round(rowmean,.001) || round(roweff,.001) ) //
            ( round(colmean,.001) || round(all,.001) || 0 ) //
            ( round(coleff,.001)  || 0       || round(all,.001) );
 
     rl    = shape(rlabel,1,r) || {'ColMean' 'ColEff'};
     cl    = shape(clabel,1,c) || {'RowMean' 'RowEff'};
     print 'Data, Means and Effects',,
	 	table [rowname=rl colname=cl f=best7.];

    table = ( resid   ||  round(roweff,.001) ) //
            ( round(coleff,.001)  || round(all,.001) );
 
     rl    = shape(rlabel,1,r) || {'ColEff'};
     cl    = shape(clabel,1,c) || {'RowEff'};
	 
     print 'Residuals and Effects',
	 	table [rowname=rl colname=cl f=best7.];
	reset name;
 	finish;

start medpolish(y, rlabel, clabel, roweff, coleff, all, resid);
	niter=5;
	verbose=0;
	r = nrow( y );
	c = ncol( y );

	*-- initialize effects to 0;
	roweff = j(r,1,0);
	coleff = j(1,c,0);
	all=0;
	
*	drow = j(1, r,0);
	dcol = j(1, c,0);

	resid = y;
	change=1;
			
	do iter = 1 to niter until(change < .005);
		drow= t(median( t(resid) ));
		dmcol= median( t(dcol) );
		rest = resid - t(shape(drow,c, r));
	
		dcol= median(rest);
		dmrow = median(roweff + drow);
		resid = rest - shape(dcol, r,c);

		*-- update effects;	
		all = all + dmrow + dmcol;
		roweff = roweff + drow - dmrow;
		coleff = coleff + dcol - dmcol;
		change = max( abs(( t(drow) || dcol )) ) / abs(all);

		if (verbose) then do;
			print "Iteration" iter change;
			print resid[format=best8.] roweff[format=best8.];
			print coleff[format=best8.] all[format=best8.];
			end;
	end;
	
	*-- Adjust all and column effects one last time;
	all = all + median(T(coleff));
	coleff = coleff - median(T(coleff));
	
	rowmed = roweff + all;
	colmed = coleff + all;

	reset noname;
    table = ( y   || round(rowmed,.001) || round(roweff,.001) ) //
            ( round(colmed,.001) || round(all,.001) || 0 ) //
            ( round(coleff,.001)  || 0       || round(all,.001) );
 
     rl    = shape(rlabel,1, r) || {'ColMed' 'ColEff'};
     cl    = shape(clabel,1, c) || {'RowMed' 'RowEff'};
     print 'Data, Medians and Effects',,
	 	table [rowname=rl colname=cl f=best7.];
	
    table = ( resid   ||  round(roweff,.001) ) //
            ( round(coleff,.001)  || round(all,.001) );
 
     rl    = shape(rlabel,1, r) || {'ColEff'};
     cl    = shape(clabel,1, c) || {'RowEff'};
     print 'Residuals and Effects',,
	 	table [rowname=rl colname=cl f=best7.];
	reset name;
	finish;

/*--------------------------------------------------------------------*
 *  Module to compute median of each column of a matrix               *
 *                                                                    *
 *  INPUT :  X   =  n x m matrix                                      *
 *  OUTPUT:  returns 1 x m matrix, with each column containing        *
 *           the median of the corresponding column in the input.     *
 *--------------------------------------------------------------------*/
 
start median (x);
  n = nrow(x);
  if (n = 1) then return (x);
  nc = ncol(x);
  q  = j(1,nc);
  do i=1 to nc;
    y = x[,i];
    rx = rank(y);
    s = y;
    s[rx,] = y;
    med = n/2;
    if (med > floor(med)) then med = med+1;
    else med = med || (med+1);
    q[,i] =  (s[med])[:];
  end;
  return (q);
finish;
 
start boxval (x);
   rx=rank(x); s=x;
   s[rx,]=x;
   n=nrow(x);
 
   m=floor( ((n+1)/2) || ((n+2)/2) );
   m=(s[m,])[+,]/2;
   q1=floor( ((n+3)/4) || ((n+6)/4) );
   q1=(s[q1,])[+,]/2;
   q2=ceil( ((3*n+1)/4) || ((3*n-2)/4) );
   q2=(s[q2,])[+,]/2;
   h=&rfactor*(q2-q1);
   u=q2+h;
   l=q1-h;
   u=(u>s)[+,]; u=s[u,];
   l=(l>s)[+,]; l=s[l+1,];
   bval=u//q2//m//q1//l;
   return(bval);
   finish;
 
/*--- Main routine */
	%if &id = %str() %then %do;
		run readtab("&data", "&response", { &var}, y, dim, lnames);
		y = shape(y,dim[2], dim[1]);
		clabel = lnames[1,];
		rlabel = lnames[2,];
*		print clabel, rlabel;
		
		%end;
   %else %do; 
	  use &data;
     read all into  y[colname=clabel rowname=&id] var { &var };
	  rlabel = t(&id);
	  %end;

	%if &power ^= 1 %then %do;
		y = power(y, &power);
		%end;

     r = nrow( y);
     c = ncol( y);
     jc = j( r , 1);
     jr = j( 1 , c);

	 /*
     rowmean = y[ , :];
     colmean = y[ : ,];
     all = y[ :  ];                        * grand mean ;
 
     roweff = rowmean - all;                 * row effects;
     coleff = colmean - all;                 * col effects;
     data = ( y       || round(rowmean,.001) || round(roweff,.001) ) //
            ( round(colmean,.001) || round(all,.001) || 0 ) //
            ( round(coleff,.001)  || 0       || 0 );
 
     rl    = rlabel || {'ColMean' 'ColEff'};
     cl    = clabel || {'RowMean' 'RowEff'};
     print , data [rowname=rl colname=cl f=7.2];
	 
     jc = j( r , 1);
     jr = j( 1 , c);
     e = y - (rowmean * jr) - (jc * colmean) + all;
     print 'Interaction Residuals ',
           e [ rowname=rl colname=cl format=8.3 ];
	*/

%if &method=MEAN %then %do;
	run meanfit(y, rlabel, clabel, roweff, coleff, all, e);
     sse = e[ ## ];
     dfe = ( r - 1 ) # ( c - 1 );
 
     ssrow = roweff[## ,];  ssa = c * ssrow;
     sscol = coleff[ ,##];  ssb = r * sscol;
     product = ( roweff * coleff ) # y;
     d = product[ + ]  / ( ssrow # sscol );
 
     ssnon = ( ( product[ + ] ) ## 2 ) / ( ssrow # sscol );
     sspe = sse - ssnon;
     ss   = ssa   // ssb // sse // ssnon // sspe ;
     df   = (r-1) //(c-1)// dfe //  1    // dfe-1;
 
     ms = ss / df ; 
	 mspe=sspe/(dfe-1);
	 sigma = sqrt(mspe);
     f  = ms / (ms[{3 3 3 5 5},]);
	 f[{3,5}]=.;
 
     source=  { "Rows","Cols","Error"," Non-Add"," Pure Err"};
     srt = "Source   ";
     sst = "   SS    ";
     dft = "   df    ";
     mst = "   MS    ";
     ft  = "   F     ";
     reset noname;
     print "Analysis of Variance Summary Table for &Response"
	  		%if &power ^=1 %then %str(" ## &power");
	  			,
           'with Tukey 1 df test for Non - Additivity ',,
           source[ colname=srt ]
           ss[ colname=sst format=9.3]  df[ colname=dft format=best8. ]
           ms[ colname=mst format=9.3]  f [ colname=ft format=9.3];
	showres = abs(e) > &rfactor # sigma;
	%end;

%else %do;

	run medpolish(y, rlabel, clabel, roweff, coleff, all, e);
	outside = (boxval(shape(e, r#c)))[{5,1}];
*	outside = (boxval(shape(e, r#c)))[{4,2}];
	showres = e < outside[1] | e > outside[2];
	%end;


	 fit = y - e;
     re = ( roweff * jr );
     cf = ( jc * coleff )  + all;
 
     compare = ( roweff * coleff ) / all;
     compare = shape( e ,0 , 1) ||
               shape( compare ,0 , 1) ||
               shape( re ,0 ,1) ||
               shape( cf ,0, 1);
     vl = { 'residual' 'compare' 'roweff' 'colfit'};
     create &out from compare[ colname=vl ];
     append from compare;
 
     /* Calculate slope of Residuals on Comparison values */
     /* for possible power transformation                 */
     xy = compare[,{2 1}];
     slope = sum(xy[,1] # xy[,2]) / xy[##,1];
	coef = slope // (1-slope);
	label= {'Slope of residual on comparison values', 'Power = 1-slope'};
	if type(d)='N' then do;
		coef = d // coef;
		label = 'Coefficient of a(i)*b(j)' // label;
		end;
	print 'Test for Removable Interaction';
    print coef[rowname=label];

/*		
     slope = d || slope || (1-slope);
     D1={"D" "Slope" "Power"};
	 print 'Test for Removable Interaction';
     print 'D = Coefficient of alpha ( i ) * beta ( j ) ' ,
           'Slope of regression of Residuals on Comparison values',
           '1 - slope = power for transformation',,
           slope[ colname=D1];
*/

    %if %index(&plot,FIT) > 0 %then %do;
	htext=&htext;
    run twoway;
    %end;
 
quit;
 
%if %sysevalf(&sysver >6.10) %then %do;
	%*-- Restore original plot size;
	* This generates an error because, e.g., vso=7.000 in.
*	goptions hsize=&hso vsize=&vso;
   %end;

data &out;
   set &out;
   fit = colfit + roweff;
   data= fit + residual;
   diff= colfit - roweff;
     label residual = 'Interaction Residual'
           compare  = 'Comparison Value'
		   fit = 'Additive Fitted Value'
		   colfit = 'Column fit'
		   roweff = 'Row effect'
		   ;
 
   /* Print values for fit and diagnostic plots */
%if %length(&print) > 0 %then %do;
proc print data=&out;
	var data roweff colfit fit residual compare;
	run;
	%end;
	
%if %index(&plot,PRINT) > 0 %then %do;
proc plot data=&out;
     plot data* diff = '+'
          fit * diff = '*'    / overlay;
proc plot;
     plot residual * compare / vpos=45;
%end;
 
%if %index(&plot,DIAGNOSE) > 0 %then %do;

%regpower(data=&out, y=residual, x=compare, out=anno);

goptions reset=symbol;
proc gplot data=&out gout=&gout;
     plot residual * compare
        / vaxis=axis1    /*haxis=axis2*/
          vminor=1 hminor=1 name="&name"
		  anno=anno
          des="Two-way diagnostic plot for &data";
     symbol1 v=circle h=1.4 c=black i=rl;
     axis1 label=(a=90 r=0);
*     axis2 label=(h=1.5);
run; quit;
%end;
 
%DONE:
goptions reset=symbol;
%mend;

/*
Calculate an annotate data set to label a plot with
the slope and power=(1-slope)
*/

%macro regpower(
	data=,
	x=,
	y=,
	ax=10,
	ay=90,
	htext=,
	out=
	);

proc reg data=&data outest=_parms_ noprint;
     model &y = &x;
	 run;

data &out;
   set _parms_(keep=&x);
   xsys='1'; ysys='1';
   length text $16 function $8;
   x = &ax;   y=&ay;
   function = 'LABEL';
   %if %length(&htext) %then %do;
	   size = &htext;
	   %end;
   power = round(1-&x, .5);
   position='6'; text = 'Slope: ' || put(&x,     f5.2);  output;
   position='9'; text = 'Power: ' || put(power,  f6.1);  output;
   run;
%mend;

