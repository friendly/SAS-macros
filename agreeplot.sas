 /*--------------------------------------------------------------*
  *    Name: agreeplot.sas                                       *
  *   Title: Macro program for agreement plots                   *
        Doc: http://www.datavis.ca/sasmac/agreeplot.html   
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 15 Mar 2007 10:12:15                                *
  * Revised: 24 Jan 2012 14:44:37                                *
  * Version: 1.1-0                                               *
    - Added FILLS= to allow other fill patterns
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The AGREEPLOT macro produces a graphic display of agreement in a 
 square n x n contingency table, typically arising when observations
 are classified on the same scale by two raters or instruments.
 The display shows both the observed agreement and the expected
 agreement by superposed shaded and white rectangles, respectively.

 This macro provides a more convenient macro interface to
 the SAS/IML modules for agreement plots, so that you can input
 a SAS data set (in frequency form), rather than constructing the
 arrays used by the IML modules. It also provides the
 means to produce multiple agreement plots for two-way tables stratified
 by other BY= variables.

 In SAS 9.3, these plots are now provided directly by PROC FREQ under
 ODS graphics, when the AGREE option is specified on the TABLES statement.
 Other PLOT()=AGREEPLOT sub-options allow some control of the details of 
 these plots.

=Usage:

 The AGREEPLOT macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%agreeplot(data=sexfun, var=Husband Wife);
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* VAR=        Names of factor variables, including BY= variables.  The first
              two variables are used as the rows and columns of the
			  agreement plot.

* BY=         Specifies the names of one (or more) By variables.  Partial
              agreement plots are produced for each combination of the levels
              of the BY= variables.  The BY= variable(s) *must* be listed among
              the VAR= variables.


* COUNT=      Name of the frequency variable [Default: COUNT=COUNT]
                
* WEIGHTS=    Specifies weights for exact agreement and for steps removed 
              from exact agreement, both in the plot and in the calculation
			  of the Bangdiwala agreement strength statistic. WEIGHTS=1 counts
			  only exact agreement. WEIGHTS=LIN2 or WEIGHTS=CA2 uses inverse
			  linear (Cicchetti-Allison) weights: 1, 1/(n-1) for an n x n
			  table.  WEIGHTS=SQ2 or WEIGHTS=FC2 uses inverse square
			  (Fleiss-Cohen) weights, 1, 1/(n-1)**2. You can also use an
			  IML expression that evaluates to a row vector starting with 1,
			  e.g., WEIGHTS=1 || 1-1/(n-1) or WEIGHTS={1 0.5}.

* COLORS=     Specifies colors for exact agreement and for one or more steps 
              removed from exact agreement.  [Default: COLORS=BLACK GRAY]

* FILLS=      Specifies fill patterns for exact agreement and for one or more steps 
              removed from exact agreement.  [Default: FILLS=SOLID LR]

* HTEXT=      Height of text value labels. Variable names are scaled up from
              this by a factor of 1.4 [Default: HTEXT=1.5]

* FONT=       Font for text labels. Internal code uses the hardware font
              HWPSl009 by default for PostScript devices and SWISS otherwise.

* LABROT=     Rotation angles for the horizontal and  vertical value labels 
              [Default: LABROT=0 90]

* TITLE=      Title for plot(s).  When there are BY= variables, a string
              indicating their levels is appended.

* GOUT=       The name of the graphics catalog [Default: GOUT=WORK.GSEG]

* NAME=       The name of the graph in the graphic catalog [Default: NAME=AGREE]
                
=Examples:

  proc format;
	value rating 1='Never_fun' 2='Fairly_often' 
            	 3='Very_often' 4='Almost_always';
  data sexfun;
	 label Husband = 'Husband rating'
           Wife    = 'Wife Rating';
	 format Husband Wife rating.;
	 do Husband = 1 to 4;
	 do Wife    = 1 to 4;
    	input count @@;
    	output;
    	end; end;
  datalines;
   7     7     2      3
   2     8     3      7
   1     5     4      9
   2     8     9     14
  ;

  *-- Convert numbers to formatted values;
  %table(data=sexfun, var=Husband Wife, char=true, weight=count, out=table);
  %agreeplot(data=table, var=Husband Wife, title=Husband and Wife Sexual Fun);

=Bugs:

 With BY= variables, the graphs are correct, but the BN statistics
 dont use the given WEIGHTS.


 =*/
%macro agreeplot(
   data=_last_,     /* Name of input dataset                       */
   var=,            /* Names of factor variables                   */
   by=,             /* Name(s) of BY variables                     */
   count=count,     /* Name of the frequency variable              */
   weights = LIN2,  /* Weights for exact, 1-step, ... agreement    */
   colors=black gray, /* colors for exact and 1-step, ...          */
   fills=solid LR,   /* fill patterns for exact and 1-step, ...     */
   htext=1.5,       /* height of text labels                       */
   font=,           /* font for text labels                        */
   labrot=0 90,     /* rotation angles for horiz, vert labels      */
   title=,          /* title for plot(s)                           */
   gout=,           /* name of the graphics catalog                */
   name=agree
   );

%if %upcase(&data)=_LAST_ %then %let data = &syslast;
%if %length(&var)=0 %then %do;
   %put ERROR: You must specify the VAR= classification variables;
   %goto done;
   %end;
%if %length(%scan(&var,2))=0 %then %do;
   %put ERROR: You must specify two VAR= classification variables;
   %goto done;
   %end;


%*-- handle symbolic expressions for weights;
%if %qupcase(%bquote(&weights))=LIN2 or %qupcase(%bquote(&weights))=CA2
	%then %let weights = 1 || 1-1/(n-1);
%else %if %qupcase(%bquote(&weights))=SQ2 or %qupcase(%bquote(&weights))=FC2
	%then %let weights = 1 || 1-1/(n-1)**2;

proc iml symsize=256;
	*-- Include the agree.sas IML modules; 

start agree(freq, w, vnames, lnames, title )
   global (font, htext, colors, gout, name, labrot, fills);

   if type(font ) ^= 'C' then do;
      call execute('device  = upcase("&sysdevic");');
      if index(device,'PS') > 0 then
         font= 'hwpsl009';         /* Helvetica for PS drivers */
         else font = 'SWISS';
      end;
   if type(htext ) ^= 'N' then htext=1.2;
   if type(colors)   ^='C'  then colors= {BLACK GRAY};
   if type(fills)    ^='C'  then fills= {solid LR};
   if type(gout    ) ^= 'C' then gout='WORK.GSEG';
   if type(labrot  ) ^= 'N' then labrot= {0 90};

   row_sum = freq[,+];
   col_sum = freq[+,];
   n       = freq[+,+];
   k       = nrow(freq);

   reset noname;
   print (( freq || row_sum ) //
          ( col_sum || n ) )[r=(lnames ||'Total') c=(lnames ||'Total')];
   obs_agr = ssq( vecdiag(freq) );
   tot_agr = col_sum * row_sum ;

*   call gstart;
   call gstart(gout);
   call gopen(name,0, 'Agreement plot');
   call gwindow( (-.15#J(1,2,n)) // (1.1#J(1,2,n)) );

   call gset('FONT', font);
   height = htext;
   call gstrlen(len,lnames,height);

   corner= { 0 0 };
   fill  = 'EMPTY';
   colr  = ' ';
   *-- construct marginal rectangles and locate row/col labels --;
   do s = 1 to k;
      thisbox = corner || row_sum[s] || col_sum[s];
      boxes   = boxes  // thisbox;
      fill    = fill   // 'EMPTY';
	  colr    = colr   // ' ';
      center  = corner +((row_sum[s] || col_sum[s])
                       - (len[s]     || len[s]    ))/2;
      labelx  = labelx //(center[1] || (-.06#n)  || labrot[1] )
                       //((-.04#n)  || center[2] || labrot[2] );
      labels  = labels // lnames[s]
                       // lnames[s];
      corner  = corner + (row_sum[s] || col_sum[s]);
      ht      = ht     // height // height;
      end;

   *-- variable names;
   height = 1.4#htext;
   call gstrlen(len,vnames,height);
   center = ((1.0#n) - len)/2;
   labelx  = labelx // ( center[1] || (-.12#n)  || 0 )
                    // ( (-.10#n)  || center[2] || 90);
   labels  = labels // vnames[1]
                    // vnames[2];
   ht      = ht     // height // height;

   *-- surrounding frame, for all observations;
   boxes = boxes // ( { 0 0 } || n || n ) ;

   corner= { 0 0 };
   *-- construct agreement squares and scores;
   q = ncol(w) - 1;
   a = J(q+1,k,0);
   *-- b indexes distance from main diagonal for agreement;
   do b = q to 0 by -1;
   do s = 1 to k;
      agr = max(1, s-b) : min(k, s+b) ;    * cells which agree;
      dis = 1 : max(1, s-b-1) ;            * disagre;
      box_loc = choose( (s-b-1)>0,
                        (sum(freq[s,dis]) || sum(freq[dis,s])),
                        { 0 0 } );
 /*   box_size= choose( (s-b) > 0,
                        (sum(freq[s,agr] ... */
      if s=1 then corner = {0 0};
             else corner = boxes[s,1:2] + box_loc;
      thisbox = corner || sum(freq[s,agr]) || sum(freq[agr,s]);
      boxes   = boxes  // thisbox;
      if b>0 then a[b+1,s] =thisbox[3] # thisbox[4];
 
      if b=0 then do;
	  	fill    = fill   // fills[1];
		colr    = colr   // colors[1];
		end;
      else do;
	  	if fills[2]="LR" then do;
         if mod(b,2)=1 then dir='L';
                       else dir='R';
         dens = int((b+1) / 2);
         fill    = fill   // (dir + char(dens,1));
		 colr    = colr   // colors[2];
         end;
		else do;
	  		fill    = fill   // fills[2];
			colr    = colr   // colors[2];
		 end;
       end;
      end;
   end;
   print 'Bangdiwala agreement scores';
   part = diag(w) * A;
   weights = shape(w,0,1);
   steps = t(0:q);
   BN =  1 - ( ( tot_agr - obs_agr - part[,+] ) / tot_agr );
   reset name;
   print steps weights[f=8.5] BN[f=8.4];
*  print boxes[c={'BotX' 'BotY' 'LenX' 'LenY'}] fill colr;
*  print labels labelx[c={X Y ANGLE}] ht;
 
   run gboxes( boxes, labels, labelx, fill, ht, title, colr );
   call gstop;
finish;
 
*-- Draw and label the agreement display --;
start gboxes( boxes, labels, labelx, fill, ht, title, colr )
   global ( htext,  colors);


   *-- locate the 4 corners of each box;
   ll = boxes[,{1 2}];
   lr = boxes[,{1 3}][,+] || boxes[,2] ;
   ul = boxes[,1] || boxes[,{2 4}][,+] ;
   ur = boxes[,{1 3}][,+] || boxes[,{2 4}][,+];

   xy = ll || ul || ur || lr;
   max = max(ur[,1]) || max(ur[,2]);
   do i=1 to nrow(boxes);
      box = shape(xy[i,], 4);
      color=colr[i];
      pat = fill[i];
      call gpoly( box[,1], box[,2], 1, color, pat, color);
   end;
 
   *-- Draw dotted diagonal line to show marginal homogeneity--;
   call gdrawl( {0 0}, max, 3 , 'RED' );
 
   do f=1 to nrow(labels);
      lxya = labelx[f,];
      labl = labels[f ];
      height = ht[f];
      call gscript( lxya[,1], lxya[,2], labl, lxya[,3], 0, height);
   end;
   *-- title;

   if length(title)>1 then do;
	height = 1.4#htext;
	call gstrlen(len, title, height);
	tx = (max[1] - len)/2;
	call gscript(tx, max[2]#1.05, title, 0, 0, height);
    end;

   call gshow;
finish;

/* --------------------------------------------------------------------
 Routine to read response and index/factor variables from a SAS dataset
 and construct the appropriate levels, and lnames variables

 Input:  dataset - name of SAS dataset (e.g., 'mydata' or 'lib.mydata')
         variable - name of variable(s) containing response(s)
				e.g., 'count' or {count} or {y resid}
		   vnames - character vector of names of index/factor variables
 Output: dim    (numeric levels vector)
         lnames (K x max(dim))
			table  (dim[#] x ncol(variable)) 
  --------------------------------------------------------------------*/
 
start readtab(dataset, variable, vnames, table, dim, lnames);
	if type(dataset)^='C'
		then do; 
			print 'You must supply a dataset name';
			return;
		end;		
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
	print 'Variable' variable 'read from dataset' dataset,
		'with factors ordered' vnames lnames;
	finish;

/* Read variable index labels from an open dataset, construct a dim 
   vector and lnames matrix so that variables are ordered correctly
   for mosaics and ipf (first varying most rapidly).

	The data set is assumed to be sorted by all index variables.  If
	the observations were sorted by A B C, the output will place
	C first, then B, then A.
   Input:    vnames (character K-vector)
 */

start readlab(dim, lnames, vnames);
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
	tmp = span;
	span[,order] = tmp;
	tmp = dim;
	dim[,order] = tmp;
	tmp = lnames;
	lnames[order,] = tmp;
	tmp = vnames;
	vnames[order,] = tmp;
	finish;

 /*---------------------------------------------------------------------
               Generalized transpose of an n-way array      

   Reorders the dimensions of an n-way table.  Order is a permutation
	of the integers 1:ncol(dim), such that order[k]=i means that
	dimension k of the array table becomes dimension i of the result.
	Alternatively, order can be a character vector of the names
	of variables (vnames) in the new order.

   Note: to restore a reordered table to its original form, use
	the anti-rank of the original order;
   Use to rearrange the table prior to calling mosaics
 ---------------------------------------------------------------------*/	
start transpos(dim, table, vnames, lnames, order);
   if nrow(order) =1 then order = t(order);
	if type(order)='C' then do k=1 to nrow(order);
		ord = ord // loc(upcase(order[k,]) = upcase(vnames));
		end;
	else ord = order;

	*-- Dont bother if order = 1 2 3 ... ;
	if all( row(ord)=1:ncol(row(ord)) ) then return;

   if nrow(dim  ) =1 then dim  = t(dim);
   if nrow(vnames)=1 then vnames= t(vnames);
   run marg(loc,newtab,dim,table,ord);
   table = row(newtab);
   dim   = dim[ord,];
   vnames = vnames[ord,];
   lnames = lnames[ord,];
	finish;

start row (x);
   *-- convert a matrix into a row vector;
 
   if (nrow(x) = 1) then return (x);
   if (ncol(x) = 1) then return (x`);
   n = nrow(x) * ncol(x);
   return (shape(x,1,n));
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

start agreeby(dim, table, w, vnames, lnames, title, byvar)
      global(htext, font, sep);
   factors = max(nrow(dim), ncol(dim));

	if type(byvar) = 'C'
		then byvar = name2num(row(byvar), row(vnames));
	if all(byvar=0) then do;
		print 'Error: BYVAR out of bounds in AGREEBY';
		show vnames byvar; print byvar;
		return;
		end;
	others = remove( (1:factors), (byvar) );

	bydim = (shape(dim[byvar],1))`;
	byvn  = (shape(vnames[byvar],1))`;
	nby   = nrow(bydim);        *-- number of by variables;
	modim = dim[others];        *-- dimensions for each slab;
	rows  = modim[#];
	cols  = bydim[#];           *-- number of slab displays;
	* print nby byvar byvn bydim modim rows;
	cl = lnames[byvar,];
	if type(sep) ^= 'C' then sep=', ';
		
	* transpose table to put byvars first;
	order = shape(byvar,1)||others;
	dm = dim;
	vn = vnames;
	ln = lnames;
	tab = table;
	run transpos(dm, tab, vn, ln, order);
	* print dm vn ln tab;
	tab = shape(tab,rows);
	
	*-- construct labels for byvars;
	do i=1 to nby;
      cur = ln[i,]`;
      if i=1 then cl=cur;
         else do;        /* construct row labels for prior factors */
			sp = sep[,min(i-1, ncol(sep))];
            ol = repeat(cl, 1, dm[i]);
            ol = shape(ol,  dm[i]#nrow(cl), 1);
            nl = repeat( (cur[1:(dm[i])]), nrow(cl));
            cl = trim(rowcatc(ol || shape(sp,nrow(ol),1) ||nl));
         end;
		end;
	* print 'transposed, reshaped tab' tab[c=cl];
	print 'Agreement plots for levels of', byvn cl;
	
	pn = vn[1:nby];
	dm = dm[(nby+1):factors];
	vn = vn[(nby+1):factors];
	ln = ln[(nby+1):factors,];

   call gstart;
	do ip = 1 to cols;
		bylev = cl[ip];
		if nby=1
			then titl = trim(pn[1]) + ': ' + bylev;
			else titl = trim(cl[ip]);
		if length(title)>0 then titl = title + ' ' + titl;
		ptab = tab[,ip];
		print 'Slab' ip byvn bylev vn ln ; * ptab;
		ptab = t(shape(ptab, dm[1], dm[2]));
*		print 'ptab Re-shaped', ptab;
 		run agree(ptab, w, vn, ln[1,], titl);
		run gskip;
	end;
	finish;


/*  Translate a character matrix MAT to a numeric matrix of
    the indices of each element in a vector of NAMES.
	 Returns a numeric matrix of the same shape as MAT  */
start name2num(mat, names);
	new = j(nrow(mat), ncol(mat), 0);
	do i=1 to nrow(mat);
		do j=1 to ncol(mat);
			l = loc(trim(upcase(mat[i,j])) = upcase(names));
			if type(l)^='U' then new[i,j] = l;
			end;
		end;
	return(new);
	finish;

/*--------------------------------------------------------------*
 |  IML module to handle multiple output EPS files (or other    |
 |  device-dependent multiple plot circumstances).              |
 |  - This implementation requires a macro variable, DEVTYP to  |
 | be set.  Initialize the FIG variable to 1.  
 *--------------------------------------------------------------*/
*global fig gsasfile devtyp;
start gskip;
	call execute('_dev_ = upcase("&DEVTYP");');
	call execute('_disp_ = "&DISPLAY";');
	if upcase(trim(_disp_)) ^= 'OFF' then do;
	   if (_dev_ = 'EPS') | (_dev_ = 'GIF') | (_dev_ = 'JPEG') then do;
		   _dev_ = lowcase(_dev_);
		   call execute('%let fig = %eval(&fig + 1);');
		   call execute('%let gsas = %scan(&gsasfile,1,.)&fig..', _dev_, ';');
		   call execute('%put NOTE: gsasfile now: &gsas;');
		   call execute('filename gsas&fig  "&gsas";');
		   call execute('goptions gsfmode=replace gsfname=gsas&fig;');
	   end;
/*
	   else if (_dev_ = 'GIFANIM') then do;
		   call execute('_fig_ = "&FIG";');
		   call execute('%let fig = %eval(&fig + 1);');
		   call execute('%let gsas = %scan(&gsasfile,1,.)', 'gif', ';');
		   call execute('%put NOTE: gsasfile still: &gsas;');
		   call execute('filename gsas  "&gsas";');
		   call execute('goptions gsfmode=append gsfname=gsas;');
	   end;
*/
    free _dev_ _disp_;
	end;
	finish;
%*-- Main routine;

   vnames = str2vec("&var");    *-- Preserve case of var names;

	%*-- Set global variables.  For those which have no default value
	     assigned here, the defaults in the agree module are used.;

	%if %length(&colors) %then %str(colors={&colors};) ;
	%if %length(&fills)   %then %str(fills={&fills};) ;
	%if %length(&labrot) %then %str(labrot={&labrot};) ;
	%if %length(&htext)  %then %str(htext=&htext;) ;
	%if %length(&font)   %then %str(font={&font};) ;
	%if %length(&gout)   %then %str(gout={&gout};) ;
	%if %length(&name)   %then %str(name="&name";) ;
	%if %length(&title)=0   %then %str(title=' ';) ;
		%else  %str(title="&title";) ;

   %*-- Read and reorder counts;
   run readtab("&data","&count", vnames, table, levels, lnames);
   vorder  = {&var};
   run transpos(levels, table, vnames, lnames, vorder);
   ln = lnames[1,];   *-- only need one set;
   n = levels[1];         *-- for use in weights expression;
   w = &weights;
  
	%if %length(&by)=0 %then %do;
		table = t(shape(table, levels[1], levels[2]));
		* print levels, lnames table;
 		run agree(table, w, vnames, ln, title );
		run gskip;
		%end;
	%else %do;
 		run agreeby(levels, table, w, vnames, lnames, title, {&by} );
		%end;
  quit;
%done:

%mend;
