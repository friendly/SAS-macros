 /*--------------------------------------------------------------*
  *    Name: sieveplot.sas                                       *
  *   Title: Macro program for sieve diagrams                    *
        Doc: http://datavis.ca/sasmac/sieveplot.html   
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 24 May 2004 11:20:12                                *
  * Revised: 27 May 2004 11:57:21                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The SIEVEPLOT macro provides a more convenient macro interface to
 the SAS/IML modules for sieve diagrams, so that you can input
 a SAS data set (in frequency form), rather than constructing the
 arrays used by the IML modules. It also provides the
 means to produce multiple sieve plots for two-way tables stratified
 by other BY= variables.

=Usage:

 The SIEVE macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%sieveplot(data=haireye, var=Eye Hair, filltype=obsp)
 
==Parameters:

* DATA=       Name of input dataset [Default: DATA=_LAST_]

* VAR=        Names of factor variables, including BY= variables.  The first
              two variables are used as the rows and columns of the
			  sieve diagram.

* BY=         Specifies the names of one (or more) By variables.  Partial
              sieve plots are produced for each combination of the levels
              of the BY= variables.  The BY= variable(s) *must* be listed among
              the VAR= variables.


* COUNT=      Name of the frequency variable [Default: COUNT=COUNT]
                
* COLORS=     Colors for + and - residuals [Default: COLORS=BLUE RED]

* FILLTYPE=   Specifies how each cell is filled, one of OBS, OBSP, DEV, 
              EXL, EXP [Default: FILLTYPE=OBS]
              OBS: fill cells in proportion to observed frequencies
             'OBSP: like OBS, but also write obs. freq. in cell
             'DEV: fill in proportion to deviations from independence.
             'EXL: no fill, write expected frequency in cell
             'EXP: expfill, write expected frequency in cell


* MARGINS=    Empty or TOTAL

* HTEXT=      Height of text labels [Default: HTEXT=1.5]

* FONT=       Font for text labels

* TITLE=      Title for plot(s)

* GOUT=       The name of the graphics catalog

* NAME=       The name of the graph in the graphic catalog
                
==Examples:

   *-- Three way table, Hair color x Eye color x Sex;
   %include catdata(hairdat3);

   *-- Separate plots for males and females;
   %sieveplot(data=haireye, var=Eye Hair Sex, by=Sex, title=Hair-Eye data--,
    	 filltype=obsp);

   *-- Collapse the table over Sex;
   %table(data=haireye, var=Eye Hair, order=data,
    	  weight=count, out=haireye2);

   %sieveplot(data=haireye2, var=Eye Hair, filltype=obsp);

 
 =*/
 

%macro sieveplot(
   data=_last_,     /* Name of input dataset                */
   var=,            /* Names of factor variables            */
   by=,             /* Name(s) of BY variables              */
   count=count,     /* Name of the frequency variable       */
   
   colors=blue red, /* colors for + and - residuals         */
   filltype=,       /* OBS, OBPS, DEV, EXL, EXP             */
   margins=,        /* empty or TOTAL                       */
   htext=1.5,       /* height of text labels                */
   font=,           /* font for text labels                 */
   title=,          /* title for plot(s)                    */
   gout=,           /* name of the graphics catalog         */
   name=sieve
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

proc iml symsize=256;
	*-- Include the sieve.sas IML modules.  Note: you may want to
	    simply insert them inline here;
   *include catdata(sieve); 
start sieve (f, vnames, lnames, title )
      global(filltype, margins, colors, gout, name, font, htext);
   if type(filltype) ^= 'C' then filltype='OBS';
   if type(margins ) ^= 'C' then margins ='';
   if type(colors)   ^='C'  then colors= {BLUE RED};
   if type(gout    ) ^= 'C' then gout='WORK.GSEG';
   if type(name    ) ^= 'C' then name='SEIVE';
   if type(lnames  )  = 'N' then lnames = trim(left(char(lnames)));
   if type(htext  ) ^=  'N' then htext=1.1;
   margins = upcase(margins);
   filltype = upcase(filltype);

	*-- Set default font based on device driver name;
   if type(font    ) ^= 'C' then do;
		call execute('device  = upcase("&sysdevic");');
		if index(device,'PS') > 0 then
         font= 'hwpsl009';         /* Helvetica for PS drivers */
		else /* if device='WIN' then */
			font = 'SWISS';
		end;

   filltype = upcase(filltype);
   margins = upcase(margins);
   verbose=0;
   print filltype margins colors htext;  *show filltype;
 
   nr= nrow(f);
   nc= ncol(f);
   nrc=nr#nc;
   r = f[,+];              * row totals;
   c = f[+,];              * col totals;
   n = c[+];               * grand total;
 
   e = r * c / n;          * expected frequencies;
   d = (f - e) / sqrt(e);  * standard deviates;
   print "Observed Frequencies",
         f [r=(lnames[1,]) c=(lnames[2,]) format=7.0];
   print "Expected Frequencies",
         e [r=(lnames[1,]) c=(lnames[2,]) format=7.1];
   print "Standardized Pearson Deviates",
         d [r=(lnames[1,]) c=(lnames[2,]) format=7.2];
 
   chisq = chisq(f,e);
   df = (nr-1)#(nc-1);
   prob = 1 - probchi(chisq,df);
   print "Test of Independence",
         df ' ' chisq[r={'G.F.' 'L.R.'} format=9.3]
         prob [format=8.4];
 
   rl= (100-nr+1) # r / n;
   cl= (100-nc+1) # c / n;
   cy = 101;
   do i = 1 to nr;
      cx = 0;
      cy = cy - rl[i,] - 1;
      do j = 1 to nc;
         if j= 1 then cx = 0;
         else cx = sum(cl[,1:(j-1)]) + j-1;
         boxes = boxes // ( cx || cy || cl[j] || rl[i] );
      end;
   end;
*  print boxes[c={'BotX' 'BotY' 'LenX' 'LenY'} format=8.3];
 
   call gstart(gout);
   call gopen(name,1);
   call gwindow({ -20 -20 110 110});
 
   if margins = 'TOTALS'          /* y-values for col labels */
      then lcl = {-11 -17 -6};    /* lnames, vnames, totals  */
      else lcl = {-6  -13};
   *-- category names;
   height = htext;
   call gset('FONT',font);
   lrx = 15;
   run gheight(lnames,font,lrx-1,1.75, len,height);
*  print lnames len[format=8.2] ;
 
      *-- row labels;
   lry = boxes[nc#(1:nr),2] + rl/2;
   lrx = repeat((-lrx), nr, 1);
   labelx = labelx // ( lrx || lry || repeat((0||height),nr,1) );
   labels = labels // ( shape(trim(lnames[1,]), nr,1) );
 
      *-- col labels;
   lcx = boxes[1:nc,1] + (cl/2)`;
   lcy = repeat(lcl[1], nc, 1);
   labelx = labelx // ( (lcx-(len[2,1:nc]`)/2) || lcy
                   || repeat((0||height),nc,1) );
   labels = labels // ( shape(trim(lnames[2,]), nc,1) );
 
   *-- variable names;
   ht = 1.1 # height;
   call gstrlen(len,vnames,ht,font);
   center = ( 100 - len)/2;
   labelx  = labelx // (  -16.5    || center[1] || 90 || ht)
                    // ( center[2] || lcl[2]    ||  0 || ht);
   labels  = labels // trim(vnames[1])
                    // trim(vnames[2]);
   *-- title;
   if length(title)>1 then do;
      ht = 1.8;
      run gheight(title,font,100,2.2,len,ht);
      center = ( 100 - len)/2;
      labelx = labelx // ( center || 105 ||  0 || ht );
      labels = labels // trim(title);
   end;
 
   *-- cell frequencies;
   if filltype='OBS' then do;
      f1 = loc(f = 1);
      if ncol(f1)>0  then value = repeat({"1"}, ncol(f1), 1);
   end;
   if filltype='EXL' | filltype='EXP' then do;
      f1 = 1:nrc;
      value = compress(char(shape(e, nrc, 1),5,1));
   end;
   if filltype = 'OBSP' then do;
      f1 = 1:nrc;
      value = compress(char(shape(f, nrc, 1),5,0));
   end;

   if /*filltype^='EXP' & */ ncol(f1)>0 then do;
      ht = height - .05;
      center = boxes[f1,] * ( I(2) // .5#I(2) );
      call gstrlen(len, value,ht,font);
      center[,1] = center[,1] - len/2;
      labelx = labelx // ( center || repeat((0||ht), ncol(f1), 1) );
      labels = labels // value;
   end;
   if margins = 'TOTALS' then do;
      ht = height - .05;
      center = repeat({101},nr,1) || lry;
      labelx = labelx // ( center || repeat((0||ht),nr,1) );
      labels = labels // char(shape(r, nr, 1),4,0);
 
      value  = char( shape( c||n , nc+1, 1), 4,0 );
      call gstrlen(len, value,ht,font);
      lcx = lcx // {101};  len[nc+1,]=0;
      center = (lcx -len/2)  || repeat(lcl[3],nc+1,1);
      labelx = labelx // ( center || repeat((0||ht),nc+1,1) );
      labels = labels // value;
   end;
 
   reset fw=7;
   labelx = round(labelx,.001);
*  print labelx[c={'X' 'Y' 'Angle' 'Ht'}] labels[format=$25.];
   d    = shape(d, nrc,1);
   if filltype = 'EXL'    then fill = shape({0}, nrc, 1);
   if filltype = 'DEV'    then fill = shape( d , nrc, 1);
   if filltype = 'OBS'    then fill = shape( f , nrc, 1);
   if filltype = 'OBSP'   then fill = shape( f , nrc, 1);
   if filltype = 'EXP' then do;
      d = j(nrc,1,0);          fill = shape( e , nrc, 1);
      end;
   run gboxes(boxes, labels, labelx, fill, d );
   call gshow;
   finish;
 
start gheight(label, font, maxlen, maxht, len, height);
   *-- determine height<maxht, to not exceed maxlen in length;
   *-- returns len and height;
   call gstrlen(len,label,height,font);
   max = max(len);
   if max > maxlen | max < .80#maxlen then do;
      height = min(maxht, height # .1 # int(10 # maxlen / max));
      call gstrlen(len,label,height,font);
      end;
   finish;
 
start gboxes( boxes, labels, labelx, fill,dev);
   call gopen('sieve');
   *-- locate the 4 corners of each box;
   ll = boxes[,{1 2}];
   lr = boxes[,{1 3}][,+] || boxes[,2] ;
   ul = boxes[,1] || boxes[,{2 4}][,+] ;
   ur = boxes[,{1 3}][,+] || boxes[,{2 4}][,+];
 
   xy = ll || ul || ur || lr;
   max = max(ur[,1]) || max(ur[,2]);
   do i=1 to nrow(boxes);
      box = shape(xy[i,], 4);
      color='BLACK';
      pat = 'EMPTY';
      call gpoly( box[,1], box[,2], 1, color, pat, color);
   end;
   run fillbox(boxes,fill,dev);
   height = 1;
   do f=1 to nrow(labels);
      lxya = labelx[f,];
      ht   = lxya[,4];
      labl = labels[f ];
      call gscript( lxya[,1], lxya[,2], labl, lxya[,3], 0, ht);
   end;
   finish;
 
start fillbox(boxes, fill, sign)
      global(filltype, colors);
   *-- fill each box proportional to abs(fill);
   w = boxes[,3];
   h = boxes[,4];
   totarea = sum(h#w);
   totfill = sum(abs(fill));
   scale = 1;
   if totfill>.1 & filltype='DEV' /* ^all( fill = int(fill) ) */
      then scale = sqrt(totarea / totfill);
*  print totarea totfill scale;
 
   if filltype='DEV'         /* standardized deviations */
      then do;
      s = sqrt(100 / (abs(fill)) ) ;
      s = s + 100#(abs(fill)<2) ;
      end;
      else
      s = sqrt(h # w / (abs(fill)#scale) );    *space between lines;
   nxl = int(w/s);
   nyl = int(h/s);
*  print 'Width, Height, Square', w h s fill nxl nyl;
   do i = 1 to nrow(boxes);
      if sign[i] >= 0
      then do; line = 1; color = colors[1];   /* 'BLUE' */
      end;
      else do; line = 3; color = colors[2];   /* 'RED ' */
      end;
      if filltype='EXP' then line=34;
      if nxl[i]>0 then do;
         from  = ((boxes[i,1] + (1:nxl[i]) # s[i]))`||
                 shape(boxes[i,2], nxl[i], 1);
         to    = from[,1] || shape(sum(boxes[i,{2 4}]), nxl[i], 1);
         call gdrawl( from, to, line, color );
      end;
      if nyl[i]>0 then do;
         from  = shape(boxes[i,1], nyl[i], 1) ||
                 ((boxes[i,2] + (1:nyl[i]) # s[i]))` ;
         to    = shape(sum(boxes[i,{1 3}]), nyl[i], 1) || from[,2] ;
         call gdrawl( from, to, line, color );
      end;
   end;
   finish;
 
start chisq(obs, fit);
   *-- Find Pearson and likelihood ratio chisquares;
   gf = sum ( (obs - fit)##2 / ( fit + (fit=0) ) );
   lr = 2 # sum ( obs # log ( (obs+(obs=0)) / (fit + (fit=0)) ) );
   return (gf // lr);
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

start sieveby(dim, table, vnames, lnames, title, byvar)
      global(filltype,  htext, font, sep);
   factors = max(nrow(dim), ncol(dim));

	if type(byvar) = 'C'
		then byvar = name2num(row(byvar), row(vnames));
	if all(byvar=0) then do;
		print 'Error: BYVAR out of bounds in SIEVEBY';
		show vnames byvar; print byvar;
		return;
		end;
	others = remove( (1:factors), (byvar) );

	bydim = (shape(dim[byvar],1))`;
	byvn  = (shape(vnames[byvar],1))`;
	nby   = nrow(bydim);        *-- number of by variables;
	modim = dim[others];        *-- dimensions for each sieve;
	rows  = modim[#];
	cols  = bydim[#];           *-- number of sieve displays;
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
	print 'Sieve plots for levels of', byvn cl;
	
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
 		run sieve(ptab, vn, ln, titl);
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
	     assigned here, the defaults in the sieve module are used.;
	%if %length(&filltype) %then %str(filltype={&filltype};) ;
	%if %length(&margins) %then %str(margins={&margins};) ;
	%if %length(&colors) %then %str(colors={&colors};) ;
	%if %length(&htext)  %then %str(htext=&htext;) ;
	%if %length(&font)   %then %str(font={&font};) ;
	%if %length(&gout)   %then %str(gout={&gout};) ;
	%if %length(&name)   %then %str(name="&name";) ;
	%if %length(&title)=0   %then %str(title='';) ;
		%else  %str(title="&title";) ;

   %*-- Read and reorder counts;
   run readtab("&data","&count", vnames, table, levels, lnames);
   vorder  = {&var};
   run transpos(levels, table, vnames, lnames, vorder);
  
	%if %length(&by)=0 %then %do;
		table = t(shape(table, levels[1], levels[2]));
		* print levels, lnames table;
 		run sieve(table, vnames, lnames, title );
		run gskip;
		%end;
	%else %do;
 		run sieveby(levels, table, vnames, lnames, title, {&by} );
		%end;
  quit;
%done:

%mend;
