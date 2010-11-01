 /*--------------------------------------------------------------*
  *    Name: ffold.sas                                           *
  *   Title: Macro for fourfold displays of 2 x 2 x K tables     *
        Doc: http://www.datavis.ca/sasmac/ffold.html          
  *     Ref: Friendly (2000), 'Visualizing Categorical Data', SI *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 13 Nov 1999  9:04:23                                *
  * Revised: 27 Mar 2003 09:56:46                                *
  * Version: 1.2-0                                               *
  * 1.1 - Completed macro interface to fourfold.sas              *
  *       Prepared external documentation                        *
  * 1.2 - Inlined fourfold2 IML code to avoid dependence on an   *
  *       enternal file                                          *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The FFOLD macro produces fourfold displays for one or more 2 x 2
 tables.  Each 2x2 table may be standardized so that the marginal
 frequencies of either, or both variables are equated, while
 preserving the odds ratio.  The collection of fourfold displays
 may be arranged on one or more pages, each of which has DOWN=
 rows, and ACROSS= columns.

=Usage:

 The FFOLD macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%ffold(data=toxaemia, var=Hyper Urea, by=Smoke Class);
 
==Parameters:

* DATA=       Name of the input dataset, which must represent a
              2 x 2 x K x ... contingency table in frequency form
              (one observation per cell, with a COUNT= variable
              giving the cell frequency.  [Default: DATA=_LAST_]

* VAR=        Names of two two-level factor variables defining the
              2 x 2 tables.  The variables may be character or
              numeric.  The order in which the VAR= variables 
              (and BY=, if any) are listed determines their order
              in the displays.

* BY=         Name(s) of BY variables, determining the panels of the
              plot.  If there are two or more BY= variables, the panels
              are produced so that the last BY= variable varies most
              rapidly.

* COUNT=      Name of the frequency variable in the input data set.
              [Default: COUNT=COUNT]

* STD=        Specifies how to standardize the 2 x 2 table(s).
              STD='MARG' standardizes each 2x2 table to equal margins, 
              keeping the odds ratio fixed (see CONFIG). STD='MAX' 
              standardizes each table to a maximum cell frequency of 100,
              equating the maximum frequency across all 2 x 2 tables.
              STD='MAXALL' standardizes all tables to max(f[i,j,k])=100.
              STD='TOTAL' standardizes to the maximum total over all tables.

* CONFIG=     Specifies the margins to standardize when STD=MARG.
              CONFIG=1 2 equates both margins. CONFIG=1 equates the
              margins for the first VAR= variable.  CONFIG=2 equates
              the second VAR= variable.

* DOWN=       The number of rows of panels down each page.

* ACROSS=     The number of columns of panels across each page.

* ORDER=      Specifies whether the panels are displayed down
              the columns (ORDER=DOWN) or across the rows (ORDER=ACROSS)
              [Default: ORDER=DOWN]

* SANGLE=     Angle for labels at the left and right sides.
              [Default: SANGLE=90]

* SHADE=      Shading levels for residuals, determined by the
              magnitude of the Z-value corresponding to the
              log odds ratio in a given 2x2 table. [Default: SHADE=2 4]

* ALPHA=      Error rate for confidence rings on odds ratios;
              0 to suppress

* COLORS=     Colors for + and - residuals [Default: COLORS=BLUE RED]

* FILL=       Fill type for + and - residuals [Default: FILL=HLS HLS]

* HTEXT=      Height of text labels [Default: HTEXT=1.5]

* FONT=       Font for text labels [Default: FONT=HWPSL009 for PS,
              otherwise, SWISS]

* PTITLE=     Panel title for plot(s), a character string, which may
              include &V to list the name(s) of the panel BY= variables,
              and &L to list the value(s) of those variables in the
              current panel.

* FRAME=      Line style for panel frame.  Use FRAME=0 to suppress.

* OUTSTAT=    Name of an output data set, containing the odds ratio (OR),
              log odds ratio (LOGOR), std. error (SELOGOR), Z value,
              and PRZ, for each 2x2 table.  
               

 =*/
%macro ffold(
   data=_last_,     /* Name of input dataset                */
   var=,            /* Names of 2x2 factor variable         */
   by=,             /* Name(s) of BY variables              */
   count=count,     /* Name of the frequency variable       */
   std=,            /* How to standardize tables            */
   config=,         /* margins to standardize               */
   down=,           /* number of panels down each page      */
   across=,         /* number of panels across each page    */
   order=,          /* DOWN|ACROSS - arrange multiple plots */
   sangle=,         /* angle for side labels (0|90)         */
   shade=2 4,       /* shading levels for residuals         */
   alpha=,          /* error rate for confidence rings      */
   colors=blue red, /* colors for + and - residuals         */
   fill=HLS HLS,    /* fill type for + and - residuals      */
   htext=1.5,       /* height of text labels                */
   font=,           /* font for text labels                 */
   ptitle=,         /* title for plot(s)                    */
   frame=,          /* line style for panel frame           */
   outstat=         /* name of output data set              */
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

	%*--Becuase of the large number of modules loaded, it may be
	    necessary to adjust the symsize value;
proc iml symsize=256;
   %*include catdata(fourfold2.sas); 

goptions gunit=pct;
start fourfold(dim, table, vnames, lnames)
  global (std, config, down, across, name, sangle, colors, patterns,
          alpha, conf, font, order, odds, bounds, verbose, htext, frame,
			 filltype, shade, ptitle, outstat );

   if nrow(vnames)=1 then vnames=vnames`;
   if nrow(dim)=1 then dim=dim`;
   print  vnames dim '  ' lnames;
 
   *-- Check conformability of arguments --;
   f = nrow(vnames)||nrow(lnames);
   if dim[#] ^= nrow(table)#ncol(table)
      then do;
         print 'ERROR: TABLE and LEVEL arguments not conformable';
			show dim table;
         goto done;
      end;
   if ^all(f = nrow(dim))
      then do;
         print 'ERROR: VNAMES or LNAMES not conformable with dim';
			show dim vnames lnames;
         goto done;
      end;
 
  /*-- Set global defaults --*/
  if type(std)     ^='C' then std='MARG';
  if type(config)  ^='N' then config={1 2};
  if type(name)    ^='C' then name='FFOLD';
  if type(sangle)  ^='N' then sangle=90;
  if type(colors)  ^='C' then colors= {BLUE RED};
  if type(patterns)^='C' then patterns={solid solid};
  if type(filltype) ^= 'C' then filltype = {HLS HLS};
  if type(shade) ^= 'N'
      then shade = {2 4};            /* shading levels            */
  if type(alpha)   ^='N' then alpha=.05;
  if type(conf)    ^='C' then conf='Individual';
  if type(htext)   ^='N' then htext=2;
  if type(order)   ^='C' then order='DOWN';
  if type(verbose) ^='C' then verbose = 'NONE';
  if type(frame)   ^='N' then frame=1;        *-- line style for frame;
  if type(outstat)  ^='C' then outstat = '';

	*-- Set default font based on device driver name;
   if type(font ) ^= 'C' then do;
		call execute('device  = upcase("&sysdevic");');
		if index(device,'PS') > 0 then
         font= 'hwpsl009';         /* Helvetica for PS drivers */
		else /* if device='WIN' then */
			font = 'SWISS';
		end;

  nf = nrow(dim);
  if nf<3 then k=1;     * number of panels;
          else k = (dim[3:nf])[#];

	sysver=&sysver;
	if sysver<6.08 then do;
		*-  Viewports are broken in SAS 6.07;
		if type(down)    ^='N' then down=1;
		if type(across)  ^='N' then across=1;
		end;
	else do; *(;
		if type(across)  ^='N' & type(down)='N' then down=ceil(k/down);
	else if type(down)   ^='N' & type(across)='N' then down=ceil(k/across);
	else if type(down)    ='N' & type(across)='N' then ;
	else if nf>3 then do; 
		across = dim[3];
		down = (dim[4:nf])[#];
		end;
	else do;
		across = int(sqrt(k));
		down = ceil(sqrt(k));
		end;
	end; *);
   
  if type(ptitle)  ^='C' & (down > 1 | across > 1)
  	then ptitle = '&V : &L';
	else ptitle = '';
  
  print 'Global Options',
         std config down across name sangle,
			colors patterns alpha conf font order verbose htext frame
			filltype shade ;

  /*-- Establish viewports --*/
  np = max(down,across);
  pd = np - (across||down);
  size = int(100 / np);
  if order='DOWN' then
     do;
        do i = 1 to across;
           px = size # ((i-1) // i) + (pd[1] # size/2);
           do j = 1 to down;
              py = 100 - (size # (j//(j-1)) + (pd[2] # size/2));
              ports = ports // shape( (px||py), 1);
           end;
        end;
     end;
     else do;
        do j = 1 to down;
           py = 100 - (size # (j//(j-1)) + (pd[2] # size/2));
           do i = 1 to across;
              px = size # ((i-1) // i) + (pd[1] # size/2);
              ports = ports // shape( (px||py), 1);
           end;
        end;
     end;
  nport=nrow(ports);
 
  if ncol(table) ^= 2 then table=shape(table,0,2);
	if nf>2 then run facnames(dim, lnames, nf, 3, ':', plab);

  run odds(dim, table, lnames);
  if k>1 then run tests(dim, table, vnames);
  if type(filltype) = 'C' then do; 
  	run fillpat(fcolors, fpattern);
  	* print 'Selecting colors from', fcolors fpattern;
	end;
  
  page = 0;                    * number of pages;

  pvar = '';
  if k>1 then do j=nf to 3 by -1;
  	if pvar ^= '' then pvar = pvar + ':';
	pvar = pvar + trim(vnames[j]);
	end;
	 
  do i=1 to k;
     r = 2#i;                  * row index, this table;
     t=table[((r-1):r),];      * current 2x2 table;
 
     /* construct top label for this panel */
	  title = ptitle;
     if k > 1 & length(ptitle)>1 then do;
		title = ptitle;
		call change(title, '&V', pvar);
		call change(title, '&L', plab[i,]);
		call change(title, '&O', char(odds[i,1],4,2));
		end;
 
     /* standardize table to fit 100x100 square */
     run standize(fit, t, table);
 
     if verbose ^= 'NONE' then do;
     print title;
     print fit[c=(lnames[1,]) r=(lnames[2,]) f=8.2] ' '
             t[c=(lnames[1,]) r=(lnames[2,]) f=8.0] ;
     end;
 
     /*-- start new page if needed --*/
     if nport=1 | mod(i,nport)=1 then do;
        call gstart;
        page = page+1;                * count pages;
        gname =rowcatc(trim(name) || char(page,2,0));
        call gopen(gname);            * name uniquely;
        end;
 
     /*-- set viewport --*/
     if nport>1 then do;
     ip = 1 + mod(i-1,nport);         * viewport number;
     port = ports[ip,];               * coordinates;
     call gport(port);
     end;
 
		colsave = colors; patsave=patterns;
		colors = fcolors[{2 1}, 1+sum(abs(odds[i,4])>shade)]`;
		patterns = fpattern[{2 1}, 1+sum(abs(odds[i,4])>shade)]`;
	*	 print 'Using colors:' colors patterns;

     /*-- draw this panel, display if end-of page --*/
     call gpie2x2(fit, t, lnames, vnames, title, np, odds[i,2]);
	  colors = colsave; patterns = patsave;
     if alpha>0 then run conflim(dim, t, bounds[i,], table);
     if mod(i,nport)=0 | i=k then call gshow;
   end;
   call gclose;

	done:
finish;
 
start standize(fit, t, table) global(std, config);
  /*-- standardize table to equal margins --*/
  if std='MARG' then do;
     newtab = {50 50 , 50 50 };
	  if any(t ^=0)
        then call ipf(fit,status,{2 2},newtab,config,t);
		  else fit = j(2,2,0);
  end;
 
  /*-- standardize to largest cell in EACH table --*/
  else if std='MAX' then do;
     n = t[+,+];
	  if n>0 then do;
			fit = (t/n)#200 ;
			fit = fit# 100/max(fit);
	  end;
	  else fit = j(2,2,0);
  end;
 
  /*-- standardize to largest cell in ALL tables --*/
  else if std='MAXALL' then do;
	  fit = t # 100 / max(table);
  end;
  
  /*-- Standardize to total in largest table --*/
  else if std='TOTAL' then do;
		tot = (shape(table[,+],0,2))[,+];
		fit = t # 200 / max(tot);
  end;
  
  else fit = t;   /* raw counts */
finish;
 
start gpie2x2(tab,freq,lnames,vnames,title,np,d)
      global(sangle, colors, patterns, font, htext, frame);
  /*-- Draw one fourfold display --*/
  t = shape(tab,1,4);     * vector of scaled frequencies;
  r = 5 * sqrt(t);        * radii of each quarter circle;
 
  /*-- set graphic window, font, and text height */
  call gwindow({-16 -16 120 120});
  call gset('FONT',font);
  ht = htext # max(np,2);
  call gset('HEIGHT',ht);
 
  /*-- set shading patterns for each quadrant */
  /* cell:[1,1] [1,2] [2,1] [2,2] */
  angle1 = { 90    0   180   270 };
  angle2 = {180   90   270   360 };
  patt   = (shape(patterns[,{1 2 2 1 2 1 1 2}],2))[1+(d>0),];
  color  = (shape(  colors[,{1 2 2 1 2 1 1 2}],2))[1+(d>0),];
	*print patt color;
 
  /*-- draw quarter circles, with color and shading */
  do i = 1 to 4;
     call gpie(50,50, r[i], angle1[i], angle2[i],
               color[,i], 3, patt[,i]);
     if int(abs(i-2.5)) = (d>0) then do;
			rad = (r[i] + {0, 10}) / 50;
			ang = repeat( (angle1[i]+angle2[i])/2, 2, 1 );
			call gpiexy(xx,yy, rad, ang,{50 50}, 50);
		*	print 'Slice markers' i rad ang xx yy;
			call gdraw(xx, yy);
			end;
     end;
 
  /*-- draw frame and axes --*/
  call gxaxis({0 50},100,11,1,1);
  call gyaxis({50 0},100,11,1,1);
  if frame>0 then call ggrid({0 100}, {0 100}, frame);
 
  /*-- set label coordinates, angles --*/
  lx = { 50, -.5, 50, 101};
  ly = { 99, 50, -1,  50};
  ang= {  0,  0,  0,   0};
  /*-- label justification position --*/
       /*  ab  lt  bl   rt  */
  posn = {  2,  4,  8,   6};
  if vnames[1] = " " then vl1 = '';
     else vl1= trim(vnames[1])+': ';
  vl2='';
 
  /*-- are side labels rotated? --*/
  if sangle=90 then do;
     ang[{2 4}] = sangle;
     posn[ {2 4}] = {2 8};
     if vnames[2] ^= " "
        then vl2= trim(vnames[2])+': ';
     end;
  labels = (vl2 + lnames[2,1])//
           (vl1 + lnames[1,1])//
           (vl2 + lnames[2,2])//
           (vl1 + lnames[1,2]);
  run justify(lx,ly,labels,ang,posn,ht,xnew,ynew,len);
 
  /*-- write actual frequencies in the corners --*/
  cells = compress(char(shape(freq,4,1),6,0));
  lx = {  5, 95,  5, 95};
  ly = { 94, 94,  4,  4};
  ang= {  0,  0,  0,  0};
  posn={  6,  4,  6,  4};
  run justify(lx,ly,cells,ang,posn,ht,xnew,ynew,len);
 
  /*-- write panel title centered above */
  if length(title)>1 then do;
     ht=1.25#ht;
     call gstrlen(len,trim(title),ht);
     call gscript((50-len/2),112,title,,,ht);
  end;
finish;
 
start justify(x, y, labels, ang, pos, ht, xnew, ynew, len);
  /* Justify strings a la Annotate POSITION variable.
     x, y, labels, ang and pos are equal-length vectors.
     Returns justified coordinates (xnew, ynew)
  */
 
  n = nrow(x);
  call gstrlen(len,labels);
*print len labels;
  xnew = x; ynew =  y;
 
  /* x and y offset factors for each position   */
  /*       1   2   3   4   5   6   7   8    9   */
  off1 = {-1 -.5   0  -1 -.5   0  -1   -.5   0  };
  off2 = { 1   1   1   0   0   0  -1.6 -1.6 -1.6};
 
  do i=1 to n;
     if ang[i]=0 then do;
        xnew[i] = x[i] + (off1[pos[i]]#len[i]);
        ynew[i] = y[i] + (off2[pos[i]]#ht);
     end;
     else if ang[i]=90 then do;
        ynew[i] = y[i] + (off1[pos[i]]#len[i]);
        xnew[i] = x[i] - (off2[pos[i]]#ht);
     end;
  call gscript(xnew[i], ynew[i], trim(labels[i]), ang[i]);
  end;
  len = round(len,.01);
finish;
 
start odds(dim, table, lnames)
      global(alpha, conf, odds, bounds, outstat);
  /*-- Calculate odds ratios for 2x2xK table --*/
  free odds bounds;

  nf = max(ncol(dim), nrow(dim));
  if nf<3 
  	then do;  k = 1;       rl='';  end;
   else do;  
		k = (dim[3:nf])[#];
		run facnames(dim, lnames, nf, 3, ':', rl);
		end;

  do i=1 to k;
     r = 2#i;
     t=table[((r-1):r),];
	  if any(t=0) then t=t+0.5;
     d = (t[1,2]||t[2,1]);
     or = or // ( t[1,1]#t[2,2])/ ((d + .01#(d=0))[#]);
     selogor = selogor // sqrt( sum( 1 / (t + .01#(t=0)) ) );
     end;
  logor = log(or);
  z = log(or)/selogor;
  prz = 2#(1 - probnorm(abs(z)));
  odds = or || logor || selogor || z || prz;
 
  title= 'Odds (' + trim(lnames[1,1]) + '|' + trim(lnames[2,1])
         + ') / ('+ trim(lnames[1,1]) + '|' + trim(lnames[2,2]) + ')';
  reset noname;
  print title;
  cl={'Odds Ratio' 'Log Odds' 'SE(LogOdds)' 'Z' 'Pr>|Z|'};
  print odds[r=rl c=cl format=9.3];

  if outstat ^= '' then do;
  		vn = 'rl or logor selogor z prz';
		call execute('%let outstat=', outstat, ';');
		call execute("create &outstat var{", vn,  "};");
		append;
     end;
 
  /* Find confidence intervals for log(odds) and odds            */
  if nrow(alpha)>1 then alpha=shape(alpha,1);
  cl={'Lower Odds' 'Upper Odds' 'Lower Log' 'Upper Log'};
  if substr(conf,1,1)='I'
     then conf='Individual';
     else conf='Joint';
  do i=1 to ncol(alpha);
     if conf='Individual'
        then pr=alpha[i];
        else pr= 1 - (1-alpha[i])##(1/k);
     if pr>0 then do;
      z = probit(1 - pr/2);
      ci= (odds[,2] * j(1,2)) + (z # selogor * {-1 1});
      co= exp(ci);
      bounds = bounds || co;
      ci= co || ci;
      print conf 'Confidence Intervals, alpha=' pr[f=6.3]  ' z=' z[f=6.3  ],
            ci[r=rl c=cl f=9.3];
      end;
     end;
  reset name;
finish;
 
start tests(dim,table, vnames);
	dm = dim;
	vn = vnames;
	nf = max( nrow(dm), ncol(dm) );
	if nf>3 then do;
		dm[3] = (dim[3:nf])[#];  dm=dm[1:3];
		vn[3] = rowcat( trim(shape(vn[3:nf],1)) );
		end;

  config = {1 1 2,           /*  Test homogeneity of odds ratios */
            2 3 3};          /*  (no three-way association)      */
  call ipf(m, status, dm, table, config);
  chisq = sum ( (table - m)##2 / ( m + (m=0) ) );
  chisq = 2 # sum ( table # log ( (table+(table=0)) / (m + (m=0)) ) );
  df  = dm[3] - 1;
  prob= 1- probchi(chisq, df);
  out = chisq || df || prob;
  test={'Homogeneity of Odds Ratios'};
  print 'Test of Homogeneity of Odds Ratios (no 3-Way Association)',
         test chisq[f=8.3] df prob[f=8.4];
 
  config = {  1 2,           /*  Conditional independence */
              3 3};          /*  (Given no 3-way)         */
  call ipf(m, status, dm, table, config);
  chisq = 2 # sum ( table # log ( (table+(table=0)) / (m + (m=0)) ) )
          - chisq;
  df  = 1;
  prob= 1- probchi(chisq, df);
 
  *-- CMH test (assuming no 3-way);
  free m;
  k = dm[3];
  do i=1 to k;
     r = 2#i;                  * row index, this table;
     t=table[((r-1):r),];      * current 2x2 table;
     n = n // t[1,1];
     s = t[+,+];
     m = m // t[+,1] # t[1,+] / (s + (s=0));
     v = v // (t[+,][#]) # (t[,+][#]) / (s#s#s-1);
     end;
  cmh = (abs(sum(n) - sum(m)) - .5)##2 / sum(v);
 
  chisq = chisq // cmh;
  df = df // df;
  prob = prob // (1 - probchi(cmh,1));
  out = out // (chisq || df || prob);
  test = {'Likelihood-Ratio','Cochran-Mantel-Haenszel    '};
  head = 'Conditional Independence of '+trim(vn[1])
         +' and '+trim(vn[2])+' | '+trim(vn[3]);
  reset noname;
  print head, '(assuming Homogeneity)';
  reset name;
  print test chisq[f=8.3] df prob[f=8.4];
finish;
 
start conflim(dim, t, bounds, table)
      global(std);
  do l=1 to ncol(bounds);
  run limtab(dim, t, bounds[l], tbound);
     *-- standardize the fitted table the same way as data;
  run standize(fit, tbound, table);
  s = shape(fit,1,4);     * vector of scaled frequencies;
  r = 5 * sqrt(s);        * radii of each quarter circle;
  angle1 = { 90    0   180   270 };
  angle2 = {180   90   270     0 };
  pat = 'EMPTY'; color='BLACK';
  do i = 1 to 4;
     call gpie(50,50, r[i], angle1[i], angle2[i],
               color, 3, pat);
     end;
  end;
finish;
 
start limtab(dim, t, bounds, tbound)
      global(std);
  /* find 2x2 tables whose odds ratios are at the limits of the
     confidence interval for log(odds)=0 */
     odds = bounds[1];
     a = sqrt(odds)/(sqrt(odds)+1);
     b = 1 - a;
     ltab =  (a || b) // (b || a);
     *-- construct a fitted table with given odds ratio,
         but same marginals as data;
     config = {1 2};
     call ipf(tbound,status,{2 2},t,config,ltab);
  *  print odds[f=7.3] ltab[f=6.3] t[f=5.0] tbound[f=8.2];
finish;

start fillpat(fcolors, fpattern)
      global(filltype, shade, colors);
   *-- Set colors and patterns for shading fourfold areas;
   ns = 1+ncol(shade);
*	ns  = 2;
   if ns<5
      then dens=char({1 3 4 5}[1:ns]`,1,0);
      else dens={'1' '2' '3' '4' '5'};
   fpattern = J(2,ns,'EMPTY   ');
   if ncol(colors)=1 then colors=shape(colors,1,2);
   colors = substr(colors[,1:2] + repeat('       ',1,2),1,8);
   fcolors = repeat(colors[1],1,ns) // repeat(colors[2],1,ns);
 
   if ncol(filltype)=1 then      /* if only one filltype */
      do;
         if filltype='M0'  then filltype={M0 M90};
         else if filltype='M45' then filltype={M135 M45};
			else if filltype='LR' then filltype=cshape(filltype,2,1,1);
         else filltype=shape(filltype,1,2);
      end;
 
   do dir=1 to 2;                 * for + and - ;
      ftype = upcase(filltype[dir]);      * fill type for this direction;
      if substr(ftype,1,1)='M' then
         do;
            angle = substr(ftype,2);
            if angle=' ' then angle='0';
            if ns=2 then fpattern[dir,] = {M1N M1X}+angle;
               else fpattern[dir,] = {M1N M3N M3X M4X M5X}[1:ns]`+angle;
         end;
      else if ftype='L' | ftype='R' then
            fpattern[dir,] = trim(ftype)+dens;
      else if substr(ftype,1,4)='GRAY' then
         do;
            if length(ftype)=4
               then step=16;
               else step = num(substr(ftype,5));
            dark = 256 - step#(1:ns)`;
            *-- convert darkness values to hex;
            fcolors = substr(fcolors,1,max(6,length(fcolors)));
            fcolors[dir,] = compress('GRAY'+hex(dark)`);
            fpattern[dir,] = repeat('SOLID',1,ns);
         end;
      else if substr(ftype,1,3)='HLS' then
         do;
         /* lightness steps for varying numbers of scale values */
         step={ 0 100 0 0 0,
               0 90 100 0 0,
               0 25 70 100 0,
               0 20 45 70 100}[ns-1,];
         /* make step=100 map to '80'x, =0 map to 75% of the way to 'FF' */
         step =  25 + 0.75#step;
         step = step[1:ns]#255/100;
			dark = 255-ceil(step/2);
         lval= hex(dark)`;

         clr = upcase(colors[dir]);
         hue = {RED GREEN BLUE MAGENTA CYAN YELLOW,
               '070' '100' '001' '03C' '12C' '0B4'  };
         col = loc(hue[1,]=clr);
         if ncol(col)=0 then col=dir;
         hval= hue[2,col];
          fcolors[dir,] =  compress('H'+hval+lval+'FF');
         fpattern[dir,] = repeat('SOLID',1,ns);
         end;
      end;
   finish;
 
start hex(num);
   *-- Convert 0<num<256 to hex;
   chars = cshape('0123456789ABCDEF',16,1,1);
   if num>255 then num = mod(num,256);
   h = rowcat(chars[1+floor(num/16),] || chars[1+mod(num,16),]);
   return(h);
   finish;
   
*-- Construct factorial combinations of level names;
start facnames (levels, lnames, first, last, sep, rl);
	free rl;
	do i=first to last by sign(last-first)+(first=last);
		cl = lnames[i,1:levels[i]]`;
		if i=first then rl=cl;
		else do;        /* construct row labels for prior factors */
			ol = repeat(rl, 1, levels[i]);
			ol = shape(ol,  levels[i]#nrow(rl), 1);
			nl = shape( (cl[1:(levels[i])]), nrow(ol), 1);
			rl = trim(rowcatc(ol || shape(sep,nrow(ol),1) || nl));
			end;
		end;
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

%*-- Main routine;

   vnames = str2vec("&var &by");    *-- Preserve case of var names;

   %*-- Read and reorder counts;
   run readtab("&data","&count", vnames, table, levels, lnames);
	vorder  = { &var &by };
	run transpos(levels, table, vnames, lnames, vorder);

	%*-- Set global variables.  For those which have no default value
	     assigned here, the defaults in the fourfold module are used.;
   filltype={&fill};
	%if %length(&colors) %then %str(colors={&colors};) ;
	%if %length(&htext)  %then %str(htext=&htext;) ;
	%if %length(&shade)  %then %str(shade={&shade};) ;
	%if %length(&ptitle) %then %str(ptitle="&ptitle";) ;
	%if %length(&font)   %then %str(font={&font};) ;
	%if %length(&std)    %then %str(std={&std};) ;
	%if %length(&down)   %then %str(down=&down;) ;
	%if %length(&across) %then %str(across=&across;) ;
	%if %length(&order)  %then %str(order={&order};) ;
	%if %length(&frame)  %then %str(frame=&frame;) ;
	%if %length(&alpha)  %then %str(alpha=&alpha;) ;
	%if %length(&outstat) %then %str(outstat={&outstat};) ;
	%if %length(&config) %then %str(config={&config};) ;

	run fourfold(levels, table, vnames, lnames);
   quit;
%done:

%mend;

