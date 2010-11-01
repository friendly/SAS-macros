/* Copyright(c) 2003 by Unicredit Banca S.p.A. - Milan, Italy   */                                                                                                                                                                                       

/* Modified: 11 Dec 2003 10:32:17 MF
   -  Placed scatmve inline
   -  Removed device dependencies (set before the call)
   -  Added control of colors, font, htext, title, symbol size
   -  Added ID= variable
   -  Cleaned up lots of scalar code
*/
 
/****************************************************************/
/*                                                              */
/*    NAME : OUTLIERS                                           */
/*   TITLE : OUTLIERS - Outliers Detection                      */
/* PRODUCT : IML GRAPH                                          */
/*  SYSTEM : ALL                                                */
/*    KEYS : Data Auditing                                      */
/*   PROCS : IML                                                */
/*    DATA : EXTERNAL                                           */
/*                                                              */
/* SUPPORT : Alfredo.Roccato@Unicredit.it   UPDATE:  7DEC2002   */
/*     REF : SAS/IML Software: Changes and Enhancements,        */ 
/*           Release 8.1                                        */
/*    MISC :                                                    */
/*                                                              */
/* USAGE   : %let folder=folder-name;                           */
/*         : %inc "&FOLDER/outliers.sas";                       */
/*           %outliers (data=stackloss,                         */
/*                      vars=x2 x3 x4,                          */
/*                      method=MVE,                             */
/*                      out=_outliers_,                         */
/*                      graph=Y);                               */
/*                                                              */
/* PARAMETERS :                                                 */
/*           data=      Specifies the name of the input data    */
/*						set to be analyzed.                     */				
/*           vars=   	Specifies the names of the table        */ 
/* 						variables.										 */
/*           method=STD Specifies the estimation method:        */ 
/*                      STD (Standard Deviation)                */
/*                      IQR (InterQuartile Range)               */
/* 						MVE (Minimum Volume Ellipsoid) or       */
/* 						MCD (Median Absolute Deviation)         */
/* http://acad.uis.edu/sas/iml/chap17/sect151.htm               */
/* 						MAD (Median Absolute Deviation)         */
/*                      Default coefficient 3.5 (confidence 95%)*/
/* http://acad.uis.edu/sas/iml/chap17/sect146.htm               */
/* 						MHL (Mahalanobis distances)             */
/* http://www.math.yorku.ca/SCS/sssg/outlier.html               */
/*           opt5=-1   	Options for the MVE and MCD methods.    */ 
/*                      Specifies the number NRep of subset     */
/*                      generations. If omitted and nobs>30     */
/*                      the defualt is 3000                     */ 
/*           madthrs=3.5 Option for the MAD method. It is the   */
/*                      Hampel's threshold.                     */
/*           pvalue=.05 Option for the MHL method. Probability  */
/*                      value of chi2 statistic used to trim obs*/
/*           passes=2   Option for the MHL method. Number of    */ 
/*                      passes of the iterative trimming proced.*/
/*           stdmult=2   Option for the IQR method. Rule for    */
/*                      detecting suspect outliers: 99.73%      */ 
/*                      (with 2: 95.45%)                        */
/*           iqrmult=1.5 Option for the IQR method. Rule for    */
/*                      detecting suspect outliers.             */ 
/*           out=_OUT_  Specifies the name of the output data   */ 
/* 						set corresponding to the input DATA=	 */ 
/*                      data set. Label outliers with 'Y'.      */ 
/*				 test=      Optional. Test description.             */
/*           graph=	 	Option for MVE and MCD methods. If non- */
/*                      blank, plots of the classical and robust*/
/*                      tolerance ellipsoids, one plot for each */
/*                      pair of variables. IML modules in the   */
/*                      scatmve.sas file required.              */
/*           options=	Options for the PROC IML Statements      */ 
/*           qqplot=    If non-black creates a quantile-quantile*/ 
/*                      plot                                    */ 
/****************************************************************/

%macro outliers(
	data=,
	vars=,
	id=,            /* name of id variable */
	method=MHL,
	opt5=-1,
	madthrs=3.5,
    passes=2,
	pvalue=0.05,
	iqrmult=1.5,
	stdmult=3, 
    out=_OUT_,
	graph=, 
	test=, 
	debug=N, 
    options=, 
	qqplot=,
	colors= red blue,  /* colors for classical and robust ellipses */
	lines= 3 1,        /* lines for classical and robust ellipses */
	font=,
	htext=1.5,
	hsym=1.5,         /* height of plot symbols */
	title=,
	jitter=0 );

%global ntest;

 %let dsid=%sysfunc(open(&data));
 %let nobs=%sysfunc(attrn(&dsid,NOBS)); 
 %let rc=%sysfunc(close(&dsid));

%let method = %upcase(&method);

 %if &NOBS GT 30 AND &OPT5 EQ -1 %then %let opt5=3000; 

%let nvars=0; %let var=X; 
%do %until(&var eq ); 
   %let nvars=%eval(&nvars+1); 
   %let var=%scan(&vars,&nvars, %str( )); 
   %end; 
%let nvars=%eval(&nvars-1);

/* Set default font if not specified */
%if %length(&font)=0 %then %do;
	%if %index(%upcase(&sysdevic),PS)>0
		%then %let font=hwpsl009;
		%else %let font=swiss;
	%end;

	options nodate CLEANUP;
*	goptions reset=all dev=win ;

PROC IML &OPTIONS ;

/*****************************************************************************************/
start scatmve(job, optn, prob, data, vname, title, id)
	global(colors, lines, htext, hsym);

      /* job=1: draw confidence ellipse for mean,
         job=2: draw confidence ellipse for prediction
      */

      start ellipse1(job,prob,nobs,xm,covm,nn,corn,elp);
        /* This is my version */
        /* job=1: draw confidence ellipse for mean,
           job=2: draw confidence ellipse for prediction */
        tf = 2. * (nobs-1) / (nobs * (nobs - 2));
        ff = tf * FINV(prob,2,nobs-2);
        if job = 2 then ff = ff * (nobs + 1.);
        ff = sqrt(ff);

        /* Eigenvectors are stored in columns */
        /* get orthogonal rotation matrix */
        /* since length of eigenvectors is 1:
           rot[1,1]= rot[2,2]= cos(alfa),
           rot[2,1]= sin(alfa) = -rot[1,2] */
        CALL EIGEN(eval,evec,covm);
        if covm[1,1] > covm[2,2] then do;
           t = eval[1];   eval[1] = eval[2];     eval[2] = t;
           t = evec[1,1]; evec[1,1] = evec[1,2]; evec[1,2] = t;
           t = evec[2,1]; evec[2,1] = evec[2,2]; evec[2,2] = t;
        end;
		da = sqrt( max(eval[1], 0));
		db = sqrt( max(eval[2], 0));
		
        rot = j(2,2,0.);

        rot[1,1] =  da * ff * evec[1,1];
        rot[2,1] =  da * ff * evec[1,2];
        rot[1,2] = -db * ff * evec[1,2];
        rot[2,2] =  db * ff * evec[1,1];

        /* Get axes of ellipsoid: a axis: 1 -> 2
                                  b axis: 3 -> 4 */
		corn = { -1  0,
		          1  0,
				  0 -1,
				  0  1};
        corn = corn * rot` + j(4,1,1) * xm;

        /* Points of ellipsoid: elp[nn+1,2] */
/*
        pi = 3.141592653589; pi2 = 2. * pi;
        elp = j(nn+1,2,0.);
        phi = 0.; dp = pi2 / nn;
        do j = 1 to nn+1;
          x = cos(phi); y = sin(phi);
          elp[j,] = (x || y) * rot` + xm;
          phi = phi + dp;
        end;
*/		
		phi = t( (0:nn) # atan(1)#8 / nn );
		x = sin(phi);
		y = cos(phi);
		elp = (xm @ j(nn+1,1)) + (x || y) * t(rot);

      finish ellipse1;


      start ellipse2(job,prob,nobs,xm,covm,nn,corn,elp);

        /* This is the INSIGHT version */
        /* job=1: draw confidence ellipse for mean,
           job=2: draw confidence ellipse for prediction */
        tf = 2. * (nobs-1) / (nobs * (nobs - 2));
        ff = tf * FINV(prob,2,nobs-2);
        if job = 2 then ff = ff * (nobs + 1.);
        ff = sqrt(ff);

        /* Eigenvectors are stored in columns */
        CALL EIGEN(eval,evec,covm);
        if eval[1] <= 0 then da = 0.;
                        else da = sqrt(eval[1]);
        if eval[2] <= 0 then db = 0.;
                        else db = sqrt(eval[2]);

        v0 = da * evec[1,1] * evec[1,1]
           + db * evec[1,2] * evec[1,2];
        v1 = da * evec[1,1] * evec[2,1]
           + db * evec[1,2] * evec[2,2];
        v2 = da * evec[2,1] * evec[2,1]
           + db * evec[2,2] * evec[2,2];
        elp = j(nn+1,2,0.);
        phi = 0.; dp = 1. / nn; s2 = 4. * sqrt(2.);
        do j = 1 to nn+1;
          if phi <= .25 then do;
            v = phi - .125; xv = s2 * v;
            yv = -sqrt(1. - xv * xv);
          end; else
          if phi <= .5 then do;
            v = phi - .375; yv = s2 * v;
            xv = sqrt(1. - yv * yv);
          end; else
          if phi <= .75 then do;
            v = .625 - phi; xv = s2 * v;
            yv = sqrt(1. - xv * xv);
          end; else do;
            v = .875 - phi; yv = s2 * v;
            xv = -sqrt(1. - yv * yv);
          end;
          elp[j,1] = ff * (v0 * xv + v1 * yv) + xm[1];
          elp[j,2] = ff * (v1 * xv + v2 * yv) + xm[2];
          phi = phi + dp;
        end;

        /* Get axes of ellipsoid: a axis: 1 -> 2
                                  b axis: 3 -> 4 */
        if covm[1,1] > covm[2,2] then do;
           t = da; da = db; db = t;
           t = evec[1,1]; evec[1,1] = evec[1,2]; evec[1,2] = t;
           t = evec[2,1]; evec[2,1] = evec[2,2]; evec[2,2] = t;
        end;
        rot = j(2,2,0.);
        rot[1,1] =  da * ff * evec[1,1];
        rot[2,1] =  da * ff * evec[1,2];
        rot[1,2] = -db * ff * evec[1,2];
        rot[2,2] =  db * ff * evec[1,1];
        corn = j(4,2,0.);
        corn[1,1] = -1.; corn[1,2] =  0.;
        corn[2,1] =  1.; corn[2,2] =  0.;
        corn[3,1] =  0.; corn[3,2] = -1.;
        corn[4,1] =  0.; corn[4,2] =  1.;
        corn = corn * rot` + j(4,1,1) * xm;
      finish ellipse2;

      /* Initialization */
      n = ncol(data); nobs = nrow(data);
      nn = 60; /* number of ellipsoid points */

      /* Compute classical scatter matrix */
      muc = data[+,] / nobs;
      covc = (data` * data - muc` * muc * nobs) / (nobs - 1.);

      /* Compute robust scatter matrix */
/* we could use:
      CALL &METHOD (sc,xmve,dist,optn,data);
*/ 
      CALL MVE(sc,xmve,dist,optn,data);
      mur = xmve[1,]; 
	  covr = xmve[3:2+n,];
      wgts = dist[3,];

      /*--- do plots for all pairs of variables ---*/
      ig = 0;
      do jy = 2 to n;
      do jx = 1 to jy-1;
	  	jxy = jx || jy;
        ig = ig + 1;
       *-- call gopt(ipsf,filn,n,jx,jy);

        x = data[,jx]; 
		y = data[,jy];
		mu1 = muc[ , jxy ];
		cov1 = covc[ jxy, jxy];
        /* Compute points of classic Ellipsoid */
        call ellipse1(job,prob,nobs,mu1,cov1,nn,corn1,elp1);

		mu2 = mur[ , jxy ];
		cov2 = covr[ jxy, jxy];
        /* Compute points of robust Ellipsoid */
        call ellipse1(job,prob,nobs,mu2,cov2,nn,corn2,elp2);

        /* Get dimension of axes */
        /* get [xmin,xmax] and [ymin,ymax] */

        xmin1 = min( x[><], elp1[><,1], elp2[><,1] ); 
		xmax1 = max( x[<>], elp1[<>,1], elp2[<>,1] );
        ymin1 = min( y[><], elp1[><,2], elp2[><,2] ); 
		ymax1 = max( y[<>], elp1[<>,2], elp2[<>,2] );

        xd1 = xmax1 - xmin1; xc1 = .1 * xd1;
        xmin2 = xmin1 - xc1; xmax2 = xmax1 + xc1;
        xd2 = xmax2 - xmin2; xc2 = .1 * xd2;
        xme = .5*(xmin2+xmax2);

        yd1 = ymax1 - ymin1; yc1 = .1 * yd1;
        ymin2 = ymin1 - yc1; ymax2 = ymax1 + yc1;
        yd2 = ymax2 - ymin2; yc2 = .1 * yd2;
        yme = .5*(ymin2+ymax2);

        /* prepare the plot */
        xbox = { 0 100 100   0};
        ybox = { 0   0 100 100};
        wind = (xmin2 || ymin2) // (xmax2 || ymax2);

        /* create the plot */
        call gstart;
        call gopen("mve");
        call gset("font","&font"); /* set character font */
        call gpoly(xbox,ybox);     /* draw a box around plot */
        call gwindow(wind);        /* define window */
        call gport({10 10 , 90 90}); /* define viewport */
        call gset("height", htext);
        call gxaxis((xmin2 || ymin2), xd2, 11, , ,"4.1");
        call gyaxis((xmin2 || ymin2), yd2, 11, , ,"4.1");

        /* plot xnam, ynam variable names of axis and the title */
        call gstrlen(len,vname[jx]);
        call gtext(xmax2-len,ymin2-yc2,vname[jx]);
        call gvtext(xmin2-xc2,ymax2,vname[jy]);
        call gset("height",2.0);
        call gscenter(xme,ymax2,title);

		/* set colors for classical and robust scatters */
		c1= colors[1];
		c2= colors[2];

        /* draw location point */
        call gpoint(mu1[1],mu1[2],"circle",c1, 2#hsym);
        /* draw points of ellipsoid */
*        call gpoint(elp1[,1],elp1[,2],"diamond",c1,0.5);
        call gdraw(elp1[,1],elp1[,2], lines[1], c1);

        /* plot the two ellipsoid axes */
        call gdrawl(corn1[1,1:2],corn1[2,1:2], ,c1);
        call gdrawl(corn1[3,1:2],corn1[4,1:2], ,c1);

        /* draw location point */
        call gpoint(mu2[1],mu2[2],"circle",c2, 2#hsym);
        /* draw points of ellipsoid */
        /* call gpoint(elp2[,1],elp2[,2],"dot",c2,0.5); */
        call gdraw(elp2[,1],elp2[,2], lines[2], c2);

        /* plot the two ellipsoid axes */
        call gdrawl(corn2[1,1:2],corn2[2,1:2], ,c2);
        call gdrawl(corn2[3,1:2],corn2[4,1:2], ,c2);

        /* print the scatter points */
        /* enumerate the zero weight points */
        do i=1 to nobs;
         xi = x[i] + &jitter#(uniform(123434241) - .5); 
		 yi = y[i] + &jitter#(uniform(123434241) - .5);
         if wgts[i] = 0 then do;
          z = char(i,2,0);
		  z = id[i];
          call gpoint(xi,yi,"dot",c1,hsym);
          xi = xi + .01 * xd2;
          call gscript(xi,yi,z,,,);
         end;
         else do;
          call gpoint(xi,yi,"dot",c2,hsym);
         end; 
		end;

        call gshow;
        call gclose;
        call gstop;
      end; end;
   leave:
  finish scatmve;

start gscenter(x,y,str);
  if length(str) > 1 then do;
     call gstrlen(len,str);
     call gscript(x-len/2,y,str);
  end;
finish gscenter;

start gopt(ipsf,filn,n,jx,jy);
  if ipsf then do;
   if n < 10 then do;
     post = jy * 10 + jx;
     post = char(post,2,0);
   end; else do;
     post = jy * 100 + jx;
     post = char(post,4,0);
   end;
   fnam = (filn || post || ".ps");
   fnam = rowcatc(fnam);
   print fnam;
   call execute("filename gsasfile'",fnam,"';");
   if ipsf = 1 then do;
    call execute('goptions reset=goptions dev=pslepsf
    gsfmode=replace gaccess=gsasfile gsfname=GSASFILE
    hsize=5.625 in vsize=4.65 in htext=3.0 pct border;');
   end; else do;
    call execute('goptions reset=goptions dev=pscolor
    gsfmode=replace gaccess=gsasfile border erase;');
   end;
  end; else do;
   call execute('goptions reset=goptions ;');
  end;
finish gopt;


/***********************************************************************/

START QQPLOT (MX, VO);
	X=t(MX);
	n=ncol(X);
	p=nrow(X);
	O=j(n,1,1);
	XMEANS=(1/n)*X*O*t(O);
	XRESID=X-XMEANS;
	S=(1/(n-1))*(X-XMEANS)*t(X-XMEANS);
	SINV=inv(S);
	BIG=t(XRESID)*SINV*XRESID;
	V=j(n,1,0);
	Q=j(n,1,0);
	V=vecdiag(BIG);
	TMP=V;
	V[rank(V),]=TMP;
	QMAX=-999999999999; 
	do i=1 to n;
		Q[i]=cinv((i-.5)/n,p);
		if Q[i]>QMAX then QMAX=Q[i]; 
	end;
	vmax=V[n]; qmax0=qmax;
	VMAX=75/VMAX; QMAX=75/QMAX; 
	V=VMAX*V+10; Q=QMAX*Q+10;
	call gstart;
	call gopen;
	xbox={5 95 95 5};
	ybox={5 5 95 95};
	call gpoly(xbox,ybox);
	aspect=0.75; 
	altezza=1;
	call gset("font","&font");
	call gset("aspect",aspect);
	call gset("height",altezza);
	call gscript(40,2,"Chi-square quantile",,,1,"&font");
	call gscript(2,70,"Squared distance",-90,90,1,"&font");
	m=1:n; m=t(m); 
	l=char(m,8,0); l=compbl(l); 
	call gpoint(Q,V,'dot','red',(altezza-0.25));
	ll=l[VO,]; qq=q[VO,]; vv=v[VO,];
	call gstrlen(len,ll); 
	call gscript(qq-(len/1.5),vv-aspect+(1-altezza),ll,,,,,'blue');
	x0=10; y0=10; 
	lc=x0||y0; 
	x1=(qmax*qmax0)+10; y1=(vmax*qmax0)+10; 
	if y1>90 then do; 
		yclip= 90; x1 = x0 + (YCLIP-y0)*(x0-x1)/(y0-y1); y1=yclip; end;
	if x1>90 then do; 
		xclip= 90; y1 = y1 + (XMAX-x1)*(y1-y0)/(x1-x0); x1=xclip; end;
	uc=x1||y1;
	call gdrawl(lc,uc,,"blue");
	call gshow;
FINISH;

   use &data;
   read all var {&vars} into data [colname=itemname]; 
     %if %length(&id) %then %do;
        read all var{&id} into  obs;
		  if type(obs) = 'N' then obs = trim(left(char(obs,4,0)));
		  else obs=trim(left(obs));
     %end;
     %else %do;
        obs = char(1:nrow(x),3,0);
     %end;


   %if &GRAPH EQ Y /*AND &METHOD EQ MVE*/  %THEN %DO;
	   %*inc "&FOLDER\programmi\scatmve.sas";

      optn = j(8,1,.);
      optn[1]= 0;              /* prints no output */
      optn[2]= 0;              /* pcov: print COV */
      optn[3]= 0;              /* pcor: print CORR */
		%let vg=;
      %do i=1 %to %eval(&nvars-1);
          %let VG=&VG "%scan(&vars,&i,' ')",; 
	   %end;  
		 %let VG={ &VG "%scan(&vars,&nvars,' ')" }%str(;);   
          optn = j(8,1,.); optn[6]=-1;
          vname = &VG ;
          filn = "brl";
          title = "&title";
		  colors = {&colors};
		  lines = {&lines};
		  htext = &htext;
		  hsym = &hsym;
          call scatmve(2,optn,.9,data,vname,title, obs);
   %end;

   %IF %upcase(&METHOD) EQ MVE OR %upcase(&METHOD) EQ MCD %THEN %DO;
      optn = j(8,1,.);
      optn[1]= 0;              /* prints no output */
      optn[2]= 0;              /* pcov: print COV */
      optn[3]= 0;              /* pcor: print CORR */
      optn[5]= &OPT5;          /* nrep: use all subsets */
      CALL &METHOD (sc,xmve,dist,optn,data); 
	   %if &debug eq Y %then %do; z1=dist[3,]; %end;
	   i=loc(dist[3,]=0); %if &debug eq Y %then %do; print z1 i; %end;
   %END;

   %if %upcase(&METHOD) EQ MAD %THEN %DO;
      rmad1=repeat(MAD(data),nrow(data)); 
      xm=repeat(MEDIAN(data),nrow(data)); 
      z= probit(.75)*((data-xm)/rmad1) ;  
      thrs=repeat(&MADTHRS,nrow(data),ncol(data)); 
      z1=abs(z)>thrs; z2=z1[,+]>0; 
      i=loc(z2); 
      %if &debug eq Y %then %do; print z1 z2 i; %end;
   %END;

   %if %upcase(&METHOD) EQ STD %THEN %DO; 
      mean=data[:,]; 
      b=data-repeat(mean,nrow(data),1); 
      ss=b[##,]; 
	  std=sqrt(ss/(nrow(data)-1)); 
      l= repeat(mean- &stdmult # std, nrow(data)); 
      u= repeat(mean+ &stdmult # std, nrow(data)); 
		l1=data<l;
		l2=data>u; 
      z1=l1+l2; z2=z1[,+]>0; 
	   i=loc(z2); %if &debug eq Y %then %do; print z1 i; %end;
   %END;

   %if %upcase(&METHOD) EQ IQR %THEN %DO;
      q=quartile(data); 
	  q1=repeat(q[2,],nrow(data)); 
	  q3=repeat(q[4,],nrow(data)); 
      iqr=q3-q1; 
	   l1=data<(q1- &iqrmult # iqr);
	   l2=data>(q3+ &iqrmult # iqr); 
      z1=l1+l2; z2=z1[,+]>0;
	   i=loc(z2); %if &debug eq Y %then %do; print z1 i; %end;
   %END;

   %if %upcase(&METHOD) EQ MHL %THEN %DO; 
      trimmed=t(1:nrow(data));
	   df=ncol(data); b=data;
	   do pass=1 to &PASSES;
			X=t(b); 
			n=ncol(X); 
			p=nrow(X); 
			O=j(n,1,1); 
			XMEANS=(1/n)*X*O*t(O); 
			XRESID=X-XMEANS;
			S=(1/(n-1))*(X-XMEANS)*t(X-XMEANS);
			SINV=inv(S);
			BIG=t(XRESID)*SINV*XRESID; 
			V=vecdiag(BIG);
			prob=1-probchi(V,df); z2=prob < &PVALUE; 
			if z2[+]>0 then do;                       /* check outliers, if any  */
            pout=loc(z2);                          /* pointer for outliers    */     	
            out=trimmed[pout];                     /* outliers rows vector    */
            if pass=1 then i=out; else i=i//out;   /* add previous vector     */
            ptrim=loc(1-z2);                       /* pointer for trimmed obs */  
            trimmed=trimmed[ptrim,1];              /* trimmed rows vector     */ 	
				b=data[trimmed,];                         /* selected obs matrix     */  
				cc=prob[pout];
          end;
		end;
      %if &debug eq Y %then %do; print prob i; %end;
   %END;

   outlier=repeat(' ',nrow(data)); 
   if ncol(i) > 0 then outlier[i]='Y'; 

   create OUTNEW var {outlier};
   append;
   close OUTNEW;

   %IF &QQPLOT NE %THEN %DO; 
		run qqplot (data,i);
	%END;

quit;

%let rc = 0;
%let rc = %sysfunc(exist(&OUT));

%if &RC EQ 0 %THEN %LET NTEST=1;
%ELSE              %LET NTEST=%EVAL(&NTEST+1);

%IF &TEST EQ %THEN  %LET TEST=%TRIM(&METHOD); 

data &OUT;
   merge &DATA 
   %IF &RC NE 0 %THEN %DO; 
         &OUT(keep=TEST: ) 
   %END; 
         OUTNEW (rename=(outlier=test&NTEST));
	label test&NTEST="&TEST";
run;

proc datasets lib=work nolist; delete outnew; quit;

%mend outliers ;
