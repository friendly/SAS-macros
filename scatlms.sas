%macro scatlms(
	data=_last_,
	x=,
	y=,
	show=LS LMS,
	);

proc iml;

  /*------------------------------------------------------------*/

start scatlms(show, opt, x, y);
	
	noint = opt[1]; 
	nobs = nrow(x); p = ncol(x);
	if nrow(y) ^= nobs | ncol(y) ^= 1 then do;
		print "Data vectors x and y not compatible";
		goto leave;
	end;
   ils = 0; iwls = 0; ilms = 0; ilts = 0; il1 = 0;
	ils  = any(show = 'LS');
	iwls = any(show = 'WLS');
	ilts = any(show = 'LTS');
	ilms = any(show = 'LMS');
	il1  = any(show = 'L1');

	if (ilms) then do;
		/*--- 1. Compute LS and LMS ---*/
		opt[3]= 0 + 2#ils;    /* ilsq:  LMS + LS? */
		CALL LMS(sc,coef,wgt,opt,y,x);
		print "End of LMS", coef;

		kls= 1; b = coef[3,1];
		if noint=0 then a = coef[3,2]; else a = 0.;
		nr = nr + 1; nreg[nr]= "LS";
		vreg[nr,1]= xmin1 * b + a;
		vreg[nr,2]= xmax1 * b + a;

		klms=1; b = coef[1,1];
		if noint=0 then a = coef[1,2]; else a = 0.;
		nr = nr + 1; nreg[nr]= "LMS";
		vreg[nr,1]= xmin1 * b + a;
		vreg[nr,2]= xmax1 * b + a;


leave:
	finish;

  start scatlms0(job,optn,x,y,xnam,ynam,titl,ipsf,filn);

   /* job = 0: only draw scatter plot without regression
          = 1: LS
          = 2: LS + LMS
          = 3: LS + LMS + WLS
          = 4: LS + LTS
          = 5: LS + LTS + WLS
          = 6: LS + LMS + LTS
          = 7: LS + LMS + LTS + WLS (WLS same LMS and LTS)
          * the next 8-14 are 1-7 but with L1 regression *
          = 8: LS + L1
          = 9: LS + L1 + LMS
          =10: LS + L1 + LMS + WLS
          =11: LS + L1 + LTS
          =12: LS + L1 + LTS + WLS
          =13: LS + L1 + LMS + LTS
          =14: LS + L1 + LMS + LTS + WLS (WLS same LMS and LTS)
     optn = options for LMS / LTS call

      ipsf: specifies GOPTIONS statement:
      ipsf=0: draw interactively
      ipsf=1: write file with smaller picture for document
      ipsf=2: write file with larger picture for color printer
      filn: name for graphic segment */

      start gopt(ipsf,filn);
        if ipsf then do;
         fnam = (filn || ".ps");
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
/*         call execute('goptions reset=goptions dev=xcolor;'); */
        end;
      finish gopt;

      noint = optn[1]; nobs = nrow(x); n = ncol(x);
      if nrow(y) ^= nobs | ncol(y) ^= 1 | n ^= 1 then do;
        print "Data vectors x and y not compatible";
        goto leave;
      end;

      /* get [xmin,xmax] and [ymin,ymax] */
      xmin1 = x[><]; xmax1 = x[<>];
      xd1 = xmax1 - xmin1; xc1 = .1 * xd1;
      xmin2 = xmin1 - xc1; xmax2 = xmax1 + xc1;
      xd2 = xmax2 - xmin2; xc2 = .1 * xd2;
      xme = .5*(xmin2+xmax2);

      ymin1 = y[><]; ymax1 = y[<>];
      yd1 = ymax1 - ymin1;

      ils = 0; iwls = 0; ilms = 0; ilts = 0; il1 = 0;
      if job > 0 then ils = 1;
      if job = 3 | job = 5 | job = 7 then iwls = 1;
      if job =10 | job =12 | job =14 then iwls = 1;
      if job = 2 | job = 3 | job = 6 | job = 7 then ilms = 1;
      if job = 9 | job =10 | job =13 | job =14 then ilms = 1;
      if job = 4 | job = 5 | job = 6 | job = 7 then ilts = 1;
      if job =11 | job =12 | job =13 | job =14 then ilts = 1;
      if job > 7 & job < 15 then il1 = 1;
      /* print "LS=" ils "WLS=" iwls "L1=" il1; */
      /* print "LMS=" ilms "LTS=" ilts; */

      /* enumerate outlier points if either LMS or LTS */
      klmts = 0;
      if job = 2 | job = 3 | job = 9 | job =10 then klmts = 1;
      if job = 4 | job = 5 | job =11 | job =12 then klmts = 2;

      nr = 0;
      kls = 0; kwls = 0; klms = 0; klts = 0; kl1 = 0;
      vreg = j(10,2,0); nreg = j(10,1,"        ");
      if job then do;

        /* A. Do LMS and LTS */
        if (ilms) then do;
          /*--- 1. Compute LS and LMS ---*/
          optn[3]= 2;    /* ilsq: LS and LMS */
          /* print "LS and LMS"; */
          CALL LMS(sc,coef,wgt,optn,y,x);
          /* print "End of LMS" coef; */

          kls= 1; b = coef[3,1];
          if noint=0 then a = coef[3,2]; else a = 0.;
          nr = nr + 1; nreg[nr]= "LS";
          vreg[nr,1]= xmin1 * b + a;
          vreg[nr,2]= xmax1 * b + a;

          klms=1; b = coef[1,1];
          if noint=0 then a = coef[1,2]; else a = 0.;
          nr = nr + 1; nreg[nr]= "LMS";
          vreg[nr,1]= xmin1 * b + a;
          vreg[nr,2]= xmax1 * b + a;

          if ilts then do;
            /*--- 2. Compute LTS and WLS ---*/
            if iwls then optn[3]= 1;    /* ilsq: LTS and WLS */
                    else optn[3]= 0;
            /* print "LTS and WLS"; */
            CALL LTS(sc,coef,wgt,optn,y,x);
            /* print "End of LTS" coef; */

            klts=1; b = coef[1,1];
            if noint=0 then a = coef[1,2]; else a = 0.;
            nr = nr + 1; nreg[nr]= "LTS";
            vreg[nr,1]= xmin1 * b + a;
            vreg[nr,2]= xmax1 * b + a;

            if iwls then do;
              kwls=1; b = coef[3,1];
              if noint=0 then a = coef[3,2]; else a = 0.;
              nr = nr + 1; nreg[nr]= "WLS";
              vreg[nr,1]= xmin1 * b + a;
              vreg[nr,2]= xmax1 * b + a;
            end;
          end; else if iwls then do;
            /*--- 3. Compute LMS and WLS: if no LTS is asked ---*/
            optn[3]= 1;    /* ilsq: LMS and WLS */
            /* print "LMS"; */
            CALL LMS(sc,coef,wgt,optn,y,x);
            /* print "End of LMS" coef; */

            kwls=1; b = coef[3,1];
            if noint=0 then a = coef[3,2]; else a = 0.;
            nr = nr + 1; nreg[nr]= "WLS";
            vreg[nr,1]= xmin1 * b + a;
            vreg[nr,2]= xmax1 * b + a;
          end;
        end; else if ilts then do;

          /* 4. Do LS and LTS: if no LMS is asked */
          optn[3]= 2;    /* ilsq: LS and LTS */
          /* print "LS and LTS"; */
          CALL LTS(sc,coef,wgt,optn,y,x);
          /* print "End of LTS" coef; */

          kls= 1; b = coef[3,1];
          if noint=0 then a = coef[3,2]; else a = 0.;
          nr = nr + 1; nreg[nr]= "LS";
          vreg[nr,1]= xmin1 * b + a;
          vreg[nr,2]= xmax1 * b + a;

          klts=1; b = coef[1,1];
          if noint=0 then a = coef[1,2]; else a = 0.;
          nr = nr + 1; nreg[nr]= "LTS";
          vreg[nr,1]= xmin1 * b + a;
          vreg[nr,2]= xmax1 * b + a;

          if iwls then do;
            /*--- 3. Compute LTS and WLS: if no LMS is asked ---*/
            optn[3]= 1;    /* ilsq: LMS and WLS */
            CALL LTS(sc,coef,wgt,optn,y,x);

            kwls=1; b = coef[3,1];
            if noint=0 then a = coef[3,2]; else a = 0.;
            nr = nr + 1; nreg[nr]= "WLS";
            vreg[nr,1]= xmin1 * b + a;
            vreg[nr,2]= xmax1 * b + a;
          end;
        end;

        /* B. Do L1 and leftover LS */
        if il1 then do;
            l1opt = j(2,1,0); l1opt[1]=1000;
            if noint=0 then xx = x || j(nobs,1,1.);
                       else xx = x;
            CALL LAV(rc,coef,xx,y,,l1opt);

            kl1=1; b = coef[1,1];
            if noint=0 then a = coef[1,2]; else a = 0.;
            nr = nr + 1; nreg[nr]= "L1";
            vreg[nr,1]= xmin1 * b + a;
            vreg[nr,2]= xmax1 * b + a;
        end;
        if ils && kls=0 then do;
          if noint=0 then do;
            xx = x || j(nobs,1,1.);
            sxx = xx` * xx;
            sxy = xx` * y;
            xls = inv(sxx) * sxy;
            kls=1; b = xls[1]; a = xls[2];
           end; else do;
            sxx = x` * x; sxy = x` * y;
            kls=1; b = sxy / sxx; a = 0.;
          end;
          nr = nr + 1; nreg[nr]= "LS";
          vreg[nr,1]= xmin1 * b + a;
          vreg[nr,2]= xmax1 * b + a;
        end;

        /* correct ymin and ymax */
        do ir=1 to nr;
          a = vreg[ir,1]; b = vreg[ir,2];
          if a < ymin1 then ymin1 = a;
          if b < ymin1 then ymin1 = b;
          if a > ymax1 then ymax1 = a;
          if b > ymax1 then ymax1 = b;
        end;
      end;
      /* print "NR=" nr; */

      /* now with corrected ymin and ymax */
      yd1 = ymax1 - ymin1; yc1 = .1 * yd1;
      ymin2 = ymin1 - yc1; ymax2 = ymax1 + yc1;
      yd2 = ymax2 - ymin2; yc2 = .1 * yd2;
      yme = .5*(ymin2+ymax2);

      /* No Intercept: regression through origin;
                       start axis with zero */
      if noint^=0 & xmin1 > 0 & xmin2 < xd2
                              & ymin2 < yd2 then do;
        xmin1 = 0.; xmin2 = 0.;
        xd2 = xmax2 - xmin2; xc2 = .1 * xd2;
        xme = .5*(xmin2+xmax2);
        ymin1 = 0.; ymin2 = 0.;
        yd2 = ymax2 - ymin2; yc2 = .1 * yd2;
        yme = .5*(ymin2+ymax2);
        do ir=1 to nr;
          vreg[ir,1] = 0;
        end;
      end;

      /* prepare the plot */
      xbox = { 0 100 100   0};
      ybox = { 0   0 100 100};
      wind = (xmin2 || ymin2) // (xmax2 || ymax2);

      /* create the plot */
*      call gopt(ipsf,filn);
      call gstart;
      call gopen(filn);
      call gset("font","swiss"); /* set character font */
      call gpoly(xbox,ybox);     /* draw a box around plot */
      call gwindow(wind);        /* define window */
      call gport({15 15 , 85 85}); /* define viewport */
      call gxaxis((xmin2 || ymin2),xd2,11, , ,"4.",1.);
      call gyaxis((xmin2 || ymin2),yd2,11, , ,"4.",1.);

      /* plot xnam, ynam variable names of axis and the title */
      call gset("height",1.5);
      call gstrlen(len,xnam);
      call gtext(xmax2-len,ymin2-yc2,xnam);
      call gvtext(xmin2-xc2,ymax2,ynam);
      call gset("height",2.0);
      call gscenter(xme,ymax2,titl);

      /* draw regression lines */
      xa = xmin1; xb = xmax1;
      do ir=1 to nr;
        ya = vreg[ir,1]; yb = vreg[ir,2];
        call gdraw((xa || xb),(ya || yb));
        call gscript(xb,yb,nreg[ir]);
      end;

      /* print the scatter points */
      call gpoint(x,y,"dot","magenta",1.5);

      /* enumerate the zero weight points */
      if klmts>0 then do;
       do i=1 to nobs;
        if wgt[i] = 0 then do;
         z = char(i,2,0);
         nzwx = x[i] + .01 * xd2;
         nzwy = y[i];
         call gscript(nzwx,nzwy,z,,,1.);
      end; end; end;

      call gshow;
      call gclose;
      call gstop;
   leave:
  finish;
