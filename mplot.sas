*version(6.08);
%macro mplot(data=_last_,
	response=,
	factor=,
	method=bon,
	 alpha=.05);
proc summary data=&data nway;
   class &factor;
   output out=_sum_ mean=mean;
   var &response;
 
proc glm data=&data outstat=_stat_;
   class &factor;
   model &response = &factor;
   means &factor / &method lines;
   lsmeans &factor  / stderr out=_means_;
 
data _null_;
   set _stat_;
   if _source_='ERROR' then do;
      dfe = df;
      mse = ss/dfe;
      call symput('mse', put(mse,best8.));
      call symput('df' , put(dfe,best8.));
      end;
 
data _means_;
   merge _sum_(keep=_freq_ &factor) _means_;
   by &factor;
   drop _name_ lsmean;
   &response = lsmean;
proc sort data=_means_;
   by &response;
proc print data=_means_;
 
proc iml;
   use _means_;
   read all into  response var { &response };
   read all into  factor   var { &factor   };
   read all into  n        var { _freq_    };
 
   if type(factor) = 'N'
      then id=char(factor);
      else id=factor;
   r = nrow(response);
   g = r#(r-1)/2;
   if upcase("&method") = 'TUKEY'
      then q  = probmc('range', ., 1-&alpha, &df, r) / sqrt(2);
      else q  = tinv ( 1-((&alpha)/(2#g)), &df);
 
   s = sqrt(&mse);
 
   do i=1 to r;
      diff = diff // (response[i] - response`);
      plus = plus // (response[i] + response`);
      v = sqrt((1/n[i]) + 1/n);
      half = half // (q # s # v`);
      end;
   print 'Mean Differences' diff[r=id c=id];
   print 'Half Widths     ' half[r=id c=id];
 
start meanplt;
    clo  = response[><];
    chi  = response[<>];
    do i=1 to r;
       from = from // (clo-response[i] || clo+response[i]);
       to   = to   // (chi-response[i] || chi+response[i]);
       labl = labl // id[i];
       end;
    do j=1 to r;
       to   = to   // (response[j]-clo || response[j]+clo);
       from = from // (response[j]-chi || response[j]+chi);
       labl = labl // id[j];
       end;
 
   l =  1 - symsqr(I(nrow(diff)));   /* remove diag elements */
   x =  (symsqr(diff))[loc(l)];
   y =  (symsqr(plus))[loc(l)];
   h =  (symsqr(half))[loc(l)];
   rr=  r#(r-1);
   plt = (x || y) // ((x-h)||y) // ((x+h)||y);
   barf = ((x-h)||y) ;
   bart = ((x+h)||y);
   plt = plt // from // to;
/*
   sym = repeat('+', rr, 1)        //
         repeat('<', rr, 1)        //
         repeat('>', rr, 1)        //
         repeat('›', nrow(from),1) //
         repeat('/', nrow(to),1) ;
   call pgraf(plt,sym,'Difference in Means');
*/
   call gstart;
   call gopen;
*  call gxyscale(plt[,1],plt[,2],window);
   call gxyplot(plt[,1],plt[,2]);
 
   call gset('height',1.3);
   call gpoint (x,y);
   call gdrawl( from, to,   3,"RED  ");
   call gset('thick', 5);
   call gdrawl( barf, bart, 1,"BLACK");
   call gset('thick');
 
   zero = (0||plt[<>,2]) // (0||plt[><,2]);
   call gdrawl( zero[1,], zero[2,]) style=1  color={"BLACK"};
 
    /*----------------------------------------*
     | Plot row and column labels at margins; |
     *----------------------------------------*/
   call gstrlen(len, labl);
   xoffset=.05 * (to[<>,1]-to[><,1]);
   xoffset=len;
   yoffset=(len/2) # (repeat({-1},r,1) // repeat({1},r,1));
   xt = from[1:(2#r),1] - xoffset;
   yt = from[1:(2#r),2] + yoffset;
   call gtext( xt, yt, labl );
*  print len xt yt labl;
   call gshow;
   finish;
 
start gxyscale(x,y,window);
    call gport({10 10, 90 90});
    call gscale( yscale, y, 5);
    call gscale( xscale, x, 5);
    window = xscale[1:2] || yscale[1:2];
    origin = window[,1];
    len = xscale[2]-xscale[1];
    ntick = 1+ len/xscale[3];
    call gxaxis( {10 10}, 80, ntick , ,,, '5.1' );
    xl = .4#len;
    yl = yscale[1] - .05#(yscale[2]-yscale[1]);
    call gscript(xl,yl,'Difference in means',,3);
    call gwindow(window);
 print xscale,yscale,window;
/*
    call gyaxis( {10 10}, 80, y, 5, 0, '5.0') ;
 
*/
finish;
start gxyplot (x,y);
 
 /*---find world---*/
 world = min(x) || min(y) // (max(x) || max(y));
 
 leng = abs(world[2,] - world[1,]);
 
 /*---scale up world by 20% in all directions---*/
 window = world + 0.2 * {-1 , 1} * leng;
 
 /*---origin 5% below minx, miny---*/
 origin = world[1,] - 0.05 * leng;
 
 /*---axis length 5% extra over range on both ends---*/
 alen = 1.1 * leng;
 
 call gwindow (window);
 
 /*---draw axes---*/
 call gxaxis (origin, alen[1], 5,,,'5.1') ;
 call gyaxis (origin, alen[2], 5,,,'5.1') ;
 
finish;
   run meanplt;
reset fw=6;
   print from to labl;
quit;
run;
%mend;
