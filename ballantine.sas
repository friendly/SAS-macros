* Draw ballantine diagram for three variables;
* From Earl Hunt,
  The design of ballantines
  Behav. Research Methods, Instru. & Computers, 1986 18:3, 277-284 article;

**-- Enter the three correlations here;

%include data(therapy);
proc corr data=therapy nosimple;
	var therapy perstest intext;

*include goptions;
goptions vsize=7 in hsize=7 in;


%gsize(hsize=6, vsize=6, options=htext=1.8);

**-- Enter the three correlations here;
%let ryx1 = .60;
%let ryx2 = .77974;
%let rx1x2 = .0513;
**-- Names of the three variables, response var in the middle;
%let vars ='X1' 'Y' 'X2';

%macro ballantine(data=_last_, y=, x1=, x2=, 
	colors=red black green,
	patterns = m2n45 empty m2n135
	);
	

proc iml;
   reset fw=6;

start centdist( r );
   *-- Compute distance between centers of circles from rsq;
 
   if r = 1 then return( 0 );         /* complete overlap     */
   else if r = 0 then return( 2 );    /* completely disjoint  */
   else do;                           /* intersecting circles */
      pi = 3.14159;
      lo = 0;  hi = pi;     /* angle theta, 0 <= theta <= pi radians */
      q = r # pi;           /* sector area of overlap */
      old = 0;
      alpha = pi/2;
      delta = 1;
      iter=0;
      do until (delta < .001);
         iter = iter + 1;
         if iter > 20 then goto done;
         z = alpha - sin(alpha);
         if z = q then delta = 0;
         else do;          /* adjust */
            old = alpha;
            if z > q then do;
               alpha = alpha - (alpha-lo)/2;
               hi = old;
            end;
            else do;
               alpha = alpha + (hi-alpha)/2;
               lo = old;
            end;
            delta = abs(old-alpha);
         end;
      end;
done:
*  print iter alpha delta lo hi;
   alpha = alpha/2;
   dist = 2 # cos(alpha);
   end;
   return( dist );
finish;
 
start partial( r12, r13, r23 );
   *-- find partial correlation between vars 1&2, partialling 3;
   r = (r12 - r13#r23) / sqrt( (1-r13##2)#(1-r23##2) );
   return(r);
finish;
 
start spartial( r12, r13, r23 );
   *-- semi-partial correlation between vars 2&3, partialling 1 from 3;
   r = (r23 - r13#r12) / sqrt( (1-r13##2) );
   return(r);
finish;
 
start bardiag( lx, ly, wx, wy, p, label);
  lp= 0 || cusum(p) || 1;
  print 'Bar diagram proportions of variance', p, lp;
  x = lx + wx # lp;
  y = ly + wy # { 0 1 };
  call ggrid (x, y);
  tp = "0.0" ||char(cusum(p),3,2) || "1.0";
  call gstrlen(len,tp);
  xl = x-len/2;
  if p[2]<.05 then do;
     xl[3]=xl[2]+len[2]/2;
     xl[2]=xl[2]-len[2]/2;
     end;
  call gscript(xl, ly-3, tp);
 
  call gscript(x[1],y[2]+1,'% Variance Accounted for');
  call gstrlen(len,label);
  do i=1 to 1+ncol(p);
     xx = (x[i]+x[i+1]-len[i])/2;
     yy = (y[1]+y[2])/2 - 1;
     call gscript(xx, yy, label[i]);
     end;
finish;
 
start twocirc(cx, cy, radius, r, labels, colors, patterns);
   *-- Draw two circles showing partial relations;
   rsq = r##2;
   d = centdist( rsq ) # radius;
   x  = (cx - d/2)//
        (cx + d/2);
   y  = cy // cy;
   print 'Circle centers', x y radius rsq;
   call gpie(x, y, radius, 0, 360, colors, , patterns);
   call gpiexy(lx1, ly1, 1.25, 240, x[1]||y[1], radius);
   call gpiexy(lx2, ly2, 1.25, 300, x[2]||y[2], radius);
   call gstrlen(len,labels);
   lx = lx1-len[1] // lx2;
   ly = ly1 // ly2;
   call gscript(lx, ly, labels);
finish;
 
start threcirc(cx, cy, radius, r, labels, colors, patterns);
   *-- Draw three circles showing pairwise correlations;
   rsq = r##2;
   d =(centdist( rsq[1] ) //
       centdist( rsq[2] ) //
       centdist( rsq[3] )) ;
print d;
*reset print;
/*
   s = sum( d )/2;
   v = ((s-d)[#])/s;
   v = sqrt(v);
   xx = s - d[1];
   theta = 2 # atan( v/xx);
*/
   dsq = d##2;
 
   cosa =  min(1,( {1 1 -1}*dsq / (2#d[1]#d[2]) ));
   theta = arcos( cosa );

   *print theta (theta#45/atan(1));
   dy = sin(theta)#d[2];
   dx = cos(theta)#d[2];
   cc = j(3,2,0);
   cc[1,1] = cx - radius # d[1]/2;
   cc[1,2] = cy + radius # dy/2;
   cc[2,1] = cx + radius # d[1]/2;
   cc[2,2] = cc[1,2];
   cc[3,1] = cc[1,1] + radius # dx;
   cc[3,2] = cc[1,2] - radius # dy;
   print 'Circle centers', cc[r=labels c={X Y}];
   call gpie(cc[,1], cc[,2], radius, 0, 360, colors, , patterns);
   call gpoint(cc[,1], cc[,2], 'point');
   ang= choose(cosa<.95,{120 60 270},
                        {270 90 270});
   call gpiexy(lx1, ly1, 1.25, ang[1], cc[1,], radius);
   call gpiexy(lx2, ly2, 1.25, ang[2], cc[2,], radius);
   call gpiexy(lx3, ly3, 1.25, ang[3], cc[3,], radius);
   call gstrlen(len,labels);
   lx = lx1 || lx2 || lx3;
   ly = ly1 || ly2 || ly3;
   lx = lx - len/2;
   call gscript(lx, ly, labels, 0,0);
   *print 'three circles label positions', lx, ly, labels;
finish;
 
start demo;
   cx = 20; cy=62; radius = 14;
   rsq = r##2;
   call gset('height',1.5);
   call gscript(12,95,'Balantine',0,0,2.0);

   colors = { 'CXFF00000080', 'black', 'CX0000FF80' };
   colors = { &colors };
   patterns = { &patterns};
   *patterns = {'m2n45', 'empty', 'm2n135'};
   run threcirc(cx, cy, radius, r, vars, colors, patterns);
 
   r23_1 = partial(r[3], r[3], r[1]);
   v_2 = trim(vars[2])+'.'+vars[1] || trim(vars[3])+'.'+vars[1] ;
   cx = 75; cy=75;
   rad = radius # (1-rsq[3]);
   run twocirc(cx, cy, rad, r23_1, v_2, colors[{1 3}], patterns[{1 3}]);
 
   r12_3 = partial(r[1], r[2], r[3]);
   v_3 = trim(vars[2])+'.'+vars[3] || trim(vars[1])+'.'+vars[3] ;
   cx = 75; cy=40;
   rad = radius # (1-rsq[1]);
   run twocirc(cx, cy, rad, r12_3, v_3, colors[{1 3}], patterns[{1 3}]);
   call gscript(60,95,'Partial Relations',0,0,2.0);
 
   p = spartial(r[1], r[2], r[3]);
   p = rsq[1] || p##2;
   label = vars[1] || trim(vars[3])+'.'+vars[1] || 'Error';
   call bardiag(5,3, 85,10, p, label);
   call gshow;
finish;
 
start cor (x);
  d = dev(x);                      * correct for means;
  xpx=t(d)*d;                      * crossproduct;
  v = vecdiag(xpx);                * diagonal values;
  v = 1/sqrt(choose(v=0,.,v));     * account for constants;
  v = choose(v=.,0,v);
  corr= diag(v) * xpx * diag(v);   * correlation matrix;
  return(corr);
finish;

start dev(x);
*   d = x - repeat(x[:,], nrow(x), 1);
   return(x - repeat(x[:,], nrow(x), 1));
   finish dev;

start nomiss(y, X, obsnames);
   *-- Remove rows with any missing data from matrix and obsnames;
   *   (pass ' ' for obsnames if no row labels are present);
	matrix = y || X;
   miss = loc(matrix <=.Z);
   if type(miss)='U'
      then return;           /* no missing data */
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
            end;
         end;
			y = matrix[,1:ncol(y)];
			X = matrix[,(1+ncol(y)):ncol(matrix)];
finish;

*   r = {.60 .0513 .77974};
*    vars={"PersTest" "Therapy" "IntExt"};

	r = { &ryx1  &rx1x2 &ryx2 };
	vars = {&vars};
 
   r = {.60 .0513 .77974};
    vars={"PersTest" "Therapy" "IntExt"};

*  r   = { .47 .42 .30 };         *-- Example from paper ;
*  vars= { X1 Y X2 };
 
   use &data;
   read all var{&y &x1 &x2} into  x[ colname=vars ];
   obs = char(1:nrow(x),3,0);
   y = x[,1];
   X = x[,2:3];
   run nomiss(y, X, obs);
   x = y || X;
   Ryx = cor(X);
   print Ryx [r=vars c=vars];
   
   r = Ryx[1,2] || Ryx[2,3] || Ryx[1,3];
   
   call gstart;
   call gwindow({-5 -5 , 104 104});
   run demo;
 
   call gwindow({0 0 , 100 100});
   vars= { X1 Y X2 };
   cx = 55; cy=58; radius = 20;
   r   = { .50 .0 .30 };
   rsq = r##2;
   call gopen;
   call gset('height',3.2);
   run threcirc(cx, cy, radius, rsq, vars, colors, patterns);
   call gscript(30, 8,'X1, X2 Independent',0,0,3.5);
   call gshow;
 
   r   = { .50 .40 .30 };
   cx = 40;
   rsq = r##2;
   call gopen;
   call gset('height',3.2);
   run threcirc(cx, cy, radius, rsq, vars, colors, patterns);
   call gscript(22, 8,'X1, X2 Correlated',0,0,3.5);
   call gshow;

quit;
%mend;

%ballantine(data=therapy, y=Therapy, x1=PersTest, x2=IntExt);
