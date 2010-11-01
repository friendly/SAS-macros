%macro dsquare(
       data=_LAST_,
       var=,
       id=,
       out=dsq);
proc iml;
reset;
start dsquare;
   use &data ;
   read all var {&var} into  X[ colname=vars rowname=&id ];
 
   n = nrow(X);
   p = ncol(X);
   rl= &id;
 
   *---- Compute covariance matrix of X;
   M = X[ : , ];             *-- means ;
   D = X - J( n , 1) * M;
   S = D` * D / ( n - 1 );   *-- var-cov matrix ;
 
   *---- Calculate Mahanalobis distances;
   dsq = vecdiag( D * inv(S) * D`);
 
   *---- Rank them in ascending order;
   r = rank( dsq);
   val = dsq;  dsq [ r , ] = val;
   val = rl;   &id [ r ] = val;
 
   *---- Calculate Chi-square percentiles;
   z = ( (1:n)` - .5 ) / n ;
   chisq = 2 * gaminv( z, p/2);
 
   *---- Join & output;
   result = dsq || chisq;
   cl = { 'DSQ' 'CHISQ'};
   create &out from result[ colname=cl rowname=&id];
   append from result [ rowname=&id];
finish;
 
run dsquare;
quit;
Proc Print data=&out;
   var &id dsq chisq;
/*---------------------------------------------------------------*
 |  If the sample comes from a multivariate normal distribution, |
 |  the plot of D-square vs. Chi-square percentiles should plot  |
 |  as a straight line.                                          |
 *---------------------------------------------------------------*/
 
proc gplot data=&out;
     plot dsq * chisq  /vaxis=axis1 vm=4
                        haxis=axis2 hm=4;
     symbol f=special v=K h=1.7 l=8 i=rl;
     axis1  label=(a=90 r=0 h=1.5 f=duplex) ;
     axis2  label=(h=1.5 f=duplex)
            offset=(1);
     label chisq = 'Chi-square quantile'
           dsq   = 'Mahalanobis D-square';
%mend;
