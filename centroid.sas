/*
Original from: Mike Zdeb, Maps Made Easy with SAS

*/

* MAP - NAME OF MAP DATA SET
* TYPE - VARIABLE THAT IDENTIFIES GEOGRAPHIC AREAS IN MAP (E.G. COUNTY)
;

%macro centroid(
	map,       /* name of map data set            */
	type,      /* name of region ID variable      */ 
	copy=,     /* map variables to copy to output */
	xc=xc,     /* name for X centroid variable    */
	yc=yc,     /* name for Y centroid variable    */ 
	minmax=0,  /* include XMIN, XMAX, YMIN, YMAX? */
	out=centers
	);

%let extra=;
*-- Create two data sets - number of points per area and points;
data
   _map_(drop=npoints)
   _points_(keep=x y npoints &copy rename=(x=xlast y=ylast));
   set &map;
   %if %length(&type) %then %do; 
	   by &type;
    %end;
   where x ne .;
   output _map_;
   npoints+1;

   if last.&type then do;
      output _points_;
      npoints=0;
   end;
run;

*-- Calculate centroids;
data
   &out(keep=&type x y &copy rename=(x=&xc y=&yc));
   retain savptr 1 xold yold 0;
   set _points_;

   xcg=0; ycg=0;
   aresum=0;
   firstpnt=1;
   endptr=savptr + npoints - 1;
   do ptrm=savptr to endptr;
      set _map_ point=ptrm nobs=nobsm;
      if firstpnt then do;
         xold=x; yold=y;
         savptr=ptrm + npoints;
         firstpnt=0;
      end;
      aretri=((xlast-x)*(yold-ylast)) + ((xold-xlast)*(y-ylast));
      xcg + (aretri*(x+xold));
      ycg + (aretri*(y+yold));
      aresum+aretri;
      xold=x; yold=y;
   end;
   areinv=1.0/aresum;
   x=(((xcg*areinv)+xlast) * (1/3));
   y=(((ycg*areinv)+ylast) * (1/3));
   output;
   label &xc = 'X centroid'
    &yc='Y centroid';
run;

%if &minmax %then %do;
	proc summary data=&map nway;
	  %if %length(&type) %then %do; 
		  class &type;
       %end;
		var x y;
		output out=_minmax_ min=xmin ymin max=xmax ymax;
	data &out;
		merge &out _minmax_(drop=_type_ _freq_);
	  %if %length(&type) %then %do; 
		  by &type;
       %end;
	%let extra=_minmax_;
	%end;


proc datasets nolist nowarn;
	delete _map_ _points_ &extra;
	run; quit;
%mend;
/*
%centroid(maps.states,state);
*/
