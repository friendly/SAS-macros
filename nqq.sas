%macro nqq(
	data=_last_,
	var=,
	mu=median,
	sigma=hspr,
	stderr=yes,
	detrend=yes,
	out=nqq,
	name=nqq
	);

%let stderr=%upcase(&stderr);
%let sigma=%upcase(&sigma);
%let detrend=%upcase(&detrend);
%if &sigma=HSPR    %then %let std=hspr/1.349;
	%else %let std=std;


data pass;
  set &data;
  _match_=1;
  if &var ne . ;                * get rid of missing data;
 
proc univariate noprint;        * find n, median and hinge-spread;
   var &var;
   output out=n1 n=nobs median=median qrange=hspr mean=mean std=std;
 
proc sort data=pass;
   by &var;
run;
 
data &out;
   set pass;
   if _n_=1 then do; 
    	set n1;
		sigma = &std;
		mu = &mu;;
		call symput("mu", put(mu, best8.));
		call symput("sigma", put(sigma, best8.));
		retain mu sigma;
		end;
   drop sigma hspr nobs median std mean ;

   _p_=(_n_ - .5)/nobs;                 * cumulative prob.;
   _z_=probit(_p_);                     * unit-normal Quantile;
   _se_=(sigma/((1/sqrt(2*3.1415926))*exp(-(_z_**2)/2)))
      * sqrt(_p_*(1-_p_)/nobs);          * std. error for normal quantile;
  _normal_= sigma * _z_ + &mu ;         * corresponding normal quantile;
   _res_ = &var - _normal_;           * deviation from normal;
   _lower_ = _normal_ - 2*_se_;         * +/- 2 SEs around fitted line;
   _upper_ = _normal_ + 2*_se_;
   _reslo_  = -2*_se_;                  * +/- 2 SEs ;
   _reshi_   = 2*_se_;
  label _z_='Normal Quantile'
        _res_='Deviation From Normal';
  run;

/*
%if &sigma=HSPR 
	%then %let sigma=qrange(&var)/1.349;
	%else %let sigma=stddev(&var); 
*/

proc template;
   define statgraph MyStat.QQPlot;
      notes "QQPlot";
	  mvar mu sigma;
	  dynamic var varlabel;
/*
      layout lattice / rows=1 columns=1;
         sidebar / align=top;
            layout overlay / padbottom=5;
*               entry _MODELLABEL / halign=left valign=top;
               layout gridded / columns=3 valign=top;
                  entrytitle "Normal Quantiles";
                  entrytitle "for";
                  entrytitle "&var";
               endlayout;
            endlayout;
         endsidebar;
*/
         layout overlay / yaxisopts=( label="&var" ) xaxisopts=( label="Normal Quantile" );
            lineparm slope=eval (sigma) yintercept=eval(mu) / 
			   extreme=true 
			   linecolor= GraphReferenceLines:foreground 
			   linethickness= GraphReferenceLines:linethickness 
			   linepattern= GraphReferenceLines:linestyle;
			band x=_z_  
				ylimitupper=_upper_ 
				ylimitlower=_lower_ /
				extreme=true
				fill=true
				lines=false
				fillcolor=pink;
/*
            scatterplot y=&var x=eval
               (PROBIT((NUMERATE(SORT(DROPMISSING(&var)))-0.375)/(0.25+N(&var)))) / 
*/
            scatterplot y=&var x=_z_ / 
			   markersymbol= GraphDataDefault:markersymbol 
			   markercolor= GraphDataDefault:contrastcolor 
			   markersize= GraphDataDefault:markersize;
         endlayout;
*      endlayout;
   end;
run;

ods latex style=minimal stylesheet="sas.sty" (url="sas");
ods graphics on / reset imagefmt=ps imagename="&name";

data _null_;
	set &out;
	file print ods=(template="MyStat.QQPlot", dynamic=(var="&var"));
	put _ods_;
	run;

ods latex close;
ods graphics off;

%mend;
