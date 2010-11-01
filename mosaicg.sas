/* 
title:  Mosaic displays with desmat/genmod
*/

%macro mosaicg(
   data=_last_,     /* Name of input dataset           */
   var=,            /* Names of factor variables       */
   count=count,     /* Name of the frequency variable  */
   weight=,         /* Weight variable                 */
   model=,          /* Model terms                     */
   para=,           /* Parameterizations               */
   print=fitall bic,
   resid=stresdev,
   shade=2 4,       /* shading levels for residuals    */
   colors=blue red, /* colors for + and - residuals    */
   fill=HLS HLS,    /* fill type for + and - residuals */
   split=V H,       /* split directions                */
   vorder=,         /* order of variables in mosaic    */
   htext=1.5,       /* height of text labels           */
   font=,           /* font for text labels            */
   title=,          /* title for plot(s)               */
   space=,          /* room for spacing the tiles      */
   cellfill=,
   vlabels=,        /* Number of variable names used as plot labels */
	out=obstats
   );

proc iml;
    %desmat(data=&data,model=&model,para=&para);
    %genmod(data=&data,count=&count,obstats=obstats, weight=&weight);
show storage;

%*-- Use a separate IML step to avoid running out of storage;
proc iml  symsize=256 ;
   reset storage=mosaic.mosaic;
   load module=_all_;

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

   vnames = str2vec("&var");    *-- Preserve case of var names;

   %*-- Read and reorder counts;
   run readtab("obstats","&count", vnames, table, levels, lnames);
   %if %length(&vorder) %then %do;
      vorder  = { &vorder };
      run reorder(levels, table, vnames, lnames, vorder);
      %end;

   %*-- Read and reorder residuals if specified;
   %if %length(&resid)>0 %then %do;
      vn = vnames;
      run readtab("obstats","&resid", vn, dev,   lev, ln);
		if any(dev = .) then dev[loc(dev=.)] = 0;
      %if %length(&vorder) %then %do;
         vorder  = { &vorder };
         *-- marg bug workaround: subtract min value, then add back in;
*         reset print;
         mdev = min(dev);
         dev = dev - mdev;
         run reorder(lev, dev, vn, ln, vorder);
         dev = dev + mdev;
*         reset noprint;
         %end;
   %end;

   shade={&shade};
   colors={&colors};
   filltype={&fill};
   split={&split};

   htext=&htext;
   title = "&title";

   %if %length(&space)>0    %then %do; space={&space};  %end;
   %if %length(&font)>0     %then %do; font = "&font";   %end;
   %if %length(&vlabels)>0  %then %do; vlabels=&vlabels; %end;
   %if %length(&cellfill)>0 %then %do; cellfill="&cellfill";  %end;

	%include mosaics(mosaicd);
	run mosaicd(levels, table, vnames, lnames, dev, title); 
%mend;
