/*
Overlay a series of graphs
*/

%macro goverlay(
	n=,
	graphs=,
	at=,
	size=,
   gin=gseg,              /* name of input graphic catalog              */
   gout=gseg,              /* name of output graphic catalog             */
   name=goverlay
	);
%mend;


%macro goverlay0(
	n=,
	graphs=,
	at=,
	size=,
   gin=gseg,              /* name of input graphic catalog              */
   gout=gseg,              /* name of output graphic catalog             */
   name=goverlay
	);

%let graphs = %quotelst(&graphs);
%put graphs=&graphs;

data _null_;

	array at{&n, 2}     _temporary_ (&at);
	array size{&n, 2}   _temporary_ (&size);
	array graphs {&n} $ _temporary_ (&graphs); 
	
    /* set the catalog to the one you are working with */
    rc = gset('catalog', 'work', "&gin");
    rc = ginit();
	n = &n;
    *call gask('graphlist', n, of graphs{*}, rc);
    /* open the matrix graph */
    rc = graph('clear', "&name", "overlay graph");
	
	do i=1 to n;
		x1 = at[i,1];
		y1 = at[i,2];
		x2 = x1+size[i,1];
		y2 = y1+size[i,2];
		put x1= y1= x2= y2=;

        rc = gset('viewport', i, x1, x2, y1, y2);
        rc = gset('window', i, 0, 0, 100, 100);
        rc = gset('transno', i);
        rc = graph('insert', graphs{i});

		end;
%mend;

*options mprint;

*goverlay(n=3,
	at=0 0 30 30 50 50,
	size= 10 10 20 20 30 30,
	graphs = 1 2 plot1
	);
	
		
