/*
Rescale graphics hsize, vsize, adjusting hpos and vpos accordingly,
to keep same aspect ratio for cell size, so that text does not get
deformed.
*/

%macro gscale(h=.,v=.,unit=in,verbose=1);

	%gask(unit=&unit);              %*-- Get current h/vsize, h/vpos;

	data _null_;
		h=&h;
		v=&v;
		hcell = &hsize / &hpos;      %*-- size of horizontal cell;
		vcell = &vsize / &vpos;      %*-- size of vertical cell;
		acell = hcell / vcell;       %*-- cell aspect ratio;
		verbose = &verbose;
		if verbose then put hcell= vcell= acell=;
		
		if h=. or h=&hsize then do;
			if v=. or v=&vsize then do;
				*-- nothing has changed;
				goto done;
				end;
			else do;
				*-- only vsize has changed;
				vsize = v;
				hsize = &hsize;
				end;
			end;
		else do;
			*-- hsize has changed;
			if v=. or v=&vsize then do;
				*-- only hsize has changed;
				hsize = h;
				vsize = &vsize;
				end;
			else do;
				*-- both hsize, vsize have changed;
				hsize = h;
				vsize = v;
				end;
			end;
*		hpos = round(&hpos * (&vsize / vsize));
*		vpos = round(&vpos * (&hsize / hsize));
		hpos = round(&hpos * (&hsize / hsize));
		vpos = round(&vpos * (&vsize / vsize));

		call symput('hsize', compress(put(hsize,best6.2))); 
		call symput('vsize', compress(put(vsize,best6.2)));
		call symput('hpos',  compress(put(hpos,3.)));
		call symput('vpos',  compress(put(vpos,3.)));

		if verbose then do;
			put hsize= vsize= hpos= vpos=;
			hcell = hsize / hpos;
			vcell = vsize / vpos;
			acell = hcell / vcell;
			put hcell= vcell= acell=;
			end;
		
done:
run;
goptions hsize=&hsize &unit vsize=&vsize &unit hpos=&hpos vpos=&vpos;
%mend;
