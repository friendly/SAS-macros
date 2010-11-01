/* Front-end for textable.sas
	- creates reasonable defaults for just=, formats= and head1= using
	info from the dataset
*/
%macro vcdtable(data=,
	outfile=&data,
	outdir=,
	subdir=,
	var=,
	formats=,
	just=,
	caption=,
	ch=2,	
	head1=,head2=,head3=,head4=,head5=,
	tail1=,tail2=,tail3=,tail4=,tail5=,
	tabenv=
	);

	%*-- Parse the var= list, put the names in a data set;
	data _vars_;
		length name $8;
		_loc_=0;
		%let count=1;
		%let newvar=;
		%let word = %scan(&var,&count,%str(~ ));
		%do %while(&word^= );
			name = %upcase("&word");  _loc_=_loc_+1;
			output;
			%let count = %eval(&count+1);
			%let word = %scan(&var,&count,%str(~ ));
			%let newvar = &newvar~&word;
		%end;
		%put Newvar= &newvar;
*	proc print;
	proc sort data=_vars_;
		by name;

	%*-- Get the type, length, format info from the data set;
	proc contents data=&data out=_work_ noprint;
	proc sort data=_work_;
		by name;
	
	%let abort=0;
	data _vars_;
		merge _vars_(in=inv)
			_work_(in=inw keep=name type length label format formatl formatd);
		by name;
		if inv;
		if inw=0 then do;
			put 'ERROR:  The variable ' name " was not found in data set &data";
			call symput('abort', 1);
			end;
	
	%*-- Now resort so vars are in original order listed in var=;
	proc sort;
		by _loc_;

	proc print;
		id _loc_;

	%*-- Determine formats, just and head1 if not assigned;
	data _null_;
		set _vars_ end=eof;
		length just lab fmt $40 head formats str $200 ;
		retain just head formats '';
		
		if type=1 then do;
			just = trim(just)||'r';
			fmt = format;
			if fmt = ' ' then do;
				fmt = 'best8.';
				end;
			end;
		else do;
			just = trim(just)||'l';
			fmt = '$' || compress(put(length,3.)) || '.';
			end;
		if label = ' ' 
			then lab = trim(upcase(substr(name,1,1)))||lowcase(substr(name,2));
			else lab = label;
		head = trim(head)||'~'||lab;
		formats = trim(formats)||'~'||fmt;
		
		if eof then do;
			file print;
			head = substr(head,2);
			formats = substr(formats,2);
			put just= head= formats=;
			str="&just";
			if str=' ' then call symput('just', just);
			str="&head1";
			if str=' ' then call symput('head1', head);
			str="&formats";
			if str=' ' then call symput('formats', formats);
			end;
	run;

	%let tid = table from data set &data (%sysget(SASFILE).sas) generated &sysdate;
	%textable(data=&data, var=&var,
		formats=&formats,
		just=&just,
		caption=&caption,
		outdir=~/Library/Documents/vcd,
		subdir=ch&ch/tab,
		outfile=&outfile,
		tabname=tab:&outfile,
		locate=htb,
		tabid=&tid,
		head1=&head1,head2=&head2,head3=&head3,head4=&head4,head5=&head5,
		tail1=&tail1,tail2=&tail2,tail3=&tail3,tail4=&tail4,tail5=&tail5,
		tabenv=&tabenv
		);
%mend;

