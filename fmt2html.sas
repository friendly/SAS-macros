	* frm2html.sas ;
	* html listing of formats ;

%macro fmt2html(
	library = library,
	catalog = formats,
	desc=,
	title=,
	out=FILE,           /* output fileref: FILE, PRINT, STDOUT     */
	outfile=           /* name of output HTM[L] file              */
	);

%*-- Output is either to a file (fileref HTMLOUT) or to the listing
     (fileref PRINT).  If FILE, try to assign default name based on
	  operating system.  Not very general here (due to wide variety of
	  values that &sysscp can take on);
	  
%if %nrbquote(&out)=FILE %then %do;
	%if &outfile eq %str() %then %do;
		%if &sysscp = WIN 
			%then %let outfile = formats.htm;
			%else %let outfile = formats.html;
		%put NOTE: Output File Name was not specified, so writing to &outfile.;
		%end;	
	filename htmlout "&outfile";
	%let out=htmlout;
%end;


	%let delform = ('$FLOUC' '$PADMTIT');   * delete these formats;

	proc catalog catalog = &library..&catalog;
	contents out=formds;
	run;
proc print;
***** get index information and merge in desc *****;

data formds2; set formds;
	length keylen textlen $3;
	if type = 'FORMATC' then name = '$' || substr(name,1,7);
	keylen = scan(desc,2);
	textlen = scan(desc,3);
	if name in &delform then delete;
	label name = 'Name'
		keylen = 'Key Length'
		textlen = 'Text Length'
		date = 'Last Updated'
		desc = 'Description'
		;
	keep name keylen textlen date desc;
	run;

%if %length(&desc) > 0 %then %do;
   filename formds3 "&dirform.formds2.txt";
	data formds3; 
		infile formds3 delimiter='09'x dsd firstobs=2 pad;
		input name : $8. desc $40.;
		label desc = 'Description';
		run;

	proc sort data = formds2; by name; run;
	proc sort data = formds3; by name; run;

	data formds2; 
		merge formds2(in=in2) formds3; by name;
		if in2;
		run;
%end;

**************************************
***** get individual format lines ****;

	proc format cntlout=formdata library=&library;
	run;

	proc sort data = formdata; by fmtname type; run;

data fd2;
	set formdata;
	by fmtname type;
	length name $8;
	if type = 'C' then name = '$' || substr(fmtname,1,7);
				else name = fmtname;
	if name in &delform then delete;
	keep name start end label;
	run;

	proc sort data=fd2; by name; run;

*****************************************
***** write index portion of document ***;

data formout1;
	set formds2 end=endofile;
	length line $200;
	if _n_ = 1 then do;
		line = "<HTML><HEAD><TITLE>" || "&title" || "</TITLE>";
		output;
		line = "</HEAD><BODY><h1>" || "&title" || "</h1>";
		output;
		line = '<hr><h2><A NAME ="TableTop">Format Index</A></h2>';
		output;
		line = "<TABLE border><Tr align=left valign=bottom><td>Name" || 
 			"</td><td>Key Length </td>";
		output;
		line = "<td>Text Length </td><td>Last Updated" ||
 			"</td><td>Description</td></tr>";
		output;
		end;

	line = '<tr><td align=left>' || '<H4><A NAME=Formats HREF="#' || trim(name) ||
 		'">' || name || '</A></H4>' || '</td>';
	output;
	line = "<td>" || keylen || "</td><td>" || textlen || "</td><td>" ||
		date || "</td><td>" || desc || "</td></tr>";
	output;
	if endofile then do;
		line = "</TABLE>";
		output;
		end;
	keep line;
	run;

*****************************************
***** write individual format sections **;

data formout2;
	set fd2 end=endofile;
	by name;
	length line $200;

	if first.name then do;
		line = '<hr><H3><A NAME = "' || trim(name) || '">' || name || '</A></H3>';
		output;
		line = "<TABLE border><Tr align=left valign=bottom><td>Start" ||
 			"</td><td>End </td><td>Label </td></tr>";
		output;
		end;

      line = "<tr><td>" || trim(Start) || "</td><td>" || trim(End) ||
			 "</td><td align=left>" || trim(Label) || "</td></tr>";
		output;

		if last.name then do;
			line = '</table><H4><A NAME=Formats HREF="#TableTop">' || 
			'Back to the Index</A></H4>';
			output;
			end;
		if endofile then do;
			line = "</BODY></HTML>";
			output;
			end;
		keep line;
		run;

*****************************************
***** write ds to disk              *****;

data _null_;
	set formout1 formout2;
	file htmlout;
	put line;
	run;

%mend;
***snip here****************************************************

Lewis Carson
University Planning and Analysis, NC State University
Box 7002, 201 Peele Hall  Raleigh, NC  27695
phone (919) 515-6208   fax (919) 831-3541
;