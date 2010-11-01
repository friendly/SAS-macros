 /*-----------------------------------------------------------------*
  |     Name: rampath.sas                                           |
  |    Title: SAS macros for creating path diagrams from CALIS input|
  | ----------------------------------------------------------------|
  |  Macdefs: str2ram ram2dot                                       |
  | ----------------------------------------------------------------|
  |   Author: Michael Friendly               <friendly@yorku.ca>    |
  |  Created: 10 Feb 2001 11:32:36                                  |
  |  Revised: 16 Nov 2001 16:36:33                                  |
  |  Version: 1.1                                                   |
  *-----------------------------------------------------------------*/
 /*=
=Description:

 Path diagrams turn out to be surprisingly difficult to draw in SAS,
 because the OUTRAM= data set produced by PROC CALIS contains seemingly
 different information depending on how the model was specified originally
 (i.e., with FACTOR, COSAN, LINEQS, or RAM statements),  and because
 drawing a *nice* path diagram from a fully specified model is
 itself a difficult problem.   However, graph drawing is a well-researched
 topic, and a simpler solution is to translate the path diagram from
 one of the forms recognized by CALIS (RAM or LINEQS) to that required
 by a a graph drawing program.  These programs use 'dot', part of the
 GraphViz package (http://www.graphviz.org). 

 The STR2RAM macro translates a RAM specification for a structural
 equation or factor analysis model to a RAM-list data set which
 may be used as the input to the RAM2DOT macro.

=Usage:

 The STR2RAM macro takes 3 keyword arguments.  The VAR= and RAM= arguments
 must be specified.

==Parameters:

* VAR=               List of manifest and latent variables in the model.
                     The order of variable names in this list correspond
                     to the indices used in the TO and FROM entries in
                     the RAM-list.

* RAM=               RAM input statement, as a %str().  In SAS versions
                     prior to V7, this is limited to 200 characters.
                     In V7+, may be up to 32000 characters.

* OUT=_paths_        Name of output RAM-list data set, containing the
                     variables HEADS, FROM, TO (indices of variables
                     in the VAR= list), _FROM_, and _TO_ (names of
                     variables in the VAR= list).

* SHOW=              If non-blank, this 
=Example:

	%include macros(rampath);
	title "Lord's data: Speeded/Unspeeded tests";
	
	*-- Construct a RAM-list dataset from RAM stmt;
	%str2ram(
		var=x1 x2 y1 y2 F1 F2,
		ram=%str(
	1   1   5   b1,
	1   2   5   b2,
	1   3   6   b3,
	1   4   6   b4,
	2   1   1   e1,
	2   2   2   e2,
	2   3   3   e3,
	2   4   4   e4,
	2   5   5   1,
	2   6   6   1,
	2   5   6   r;
	),
		out=paths);
	
	%ram2dot(data=paths, var=x1 x2 y1 y2 F1 F2,
		rank=min x1 x2/
				max y1 y2/
				same F1 F2);

 These statements produce a file, paths.dot, as follows, which may then
 be fed to dot, using the command
 
 	dot -Tps -o paths.ps paths.dot

 to produce postscript output.  The file, paths.dot looks like this:

	digraph paths {
		rankdir=LR;
		size="8,8";
		node [fontname="Helvetica" fontsize=14 shape=box];
		edge [fontname="Symbol" fontsize=10];
		center=1;
		{rank=min x1 x2 }
		{rank=max y1 y2 }
		{rank=same F1 F2 }
		F1 -> x1 [label=b1] ;
		F1 [shape=ellipse ];
		F1 -> x2 [label=b2] ;
		F2 -> y1 [label=b3] ;
		F2 [shape=ellipse ];
		F2 -> y2 [label=b4] ;
		F2 -> F1 [dir=both label=r ];
	}

 Note that edge labels, using the Symbol font, appear as Greek letters.
 The resulting path diagram does not include double-headed arrows
 for the error variances, e1-e4.  They may be included by adding the
 following lines to the dot file:
 
		x1 -> x1 [dir=both label=e1];
		x2 -> x2 [dir=both label=e2];
		y1 -> y1 [dir=both label=e3];
		y2 -> y2 [dir=both label=e4];

 =*/

%macro str2ram(
	var=,        /* list of manifest and latent variables in the model */
	ram=,        /* RAM input statement, as a %str() */
	out=_paths_  /* name of output data set */
	);

%if &sysver < 7 
	%then %let maxlen=200;
	%else %let maxlen=32000;

%*-- Parse the RAM statement into edges;
data &out;
	length ram edge var $&maxlen;
	label 
		heads = 'Heads (Matrix)'
		to = 'To Index'
		from = 'From Index'
		_to_ = 'To Var'
		_from_ = 'From Var'
		parm = 'Parameter';
	ram = compbl("&ram");
	var = compbl("&var");
	edge = scan(ram, 1, ',');
	drop ram edge var test i;
	length heads 4 from to 8 _from_ _to_ $8;
	do i=1 by 1 until (edge=' ');
	put edge=;
		if i=1 then do;
			test = scan(edge, 1);
			if upcase(test)='RAM' then edge = substr(edge,1+length(test));
			end;
		heads  = input(trim(left(scan(edge,1))), 5.0);
		to   = input(trim(left(scan(edge,2))), 5.0);
		from = input(trim(left(scan(edge,3))), 5.0);
		_to_   = scan(var, to);
		_from_ = scan(var, from);
		parm = scan(edge,4);
		edge = scan(ram, i+1, ',');
		output;
		end;
run;
proc print data=&out noobs label;
run;

/*
%*-- netdraw produces a weird diagram if self-edges are included;
proc netdraw 
	data=&out(where=(from^=to)) graphics;
	actnet / id=(parm) breakcycle carcs=black arrowhead=2;
*/

%mend;

/*=
=Description:

 The RAM2DOT macro reads a RAM-list data set of the same format produced
 by STR2RAM and other macros in the SEMPATH collection.  It uses this
 information to writes an external file of specifications for drawing a
 directed graph representing the model using the 'dot' program.  

=Usage:

 The RAM2DOT macro takes keyword arguments.  The VAR= parameter value
 must be supplied.  All others have default values, but the defaults
 may not give you what you want.  Judicious use of the RANK= option
 often gives better results.

==Parameters:

* DATA=_last_        Input RAM-list data set.  The data set is assumed to
                     contain the variables HEADS, FROM, TO (indices of
                     variables in the VAR= list), _FROM_, and _TO_ 
                     (names of variables in the VAR= list).  The names of
                     these variables may be changed with the TO=, FROM=,
                     TOVAR= and FROMVAR= parameters.  The defaults reflect
							the output of other macros, e.g., STR2RAM.

* VAR=               List of manifest and latent variables in the model.
                     The order of variable names in this list corresponds
                     to the indices used in the TO and FROM entries in
                     the RAM-list.  
							
							Alternatively, if VAR= is just a
                     single word, it is assumed to be the name of an
                     additional data set containing a VARNAME variable.
                     The values of this variable are stacked into a string
                     to create the VAR= list.

* FROM=from          Name of the from index variable in the input data
                     set.

* TO=to              Name of the to index variable in the input data set.

* FROMVAR=_from_     Name of the from name variable

* TOVAR=_to_         Name of the to name variable

* PARM=parm          Name of the parameter-name variable

* VALUE=value        Name of the parmeter-value variable

* EDGE=parm value    What to use as the label on an edge: the
                     parameter name (if EDGE= includes PARM), and
                     or the parameter value (if EDGE= includes VALUE)

* SIZE=%str(8,8)     Maximum size of the dot path diagram, in inches.

* NODEFONT=Helvetica Font for nodes.  Standard PostScript fonts, e.g.,
                     'Times-Roman', 'Helvetica', etc. are portable

* NODESIZE=16        Font size for the names of nodes (manifest and
                     latent variables), in points.

* NODESTY=           Other node style parameters, e.g, 
                        NODESTY=style=filled color=yellow
							will draw nodes filled in yellow.

* EDGEFONT=Symbol    Font for edge labels.  The default, EDGEFONT=Symbol
                     is useful when the parameter names are composed of
							single lower case roman characters optionally 
							followed by subscripts, e.g., b1, b2, g, r12.  In
							this case, the parameters appear as \beta1 ... \rho12.

* EDGESIZE=12        Font size for edge labels, which represent parameters
                     in the model.

* RANKDIR=LR         Draw LR (left to right) or TB (top to bottom)?

* RANK=              List of variable 'ranks' for locating nodes, in the
                     form of one or more rank specifications separated by
                     '/'.  For example, the following says that x1 and x2,
                     y1 and y2, and F1 and F2 should have equal rank within
                     set, giving x1,x2 minimum rank, and y1,y2 maximum rank.

                          rank=min x1 x2/ max y1 y2/ same F1 F2

* RUN=,              If non-blank, RUN= specifies a system command to run
                     dot on the output .dot file.  By creating a .bat file,
							or a shell script, you can pass options to dot and
							or arrange for the resulting path diagram to be opened
							in an appropriate viewer.
         
* OUT=&data          Filename of the output .dot file (excluding the .dot
                     extension).

 =*/

%macro ram2dot(
	data=_last_,        /* input RAM-list data set                            */
	var=,               /* list of manifest and latent variables in the model */
	from=from,          /* name of from index variable                        */
	to=to,              /* name of to index variable                          */
	fromvar=_from_,     /* name of from name variable                         */
	tovar=_to_,         /* name of to name variable                           */
	parm=parm,          /* name of the parameter-name variable                */
	value=value,        /* name of the parameter-value variable               */	
	edge=parm value,    /* what to use as edge labels? PARM and/or VALUE      */
	size=%str(8,8),     /* size of dot path diagram                           */
	nodefont=Helvetica, /* font for nodes                                     */
	nodesize=14,        /* font size for nodes                                */
	nodesty=,           /* other node style parameters, e.g, style=filled     */
	edgefont=Symbol,    /* font for edge labels                               */
	edgesize=12,        /* font size for edge labels                          */
	rankdir=LR,         /* draw LR or TB ?                                    */
	rank=,              /* list of variable ranks                             */
	out=,               /* filename of output .dot file                       */
	run=                /* If non-blank, command to run dot                   */
	);

%if %upcase(&data)=_LAST_ %then %let data=&syslast;
%if %scan(&data,1,.)=WORK & %length(%scan(&data,2,.))
	%then %let data=%scan(&data,2,.);
%if %length(&out)=0
	%then %let out=%trim(&data);
%let edge=%upcase(&edge);

%*-- If VAR= is just one word, assume it is the name of a data set
     containing variable names in a VARNAME variable;

%if %length(%scan(&var,2,%str( )))=0 %then %do;
	%local ovar; %let ovar=var;
	%if &sysver>=6.12 %then %do;
		proc sql noprint;
			select varname into :var separated by ' '
			from &var;
		%end;
	%else %do;
		data _null_;
			set &var end=eof;
			length vars $200;
			retain vars;
			if _n_ = 1
				then vars = varname;
				else vars = trim(vars) || ' ' || varname;
			if eof then do;
				call symput('var', vars);
				end;
		%end;
	run;
	%put RAM2DOT: VAR=&ovar translated to VAR=&var;
	%end;

filename out "&out..dot";
data _null_;
	set &data end=eof;
	length shape $10 label edge $30;
	array seen{*} &var; retain &var;
	length rank $100;
	file out;
	if _n_=1 then do;
		put "digraph &out {";
		put "  rankdir=&rankdir;";
		put "  size=""&size"";";
		put "  node [fontname=""&nodefont"" fontsize=&nodesize shape=box];";
		%if %length(&nodesty) %then %do;
		put "  node [&nodesty];";
			%end;
		put "  edge [fontname=""&edgefont"" fontsize=&edgesize];";
		put "  center=1;";
		%if %length(&rank) %then %do;
			rank = scan("&rank", 1, '/');
			do i=1 by 1 until(rank=' ');
				put '  {rank=' rank '}';
				rank = scan("&rank", i+1, '/');
				end;
			%end;
		end;

	edge = ' ';
	%if %length(&parm) & %index(&edge,PARM) >0 %then %do;
		edge = &parm;
		%end;
	%if %length(&value) & %index(&edge,VALUE) >0 %then %do;
		sep ='\n';
		if &value ^=. then do;
			if edge=' '
				then edge = left(put(&value,8.3));
				else edge = trim(edge) || sep || left(put(&value,8.3));
			end;
		%end;
		
	if heads=1 then do;
		if edge ^=' ' then label='[label="' || trim(edge) || '"]';
		put '  ' &fromvar '-> ' &tovar label +0 ';';
		shape='box';
		if upcase(substr(&tovar,1,1))='F' then do;
			shape='ellipse';
			if seen{&to} < 1 then do;
				put '  ' &tovar '[shape=' shape '];';
				seen{&to}=1;
				end;
			end;
		if upcase(substr(&fromvar,1,1))='F' then do;
			shape='ellipse';
			if seen{&from} < 1 then do;
				put '  ' &fromvar '[shape=' shape '];';
				seen{&from}=1;
				end;
			end;
		if upcase(substr(&fromvar,1,1)) in ('D' 'E') then do; 
			shape='plaintext';
			if index(upcase(&fromvar), upcase("&var"))=0 then do;
				put '  ' &fromvar '[shape=' shape '];';
				end;
			end;
		end;

	else do;  /* heads=2 */
/*		if edge ^=' ' then label='[label=' || trim(edge) || ']'; */
		if &to=&from then do;
			shape='plaintext';
/*			put '  ' &fromvar '-> ' &tovar '[dir=both label="' edge '"];'; */
			end;
		else do;
			shape=' ';
			put '  ' &fromvar '-> ' &tovar '[dir=both label="' edge '"];';
			end;
		end;

	if eof then do;
		put "}";
		end;
	run;

	%if %length(&run)>0 %then %do;
		%if &sysscp = WIN %then %do;
			options noxsync noxwait; run;
			%sysexec(&run &out); run;
			options   xsync   xwait; run;
			%end;
		%else %do;
			%sysexec(&run &out); run;
			%end;
		%end;

%mend;
