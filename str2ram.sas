 /*--------------------------------------------------------------*
  *    Name: str2ram.sas                                         *
  *   Title: Create a RAM-list data set from RAM text strings    *
        Doc: http://www.datavis.ca/sasmac/str2ram.html     
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 10 Feb 2001 11:32:36                                *
  * Revised: 07 Mar 2008 10:08:23                                *
  * Version: 1.0-0                                               *
  *                                                              *
  *--------------------------------------------------------------*/
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

=Example:

	%include macros(str2ram);
	%include macros(ram2dot);
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


%mend;

