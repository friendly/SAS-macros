 /*--------------------------------------------------------------*
  *    Name: eqs2ram.sas                                         *
  *   Title: Parse LINEQS statements to form a RAM-list dataset  *
        Doc: http://www.datavis.ca/sasmac/eqs2ram.html     
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 15 Mar 2001 11:23:13                                *
  * Revised: 06 Mar 2008 15:41:32                                *
  * Version: 1.1-0                                               *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The EQS2RAM macro reads a data set containing a series of lines that
 would be used as input LINEQS (and STD, COV) statements to PROC CALIS.
 From these, it extracts the information about manifest and latent
 variables and the paths among them.  It produces a RAM-list output
 data set, suitable for use to produce a path diagram with RAM2DOT.  

 The parsing is slightly clever, but not very smart.  In particular,
 it assumes that each linear equation, and each STD and COV 
 specification appear on a separate input line.  In STD and
 COV statements, list notation, e.g., E1-E7 = EPS: may be used.
 
=Usage:

 The EQS2RAM macro is defined with keyword parameters.  The VAR=
 parameter is required.  
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%eqs2ram(data=lines,
		var=Y1 Y2 Y3 X1 X2 X3 X4 X5 F1 F2 F3,
		out=bagozzi);
 
==Parameters:

* DATA=       Input data set containing LINEQS/STD/COV statements

* TEXT=       Name of the text variable to be parsed [Default: TEXT=LINE]

* VAR=        List of all variables (manifest, latent)

* VERBOSE=    >0 writes debugging information to the SAS log

* OUT=        Name of output data set [Default: OUT=RAM]
                
=Example:

 The example below reads the LINEQS model for the data from Bagozzi (1980),    
 studied by Dillon & Goldstein (1984) into a data set, LINES, with a
 single text variable, LINE.

   data lines;
         input line $1-80;
   cards4;
      LINEQS
         Y1 =           z1 F3 + z2 X5 + e1,
         Y2 =    F1                   + e2,
         Y3 = z3 F1                   + e3,
         X1 =         F2              + e4,
         X2 =      z4 F2              + e5,
         X3 =              F3         + e6,
         X4 =           z5 F3         + e7,
         F1 =      z7 F2 + z6 Y1 + d1;
      STD
         e1-e7 = eps: ,
         X5    = e8 ,
         d1    = d1 ,
         F2-F3 = d2 d3 ;
      COV
         X5 F2 F3 = d4 d5 d6 ;

 This can be translated to RAM-list form, with variables  HEADS, TO, FROM,
 _TO_, and _FROM_ by EQS2RAM: 
 
	%eqs2ram(data=lines,
		var=Y1 Y2 Y3 X1 X2 X3 X4 X5 F1 F2 F3,
		out=bagozzi);
 
 =*/
%macro eqs2ram(
    data=_last_, /* input data set containing LINEQS/STD/COV statements */
    text=line,   /* name of the text variable to be parsed              */
    var=,        /* list of all variables (manifest, latent)            */
	 verbose=0,   /* print debugging info to the SAS log? */
    out=ram      /* name of output data set                             */
    );

%if %length(&var)=0 %then %do;
	%put ERROR:  The VAR= parameter must specify the list of manifest and latent variables.;
	%goto done;
	%end;

data &out;
	set &data;
	&text = left(compbl(&text));
	word = upcase(scan(&text,1));
	retain keyword errvar;
	drop keyword word edge m k v varn i j ;* &text;
	drop eqvar str sav lo hi v1 v2 errvar var rest;
	length heads to from 4 _from_ _to_ $ 8 var str sav eqvar errvar rest $100;
	label 
		heads = 'Heads (Matrix)'
		to = 'To Index'
		from = 'From Index'
		_to_ = 'To Var'
		_from_ = 'From Var'
		parm = 'Parameter';

	if word in ('LINEQS' 'STD' 'COV') then do;
		keyword = word;  
		if &verbose then put 'EQS2RAM: ' keyword=;
		&text = substr(&text, index(&text,word)+length(word)+1);
		if &text=' ' then return;
		end;

    if keyword='LINEQS' then do;
	 	if &verbose then put 'EQS2RAM: ' &text=;
		eqvar = scan(&text, 1, '=');
		rest = left(scan(&text, 2, '='));
		edge = scan(rest, 1, '+,;');
		var = "&var";
		do i=1 by 1 until (edge=' ');
			parm = scan(edge,1,' (');
			varn =  scan(edge,2,' ');
			if index(varn,'(')>0
				then varn = scan(edge,3,' ');
			if varn=' ' then do;
				varn = parm;
				parm = '1';
				end;
			heads = 1;
			if upcase(substr(varn,1,1)) in ('D', 'E')
			 & index(upcase(errvar), upcase(varn))=0
				then errvar = trim(errvar) || ' ' || varn;
			_from_ = varn; word=_from_; link locvar; from=m;
			_to_ = eqvar;  word=_to_  ; link locvar; to=m;
			*put edge= _to_= _from_=;
			edge = scan(rest, i+1, '+,;');
			if from^=0 then output;
			end;
      end;

	else if keyword='COV' then do;
		heads = 2;
		eqvar = scan(&text, 1, '=');  
		if &verbose then put eqvar=;
		rest = left(scan(&text, 2, '='));
		var = "&var" || ' ' || errvar;
		_from_ = 'XXXXX'; k=1;
		do i=1 by 1 until (_from_=' ');
			_from_ = scan(eqvar, i, ' ');
			word=_from_; link locvar; from=m;
			_to_ = 'YYYYY';
			do j=(i+1) by 1 until (_to_=' ');
				_to_ = scan(eqvar, j, ' ');
				word=_to_  ; link locvar; to=m;
				if index(rest,':')>1
					then parm = substr(rest,1,index(rest,':')-1) || put(i,1.) || put(j,1.);
					else parm = scan(rest, k, ' ;');
				k= k+1;
				if _to_ ^=' ' then output;
				_to_ = scan(eqvar, j+1, ' ');
				end;
			_from_ = scan(eqvar, i+1, ' ');
			end;
		end;

	else if keyword='STD' then do;
		heads = 2;
		eqvar = scan(&text, 1, '=');  
		if &verbose then put eqvar=;
		str = eqvar; link explist;  eqvar=str; 
		rest = left(scan(&text, 2, '=,;'));
		str = rest; link explist;  rest=str; 
		*put 'After expansion: ' rest=;

		_from_ = 'XXXXX'; k=1;
		var = "&var" || ' ' || errvar;
		do i=1 by 1 until (_from_=' ');
			_from_ = scan(eqvar, i, ' ');
			word=_from_; link locvar; from=m; to=m; _to_=_from_;
			parm = scan(rest, i, ' ');
			if from^=0 then output;
			end;
		lo=.; hi=.;
		end;
	return;

locvar:
	v = scan(var,1);
	do m=1 by 1 until (v=' ');
		if upcase(word)=upcase(v) then return;
		v = scan(var,m+1);
		end;
	m=0;
	return;

explist:
	sav = str;
	str = ' ';
	v = scan(sav,1, ' ');
	do m=1 by 1 until (v=' ');
		if index(v,'-') then do;
			v1 = scan(v,1,'-');
			v2 = scan(v,2,'-');
*			put v1= v2=;
			lo = input(substr(v1, indexc(v1, '0123456789')), 2.);
			hi = input(substr(v2, indexc(v2, '0123456789')), 2.);
			v1 = substr(v1, 1, indexc(v1, '0123456789')-1);			
			v2 = substr(v2, 1, indexc(v2, '0123456789')-1);			
*			put v1= v2= lo= hi= ;
			if lo^=. & hi^=. & upcase(v1)=upcase(v2) then do;
				do j=lo to hi;
					str = trim(str) || ' '|| trim(v1) || left(put(j,2.));
					end;
				end;
			end;
		else if index(v,':') then do;
			v1 = scan(v,1,':');
			if lo^=. & hi^=.  then do;
				do j=lo to hi;
					str = trim(str) || ' '|| trim(v1) || left(put(j,2.));
					end;
				end;
			end;
		else str = trim(str) || ' ' || v;
		v = scan(sav,m+1, ' ');
		end;
	return;
run;

proc print noobs data=&out;
	var
	heads to from _from_ _to_ parm
	%if &verbose %then ; &text;
	;
run;

%done:

%mend;

