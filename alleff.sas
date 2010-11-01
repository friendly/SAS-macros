 /*--------------------------------------------------------------*
  *    Name: alleff.sas                                          *
  *   Title: All-effects plot for a factorial ANOVA design       *
        Doc: http://www.datavis.ca/sasmac/alleff.html      
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 12 May 2000 16:22:27                                *
  * Revised: 08 Feb 2006 19:03:00                                *
  * Version: 1.2                                                 *
  *  1.1  Fixed effect labels for 3+ factors (needs %combine)    *
  *  1.2  More testing, provided separate SYMBOL=, COLORS= etc   *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The ALLEFF macro constructs a side-by-side plot of the values of
 main effects, interactions, and residuals for any linear model,
 as described in SSSG, Section 7.4.3, and Hoaglin et al. (1991), 
 `Fundamentals of Exploratory Analysis of Variance', Wiley.
 
 The goal is to display the effect values for the levels of all
 main effects and interactions, together with the values of residuals
 (whose mean square is the MSE) in a single comprehensive display.

=Usage:

 The EFFPLOT macro is defined with keyword parameters.  The RESPONSE=
 and MODEL= parameters are required.  The arguments may be listed
 within parentheses in any order, separated by commas. For example:
 
   %alleff(data=rats, response=gain, model=amount feed amount*feed);

 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* RESPONSE=   The name of the response variable in the model  

* MODEL=      A blank-separated list of terms in a MODEL statement for GLM.  In 
	          this version, all model variables are treated as CLASS
	          variables, and the terms in the model may not use GLM '|' notation.  
	          That is, a two-way factorial design should be specified
			  as MODEL=A B A*B, rather than A|B.

* PRINT=      Things to print: Any one or more of DESIGN FIT EFFECT LABEL

* SCALE=      If non-blank, the effect values are scaled by sqrt(n/df),
              so that their values are comparable as mean squares.  Use
              SCALE=1 when the goal is to see the size of the effects
              in relation to the size of the MSE.  [This should probably
              be the default, but is not.]

* VAXIS=      AXIS statement for vertical axis.  If not specified,
              the program uses AXIS1 LABEL=(A=90) and VAXIS=AXIS1.

* HAXIS=      AXIS statement for horizontal axis.  If not specified,
              the program uses AXIS2 OFFSET=(4) and HAXIS=AXIS2.  For 3+
              factors, it also uses VALUE=(A=-20) to allow longer effect labels to
              fit on the horizontal axis.

* HTEXT=      Height of text labels for effects.  If not specified, the
              global HTEXT goption is used.

* SYMBOLS=    A list of SAS/Graph symbols to be used for 1-factor (main)
              effects, 2-factor effects, 3-factor effects, ...
              There should be as many symbols as factors in the design.
			  Using the defaults for SYMBOLS= COLORS= and INTERP= causes
			  the program to generate the following SYMBOL statements:
			  
   symbol1 value=dot color=blue interpol=none h=1.5;
   symbol2 value=square color=red interpol=hilob h=1.5;
   symbol3 value=none color=green interpol=hilob h=1.5;
   symbol4 v=none i=none c=black;

 Alternatively, you can define your own SYMBOL statements,
 SYMBOL1, SYMBOL2, ... before calling ALLEFF and specify
 SYMBOLS=SYMB.  In this case, no SYMBOL statements are
 generated internally.  [Default: SYMBOLS=DOT SQUARE NONE]

* COLORS=     A list of SAS/Graph colors to be used for 1-factor (main)
              effects, 2-factor effects, 3-factor effects, ... of length
              equal to the number of factors.  Ignored if SYMBOLS=SYMB.
			  [Default: COLORS=BLUE RED GREEN]

* INTERP=     A list of SAS/Graph interpolation options to be used for 1-factor (main)
              effects, 2-factor effects, 3-factor effects, ... of length
              equal to the number of factors.   Ignored if SYMBOLS=SYMB.
			  [Default: INTERP=NONE NONE HILOB]

* HSYM=       Height of symbols in generated SYMBOL statements.  Ignored if SYMBOLS=SYMB.
              [Default: HSYM=1.5]

* RESIDS=     Specifies how to display the residuals in the plot.  At present,
              only RESIDS=BOXAXIS is recognized, which causes the program to use
			  the BOXAXIS macro to draw a boxplot of residuals after the final model
			  effect. 

* OUTFIT=     Output data set containing effect values as columns.
              The effect values are named by abbreviating the model
              variables to 1 or 2 characters. [Default: OUTFIT=OUTFIT]

* OUTEFF=     Output data set containing effect values in the form used
              for the plot.

* NAME=       Name for the graphics catalog entry. [Default: NAME=ALLEFF]

* GOUT=       Name for the graphics catalog used to store the plot. [Default: GOUT=GSEG]
                
==Requires:

 SAS/IML
 boxaxis
 combine
 expglm (experimental)
 
==Limitations:

 Only handles fully crossed designs, though not all terms need be included
 in the model.

 All model terms are treated as CLASS variables.  Under SAS V <7, there
 is a limit of 4 factors.

 Should incorporate the necessary macro code to construct the design
 matrix directly, rather than relying on IML routines from desmat.sas.
 (use GLMMOD)

==Acknowledgements:

* Jack Hamilton and Ian Whitlock helped solve some thorny problems
  in manipulating the effect data into the form required for plotting.

* John Hendrickx provided the %DESMAT macro and supporting IML routines.

 =*/


%macro alleff(
   data=_last_,     /* name of the input data set                   */
   response=,       /* name of response variable                    */
   model=,          /* terms in model statement                     */
   print=,          /* things to print                              */
   scale=,          /* scale the effects to MS?                     */
   haxis=,          /* AXIS statement for horizontal axis           */
   vaxis=,          /* AXIS statement for vertical axis             */
   htext=,          /* Height of text labels for effects            */
   symbols=dot square none,  /* symbols for main effects, interactions */
   colors=blue red green,    /* colors for main effects, interactions  */
   interp=none none hilob,  /* interpolation for main effects, interactions  */
   hsym=1.5,
   resids=boxaxis,  /* Use %boxaxis for residuals                   */
   outfit=outfit,   /* output data set of fitted effects            */
   outeff=_effect_, /* output data set of plotting effects          */
   name=alleff,     /* name for graphics catalog entry              */
   gout=gseg
   );

%let abort=0;
%let debug=1;
*options validvarname=V6;
%if %length(&response)=0 | %length(&model)=0
   %then %do;
      %put ERROR: The RESPONSE= and MODEL= parameters must be specified;
      %let abort=1;
      %goto DONE;
   %end;
%if %upcase(&data)=_LAST_ %then %let data = &syslast;
%let print=%upcase(&print);

%if %index(&model, %str(|))
	%then %do;
*options mprint symbolgen;
		%let model = %expglm(&model);
		%put ALLEFF: Model expanded to: &model;
*options nomprint nosymbolgen;
	%end;

options nonotes;

proc iml worksize=512;

 /*----------------------------------------------------------------*
  * proj(y,X) - Returns the projection of vector y on matrix X.
  * INPUT   y    (n,1)         input vector
  *         X    (n,m)         input matrix
  * The function returns an (n,1) vector, the projection of y on X 
  *----------------------------------------------------------------*/
start proj(y,X);
   if nrow(X)=1 then X = X`;
   XPX = X` * X;
   P = X * ginv(XPX) * X`;
   return( P * y );
   finish;
 
start glmfit(y, fit, error, ss)  global(design, label, pzation, effects, df);
   error = y;
   k = 1;
   X = j(nrow(y),1);
   cum = cusum(df);
   do kk=1 to nrow(df);       * for each effect... ;
      if kk=1
         then c1 = 1;         * columns from design;
         else c1 = cum[kk-1];
      c2 = cum[kk];

      xk = design[,c1:c2];    * columns of design for this effect;

      f = proj(error, xk);    * fit = projection of residual on them;
      fit = fit || f;         * append column to fit;
      error = error - f;      * remove fit from error;

      xk = xk - proj(xk, X);
      X = X || xk;
      end;

   ss = t(fit[##,] || error[##]);
   df = df // (nrow(design) - sum(df));
   ms = ss / df;
	k = nrow(ms);
	f = ms[1:k-1,] / ms[k,];
	do i=1 to k-1;
		p = p // 1-probf(f[i], df[i], df[k]);
		end;
   effects = effects // 'Error';
   print "ANOVA Summary Table for &response", 
		effects df ss[f=9.3] ms[f=9.3] f[f=8.2] p[f=6.3];
	rmse = sqrt(ms[k,]);
	run symput('rmse', char(rmse,6,2));
   finish;

*-- Create variable names for effects -- columns of the outfit data set;
start eff2vn(effects, class);
   lab = j(nrow(effects),1, '        ');
	%*-- set length of abbrev for each factor;
	if nrow(class) < 4
		then len=2;
		else len=1;
   do i = 1 to nrow(effects);
      eff = upcase(effects[i,]);
      if index(eff, '*') > 0 then do;
         lab[i,] = substr(scan(eff,1,'*'),1,len);
         do j=2 to 10 until (word=' ');
            word = substr(scan(eff,j,'*'),1,len);
            if word ^=' ' then lab[i,] = trim(lab[i,]) + '_' + word;
            end;
         end;
      else if index('CONST ERROR', trim(upcase(eff)))
         then lab[i,] = eff;
      else lab[i,] = substr(eff,1,len) + '      ';
      end;
      return(lab);
   finish;

*-- Create basis variables for effects (used to determine _TYPE_s);
start eff2bas(effects, class);
   basis = j(nrow(effects), nrow(class), 0);
   do i=1 to nrow(effects);
      eff = effects[i,];
      call change(eff, '*', ' ');
      do j=1 to nrow(class);
         if (index(eff, trim(class[j,]))) then basis[i,j]=1;
         end;
      end;
   return(basis);
   finish;
   
start symput(name, val);
   *-- Create a macro variable from a char/numeric scalar or column vector;
   if type(val) ='N'
      then value = trim(left(char(val)));
      else value = val;
	if nrow(value)>1 then value = rowcat(t(value)+' ');
   call execute('%let ', name, '=', value, ';');
   finish;

%*-- Modules used by DESMAT imported here;
start labs(varname,maxlevs,exclcat) global(levls);
  indx=1:maxlevs;
  indx[ ,exclcat]=0;
  vals=levls[ ,loc(indx)];
  if type(vals)='N' then vals=char(vals,8,0);
  labcl=repeat(varname || {'(' 'xxxxxxxx' ')'},maxlevs-1,1);
  labcl[ ,3]=vals`;
  labcl=rowcatc(labcl);
  return (labcl);
  finish labs;
 
start paras(contype,maxlevs,exclcat);
  pzat=repeat(contype || '(' || exclcat || ')',maxlevs-1,1);
  pzat=rowcatc(pzat);
  return (pzat);
  finish paras;
 
start addeff(desmat,labmat,parmat) global(design,label,pzation,df);
  if type(design)='U' then do;
    design=desmat;
    label=labmat;
    pzation=parmat;
	 df = ncol(desmat);
  end;
  else do;
    design=design || desmat;
    label=label // labmat;
    pzation=pzation // parmat;
	 df = df // ncol(desmat);
  end;
  finish;
 
start nextLab(UpToNow,new);
  upToEl=repeat(upToNow,1,nrow(new));
  upToEl=shape(upToEl,0,1);
  newEl=repeat(new,nrow(upToNow),1);
  nextEl=upToEl || j(nrow(upToEl),1,'*') || newEl;
  nextEl=rowcatc(nextEl);
  return (nextEl);
  finish nextLab;
 
start trimmat(inmat);
  if type(inmat)^='C' then return(inmat);
  trimmed=substr(inmat,1,max(length(inmat)));
  return(trimmed);
  finish;


%*-- Start Main routine;

	%*-- Get design matrix for the model;
	%desmat(data=&data, model=&model, intrcpt=Y);
   print = upcase("&print");
   if index(print, 'DESIGN') 
      then print classvar label effects df[f=3.];
	reset fw=4;

   use &data;
   read all var{"&response"} into y;
	n = nrow(y);
   nc = nrow(classvar);
   ne = nrow(effects);
   if index(print, 'DESIGN') 
      then print y design[f=4.0];
   
   run glmfit(y, fit, error, ss);
   
   if index(print, 'FIT') 
      then print y[c={'Data'}]  fit[c=effects f=8.1]  error[f=8.2];

	%*-- Effects plot attuned to Mean Squares [Sec. 8C];
	%if %length(&scale) %then %do;
		do i=2 to ncol(fit);
			fit[,i] = fit[,i] # sqrt(n / df[i,]);
			end;
		%end;   
   out = fit || error;
   var = eff2vn(effects, classvar);
   basis = eff2bas(effects,classvar);
   types = (basis[1:ne,] || j(ne,1)) * 2##t((nc:0))
	        // 2##(nc+1);
	effnum = rank(types)-1;
	factors = basis[,+];        *- number of factors in each effect;
   print effects var basis[c=classvar f=4.0] types effnum factors;

   create &outfit from out[c=var];
   append from out;

	%*-- Export macro variables as blank separated strings;
	run symput('nc', nc);       *- number of class vars;
	run symput('ne', ne);       *- number of efects;

   run symput('effvar', var);
   run symput('effnum', effnum);
	run symput('effects', effects);
	run symput('factors', factors);
   run symput('classvar', classvar);

	types = rowcat(char(t(types),2,0)+' ');
	run symput('types', types);
   quit;
run;
%if &debug>0 %then %do;
	%put ALLEFF: NC=&nc NE=&ne;
	%put ALLEFF: CLASSVAR=&classvar;
	%put ALLEFF: TYPES=&types; 
	%put ALLEFF: EFFVAR=&effvar; 
	%put ALLEFF: EFFECTS=&effects; 
	%put ALLEFF: EFFNUM=&effnum;
	%put ALLEFF: FACTORS=&factors;
%end;

data &outfit;
   merge &data &outfit;

%*-- Transpose the &outfit data set, keeping FROM as the name of the
     effect value, VALUE as its numerical value.;
data _eff_ _err_;
   set &outfit;
   keep &classvar value from;
   length from $8;
   %local count word;
   %let count=1;
   %let word = %scan(&effvar,&count,%str( ));
   %do %while(&word^= );
      from = upcase("&word");
      value = &word;
      if from not in ('CONST', 'ERROR') then output _eff_;
      if from='ERROR' then output _err_;
       %let count = %eval(&count+1);
       %let word = %scan(&effvar,&count,%str( ));
   %end;

proc summary data=_eff_ missing;
   class &classvar from;
   var value;
   output out=&outeff (drop=_freq_)
          max=;
run;


/* Use EFFVAR and TYPES to construct selections of FROM and _TYPE_
*/
data &outeff;
   set &outeff;
   if (
   %do i=2 %to &ne;
      %let eff=%scan(&effvar,&i, %str( ));
      %let typ=%scan(&types,&i, %str( ));
      (upcase(from) = upcase("&eff") and _type_ = &typ)
      %if &i < &ne %then |;
      %end;
   );

data &outeff;
   set &outeff
       _err_(in=ine);
   if ine then _type_ = 2**(&nc+1);
proc sort;
   by _type_;

*-- format for effect value labels;
proc format;
	value eff
	%do i=2 %to &ne+1;
      %let eff=%scan(&effects,&i, %str( ));
      %let num=%scan(&effnum,&i, %str( ));
		&num = "&eff"
		%end;
	;

%let vlabel = Effect Value for &response;
%if %length(&scale) %then %let vlabel=Scaled &vlabel;

data &outeff;
   set &outeff;
   by _type_;
   if first._type_ then effect+1;
	if from='ERROR'
		then factors=&nc+1;
		else factors=1+length(from)-length(compress(from,'_'));
	label value="&vlabel"
		effect='Effect';
	run;
%if %index(&print, EFFECT) %then %do;
	proc print data=&outeff;
		id effect;
		by effect;
	run;
	%end;
	
%*-- Construct the effect labels;
%combine(data=&outeff, var=&classvar, result=text, out=lab, ignmiss=1, sep=:);
/*
proc print data=lab;
title 'lab data set';
*/

data labels;
	set lab(keep=effect value factors text &classvar);
	where factors<=&nc;
	xsys='2'; ysys='2';
	text = left(tranwrd(text, '::', ':'));
	if index(text,':')=1 then text=substr(text,2);
	if index(left(reverse(text)), ':') = 1
		then text = substr(text,1, length(text)-1);
	text = ' ' || text;
		
	x = effect+.05;
	y = value;
	function = 'LABEL   ';
	position='6';
	%if %length(&htext) %then %do;
		size=&htext;
		%end;


%if %index(&print, LABEL) %then %do;
	proc print data=labels;
	run;
	%end;

%if %index(%upcase(&resids),BOXAXIS) %then %do;
	%boxaxis(data=&outfit, var=error, pos=&ne, boxwidth=.2, 
		bsys=2, out=_boxax_, 
		baxis=y, oaxis=x);
	data labels;
		set labels _boxax_;
	%end;

%if %length(&vaxis)=0 %then %do;
	%let vaxis=axis1;
	axis1 label=(a=90);
	%end;
%if %length(&haxis)=0 %then %do;
	%let haxis=axis2;
	%if &nc  < 3 %then %do;
		axis2 offset=(4);
		%end;
	%else %do;
		axis2 offset=(4) value=(a=-20);
		%end;
	%end;

/*
%local s1 s2;
%let s1=%scan(&symbols, 1, %str( ));
%let s2=%scan(&symbols &symbols, 2, %str( ));
*/
title;
proc gplot data=&outeff ;
   plot value * effect = factors /
		vref=0 lvref=34 frame nolegend anno=labels
		vaxis=&vaxis haxis=&haxis hm=0 vm=1
		name="&name" des="All effects plot for &response"
		;
	format effect eff.;

	%*-- Generate SYMBOL statements for effects according to number of
	     factors involved, unless SYMBOLS=SYMB was given;
*options mprint;
	%let msym = %upcase(%scan(&symbols, 1, %str( )))^= SYMB;
	%if &msym %then %do;
		%put ALLEFF: generating SYMBOL statements from SYMBOLS=, COLORS=, etc.;
		%do i=1 %to &nc;
			%local s&i c&i i&i;
			%let s&i=%scan(&symbols, &i, %str( ));
			%if %length(&&s&i)=0 %then %let s&i = &&s%eval(&i-1);
			%let c&i=%scan(&colors, &i, %str( ));
			%let i&i=%scan(&interp, &i, %str( ));
			symbol&i value=&&s&i color=&&c&i interpol=&&i&i h=&hsym;
			%end;
		%if %index(%upcase(&resids),BOXAXIS) %then %do;
			symbol%eval(&nc+1) v=none i=none c=black;
			%end;
		%end;
	%else %do;
		%put ALLEFF: Using your pre-defined SYMBOL statements;
		%end;
*options nomprint;
/*
	%let sym=0;
	%if %index(%upcase(&resids),BOXAXIS) %then %do;
		symbol1 v=none i=none c=black;
		%let sym=1;
		%end;
	%do i=1 %to %eval(&nc);
		%let j=%eval(&i+&sym);
		%if &i=1 %then %do;
			symbol&j v=&s1 i=none c=blue h=1.5;
			%end;
		%else %do;
			symbol&j v=&s2 i=hilob c=red ci=blue h=1.8;
			%end;
		
		%end;
options nomprint;
*/

run; quit;
%if &msym %then %do;
	goptions reset=symbol;  *-- cancel generated SYMBOL stmts;
	%end;

%done:
%if &abort %then %put ERROR: The ALLEFF macro ended abnormally.;
options notes;

%mend;

*libname  desmat '~/sasuser/glim/desnew';

/************************************************************************
Macro DESMAT (stripped down for use in alleff).  Original by:
	John Hendrickx <J.Hendrickx@maw.kun.nl>
	Department of Sociology
	Nijmegen University
	P.O. Box 9104
	6500 HE Nijmegen
	The Netherlands

%DESMAT(data=,model=,para=) -- Use within PROC IML to create a design
  matrix "DESIGN", with parametrizations specifiable per factor, and
  an accompanying column matrices "LABEL" and "PZATION", with effect
  labels and parametrization types respectively.

************************************************************************/

%macro desmat(data=&syslast,model=,para=,intrcpt=NO,add=NO);
  %* use in PROC IML;
  use &data;
  %*let model=%upcase(&model);

  %if %substr(%upcase(&add),1,1)=N %then 
  		%str(free design label pzation effects df;);

*  reset storage=desmat.imldes;
*  load module=_all_;             *-- MF --;

  %* Parse 'MODEL', create the matrices DESIGN and LABEL;
  %let indx=1;
  %let term=%quote(%scan(&model,&indx,' '));
  %do %while (&term^= );
    effects = effects // {"&term"};
    %* interaction effect;
    %if %index(&term,*)^=0 %then %do;
      %let termpar=%scan(&para,&indx,' ');
      %intrct(termpar=&termpar);
      run addeff(desin,labin,parin);
    %end;
    %else %do;
      %let cntrst=%scan(&para,&indx,' ');
      %class(var=&term,para=&cntrst);
		classvar = classvar // {"&term"};
      run addeff(descl,labcl,parcl);
    %end;
    %let indx=%eval(&indx + 1);
    %let term=%quote(%scan(&model,&indx,' '));
  %end;

  %if %substr(%upcase(&intrcpt),1,1)^=N %then %do;
    design=j(nrow(design),1,1) || design;
    label={'Const'} // label;
    pzation={'Const'} // pzation;
	 effects={'Const'} // effects;
	 df={1} // df;
  %end;

  if type(design)='N' then do;
    label=trimmat(label);
    pzation=trimmat(pzation);
  end;
%mend;

%macro class(var=,para=);
  %let para=%upcase(&para);
  %if %length(&para) > 3 %then %let par=%substr(&para,1,3);
  %else %let par=&para;

  * check first for directly specified design (term starts with '!');
  %if %substr(&var,1,1)=! %then %do;
    free descl labcl parcl;
    %let des=%substr(&var,2);
    descl=value("&des");
    %let specs=%scan(&para,2,());
    %let arg1=%scan(&specs,1,/); * effect labels;
    %let arg2=%scan(&specs,2,/); * parametrization labels;
    nparas=ncol(descl);
    %if &arg1^= %then %str(labcl=value("&arg1"););
    if type(labcl)='C' then labcl=shape(labcl,nparas,1);
    else do;
      labcl=repeat({"&des(#" 'xxxxxxxx' ')'},nparas,1);
      labcl[ ,2] = char(1:nparas,8,0)`;
      labcl=rowcatc(labcl);
    end;
    %if &arg2^= %then %str(parcl=value("&arg2"););
    if type(parcl)='C' then parcl=shape(parcl,nparas,1);
    else parcl=shape('specified',nparas,1);
    %goto defined;
  %end;

  read all var{&var} into class;

  * direct variable effect;
  %if &par=DIR %then %do;
    descl=class;
    labcl="&var";
    parcl={'direct'};
    %goto defined;
  %end;
  levls=unique(class);
  nlevs=ncol(levls);

  * parametrizations;
  %if %quote(&para)= %then %do;
    catelem=nlevs;
    catname=levls[1,nlevs];
    if type(catname)='N' then catname=char(catname,8,0);
    descl=designf(class);
    labcl=labs("&var",nlevs,catelem);
    parcl=paras('DEV',nlevs,catname);
    %goto defined;
  %end;

  /* The rest is not used here */
  %if &par=USE %then %do;
    free concl labcl parcl;
    %let specs=%scan(&para,2,());
    %let arg1=%scan(&specs,1,/); * contrast matrix;
    %let arg2=%scan(&specs,2,/); * labels;
    %let arg3=%scan(&specs,3,/); * parametrization labels;
    %if &arg1= %then %do;
      print "No user defined contrast found for variable &var",
            "Deviation contrast being used for variable &var";
    %end;
    %else %do;
      start usecon;
        concl=value("&arg1");
        if type(concl)^='N' then do;
          print "User defined contrast matrix &arg1 has invalid type",
                "Deviation contrast being used for variable &var";
          free concl;
          return;
        end;
        nparas=nrow(concl);
        nmod=ncol(concl);
        if nmod^=nlevs | nparas >= nmod then do;
          print "User defined contrast matrix &arg1
                 has incorrect dimensions:",
              concl[format=8.3],
              "Deviation contrast being used for variable &var";
          free concl;
          return;
        end;
        if det(concl*concl`)=0 then do;
          print "User defined contrast matrix &arg1"
                 "is singular",
              concl[format=8.3],
              "Deviation contrast being used for variable &var";
          free concl;
          return;
        end;
      finish;
      run usecon;
    %end;
    if type(concl)^='N' then do;
      concl=deviate(nlevs,1);
      labcl=labs("&var",nlevs,1);
      parcl=paras('DEV',nlevs,'1');
    end;
    else do;
      %if &arg2^= %then %str(labcl=value("&arg2"););
      if type(labcl)='C' then labcl=shape(labcl,nparas,1);
      else do;
        labcl=repeat("&var" || {'(#' 'xxxxxxxx' ')'},nparas,1);
        labcl[ ,3] = char(1:nparas,8,0)`;
        labcl=rowcatc(labcl);
      end;
      %if &arg3^= %then %str(parcl=value("&arg3"););
      if type(parcl)='C' then parcl=shape(parcl,nparas,1);
      else do;
        if nmod=nparas+1 then delcats='       #' || char(nmod,8,0);
        else delcats='#' || char(nparas+1,8,0) || ':' || char(nmod,8,0);
        parcl=repeat('USE(' || delcats || ')',nparas,1);
        parcl=rowcatc(parcl);
      end;
    end;
  %end;
  %else %if &par=SIM %then %do;
    %getcat;
    concl=simple(nlevs,catelem);
    labcl=labs("&var",nlevs,catelem);
    parcl=paras('SIM',nlevs,catname);
  %end;
  %else %if &par=DUM %then %do;
    %getcat;
    concl=dummy(nlevs,catelem);
    labcl=labs("&var",nlevs,catelem);
    parcl=paras('DUM',nlevs,catname);
  %end;
  %else %if &par=DIF %then %do;
    concl=differ(nlevs);
    %* default is backward difference - check for forward difference opt;
    %let cat=%scan(&para,2,());
    %if &cat= %then %let cat=B;
    %if %substr(%upcase(&cat),1,1)=F %then %do;
      concl=concl*-1;
      labcl=labs("&var",nlevs,nlevs[1,1]);
      parcl=paras('DIF',nlevs,'F');
    %end;
    %else %do;
      labcl=labs("&var",nlevs,1);
      parcl=paras('DIF',nlevs,'B');
    %end;
  %end;
  %else %if &par=HEL %then %do;
    concl=helmert(nlevs);
    labcl=labs("&var",nlevs,nlevs[1,1]);
    parcl=j(nlevs-1,1,'HEL');
  %end;
  %else %if &par=ORP %then %do;
    %let specs=%scan(&para,2,());
    %let degree=%scan(&specs,1,/);
    %let metric=%scan(&specs,2,/);
    %if &degree= %then %let degree=nlevs-1;
    %if &metric= %then %let metric=levls`;
    concl=orthpol(&degree,&metric);
    labcl=repeat({"&var" '(**' 'xx' ')'},&degree,1);
    labcl[ ,3] = char(1:&degree,2,0)`;
    labcl=rowcatc(labcl);
    parcl=repeat('ORP(' || char(&degree,2,0) || ')',&degree,1);
    parcl=rowcatc(parcl);
  %end;
  %else %if &par=BAS %then %do;
    concl=I(nlevs);
    parcl=j(nlevs,1,'BAS');
    labcl=repeat({"&var" '(' 'x' ')'},nlevs,1);
    if type(levls)='N' then labcl[ ,3]=char(levls`,3,0);
    else                    labcl[ ,3]=levls`;
    labcl=rowcatc(labcl);
  %end;
  %else %do; %* deviation contrast is default;
    %getcat;
    concl=deviate(nlevs,catelem);
    labcl=labs("&var",nlevs,catelem);
    parcl=paras('DEV',nlevs,catname);
  %end;
  descl=design(class)*concl`*inv(concl*concl`);
  %defined: ;
%mend;

%macro getcat;
  %* utility used by macro class to get the scalairs CATELEM (NUM);
  %* and CATNAME (CHAR) for redundant or contrast categories;
  %let cat=%scan(&para,2,());
  %if &cat= %then %let cat=levls[1,1];
  if type(xsect(&cat,levls))='U' then catelem=1;
  else catelem=loc(levls=&cat);
  if type(levls)='N'
  then catname=char(levls[1,catelem],8,0);
  else catname=levls[ ,catelem];
%mend;

%macro intrct(termpar=,intx=0);
  %let term=%upcase(&term);
  %let termpar=%upcase(&termpar);
  %let intx=%eval(&intx+1);
  %let main=%quote(%scan(&term,&intx,*));
  %do %while (&main^= );
    %let cntrst=%scan(&termpar,&intx,*);
    %class(var=&main,para=&cntrst);
    %if &intx = 1 %then %do;
      desin=descl;
      labin=labcl;
      parin=parcl;
    %end;
    %else %do;
      desin=hdir(desin,descl);
      labin=nextLab(labin,labcl);
      parin=nextLab(parin,parcl);
    %end;
  %let intx=%eval(&intx+1);
  %let main=%quote(%scan(&term,&intx,*));
  %end;
%mend;

