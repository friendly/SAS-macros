 /*-------------------------------------------------------------------*
  *    Name: SCATTER.SAS                                              *
  *   Title: Construct a scatterplot matrix - all pairwise plots      *
  *          for n variables.                                         *
        Doc: http://www.datavis.ca/sasmac/scatter.html          
  *                                                                   *
  * %scatter(data=, var=, group=);                                    *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  23 Oct 1996                                             *
  * Revised:  10 Oct 2006 09:41:37                                    *
  * Version:  1.2                                                     *
  *  1.2  Added WITH= for rectangular display                         *
  *-------------------------------------------------------------------*/
 
%macro scatter(data=_LAST_,
	var=_NUMERIC_,      /* variables to plot */
	with=,
	class=,             /* name of class/group variable */
   group=,             /* name of class/group variable */
	where=,              /* where clause to select observations */
	id=,
   symbols=square plus circle diamond X up down star,
   colors=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE,
	out=_paint_
   );

%if %sysprod(insight) ^= 1
   %then %do;
      %put This program requires SAS/INSIGHT;
      %goto done;
      %end;
%if &sysenv = BACK
   %then %do;
      %put This program does not run in batch;
      %goto done;
      %end;

%if &group=%str() %then %if &class ^= %str() 
   %then %let group = &class;

 *-- Parse variables list;
 data _null_;
 set &data (obs=1);
   if upcase(symget('data')) eq '_LAST_'
      then call symput('data', symget('syslast'));
   call symput('abort', put(_error_ ne 0, 1.));

   %if %index(&var,-) > 0 or %upcase(&var)=_NUMERIC_ %then %do;
 
       * find the number of variables in the list and
         convert shorthand variable list to long form;
     length _vname_ $ 8 _vlist_ $ 200;
     array _xx_ &var;
     _vname_ = ' ';
     do over _xx_;
        call vname(_xx_,_vname_);
        if _vname_ ne "&group" then do;
           nvar + 1;
           if nvar = 1 then startpt = 1;
                       else startpt = length(_vlist_) + 2;
           endpt = length(_vname_);
           substr(_vlist_,startpt,endpt) = _vname_;
        end;
     end;
     call symput( 'VAR', _vlist_ );
put nvar=;
   %end;
   %else %do;
     * find the number of variables in the list;
     nvar = n(of &var) + nmiss(of &var);
   %end;
   call symput('NVAR',trim(left(put(nvar,2.))));
 RUN;
%put nvar= &nvar;
%if &nvar < 2 or &nvar > 15 %then %do;
   %put Cannot do a scatterplot matrix for &nvar variables ;
   %goto DONE;
   %end;

%if %length(&with)=0 %then %let with=&var;

%if &group ^= %str() %then %do;
   %put Assigning color/symbol codes for &group variable... results in &out;
   %paint(data=&data, out=&out, var=&group,level=nominal,
      colors=&colors, symbols=&symbols);
   %let data=&out;
   %end;

proc insight data=&data
   %if %length(&where) %then %do;
      (where = (&where))
      %end;;
   scatter &var * &with
   %if &id ^= %str() %then %do;
      / label=&id;
      %end;;
   run;
%done:;

%mend;

