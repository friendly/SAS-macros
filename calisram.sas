%macro calisram(
	data=_last_,    /* OUTRAM= data set from PROC CALIS */
	out=ram,        /* Output data set in RAM-list format */
	varout=ramvar,  /* Output data set containing variable info */
	);

proc iml;
	use &data;
	read all var{_matnr_ _row_ _col_} into model where(_type_='MODEL');
	read all var{_estim_ _stderr_} into mtype where(_type_='MODEL');
	types1={IDE ZID DIA ZDI LOW UPP 'na' SYM GEN 'I-B' SEL EQSJ EQSB EQSG};
	types2={'' ' INV' ' IMI'};
	mattype = types1[mtype[,1]];
	if any(mtype[,2]>0)
		then mattype = mattype + types2[1+mtype[,2]];
	read all var{_name_} into matname where(_type_='MODEL');

	if any(matname='_A_') then modtype='RAM';
		else if any(matname='_F_') then modtype='FACTOR';
		else if any(matname='_BETA_') then modtype='LINEQS';
		else if any(matname='A') then modtype='COSAN';
	print 'Model type: ' modtype,
		model[r=matname c={matnr rows cols}] mattype;
	
	read all var{_name_}  into varname where(_type_='VARNAME');
	read all var{_matnr_} into varmat  where(_type_='VARNAME');
	read all var{_col_} into varcol where(_type_='VARNAME');
	print 'Variable information',
		varname varmat varcol;
	
	read all var{_name_}  into parm   where(_type_='ESTIM');
	read all var{_matnr_} into parmat where(_type_='ESTIM');
	read all var{_row_}   into to     where(_type_='ESTIM');
	read all var{_col_}   into from   where(_type_='ESTIM');
	read all var{_estim_} into value  where(_type_='ESTIM');
	
	*print 'Parameter information',
		parm  parmat to from value;
	
	if modtype='RAM' | modtype='COSAN' then do;
		heads = parmat-1;
		do i=1 to nrow(parm );
			_to_ = _to_ // varname[loc(varmat=parmat[i] & varcol=to[i])];
			_from_ = _from_ // varname[loc(varmat=parmat[i] & varcol=from[i])];
			if parm [i] = ' ' & to[i]=from[i]
				then parm [i] = 'V' + varname[loc(varmat=parmat[i] & varcol=from[i])];
			end;
		end;

	if modtype='LINEQS' then do;
		heads = 1+(parmat=4);
		do i=1 to nrow(parm );
			_to_ = _to_ // varname[loc(varcol=to[i])];
			_from_ = _from_ // varname[loc(varcol=from[i])];
			end;
		end;
		
	print 'Parameter information',
		parm  heads to from _to_ _from_ value;
	
	outvar = {heads to from _to_ _from_ parm  value};
	create &out var outvar;
	append var outvar;

	outvar = {varname varmat varcol};
	create &varout var outvar;
	append var outvar;

	quit;

proc print data=&out;

%mend;
