%macro nextlpr;
	%let dev=nextlpr;
	%let sasfn =%SYSGET(SASFILE);
	%if &sasfn=%str() %then %let sasfn=grf;
	filename gsasfile pipe 'lpr';
	goptions device=next400 gaccess=gsasfile gend='0A'x
				gsflen=80 gsfmode=append;
	goptions colors=(black);
%mend;