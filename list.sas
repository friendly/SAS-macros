%macro genlist(arg, split=%str( ), sep=%str( ), lo=1, hi=1);
	%local var i j max return;
	%let return=;
	%let max=%count(&arg, split=&split);
   %do i=1 %to &max;
		%let var=%scan(&arg, &i, &split);
		%if %index(&var, :) %then %do;
			%let var=%scan(&var, 1, :);
			%do j=&lo %to &hi;
				%if &j=&hi & &i=&max 
				%then %let return=&return.&var.&j;
				%else %let return=&return.&var.&j.&sep;
				%end;
			%end;
		%else %if &i=&max %then %let return=&return.&var;
		%else %let return=&return.&var.&sep;
	%end;
%unquote(&return)
%mend;


%macro explist(arg, split=%str( ), sep=%str( ));
	%local var var1 var2 i j k lo hi max return;

	%let max=%count(&arg, split=&split);
    %do i=1 %to &max;
        %let var=%scan(&arg, &i, &split);
        %if %index(&var, -) %then %do;
            %let var1=%upcase(%scan(&var, 1, -));
            %let var2=%upcase(%scan(&var, 2, -));
            %let lo=%bquote(%_substr(&var1, %indexc(&var1, 0123456789)));
            %let hi=%bquote(%_substr(&var2, %indexc(&var2, 0123456789)));
            %let var1=%bquote(%scan(&var1, 1, 0123456789));
            %let var2=%bquote(%scan(&var2, 1, 0123456789));

            %if %length(&lo) & %length(&hi) & "&var1"="&var2" %then %do;
		          %let k=%length(&lo);
		
                %do j=&lo %to &hi;
                    %if &j=&hi & &i=&max 
						  %then %let return=&return.&var1&j;
                    %else %let return=&return.&var1&j.&sep;
                %end;
				%end;
        %end;
        %else %if &i=&max %then %let return=&return.&var;
        %else %let return=&return.&var.&sep;

    %end;

%unquote(&return)
%mend;

%macro exlist(arg1, split=%str( ));

    %local var var1 var2 i j k lo hi suffix max return;

    %let max=%count(&arg1);

    %do i=1 %to &max;
        %let var=%scan(&arg1, &i, %str( ));

        %if %index(&var, -) %then %do;
            %let var1=%upcase(%scan(&var, 1, -));
            %let var2=%upcase(%scan(&var, 2, -));
            %let lo=%bquote(%_substr(&var1, %indexc(&var1, 0123456789)));
            %let hi=%bquote(%_substr(&var2, %indexc(&var2, 0123456789)));
            %let var1=%bquote(%scan(&var1, 1, 0123456789));
            %let var2=%bquote(%scan(&var2, 1, 0123456789));

            %if %length(&lo) & %length(&hi) & "&var1"="&var2" %then %do;
                %let j=%indexc(&lo, :ABCDEFGHIJKLMNOPQRSTUVWXYZ''"");

                %if &j %then %do;
                    %let suffix=%bquote(%substr(&lo, &j));
                    %let lo=%substr(&lo, 1, &j-1);
                    %let hi=%scan(&hi, 1, :ABCDEFGHIJKLMNOPQRSTUVWXYZ''"");
                %end;

		          %let k=%length(&lo);
		
                %do j=&lo %to &hi;
                    %if &j=&hi & &i=&max %then %let return=&return.&var1%_repeat(0, &k-%length(&j))&j.&suffix;
                    %else %let return=&return.&var1%_repeat(0, &k-%length(&j))&j.&suffix.&split;
                %end;
            %end;
            %else %if &i=&max %then %let return=&return.&var;
            %else %let return=&return.&var.&split;
        %end;
        %else %if &i=&max %then %let return=&return.&var;
        %else %let return=&return.&var.&split;
    %end;

%unquote(&return)

%mend;


/* Count 'words' in a string, delimited by split */
%macro count(arg, split=%str( ));

%local i;
%let i=0;

%do %while(%length(%nrbquote(%scan(&arg, &i+1, &split))));
	%let i=%eval(&i+1);
%end;
&i
%mend;

/* Like the indexc() data step function 
   Returns the position of the first character of &source found in 
	&excerpt.
*/
%macro indexc(source, excerpt);

%local i ls le;
%let i=1;
%let ls=%length(&source);
%let le=%length(&excerpt);

%do %while(&i<=&ls & %index(&excerpt, %bquote(%_substr(&source, &i, 1)))=0);
	%let i=%eval(&i+1);
%end;

%if &i<=&ls %then &i;
%else 0;

%mend indexc;

/* A forgiving version of %substr()
*/
%macro _substr(arg1, arg2, arg3);

%local length;
%let length=%length(&arg1);

%if 0<&length & &arg2<=&length & &arg2>0 %then %do;
	%if %length(&arg3)=0 %then %substr(&arg1, &arg2);
	%else %if &arg3>&length-&arg2+1 %then %substr(&arg1, &arg2);
	%else %if &arg3>0 %then %substr(&arg1, &arg2, &arg3);
%end;

%mend _substr;

%put %explist(e1-e10);
%put %explist(e1-e10 f1-f10);
%put %genlist(eps: gam:, hi=5);
/*
%put %exlist('10'-'20');
%put %exlist('exlist1'-'exlist10');
%put %exlist(exlist1:-exlist10:);

%put %exlist(exlist1-exlist10);
%put %exlist(exlist1);
%put %exlist(one1:-one10: two1d-two10d);
%put %exlist(two2-one1 two one);
%put %exlist(two00-two10, split=%str(,));
*/