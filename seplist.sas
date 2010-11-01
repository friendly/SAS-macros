/*-----
 * group: Macro programming
 * purpose: Emit a list of items separated by some delimiter
 * notes: Additional <a href="seplist-explain.html">explanation</a>.
 */

%macro seplist (

    items
  , indlm = %str( )
  , dlm   = %str(,)
  , prefix=
  , nest=
  , suffix=

  );

%* Richard A. DeVenezia - 990902;
%*
%* emit a list of words separated by a delimiter
%*
%* items  - list of items, separated by indlm
%* indlm  - string that delimits each item of items
%*   dlm  - string that delimits list of items emitted
%* prefix - string to place before each item
%* nest   - Q (single quote ''),
%*          QQ (double quotes ""),
%*          P (parenthesis ()),
%*          C (curly braces {}),
%*          B (brackets [])
%* suffix - string to place after each item
%*
%* Note: nest is a convenience, and could be accomplished using
%*       prefix and suffix
%*;

  %local item n emit;

  %let emit=;

  %let nest = %upcase (&nest);

  %if (&nest = Q) %then %do;
    %let prefix = &prefix.%str(%');
    %let suffix = %str(%')&suffix;
  %end;
  %else
  %if (&nest = QQ) %then %do;
    %let prefix = &prefix.%str(%");
    %let suffix = %str(%")&suffix;
  %end;
  %else
  %if (&nest = P) %then %do;
    %let prefix = &prefix.%str(%();
    %let suffix = %str(%))&suffix;
  %end;
  %else
  %if (&nest = C) %then %do;
    %let prefix = &prefix.%str({);
    %let suffix = %str(})&suffix;
  %end;
  %else
  %if (&nest = B) %then %do;
    %let prefix = &prefix.%str([);
    %let suffix = %str(])&suffix;
  %end;

  %let n = 1;
  %let item = %qscan (&items, &n, %quote(&indlm));

  %do %while (%superq(item) ne );

    %if (&n = 1)
      %then %let emit = &prefix.&item.&suffix;
      %else %let emit = &emit.&dlm.&prefix.&item.&suffix;

    %let n = %eval (&n+1);
    %let item = %qscan (&items, &n, %quote(&indlm));
  %end;

  &emit

%mend;

/*
options nosource;
%put %seplist (a b c);
%put %seplist (a b c, dlm=+);
%put %seplist (a|b|c, indlm=|, dlm=%str( ));
%put %seplist (a b c, prefix=ORACLE., nest=QQ);
%put %seplist (a$b$c, indlm=$, suffix=@mail.com, dlm=%str( ));
%put %seplist (a$b$c, indlm=$, prefix=antispam_, suffix=@mail.com, dlm=%str( ));
%put %seplist (a b c, dlm=||, nest=Q);
%put %seplist (a b c, nest=C);
%put %seplist (a b c, nest=P);
%put %seplist (a b c, nest=B);
%put %seplist (a xyz b xyz c, indlm=xyz, dlm=%str( or ), prefix=%str( not ));
*/
