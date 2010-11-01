/*
I had published a %MACRO in SUGI Proceedings (1984) to address this
problem.  The documentation is more complete in that article.  The
code is below (I know, I should submit it to Statlib...).

Lawrence H. ('Doc') Muhlbaier           muhlb001@mc.duke.edu
Assistant Research Professor            muhlb001@dukemc
Duke University Medical Center          919-286-8830 (office)
DUMC 3865                               919-286-9534 (home)
Durham, NC 27710-7510                   919-286-0570 (FAX)

-------------cut here--------------------------------------------
*/
 /*  UPFIRST -- SAS macro procedure:  Capitalize first letter in
                                      each word in a string.

Macro program to capitalize the first letter in each "word" in a
string.  The program changes the string that is its argument.
The macro program was designed primarily for converting the all
uppercase names and addresses that are commonly seen in computer files
and converting them into upper and lower case.  It uses a generous
definition of a word, allowing separation by blanks, hypens, periods,
and apostrophies.  This is to handle the variety of capitalization
seen in names and degrees.  As a special case, it also capitalizes
the letter after "Mc" and "Mac", and records " III" and " II" correctly.

Usage:        %upfirst(string);
              DROP _l_ _j_ _char;     * Do once only in the DATA step;

Example:   DATA; INPUT string $CHAR70.;
           PUT string; %upfirst(string); PUT string;
           CARDS;
           JOHN DOE   KEVIN O'BRIEN   DICK mcduff, iii
           ;
   yields
           John Doe   Kevin O'Brien   Dick McDuff, III

Author:    Doc Muhlbaier
Date:      12 March 1987, based on %UPONE by Muhlbaier & Stafford
                          SUGI 9 Proceedings, 1984, pp 613-614.
Modified:  18Mar87        Bug fix.  First character of word not
                          changed; only rest changed to lower case.
           29Mar87        DROP un-needed working variables.
           26May87        Removed DROP stmt.  Caused bomb if UPFIRST
                          used more than once in a DATA step.
                          Add II & III as special cases.
                                                                  */
%macro UPFIRST(upstring);
_l_=length(&upstring);
substr(&upstring,1,1) = upcase(substr(&upstring,1,1));
do _j_=2 to _l_;
     _char=substr(&upstring,_j_-1,1);  /* previous character */
     if _char=' '|_char='.'|_char=','|_char='-'|_char='''' then
          substr(&upstring,_j_,1) =
          upcase(substr(&upstring,_j_,1));
     else substr(&upstring,_j_,1) =
          translate(substr(&upstring,_j_,1),
          'abcdefghijklmnopqrstuvwxyz',
          'ABCDEFGHIJKLMNOPQRSTUVWXYZ');
     if (_j_<_l_) then do;
          if (_j_>=3) then if (substr(&upstring,_j_-2,2)='Mc')
               then substr(&upstring,_j_,1) =
                    upcase(substr(&upstring,_j_,1));
          if (_j_>=4) then if (substr(&upstring,_j_-3,3)='Mac')
               then substr(&upstring,_j_,1) =
                    upcase(substr(&upstring,_j_,1));
          end;
end;
IF INDEX(&upstring,' Iii')>0 THEN
  SUBSTR(&upstring,INDEX(&upstring,' Iii'),4)=' III';
IF INDEX(&upstring,' Ii')>0 THEN
  SUBSTR(&upstring,INDEX(&upstring,' Ii'),3)=' II';
%mend;