 /*--------------------------------------------------------------*
  *    Name: lastword.sas                                        *
  *   Title: Return the last word from a delimited list of words *
        Doc: http://www.datavis.ca/sasmac/lastword.html    
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 26 Jan 2006 10:51:43                                *
  * Revised: 26 Jan 2006 10:51:43                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The LASTWORD macro returns the last word from a delimited list of words.
 This is useful for some generic forms of DATA Step BY processing with
 first. and last.  BY processing.

==Method:

 In Version 8+, it uses the %scan function in the form %scan(&words,-1,&sep)
 The original version of this macro is by Richard A. DeVenezia.

=Usage:

 The LASTWORD macro is defined with positional parameters.  The first
 parameter is required for any useful result.
 The macro is often used in a %LET statement, in the form
 
	%let lastclass = %lastword(&class);
 
==Parameters:

* WORDS     A list of words separated by something 

* SEP       The word separator.  [Default: SEP=%STR( )]
                
==Example:

    %let string= Able Baker Charlie;
    %put Last word of "&string" is "%lastword(&string)";

    %let classvar = Treatment Poison;
    data test;
        set animals;
        by &classvar;
        if first.%lastword(&classvar) then do;
            ...
            end;

 =*/


%macro lastword(
    words, 
    sep=%str( )
    );

%* Richard A. DeVenezia - 940729;
%*
%* extract last word from list of words separated by a specified character;
%*
%* words  - original list of words, separated with something;
%*;

%local N W;
%if %sysevalf(&sysver  >= 8) %then %do;
    %let W = %scan(&words, -1, %quote(&sep));
    %end;    

%else %do;
    %let N=1;
    %let W=%scan(&words,&N,%quote(&sep));

    %let N=2;
    %do %while (%scan(&words,&N,%quote(&sep))^=);
        %let W=%scan(&words,&N,%quote(&sep));
        %let N=%eval(&N+1);
    %end;
%end;

  &W

%mend;

