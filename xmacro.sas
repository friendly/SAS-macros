 /****************************************************************/
 /*          S A S   S A M P L E   L I B R A R Y                 */
 /*                                                              */
 /*    NAME: xmacro                                              */
 /*   TITLE: Utility routines for PROC-like macros               */
 /* PRODUCT: stat                                                */
 /*  SYSTEM: all                                                 */
 /*    KEYS: macros                                              */
 /*   PROCS:                                                     */
 /*    DATA:                                                     */
 /*                                                              */
 /* SUPPORT: saswss                      UPDATE:  01Dec00        */
 /*     REF:                                                     */
 /*    MISC:                                                     */
 /*                                                              */
 /****************************************************************/

 /*--------------------------------------------------------------------

 DISCLAIMER:

       THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE
 TO ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
 EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
 PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
 CONTAINED HEREIN.

 --------------------------------------------------------------------*/

 /************************* Utility Macros ***************************

The macros in xmacro.sas are intended to make it (relatively) easy to
write macros that act much like SAS procedures with respect to:

 * error checking (%xchk...)
 * setting defaults (%xchk...)
 * using variable lists (%xvlist)
 * doing BY processing (%xbylist, %xdo_by)

There are macros to facilitate debugging and testing, and there are
improved versions of %SUBSTR, %SCAN, the MERGE statement, and the
LAGn function.

There's also %INDEXC with no X prefix.

Outline of use
--------------

You MUST call %XINIT before using the other x macros and you MUST call
%XTERM before terminating the main macro. It is strongly recommended
that you follow the outline below as closely as possible to get all the
features to work as intended.

   * NO defaults in macro statement. put defaults in %xchk... instead;
   %macro whatever( data=, var=, freq=, weight=, by=, singular=,
                    options=, ...);

   ************** initialize xmacros, turn off notes ************;
   %xinit( whatever)

   ************** check EVERY argument ONCE ************;
   * %xchk macros also set defaults, do range checks, echo arguments;
   %xchkdata( data, _LAST_)
   %xchklist( var, _NUMERIC_)
   %xchklist( freq, , , 1, 1)
   %xchklist( weight, , , 1, 1)
   %xchklist( by)
   %xchknum( singular, 1e-7, 0<SINGULAR and SINGULAR<1)
   ...

   ************** process options if you got any **************;
   %let print=0;  * for example;
   %let nomiss=0; * for example;

   %let n=1;
   %let token=%qscan(&options,&n,%str( ));
   %do %while(&token^=);
      %let token=%qupcase(&token);
      %if %xsubstr(&token,1,5)=PRINT %then %let print=1; %else
      %if %xsubstr(&token,1,6)=NOMISS %then %let nomiss=1; %else
      %xerrset(Unrecognized option &token);
      %let n=%eval(&n+1);
      %let token=%qscan(&options,&n,%str( ));
   %end;

   %xbug(Options:, print nomiss)

   ************** process BY variables ************;
   %xbylist;
   %xvlist( data=&data, _list=by, _name=by, valid=0123)

   ************** dummy data step to check for errors ***************;
   %xchkvar( &data, &by, &var &freq &weight)
   %if &_xrc_^=OK %then %goto chkend;

   ************** process FREQ= variable ***************;
   %xvfreq( &data)

   ************** process WEIGHT= variable ***************;
   %xvweight( &data)

   ************** process VAR= list ***************;
   %let remove=;
   %if %qupcase(&var)=_NUMERIC_ %then %do;
      %if %bquote(&by)^= %then %let remove=&remove by;
      %if %bquote(&freq)^= %then %let remove=&remove freq;
      %if %bquote(&weight)^= %then %let remove=&remove weight;
   %end;
   %xvlist( data=&data, _list=var, _name=var, _type=type, _count=nvar,
           remove=&remove, replace=1)

   ************** set more defaults if there are any ***********;
   ...

   ************** echo arguments with defaults set and
                  check for no observations in the data set **********;
%chkend:
   %xchkend(&data)
   %if &_xrc_^=OK %then %goto exit;

   ************** turn notes back on before creating
                  the real output data sets ****************;
   %xnotes(1)

   ************** do something useful here ***************;
   ...
   ...
   ...

   ************** check for errors after each
                  DATA or PROC step *******************;
   %if &syserr>4 %then %do;

      * Assign descriptive message to &_xrc_ (which %xterm checks)
        and print ERROR: message;
      %xerrset(Something went wrong while the WHATEVER macro was
               trying to do something useful);

   %end;

%exit:

   ************** issue termination message, reset notes and _LAST_ **;
   %xterm;

   %mend whatever;


Xmain macro
-----------

The %xmain macro enables use of the &_test_ global variable for
labeling test output. %xmain generates a %macro statement and a
call to %xinit. You can use the following statement before including
the main macro to automatically generate TITLEn statements to label
test output:

   %let _test_=&line;    %* Defines a title line containing the
                            macro invocation, where 1<=&line<=10;

For the example above, the %xmain macro would be used like this:

   %unquote(%xmain(
      whatever( data=, var=, freq=, weight=, by=, singular=,
                    options=, ...)
   ))

   ************** check EVERY argument ONCE ************;
   etc.

A test job would start like this:

   title "Test WHATEVER macro";
   %let _test_=2; %* echoes macro invocation in title2;
   %include 'whatever.sas';

This works by specifying the PARMBUFF option in the main macro
statement. You should not hard-code the PARMBUFF option because
of a bug that prevents the macro processor from checking the
arguments to the main macro when it is invoked--in other words,
if the user makes a typo in one of the argument names, there will
be no error message.


Checking for inclusion of xmacro
--------------------------------

Unless you copy the xmacro macros that you need into your macro file,
it's a good idea to check whether xmacro has been included. This is
especially important if you use xmain, otherwise you can get vast
quantities of error messages. You can put a macro like this at the
beginning of your macro file:

   %* checks whether 6.10 version of xmacro has been included;
   %macro xmacinc;
      %global _xmacro_;
      %if %bquote(&_xmacro_)= %then
         %put ERROR: XMACRO has NOT been included.;
      %else %if %bquote(&_xmacro_)=done %then %do;
         %let _xmacro_=;
         %put %qcmpres(ERROR: The 6.09 version of XMACRO has been
              included but the 6.10 or later version is required.);
      %end;
      %if %bquote(&_xmacro_)= %then %do;
         %put NOTE: The WHATEVER macro will not be compiled.;
         %* comment out the rest of this file;
         %unquote(%str(/)%str(*))
      %end;
   %mend xmacinc;

   %xmacinc;

If xmacro has not been included, the above macro invocation begins
a slash-star comment. You will also need to include a macro comment
containing a star-slash at the end of your macro file like this:

  %* close comment possibly generated by xmacinc *_/;

except, of course, that the _ between the * and / should be deleted.
The _ is there to keep from terminating the comment that contains this
text.

Never use slash-star comments in macros.


Debugging
---------

The following statements in open code may be useful for debugging:

   %let _notes_=1;       %* Prints SAS notes for all steps;
   %let _echo_=1;        %* Prints the arguments to the main macro;
   %let _echo_=2;        %* Prints the arguments to the main macro
                            again after defaults have been set;
   %let _debug_=1;       %* Prints debugging information from non-x
                            macros;
   %let _xdebug_=1;      %* Prints debugging information from xmacros;
   options mprint;       %* Prints SAS code generated by the macro
                            language;
   options mlogic macrogen symbolgen;
                         %* Prints lots of macro debugging info;

To turn on all diagnostic information, use %XNOISY. To turn it back
off, use %XQUIET. These can be called in open code.

In your macro, Use the &_DEBUG_ variable to determine whether to print
debugging information.  The %XBUG macro is handy for printing the values
of many macro variables conditional on &_DEBUG_. The %XBUGDO macro
generates SAS code conditional on &_DEBUG_.

To speed things up, use:

   %let _check_=1;       %* Supresses checks for excessively large
                            integers and for non-existent data sets
                            and libraries;
   %let _check_=0;       %* Supresses most argument checking;

If your macro can do extra, time-consuming error checking, make the
extra error checks conditional on &_CHECK_ being greater than 1.

Argument checking
-----------------

The %XCHK... macros are for checking and echoing the arguments to the
main macro. You should generally call an %XCHK... macro for every
argument before you do anything else with the argument, so that
dangerous values will be detected. Users often forget the = sign
for a keyword argument, so most of the %XCHK... macros check for
extraneous ='s and extra tokens. Assiduous use of the %XCHK... macros
greatly reduces the number of inscrutable error messages that users
get.

Each %XCHK.. macro echoes the argument if &_ECHO_>=1. After checking
all the arguments to the main macro and setting defaults, use
%XCHKEND to echo the arguments again if &_ECHO_>=2.

   %XCHKDEF  Sets default value. Use this if no other
             checking is possible.
   %XCHKEQ   Issues error if argument contains an equals (=) sign.
   %XCHKONE  Issues error if argument has more than one token or
             contains an equals (=) sign.
   %XCHKUINT Checks an unsigned integer argument.
   %XCHKINT  Checks an (optionally signed) integer argument.
   %XCHKNUM  Checks a numeric (integer or floating point) argument.
   %XCHKMISS Checks a numeric or missing argument.
   %XCHKNAME Checks a SAS name.
   %XCHKDSN  Checks a data set name.
   %XCHKDATA Checks an input data set name.
   %XCHKKEY  Checks a value from a list of key words or values.
   %XCHKLIST Checks a list of integers, names, quoted strings,
             or special characters.
   %XCHKEND  Checks if a data set is empty, echoes all previously
             checked arguments if &_ECHO_>=2.

You should not call any of the above macros more than once for a single
argument, since that would cause %XCHKEND to echo the argument more than
once. However, in addition to checking arguments individually, you can
call %XCHKVAR to check an input data set to see if variables from one or
more lists are in the data set:

   %XCHKVAR  Runs a dummy DATA step to check if variables exist. Does
             not echo anything.

All of the %XCHK... macros except %XCHKEND and %XCHKVAR return the
argument that is checked in quoted form.


Macro quoting
-------------

Macro quoting is an arcane art that requires much study and dedication
to master. The basic rules (which, of course, have exceptions) are:

 1.  Quote every macro variable before doing any character comparisons
     on it or using it as an argument to a macro.

 2.  Unquote every macro variable before using it to generate code.

A corollary of (1) is:

 1a. Always use the Q... or X... versions of %SUBSTR, %UPCASE, %TRIM,
     %LEFT, etc.

The main purpose of quoting is to prevent an implicit %EVAL from going
berserk. Implicit %EVALs occur in the condition of an %IF statement,
various parts of a %DO statement, and some arguments to %(Q)SCAN and
%(Q)SUBSTR. The trouble is that %EVAL evaluates _everything_ in an
expression. So, for example,

   %IF &A=&B %THEN ...

doesn't simply do a character comparison of &A and &B, but tries to
interpret each of those values as an expression. If either &A or &B
contains an operator such as AND or -, %EVAL will try do an operation.
If &A is something like X1-X99, %EVAL will try to subtract two
character strings and get very upset. For example:

   %LET A=AND;
   %LET B=%EVAL(&A=OR);
ERROR: A character operand was found in the %EVAL function or %IF
       condition where a numeric operand is required. The condition
       was: and=or

The circumvention is:

   %LET B=%EVAL(%NRBQUOTE(&A)=%STR(OR));

%NRBQUOTE quotes the value &A at execution time. %STR quotes the
literal OR at compile time. If the first operand were &&&A, then you
would have to use %BQUOTE instead of %NRBQUOTE, since the latter
would not rescan (hence the NR in the name) the argument.

Quoting is also needed when a positional argument to a macro contains
an =, since the macro processor may misinterpret it as a keyword
argument.

Quoting turns operators and other special characters into different,
nonprintable characters. Quoting can also do funny things to
tokenization. When you generate code from a macro variable, it's
supposed to be automatically unquoted, but that doesn't necessarily
work. If you get weird characters in your generated code, you forgot
to unquote something. If ordinary '' or "" quoted strings in your
generated code cause weird errors, you forgot to unquote something.

If you use the %&ABC construct, &ABC must be unquoted. For example:

   %macro hello(n); %do i=1 %to &n; %put Hello; %end; %mend;
   %let x=hello(2);
   %&x;
Hello
Hello

   %let x=%bquote(&x);
   %&x;
ERROR: %EVAL function has no expression to evaluate, or %IF statement
       has no condition.

You can do the %UNQUOTE in a separate statement:

   %let x=%unquote(&x);
   %&x;
Hello
Hello

or, if you prefer really bizarre code, you can use:

   %unquote(%str(%%)%unquote(&x))
Hello
Hello


Macro usage notes
-----------------

Macros ordinarily return a single value. The macro language does not
support call-by-address. So to return more than one value, you have to
use call-by-name. That is, you pass the name of an existing macro
variable that you want the value returned in.  In the x macros,
arguments for returning a value (or sometimes many values) by
call-by-name have leading underscores in their names.

For example, suppose you want to write a macro called %REP to repeat an
argument n times with blanks in between. You could write it to return a
value like this:

   %macro rep(arg,n);            %* call by value;
      %local r i;                %* must declare local variables;
      %let r=&arg;
      %do i=2 %to &n;
         %let r=&r &arg;
      %end;
      &r                         %* returned value;
   %mend rep;

   %let string=abc;
   %let repstr=%rep(&string,3);  %* call by value requires ampersand;
   %put repstr=&repstr;

repstr=abc abc abc

You could also write a macro to update a macro variable like this:

   %macro uprep(_arg,n);         %* underscore indicates call by name;
      %local __r __i;            %* must declare local variables and
                                    use double underscores for names;
      %let __r=&&&_arg;          %* reference to value of argument
                                    passed by name requires three
                                    ampersands;
      %do __i=2 %to &n;
         %let __r=&__r &&&_arg;
      %end;
      %let &_arg=&__r;           %* to assign a value to an argument
                                    passed by name requires one
                                    ampersand on the left side;
   %mend uprep;

   %let string=abc;
   %uprep(string,3);             %* call by name requires no ampersand;
   %put string=&string;

string=abc abc abc

Be careful to avoid conflicts between the name of an argument passed
by call-by-name and local macro variables or argumemt names. Macros
that take call-by-name should use names for local variables beginning
with 2 underscores.

The macro language does not support arrays. Instead of arrays, names are
used that are composed of a constant prefix and a varying numerical
suffix. If an array needs to be returned by a macro, the caller provides
the prefix, and the macro constructs the names and declares them global.

To reduce the chances of name conflicts, local variables should always
be declared in a %LOCAL statement. The names of global variables should
usually begin with an underscore, but this hasn't been done everywhere
yet. Global variables that users should know about should begin and
end with an underscore. Other prefix naming conventions may be needed.

The macro language cannot do floating point operations. It does not
issue an error message if you try to do a floating point comparison,
but the comparison may be incorrect. You can use %XFLOAT to run a
DATA step to evaluate a floating point expression.

Every line of SAS code that the macro language generates is saved by the
SAS supervisor in a utility file (the "spool" file) in the WORK library
with an overhead of about 100 bytes per line. Therefore, you should try
to minimize the number of lines of SAS code generated in order to
postpone running out of disk space. In batch mode, this utility file is
automatically reinitialized after each step, so it's usually not a
problem except with IML (in IML, the EXECUTE, PUSH, and QUEUE statements
add stuff to this utility file). In line mode, OPTIONS NOSPOOL may
prevent the utility file from growing until it uses up all your disk
space, so this option is set by %XINIT. In DMS mode, OPTIONS NOSPOOL has
no effect, and the user must issue a CLEAR RECALL command to
reinitialize this utility file. If you actually run out of disk space,
you should get a multiple-choice quiz that says something like this
(this is from MVS):

   Out of disk space for library WORK while trying to reserve a page
   for member @T000000.UTILITY. Select:
   ...
   3. Clear source spooling/DMS recall buffers.
   ...

Number 3 is the correct answer.


SAS usage notes
---------------

&SYSERR should be checked after all DATA and PROC steps that can
fail in a way that damages subsequent steps.

Variables created in DATA steps or IML that might conflict with
names of the user's variables should have names beginning with
underscores.

In order to support OPTIONS FIRSTOBS= and OBS= and data set options
on the DATA= data set, the following restrictions must be imposed on
code generated by macros:

 * NEVER use data set options on the DATA= data set.

 * ALWAYS use FIRSTOBS= and OBS= data set options on all other
   input data sets, usually (firstobs=1 obs=2000000000)


Weird things you should know
----------------------------

 * The maximum allowed length of a macro variable is 32767 bytes.
   The documentation about this is wrong.

 * %INCLUDE is NOT a macro statement. So if you code:

      %if %bquote(_xyz_)= %then %include 'xyz.sas';

   the semicolon terminates the %THEN statement, but the
   %INCLUDE statement is generated as regular SAS code WITHOUT
   a semicolon.

 * Quotes in non-macro comments outside of macros do not need to be
   matched. Quotes in macros or macro statements, including macro
   comments, need to be matched unless you use something like %STR(%').

 * %BQUOTE does not quote ampersands as and-operators.

 * %EVAL always evaluates digit-strings as integers, even if they
   are quoted.

 * %EVAL does not remove leading zeros.

 * The following are buggy and should be used with extreme care:
      Nested quoting functions
      PARMBUFF option on MACRO statement

 *****************************************************************/


%************************** xmain ******************************;
%* generate %macro statement for main macro and call to xinit
   enabling use of &_test_ for labeling test output;
%* arg       name of main macro with parenthesized argument list, i.e.
             a %macro statement without the keyword '%macro' and
             without a semicolon;
 /* usage:
             %unquote(%xinit(MYMACRO(arg1=,arg2=,...)))
 */

%macro xmain(arg);
   %local name;
   %let name=%qscan(&arg,1);
   %global _test_;
   %if %bquote(&_test_)^= %then %do;
      %nrstr(%%)macro &arg / parmbuff;
      %nrstr(%%)local xpbuff; %* kludge macro bug--cannot pass
                                 syspbuff as argument to a macro;
      %nrstr(%%)let xpbuff=%nrbquote(&)syspbuff;
      %nrstr(%%)xinit(&name,%nrstr(%%)nrbquote(%nrbquote(&)xpbuff))
   %end;
   %else %do;
      %nrstr(%%)macro &arg;
      %nrstr(%%)xinit(&name)
   %end;
   %nrstr(%%)if %nrbquote(&)_xdebug_ %nrstr(%%)then
      %nrstr(%%)put %qupcase(&name): xmacro initialized;
%mend xmain;


%************************** xinit ******************************;
%* initialize global variables for x macros;
%* main      name of main macro, which must use the /PARMBUFF option;
%* pbuff     (optional) local variable that has been assigned the value
             of &syspbuff;
 /* usage:
             %xinit(MYMACRO)
    or:
             %local pbuff; %let pbuff=&syspbuff;
             %xinit(MYMACRO,%nrbquote(&pbuff))
 */

%macro xinit(main,pbuff);

   ; run;
   options nospool; * prevent spool file from using up all disk space;

   %global _main_; %* name of main macro;
   %let _main_=%qupcase(&main);

   %global _digits_;
   %let _digits_=0123456789;

   %global _uplets_;
   %let _uplets_=ABCDEFGHIJKLMNOPQRSTUVWXYZ;

   %global _lolets_;
   %let _lolets_=abcdefghijklmnopqrstuvwxyz;

   %global _alpha_; %* includes underscore;
   %let _alpha_=_&_uplets_&_lolets_;

   %global _alpnum_;
   %let _alpnum_=&_alpha_&_digits_;

   %global _sq_;
   %let _sq_=%str(%');

   %global _dq_;
   %let _dq_=%str(%");

   %global _xrc_; %* return code for xmacros;
   %let _xrc_=OK;

   %global _check_; %* check main macro arguments?;
   %if %bquote(&_check_)= %then %let _check_=2;
   %else %if %verify(&_check_,&_digits_) %then %do;
      %put ERROR: Invalid value of _CHECK_=&_check_!;
      %let _check_=0;
   %end;

   %global _xdebug_; %* debugging flag for xmacros;
   %if %bquote(&_xdebug_)= %then %let _xdebug_=0;
   %else %if &_check_>1 %then
      %if %verify(&_xdebug_,&_digits_) %then %do;
      %put ERROR: Invalid value of _XDEBUG_=&_xdebug_!;
      %let _xdebug_=0;
   %end;

   %global _debug_; %* debugging flag for non-x macros;
   %if %bquote(&_debug_)= %then %let _debug_=0;
   %else %if &_check_>1 %then
      %if %verify(&_debug_,&_digits_) %then %do;
      %put ERROR: Invalid value of _DEBUG_=&_debug_!;
      %let _debug_=0;
   %end;

   %global _putall_; %* print all obs in DATA steps in non-x macros;
   %if %bquote(&_putall_)= %then %let _putall_=0;
   %else %if &_check_>1 %then
      %if %verify(&_putall_,&_digits_) %then %do;
      %put ERROR: Invalid value of _PUTALL_=&_putall_!;
      %let _putall_=0;
   %end;

   %global _echo_; %* option to echo arguments of main macro;
   %if %bquote(&_echo_)= %then %let _echo_=0;
   %else %if &_check_>1 %then
      %if %verify(&_echo_,&_digits_) %then %do;
      %put ERROR: Invalid value of _ECHO_=&_echo_!;
      %let _echo_=1;
   %end;
   %if &_debug_ %then %if ^&_echo_ %then %let _echo_=1;
   %if &_echo_ %then %put Arguments to the &_main_ macro:;

   %global _notes_; %* option to print notes from all steps;
   %if %bquote(&_notes_)= %then %let _notes_=0;
   %else %if &_check_>1 %then
      %if %verify(&_notes_,&_digits_) %then %do;
      %put ERROR: Invalid value of _NOTES_=&_notes_!;
      %let _notes_=1;
   %end;
   %xnotes(0);

   %global _test_; %* make specified title line echo macro invocation;
   %if %bquote(&_test_)^= %then %do;
      %if &_check_>1 %then
         %if %verify(&_test_,&_digits_) %then %do;
         %put ERROR: Invalid value of _TEST_=&_test_!;
         %let _test_=1;
      %end;
      %let pbuff=%qcmpres(&pbuff);
      title&_test_ "&_main_&pbuff";
   %end;

   %global _xsysver;
   %let _xsysver=%xrepstr(&sysver,.,);

   %global _xlast; %* save _LAST_ so it can be reset by xterm;
   %let _xlast=&syslast;

   %global _arglist; %* list of arguments generated by xchk... macros;
   %let _arglist=;

   %xxbug(XINIT, _main_ _notes_ _echo_ _check_ _putall_
                 _debug_ _xdebug_ _test_ _xlast _xrc_)

%mend xinit;

%macro xquiet;
   options nomprint nomlogic nomacrogen nosymbolgen;
   %global _notes_ _echo_ _putall_ _debug_ _xdebug_ _check_;
   %let _notes_=0;
   %let _echo_=0;
   %let _putall_=0;
   %let _debug_=0;
   %let _xdebug_=0;
   %let _check_=2;
   run;
%mend xquiet;

%macro xnoisy; %* generates vast amount of output;
   options notes mprint mlogic macrogen symbolgen;
   %global _notes_ _echo_ _putall_ _debug_ _xdebug_ _check_;
   %let _notes_=1;
   %let _echo_=2;
   %let _putall_=1;
   %let _debug_=1;
   %let _xdebug_=1;
   %let _check_=2;
   run;
%mend xnoisy;


%************************** xterm ******************************;
%* print termination message and set options notes and _last_ as
   appropriate;

%macro xterm;
   run;
   %xxbug(XTERM,_main_ _xrc_ _xlast)
   %xnotes(1);
   %let _xrc_=%bquote(&_xrc_);
   %if &_xrc_=%str(0) %then %do;
      %put NOTE: The &_main_ macro terminated due to an empty data set.;
   %end;
   %else %if &_xrc_^=OK %then %do;
      options _last_=&_xlast;
      %put NOTE: The &_main_ macro terminated due to error(s).;
   %end;
   %else %do;
      %put NOTE: The &_main_ macro terminated, perhaps normally.;
   %end;
%mend xterm;


%************************** xerrset ******************************;
%* set error return code and print message;

%macro xerrset(msg);
   %let _xrc_=%qcmpres(&msg);
   %put ERROR: &_xrc_..;
%mend;


%************************** xerrmisc ******************************;
%* miscellaneous error message;

%macro xerrmisc(arg);
   %let _xrc_=%qcmpres(Unknown error &arg while running
                       the &_main_ macro);
   %put ERROR: &_xrc_..;
%mend;


%************************** xnotes ******************************;
%* turn notes on or off;
%* on       1 to turn notes on
            0 to turn notes off;

%macro xnotes(on);
   %if &_notes_=0 %then %do;
      options
      %if &on %then notes;
              %else nonotes;
      ;
   %end;
%mend xnotes;


%************************** indexc **********************;
%* Return the position of the first character in &str that is
   also in &chars;

%macro indexc(str,chars);
   %local i s;
   %if %length(&chars) %then %do;
      %do i=1 %to %length(&str);
         %if %index(&chars,%qsubstr(&str,&i,1)) %then %goto exit;
      %end;
   %end;
   %let i=0;
%exit:
   &i
%mend indexc;


%************************** xverify **********************;
%* Return the position of the first character in &str that is not
   also in &chars. Accepts zero-length arguments;

%macro xverify(str,chars);
   %local i s;
   %if %length(&chars) %then %do;
      %do i=1 %to %length(&str);
         %if ^%index(&chars,%qsubstr(&str,&i,1)) %then %goto exit;
      %end;
   %end;
   %else %do;
      %do i=1 %to %length(&str);
         %if %qsubstr(&str,&i,1)^= %then %goto exit;
      %end;
   %end;
   %let i=0;
%exit:
   &i
%mend xverify;


%************************** xeq **********************;
%* Safe, slow, case-insensitive character comparison of two strings.
   Returns 1 if the two arguments are the same except for case and
   trailing blanks. Otherwise returns 0;

%macro xeq(str1,str2);
   %local i j;
   %let str1=%qtrim(&str1);
   %let i=%length(&str1);
   %let str2=%qtrim(&str2);
   %let j=%length(&str2);
   %if &i^=&j %then %do;
      0
      %goto exit;
   %end;
   %do i=1 %to &j;
      %let c1=%qsubstr(&str1,&i,1);
      %let c2=%qsubstr(&str2,&i,1);
      %if &c1^=&c2 %then
         %if %qupcase(&c1)^=%qupcase(&c2) %then %do;
            0
            %goto exit;
         %end;
   %end;
   1
%exit:
%mend xeq;


%************************** xchkerr **********************;
%* Print error message about invalid argument;
%* _arg      name of argument that is invalid;

%macro xchkerr(_arg);
   %put ERROR: "%upcase(&_arg)=%bquote(&&&_arg)" is invalid. &_xrc_..;
%mend xchkerr;


%************************** xchkech **********************;
%* Echo argument, quote it, check for excessively large integers;
%* _arg      name of argument;

%macro xchkech(_arg);
   %if &_echo_ %then %do;
      %put %str(   )%upcase(&_arg)=&&&_arg;
      %let _arglist=&_arglist &_arg;
   %end;
   %let &_arg=%superq(&_arg);
   %if &_check_>1 %then %do;
      %local __i __l __str __tok;
      %let __i=%indexc(&&&_arg,123456789);
      %if &__i %then %do;
         %let __str=&&&_arg;
         %do %while(&__i);
            %let __str=%qsubstr(&__str,&__i);
            %let __l=%verify(&__str,&_digits_);
            %if &__l=0 %then %let __l=%eval(%length(&__str)+1);
            %let __tok=%xsubstr(&__str,1,&__l-1);
            %let __str=%xsubstr(&__str,&__l);
            %xxbug(xchkech int:, __l __tok)
            %if &__l<11 %then %let __l=0;
            %else %if &__l=11 %then %do;
               data _null_; if &__tok<=2147483647 then
                  call symput("__l","0");
               run;
            %end;
            %if &__l %then %do;
               %let _xrc_=%qcmpres(The macro language cannot process
                  integers greater than 2147483647);
               %xchkerr(&_arg);
               %let &_arg=;
               %let __i=0;
            %end;
            %else %let __i=%indexc(&__str,123456789);
         %end;
      %end;
   %end;
%mend xchkech;


%************************** xchkdef **********************;
%* If argument value is blank, set it to default value.
   No other checking is done;
%* _arg      name of argument to check;
%* def       (optional) default value;

%macro xchkdef(_arg,def);
   %* set default for &_arg;
   %xchkech(&_arg);
   %if %bquote(&&&_arg)= %then %let &_arg=&def;
   %xxbug(XCHKDEF,&_arg)
%mend xchkdef;


%************************** xchkeq **********************;
%* Issue error message if an argument value contains =.
   Delete stuff following the =;
%* _arg      name of argument to check;
%* def       (optional) default value;
%* noecho    nonblank value means argument has already been echoed;

%macro xchkeq(_arg,def,noecho);
   %* check _arg for equals sign;
   %if &noecho= %then %xchkech(&_arg);
   %if %bquote(&&&_arg)= %then %let &_arg=&def;
   %else %if &_check_ %then %if %index(%bquote(&&&_arg),=) %then %do;
      %let _xrc_=%qcmpres(The value of argument %qupcase(&_arg)=
               may not contain another "=");
      %xchkerr(&_arg);
      %put WARNING: Possible missing comma in macro invocation.;
      %let &_arg=%qscan(%bquote(&&&_arg),1,%str(= ));
   %end;
   %xxbug(XCHKEQ,&_arg)
%mend xchkeq;


%************************** xchkone **********************;
%* Issue error message if an argument value has more than one token
   or contains an =. Delete extraneous tokens from argument value;
%* _arg      name of argument to check;
%* def       (optional) default value;
%* noecho    nonblank value means argument has already been echoed;

%macro xchkone(_arg,def,noecho);
   %* check _arg for more than one token;
   %if &noecho= %then %xchkech(&_arg);
   %if %bquote(&&&_arg)= %then %let &_arg=&def;
   %else %if &_check_ %then %do;
      %if %qscan(%bquote(&&&_arg),2,%str( ))^= %then %do;
         %let _xrc_=%qcmpres(Only one value for the argument
                  %qupcase(&_arg)= is allowed);
         %xchkerr(&_arg);
         %put WARNING: Possible missing comma in macro invocation.;
         %let &_arg=%qscan(%bquote(&&&_arg),1,%str( ));
      %end;
      %xchkeq(&_arg,,y)
   %end;
   %xxbug(XCHKONE,&_arg)
%mend xchkone;


%************************** xchkuint **********************;
%* Issue error message if an argument value is not a single
   unsigned integer. Delete extraneous tokens from argument value
   and return default value if the argument value is blank or
   invalid;
%* _arg      name of argument to check;
%* def       (optional) default value;

%macro xchkuint(_arg,def);
   %* check whether _arg is an unsigned integer;
   %xchkech(&_arg);
   %if %bquote(&&&_arg)= %then %let &_arg=&def;
   %else %if &_check_ %then %do;
      %xchkone(&_arg,,y)
      %if %verify(%bquote(&&&_arg),&_digits_) %then %do;
         %let _xrc_=%qcmpres(The value of the argument %qupcase(&_arg)=
            must be an unsigned integer with no decimal point);
         %xchkerr(&_arg)
         %if %bquote(&def)= %then %let &_arg=0;
                            %else %let &_arg=&def;
      %end;
   %end;
   %xxbug(XCHKUINT,&_arg)
%mend xchkuint;


%************************** xchkint **********************;
%* Issue error message if an argument value is not a single integer
   within a specified range. Delete extraneous tokens from argument
   value and return a value within the permissible range or a specified
   default value if the argument value is blank or not an integer;
%* _arg      name of argument to check;
%* def       (optional) default value;
%* lb        (optional) lower bound for value of the argument;
%* ub        (optional) upper bound for value of the argument;

%macro xchkint(_arg,def,lb,ub);
   %* check whether _arg is an integer;
   %xchkech(&_arg);
   %local __ok __tmp __iv;
   %if %bquote(&&&_arg)= %then %let &_arg=&def;
   %else %if &_check_ %then %do;
      %xchkone(&_arg,,y)
      %let __tmp=%qleft(&&&_arg);
      %if %xsubstr(&__tmp,1,1)=%str(-) %then %let __iv=2;
                                       %else %let __iv=1;
      %let __ok=0;
      %if %verify(%xsubstr(&__tmp,&__iv),&_digits_) %then %do;
         %if %bquote(&def)^= %then %let __tmp=&def;
         %else %if %bquote(&lb)^= %then %let __tmp=&lb;
         %else %if %bquote(&ub)^= %then %let __tmp=&ub;
         %else %let __tmp=0;
      %end;
      %else %if %bquote(&lb)^= & &__tmp<&lb %then %let __tmp=&lb;
      %else %if %bquote(&ub)^= & &__tmp>&ub %then %let __tmp=&ub;
      %else %let __ok=1;
      %if ^&__ok %then %do;
         %let _xrc_=%qcmpres(The value of the argument %qupcase(&_arg)=
            must be an integer with no decimal point);
         %if %bquote(&lb)^= %then %let
            _xrc_=%qcmpres(&_xrc_ greater than or equal to &lb);
         %if %bquote(&ub)^= & %bquote(&ub)^= %then %let
            _xrc_=%qcmpres(&_xrc_ and);
         %if %bquote(&ub)^= %then %let
            _xrc_=%qcmpres(&_xrc_ less than or equal to &ub);
         %xchkerr(&_arg)
         %let &_arg=&__tmp;
      %end;
   %end;
   %xxbug(XCHKINT,&_arg)
%mend xchkint;


%************************** xchknum **********************;
%* Issue error message if an argument value is not a single numeric
   constant satisfying a condition specified as a DATA step
   expression. Delete extraneous tokens from argument value
   and return a default value if the condition is not true;
%* _arg      name of argument to check;
%* def       (optional) default value;
%* cond      (optional) condition written as a DATA step expression
             using the name of the argument (without an &);

%macro xchknum(_arg,def,cond,noecho);
   %* check whether _arg is a numeric constant;
   %if &noecho= %then %xchkech(&_arg);
   %if %bquote(&&&_arg)= %then %let &_arg=&def;
   %else %if &_check_ %then %do;
      %xchkone(&_arg,,y)
      %local __ok;
      %let __ok=1;
      %if %datatyp(%bquote(&&&_arg))=CHAR %then %let __ok=0;
      %else %if %nrbquote(&cond)^= %then %do;
         data _null_;
            &_arg=input(symget("&_arg"),12.);
            __ok=%unquote(&cond);
            call symput("__ok",trim(left(put(__ok,best12.))));
         run;
      %end;
      %if ^&__ok %then %do;
         %let _xrc_=%qcmpres(The value of the argument %qupcase(&_arg)=
            must be a numeric constant);
         %if %bquote(&cond)^= %then %let _xrc_=%qcmpres(&_xrc_
            such that &cond);
         %xchkerr(&_arg)
         %let &_arg=&def;
      %end;
   %end;
   %xxbug(XCHKNUM,&_arg)
%mend xchknum;


%************************** xchkmiss **********************;
%* Issue error message if an argument value is not a missing value
   or single numeric constant satisfying a condition specified as
   a DATA step expression. Delete extraneous tokens from argument
   value and return a default value if the condition is not true;
%* _arg      name of argument to check;
%* def       (optional) default value;
%* cond      (optional) condition written as a DATA step expression
             using the name of the argument (without an &);

%macro xchkmiss(_arg,def,cond);
   %* check whether _arg is a missing value or numeric constant;
   %xchkech(&_arg);
   %if %bquote(&&&_arg)= %then %let &_arg=&def;
   %else %if &_check_ %then %do;
      %xchkone(&_arg,,y)
      %local __tmp __c1 __c2 __nonmis;
      %let __tmp=%qleft(&&&_arg);
      %let __nonmis=1;
      %if %length(&__tmp)<=2 %then %do;
         %let __c1=%xsubstr(&__tmp,1,1);
         %if &__c1=%str(.) %then %do;
            %let __c2=%xsubstr(&__tmp,2,1);
            %if &__c2= %then %let __nonmis=0;
            %else %if %index(&_alpnum_,&__c2) %then %let __nonmis=0;
         %end;
      %end;
      %if &__nonmis %then %xchknum(&_arg,&def,&cond,y);
   %end;
   %xxbug(XCHKMISS,&_arg)
%mend xchkmiss;


%************************** xchkname **********************;
%* Issue error message if an argument value is not a SAS name;
%* _arg      name of argument to check;
%* def       (optional) default value;

%macro xchkname(_arg,def);
   %* check whether _arg is a name;
   %xchkech(&_arg);
   %if %bquote(&&&_arg)= %then %let &_arg=&def;
   %else %if &_check_ %then %do;
      %xchkone(&_arg,,y)
      %if ^%xname(%bquote(&&&_arg)) %then %do;
         %let _xrc_=%qcmpres(The value of the argument %qupcase(&_arg)=
            must be a SAS name);
         %xchkerr(&_arg)
         %let &_arg=&def;
      %end;
   %end;
   %xxbug(XCHKNAME,&_arg)
%mend xchkname;


%************************** xchkdsn **********************;
%* Issue error message if an argument value is not a SAS data set name.
   Uppercases argument value;
%* _arg      name of argument to check;
%* def       default value;

%macro xchkdsn(_arg,def,noecho);
   %* check whether _arg is a data set name;
   %if &noecho= %then %xchkech(&_arg);
   %if %bquote(&&&_arg)= %then %let &_arg=&def;
   %else %do;
      %let &_arg=%qupcase(&&&_arg);
      %if &_check_ %then %do;
         %local __ind __tmp __err __lib;
         %let __lib=&&&_arg;
         %let __tmp=&__lib;
         %let __ind=%index(&__tmp,%str(%());
         %if &__ind %then %let __tmp=%xsubstr(&__tmp,1,&__ind-1);
         %let &_arg=&__tmp;
         %xxbug(xchkdsn1:, __lib __tmp &_arg)
         %xchkeq(&_arg,,y)
         %if &&&_arg^=&__tmp %then %let __tmp=&&&_arg;
         %let &_arg=&__lib;
         %let __err=0;
         %let __ind=%index(&__tmp,.);
         %xxbug(xchkdsn2:, __tmp __ind)
         %if &__ind %then %do;
            %if &__ind=1 %then %let __err=1;
            %else %if ^%xname(%xsubstr(&__tmp,1,&__ind-1))
               %then %let __err=1;
            %else %if ^%xname(%xsubstr(&__tmp,&__ind+1))
               %then %let __err=1;
            %else %if &_check_>1 %then %do;
               %let __lib=%unquote(%substr(&__tmp,1,&__ind-1));
               %xxbug(xchkdsn3:, __lib)
               data &__lib.._TEMP_; stop; run;
               %if &syserr>4 %then %let __err=1;
               %else %do;
                  proc datasets nolist nowarn library=&__lib;
                     delete _TEMP_ / memtype=data;
                  quit;
               %end;
            %end;
         %end;
         %else %if ^%xname(&__tmp) %then %let __err=1;
         %if &__err %then %do;
            %let _xrc_=%qcmpres(The value of the argument
               %qupcase(&_arg)= must be a SAS data set name);
            %xchkerr(&_arg);
            %let &_arg=_NULL_;
         %end;
      %end;
   %end;
   %xxbug(XCHKDSN,&_arg)
%mend xchkdsn;


%************************** xchkdata **********************;
%* Issue error message if an argument value is not an existing
   SAS data set. Uppercases argument value;
%* _arg      name of argument to check. Set to blank if invalid;
%* def       (optional) default value, usually _LAST_;

%macro xchkdata(_arg,def);
   %* check whether _arg is a valid input data set;
   %xchkech(&_arg);
   %xchkdsn(&_arg,&def,y)
   %if &_xrc_^=OK %then %goto exit;
   %if %bquote(&&&_arg)=_LAST_ %then %do;
      %if &_xlast=_NULL_ %then %do;
         %let _xrc_=%bquote(No input data set);
         %xchkerr(&_arg);
         %goto exit;
      %end;
      %let &_arg=&_xlast;
   %end;
   %if &_check_>1 %then %if %bquote(&&&_arg)^= %then %do;
      data _null_; set %unquote(&&&_arg); stop; run;
      %if &syserr>4 %then %do;
         %let _xrc_=%qcmpres(The data set &&&_arg
                  does not exist or cannot be opened for input);
         %xchkerr(&_arg);
         %let &_arg=;
         %goto exit;
      %end;
   %end;
%exit:
   %xxbug(XCHKDATA,&_arg)
%mend xchkdata;


%************************** xchkkey **********************;
%* Issue error message if an argument value is not in a list
   of key words/values. Uppercases everything;
%* _arg      name of argument to check;
%* def       (optional) default value;
%* keys      list of valid key words/values. each key may optionally
             be followed by a colon and a return value. If the
             argument value fails to match a prefix of a key in the
             list, an error message is issued. Otherwise, if the
             matched key has a colon, the return value is returned,
             otherwise the complete key is returned;
%* delim     (optional) delimiters in keys. if omitted defaults
             to blanks;

%macro xchkkey(_arg,def,keys,delim);
   %* check whether _arg is a name;
   %xchkech(&_arg)
   %if %bquote(&&&_arg)= %then %let &_arg=&def;
   %else %do;
      %let &_arg=%qupcase(&&&_arg);
      %if &_check_ %then %do;
         %xchkone(&_arg,,y)
         %if %nrbquote(&delim)= %then %let delim=%str( );
         %local __arg __j __k __l __token __sub;
         %let __arg=&&&_arg;
         %let __l=%length(&__arg);
         %do __j=1 %to 9999;
            %let __token=%qscan(&keys,&__j,&delim);
            %if &__token= %then %let __j=9999;
            %else %do;
               %let __token=%qupcase(&__token);
               %let __k=%index(&__token,:);
               %if &__k %then %do;
                  %let __sub=%xsubstr(&__token,&__k+1);
                  %let __token=%xsubstr(&__token,1,&__k-1);
                  %if &__arg=%xsubstr(&__token,1,&__l) %then %do;
                     %let &_arg=&__sub;
                     %goto exit;
                  %end;
               %end;
               %else %if &__arg=%xsubstr(&__token,1,&__l) %then %do;
                  %let &_arg=&__token;
                  %goto exit;
               %end;
            %end;
         %end;
         %let _xrc_=%qcmpres(The value of the argument %qupcase(&_arg)=
            must be one of the following: &keys);
         %xchkerr(&_arg)
         %let &_arg=&def;
      %end;
   %end;
%exit:
   %xxbug(XCHKKEY,&_arg)
%mend xchkkey;


%************************** xchklist **********************;
%* Issue error message if an argument value does not contain a list
   of values of designated types with the number of values in a
   designated range;
%* _arg      name of argument to check;
%* def       (optional) default value;
%* types     (optional) permitted types:
                I=integer, N=SAS name, Q=quoted string,
                any special character represents itself.
                the default is %str(N:-), which is suitable for
                variable lists;
%* nmin      (optional) minimum number of values. default 0;
%* nmax      (optional) maximum number of values. default 999999;

%macro xchklist(_arg,def,types,nmin,nmax);
   %xchkech(&_arg);
   %if %bquote(&&&_arg)= %then %let &_arg=&def;
   %else %if &_check_ %then %do;
      %xchkeq(&_arg,,y)
      %if &types= %then %let types=%str(N:-);
      %else %let types=%qupcase(&types);
      %if %bquote(&nmin)= %then %let nmin=0;
      %if %bquote(&nmax)= %then %let nmax=999999;
      %local __n __typ __tok __str;
      %let __str=&&&_arg;
      %do __n=1 %to &nmax;
         %let __typ=%xscan(__tok,__str);
         %if &__typ=B %then %do;
            %if &__n<=&nmin %then %goto error;
            %goto break;
         %end;
         %if &__typ=S %then %do;
            %if %index(&types,&__tok)=0 %then %goto error;
         %end;
         %else %if %index(&types,&__typ)=0 %then %goto error;
      %end;
%break:
      %if &__str^= %then %goto error;
   %end;
   %goto exit;
%error:
   %local __str __n;
   %let __str=;
   %if %index(&types,N) %then %let __str=&__str or SAS names;
   %if %index(&types,Q) %then %let __str=&__str or quoted strings;
   %if %index(&types,I) %then %let __str=&__str or integers;
   %if %bquote(&__str)^= %then %let __str=%substr(&__str,4);
   %let __n=;
   %if &nmin=&nmax %then %let __n=&nmin;
   %else %if &nmax>=999999 %then %let __n=&nmin or more;
   %else %let __n=from &nmin to &nmax;
   %let _xrc_=%qcmpres(The value of the argument %qupcase(&_arg)=
      must be a list of &__n &__str);
   %xchkerr(&_arg)
   %let &_arg=&def;
%exit:
   %xxbug(XCHKLIST,&_arg)
%mend xchklist;


%************************** xchkend **********************;
%* call after xchk... has been called for all arguments to
   the main macro and defaults have been set for all arguments;
%* data      (optional) data set to check for no observations.
             Sets global variable _xnobs if this argument is
             specified;

%macro xchkend(xdata);
   %if &_echo_>=2 %then
      %xput(Arguments to the &_main_ macro with defaults:,
         &_arglist,%str(   ));
   %if &_check_>1 %then %do;
      %global _xnobs;
      %let _xnobs=-1;
      %if %bquote(&xdata)^= %then %do;
         %xnobs(_xnobs,&xdata)
         %if &_xnobs=0 %then %do;
            %if &_xrc_=OK %then %let _xrc_=0;
            %put NOTE: No observations in data set &xdata..;
         %end;
      %end;
      %if &_xnobs<0 %then %let _xnobs=0;
   %end;
%mend xchkend;


%************************** xchkvar **********************;
%* dummy data step to check for errors in variable lists. Does not
   echo anything;
%* data      input data set that is supposed to have all the variables;
%* by        (optional) BY list. Must have been processed by xbylist;
%* varlists  (optional) other variable lists. you may concatenate
             several lists;

%macro xchkvar(data,by,varlists);
   %if &_check_ %then %if %bquote(&data)^= %then %do;
      data _null_;
         set %unquote(&data);
         %if %bquote(&by)^= %then %do;
            by &_bydata;
         %end;
         retain %unquote(&varlists);
         stop;
      run;
      %if &syserr>4 %then %do;
         %xerrset(Probably invalid data set %qupcase(&data)
                  or variable list);
      %end;
   %end;
%mend xchkvar;


%************************** xname **********************;
%* return 1 if argument is a SAS name, 0 otherwise;

%macro xname(arg);
   %local ok len;
   %let ok=1;
   %let arg=%qupcase(%qtrim(&arg));
   %let len=%length(&arg);
   %if &len=0 | &len>32
      %then %let ok=0;
   %else %if %index(&_digits_,%qsubstr(&arg,1,1))
      %then %let ok=0;
   %else %if %verify(&arg,&_alpnum_)
      %then %let ok=0;
   %xxbug(XNAME, arg len ok)
   &ok
%mend xname;


%************************** xscan *****************************;
%* remove the first token from a string, return the token and
   remainder of the string. Token type:
      B blank (nothing left)
      I unsigned integer
      N name
      Q quoted string
      S special
   does not recognize floating point or quoted strings yet.
   ;

%macro xscan(_tok,_str);

   %local __j __l __char;

   %if %bquote(&&&_str)= %then %do;
      B
      %let &_tok=;
   %end;
   %else %do;

      %* skip blanks;
      %let __j=%verify(&&&_str,%str( ));
      %if &__j %then %let &_str=%xsubstr(&&&_str,&__j);

      %let __char=%xsubstr(&&&_str,1,1);

      %if %index(&_digits_,&__char) %then %do;
         I
         %let __j=%verify(&&&_str,&_digits_);
      %end;

      %else %if %index(&_alpha_,&__char) %then %do;
         N
         %let __j=%verify(&&&_str,&_alpnum_);
      %end;

      %else %if &__char=&_sq_ | &__char=&_dq_ %then %do;
         Q
         %let __l=%length(&&&_str);
         %do __j=2 %to &__l;
            %if %qsubstr(&&&_str,&__j,1)=&__char %then %do;
               %if &__j=&__l %then
                  %goto matchq;
               %if %qsubstr(&&&_str,&__j+1,1)^=&__char %then
                  %goto matchq;
               %let __j=%eval(&__j+1);
            %end;
         %end;
         %let &_str=&_str&__char; %* append missing quote;
%matchq:
         %let __j=%eval(&__j+1);
         %goto done;
      %end;

      %else %do;
         S
         %let __j=2;
      %end;

%done:
      %if &__j=0 %then %let __j=%eval(%length(&&&_str)+1);

      %let &_tok=%xsubstr(&&&_str,1,&__j-1);
      %let &_str=%xsubstr(&&&_str,&__j);

   %end;

   %if &_xdebug_ %then
      %put XSCAN: &_tok=&&&_tok! &_str=&&&_str!;

%mend xscan;


%************************** xput ******************************;
%* print values of a list of macro variables to the log;
%* msg       (optional) message of whatever sort to print;
%* list      (optional) list of names of macro variables to print;
%* prefix    (optional) prefix to print before each item in list;
%* suffix    (optional) suffix to print after each item in list;

%macro xput(__msg,__list,__prefix,__suffix);
   %if %bquote(&__msg)^= %then %put &__msg;
   %local __j __token;
   %let __j=1;
   %let __token=%scan(&__list,&__j,%str( ));
   %do %while(%bquote(&__token)^=);
      %* token must be unquoted in the %put statement;
      %put &__prefix%upcase(&__token)=&&&__token.&__suffix;
      %let __j=%eval(&__j+1);
      %let __token=%scan(&__list,&__j,%str( ));
   %end;
   %if &__j>2 %then %put ;
%mend xput;


%************************** xbug ******************************;
%* print values of a list of macro variables if debugging is on;
%* msg       (optional) message of whatever sort to print;
%* list      (optional) list of names of macro variables to print;

%macro xbug(__msg,__list);
   %if &_debug_ %then %xput(&__msg,&__list,debug:%str( ),!);
%mend xbug;


%macro xbugdo(stmts);
   %if &_debug_ %then %do;
      &stmts
   %end;
%mend xbugdo;

%macro xxbug(__msg,__list);
   %if &_xdebug_ %then %xput(&__msg,&__list,xdebug:%str( ),!);
%mend xxbug;


%************************** xecho ******************************;

%macro xecho(__level,__name,__list);
   %if &_echo_>=&__level %then
      %xput(Arguments to the %upcase(&__name) macro:,&__list,%str(   ));
%mend xecho;


%************************** xdo_by *****************************;
%* define DO and END statements for looping over BY groups and
   observations in a DATA step;

%macro xdo_by;
   _end=0;
   do until(_end);
      _nby+1;
%mend;

%macro xend_by;
   end;
%mend;

%macro xdo_obs(data);
   %* expects macro variables from xbylist;
   %if %bquote(&by)^= %then %do;
      last.&_bylast=0;
      do until(last.&_bylast);
         set %unquote(&data) end=_end;
   by &_bydata;
   %end;
   %else %do;
      _end=0;
      do until(_end);
         set %unquote(&data) end=_end;
   %end;
%mend;

%macro xend_obs;
   end;
%mend;


%************************** xlag *********************************;
%* xlag is a replacement for LAGn that allows for varying lags.
   xlagi declares arrays. xlagr empties the queue. xlag adds a
   value to the queue and returns a lagged value. The xlagfq...
   versions allow a frequency variable.

   maxlag     maximum lag that will be asked for
   ret        returned lagged value (variable name)
   val        current value to be added to the lag queue
   lag        size of lag: from 0 to &maxlag
   ;

%macro xlagi(maxlag);
   drop _tmplag;
   array _lag _lag1-_lag&maxlag;
   retain _lag1-_lag&maxlag . _curlag 0;
%mend xlagi;

%macro xlagr(maxlag);
   _curlag=0;
   do _tmplag=1 to &maxlag;
      _lag[_tmplag]=.;
   end;
   %if &_xdebug_ %then %do;
      put 'xlagr ' _curlag=;
      put 'xlagr ' _lag[*]=;
   %end;
%mend xlagr;

%macro xlag(ret,val,lag,maxlag);
   %if &_xdebug_ %then %do;
      put 'xlag ' _curlag= &val= &lag=;
   %end;
   _curlag+1;
   if _curlag>&maxlag then _curlag=1;
   if &lag=0 then &ret=&val;
   else do;
      _tmplag=_curlag-&lag;
      if _tmplag<=0 then _tmplag+&maxlag;
      &ret=_lag[_tmplag];
   end;
   _lag[_curlag]=&val;
   %if &_xdebug_ %then %do;
      put 'xlag ' _curlag= &ret=;
      put 'xlag ' _lag[*]=;
   %end;
%mend xlag;

%macro xlagfqi(maxlag);
   drop _tmplag;
   array _lag _lag1-_lag&maxlag;
   array _lfq _lfq1-_lfq&maxlag;
   retain _lag1-_lag&maxlag . _lfq1-_lfq&maxlag 0
          _outlag _inlag _totlag 0;
%mend xlagfqi;

%macro xlagfqr(maxlag);
   _outlag=1;
   _inlag=0;
   _totlag=0;
   %if &_xdebug_ %then %do;
      put 'xlagfqr ' _outlag= _inlag= _totlag=;
   %end;
%mend xlagfqr;

%macro xlagfq(ret,val,fq,lag,maxlag);
   %if &_xdebug_ %then %do;
      put 'xlagfq ' _outlag= _inlag= _totlag= &val= &fq= &lag=;
   %end;
   _inlag+1;
   if _inlag>&maxlag then _inlag=1;
   _totlag+&fq;
   _tmplag=_totlag-_lfq[_outlag];
   do while(_tmplag>=&lag+1);
      _outlag+1;
      if _outlag>&maxlag then _outlag=1;
      _totlag=_tmplag;
      if _outlag=_inlag then leave;
      _tmplag=_tmplag-_lfq[_outlag];
   end;
   if _totlag<&lag+1 then &ret=.;
   else if _outlag=_inlag then &ret=&val;
   else &ret=_lag[_outlag];
   _lag[_inlag]=&val;
   _lfq[_inlag]=&fq;
   %if &_xdebug_ %then %do;
      put 'xlagfq ' _outlag= _inlag= _totlag= &ret=;
      put 'xlagfq ' _lag[*]=;
      put 'xlagfq ' _lfq[*]=;
      if ^(_totlag>=&lag+1) then put 'xlagfq fail 1';
      if ^(&lag+1>_totlag-_lfq[_outlag]) then put 'xlagfq fail 2';
   %end;
%mend xlagfq;


%************************** xmerge *******************************;
%* xmerge acts like a MERGE statement with a BY statement even if
   there are no BY variables;

%macro xmerge(data1,data2);
   %if %bquote(&by)^= %then %do;
      merge %unquote(&data1) %unquote(&data2);
      by &_bydata;
   %end;
   %else %do;
      if _end1 & _end2 then stop;
      if ^_end1 then set %unquote(&data1) end=_end1;
      if ^_end2 then set %unquote(&data2) end=_end2;
   %end;
%mend xmerge;


%************************** xsubstr ******************************;
%* substr that accepts nonpositive lengths and other non-nice input;

%macro xsubstr(str,pos,len);
   %local r;
   %let r=%eval(%length(&str)-%eval(&pos)+1);
   %if &len= %then %let len=&r;
   %else %if &len>&r %then %let len=&r;
   %if &len<=0 %then;
   %else %qsubstr(&str,&pos,&len);
%mend xsubstr;


%************************** xbylist ******************************;
%* take a BY list for a PROC step and make a BY list for a DATA step,
   make a list with only the variable names, find last BY variable,
   and construct BY line. Abbreviated variable lists may not produce
   correct BY groups if formats combine several unformatted values
   into one formatted value, hence there is a warning for abbreviated
   lists.

   input:
      &by        BY list for a PROC step, usually an argument to the
                 main macro

   global variables returned:
      _bydata    BY list for a DATA step
      _byvars    list of variable names only
      _bylast    last BY variable
      _byline    BY line
   ;

%macro xbylist;

   %global _bydata _byvars _bylast _byline;
   %local n token;

   %let _byline=;
   %let _bydata=;
   %let _byvars=;
   %let _bylast=;
   %let n=1;
   %let token=%qupcase(%qscan(&by,&n,%str( )));

   %do %while(&token^=);
      %if %index(&token,-) %then
         %put WARNING: Abbreviated BY list &token.;
      %if &token=DESCENDING |
          &token=NOTSORTED %then %do;
         %let _bydata=&_bydata &token;
      %end;
      %else %do;
         %let token=%unquote(&token);
         %let _byline=&_byline &token=;
         %let _bydata=&_bydata GROUPFORMAT &token;
         %let _byvars=&_byvars &token;
         %let _bylast=&token;
      %end;
      %let n=%eval(&n+1);
      %let token=%qupcase(%scan(&by,&n,%str( )));
   %end;

   %xxbug(XBYLIST, by _bydata _byvars _bylast _byline)

%mend xbylist;


%************************** xvfreq ******************************;
%* process FREQ variable, define FREQ statement &fqstmt;

%macro xvfreq(data);
   %global fqstmt fqopt;
   %if %bquote(&freq)^= & %bquote(&data)^= %then %do;
      %xvlist(data=&data,_list=freq,_name=freq,_count=nfreq,
              valid=01)
      %if &nfreq>1 %then %do;
         %let _xrc_=There may be at most one FREQ= variable;
         %xchkerr(freq);
         %let freq=&freq1;
      %end;
      %let fqstmt=freq &freq %str(;);
      %let fqopt=1;
   %end;
   %else %do;
      %let fqstmt=;
      %let fqopt=0;
   %end;
   %xxbug(XVFREQ, fqopt fqstmt);
%mend xvfreq;


%************************** xvweight ****************************;
%* process WEIGHT variable, define WEIGHT statement &wtstmt;

%macro xvweight(data);
   %global wtstmt wtopt;
   %if %bquote(&weight)^= & %bquote(&data)^= %then %do;
      %xvlist(data=&data,_list=weight,_name=weight,_count=nwght,
              valid=01)
      %if &nwght>1 %then %do;
         %let _xrc_=There may be at most one WEIGHT= variable;
         %xchkerr(weight);
         %let weight=&weight1;
      %end;
      %let wtstmt=weight &weight %str(;);
      %let wtopt=1;
   %end;
   %else %do;
      %let wtstmt=;
      %let wtopt=0;
   %end;
   %xxbug(XVWEIGHT, wtopt wtstmt);
%mend xvweight;


%************************** xvlist ******************************;
%* process a variable list, allowing abbreviated lists, and return
   information about each variable and/or the entire list.
 * Create a data set named according to the value of the _name
   argument prefixed by an underscore that contains information
   about the variables from PROC CONTENTS.
 * Optionally create macros containing:
      name
      type (1=numeric 2=char)
      format (optionally specify default format and length)
   for each variable.
 * Optionally remove variables from the list that have been
   specified in one or more other variable lists.
 * Optionally return the number of variables.
 * Optionally return the list type (0=empty 1=numeric 2=char 3=mixed)
   and/or validate the list type and issue an error for an invalid
   list type.
 * Optionally replace abbreviated variable lists by an expanded list
   containing each individual variable name.
 * BY lists are allowed if the list name _list=by.
   ;

%macro xvlist(data=,_list=,_name=,_type=,_fmt=,dnf=BEST,dnl=8,dcf=CHAR,
              _count=,_ltype=,valid=,remove=,replace=0,format=);

%if &_xdebug_ %then %do;

   %put xvlist: data=&data!;     %* data set name;

   %put xvlist: _list=&_list!;   %* name of variable list. The list is
                                    updated if replace=1 is specified;
   %put xvlist: list value=&&&_list!;

   %put xvlist: _name=&_name!;   %* prefix for returned macro vars with
                                    numerical suffixes for individual
                                    variable names. if blank, no macro
                                    vars for names are returned.

   %put xvlist: _type=&_type!;   %* prefix for returned macro vars with
                                    numerical suffixes for individual
                                    variable types. if blank, no macro
                                    vars for types are returned;

   %put xvlist: _fmt=&_fmt!;     %* prefix for returned macro vars with
                                    numerical suffixes for individual
                                    variable formats. if blank, no macro
                                    vars for formats are returned;

   %put xvlist: dnf=&dnf!;       %* default numeric format;

   %put xvlist: dnl=&dnl!;       %* default numeric format length;

   %put xvlist: dcf=&dcf!;       %* default character format;
                                 %* to detect variables for which no
                                    format has been specified, use:
                                       dnf=,dnl=0,dcf=
                                    and check for a format of 0.;

   %put xvlist: _count=&_count!; %* returned: number of variables.
                                    if blank, no count is returned;

   %put xvlist: _ltype=&_ltype!; %* returned: list type:
                                              0=empty list
                                              1=all numeric
                                              2=all character
                                              3=mixed
                                    if blank, no list type is returned;

   %put xvlist: valid=&valid!;   %* string of digits 0-3 specifying
                                    valid list types (_ltype). If the
                                    list type is not in this list, an
                                    error is issued. If blank, no
                                    validation is done, but some proc
                                    steps may issue warnings if the
                                    variable list is empty. Hence,
                                    valid= should always be used for a
                                    list that is allowed to be empty,
                                    such as a BY list, for which use
                                    valid=0123;

   %put xvlist: remove=&remove!; %* list of _names of other variable
                                    lists to be removed from this list.
                                    These _name values must have been
                                    processed in previous calls to
                                    xvlist. the _name values should be
                                    separated by blanks. This argument
                                    may be blank if no variables need
                                    to be removed;

   %put xvlist: replace=&replace!; %* if replace=1, the value of _list
                                    is replaced by an expanded list
                                    containing individual variable
                                    names instead of abbreviated lists.
                                    For example, X1-X3 is replaced by
                                    X1 X2 X3. However, if you use this
                                    with a BY list, the DESCENDING and
                                    NOTSORTED keywords will be lost;

   %put xvlist: format=&format!; %* format list to be used in a FORMAT
                                    statement. If blank, then no FORMAT
                                    statement is used;

   %put ;

%end;

   %global &_count &_ltype;
   %local __count __j __token;

   %************ see if empty list is ok;
   %if %bquote(&&&_list)= %then %if %index(&valid,0) %then %do;
      %if &_count^= %then %let &_count=0;
      %goto exit;
   %end;

   %************ if the _list is to be replaced, _name must
      be a valid prefix;
   %if &replace %then %if %bquote(&_name)= %then %let _name=_;

   %************ if valid= is specified, have to get list type;
   %if %bquote(&valid)^= %then %if &_ltype= %then %let _ltype=_xtype;

   ************ make a small data set so summary wont take a lot of
     time --cant use obs= data set option on the real data set ******;
   %if %bquote(&data)= %then %goto exit;
         data _XTMP0;
            set %unquote(&data);
            %if %bquote(&format)^= %then %do;
               format %unquote(&format);
            %end;
            stop;
         run;
   %if &syserr>4 %then %goto fail;

   ************ use summary to reorder the variables;
         proc summary data=_XTMP0(firstobs=1 obs=1);
            output out=_XTMP1(drop=_TYPE_ _FREQ_);
   %if %qupcase(&_list)=BY %then %do;
            by %unquote(&&&_list);
   %end;
   %else %do;
            id %unquote(&&&_list);
   %end;
         run;
   %if &syserr>4 %then %do;
      %put ERROR: "%upcase(&_list)=&&&_list" is probably invalid.;
      %goto fail;
   %end;

   ************ make data set with variable names;
         proc contents data=_XTMP1(firstobs=1)
            %if &_fmt^= & &_xsysver>=608 %then fmtlen;
            noprint out=_&_name;
         run;
   %if &syserr>4 %then %goto fail;
   %if &_xdebug_>=2 %then %do;
         proc print data=_&_name(firstobs=1 obs=2000000000); run;
   %end;

   ************ remove variables from other lists;
   %if &remove^= %then %do;
      %let __j=1;
      %let __token=%scan(&remove,&__j);
      %do %while(&__token^=);
         proc sql;
            delete from _&_name(firstobs=1 obs=2000000000)
               where name in
                  (select name
                     from _&__token(firstobs=1 obs=2000000000));
         run;
         %if &syserr>4 %then %goto fail;
         %let __j=%eval(&__j+1);
         %let __token=%scan(&remove,&__j);
      %end;
   %end;

   ************ order by position in data set, not alphabetically;
   %xnobs(_xvntmp,_&_name(firstobs=1 obs=2000000000))
   %if &_xvntmp %then %do; %* avoid spurious warning from SORT;
         proc sort force
            data=_&_name(firstobs=1 obs=2000000000);
            by npos; run;
      %if &syserr>4 %then %goto fail;
   %end;

   ************ find out how many variables and what types;
   %let __count=0;
   %if &_ltype^= %then %let &_ltype=0;
         data _null_;
   %if &_ltype^= %then %do;
            retain ltype 0;
   %end;
            set _&_name(firstobs=1 obs=2000000000) nobs=count end=end;
            if _n_=1 then do;
   %if &_xdebug_ %then %do;
               put 'xvlist: count=' count;
   %end;
               call symput("__count",trim(left(put(count,5.))));
   %if &_ltype^= %then %do;
                  ltype=type;
   %end;
            end;
   %if &_ltype^= %then %do;
            else if ltype^=type then ltype=3;
            if end then call symput("&_ltype",left(put(ltype,5.)));
   %end;
   %else %do;
            stop;
   %end;
         run;
   %if &syserr>4 %then %goto fail;

   %if &_xdebug_ %then %do;
      %put xvlist: count=&__count!;
      %if &_ltype^= %then %put xvlist: ltype=&&&_ltype;
   %end;

   %************ validate list type;
   %if %bquote(&valid)^= %then %if %index(&valid,&&&_ltype)=0 %then %do;
      %let _xrc_=The %qupcase(&_list)= variable list must;
      %if &&&_ltype=0 %then %let _xrc_=%qcmpres(&_xrc_
         contain at least one variable);
      %else %if %verify(&valid,02)=0 %then %let _xrc_=%qcmpres(&_xrc_
         contain only character variables);
      %else %if %verify(&valid,01)=0 %then %let _xrc_=%qcmpres(&_xrc_
         contain only numeric variables);
      %else %if %verify(&valid,012)=0 %then %let _xrc_=%qcmpres(&_xrc_
         not contain both numeric and character variables);
      %else %let _xrc_=%qcmpres(&_xrc_ ???);
      %put ERROR: &_xrc_..;
      %goto finish;
   %end;

   %if &_count^= %then %let &_count=&__count;

   ************ make macro variables global before defining them;
   %if &_name^= %then %do __j=1 %to &__count;
      %global &_name&__j;
   %end;
   %if &_type^= %then %do __j=1 %to &__count;
      %global &_type&__j;
   %end;
   %if &_fmt^=  %then %do __j=1 %to &__count;
      %global &_fmt&__j;
    %end;

   ************ assign variable names and formats to macro variables;
   %if &_name^= | &_type ^= | &_fmt^= %then %do;
         data _null_;
            set _&_name(firstobs=1 obs=2000000000);
      %if &_name^= %then %do;
            call symput("&_name"||left(put(_n_,5.)),trim(name));
      %end;
      %if &_type^= %then %do;
            call symput("&_type"||left(put(_n_,5.)),put(type,1.));
      %end;
      %if &_fmt^= %then %do;
            length fmt $20;
            if format=' ' then do;
               if type=1 then do;
                  if formatl=0 then format="&dnf";
               end;
               else format="&dcf";
            end;
            if formatl=0 then if format^=' ' then do;
               if type=1 then formatl=&dnl;
                         else formatl=length;
            end;
            fmt=trim(format)||
                trim(left(put(formatl,3.)))||'.';
            if type=1 then
               fmt=trim(fmt)||trim(left(put(formatd,3.)));
            call symput("&_fmt"||left(put(_n_,5.)),trim(fmt));
      %end;
         run;
      %if &syserr>4 %then %goto fail;
   %end;

   %************ replace _list with expanded list;
   %if &replace %then %do;
      %let &_list=%xconcat(&_name,&__count);
      %if &_xdebug_ %then %do;
         %put xvlist: replaced list=&&&_list;
      %end;
   %end;

   %goto finish;

%fail:
   %xerrset(Processing %qupcase(&_list) variable list failed)

%finish:
   %xdelete(_XTMP0 _XTMP1);

%exit:
%mend xvlist;


%************************** xdelete *****************************;
%* delete work data sets;
%* list      list of data sets to delete;

%macro xdelete(list);
   %if %bquote(&list)^= %then %do;
      %if &_xdebug_ %then %put XDELETE: &list;
      proc datasets nolist nowarn;
         delete %unquote(&list) / memtype=data;
      quit;
   %end;
%mend xdelete;


%************************** xfloat *****************************;
%* evaluate an expression in floating point arithmetic in a DATA step;
%* _result   name of macro variable to assign result to;
%* express   expression;
%* format    (optional) format for converting the floating point result;

%macro xfloat(_result,express,format);
   %if %bquote(&format)= %then %let format=best12.;
   %let &_result=.;
   data _null_;
      result=%unquote(&express);
      call symput("&_result",trim(left(put(result,&format))));
   run;
   %if &syserr>4 %then %xerrset(DATA step to compute floating point
                            expression &express failed);
   %else %if &_xdebug_ %then
      %put XFLOAT: &_result<-(&express)=&&&_result;
%mend xfloat;


%************************** xnobs *****************************;
%* find number of observations in a data set;
%* _xn       name of macro variable to assign number of observations;
%* data      data set;

%macro xnobs(_xn,data);
   %if &_xdebug_ %then %put XNOBS: _xn=&_xn data=&data;
   %global &_xn;
   %let &_xn=0;
   data _null_;
      if 0 then set %unquote(&data) nobs=_xn;
      call symput("&_xn",trim(left(put(_xn,12.))));
      stop;
   run;
   %if &syserr>4 %then %xerrset(DATA step to find number of
      observations in data set %qupcase(&data) failed);
   %else %if &_xdebug_ %then %put XNOBS: &_xn=&&&_xn;
%mend xnobs;


%************************** xnvar *****************************;
%* find number of variables in an array;
%* _xn       name of macro variable to assign number of variables;
%* data      data set;
%* list      list of variables in array;

%macro xnvar(_xn,data,list);
   %global &_xn;
   %let &_xn=0;
   data _null_;
      if 0 then set %unquote(&data);
      array _xnvar [*] %unquote(&list);
      call symput("&_xn",trim(left(put(dim(_xnvar),12.))));
      stop;
   run;
   %if &syserr>4 %then %xerrset(DATA step to find number of
     variables in an array in data set %qupcase(&data) failed);
   %else %if &_xdebug_ %then %put XNVAR: &_xn=&&&_xn;
%mend xnvar;


%************************** xdsinfo *****************************;
%* return libname, memname, type, label;
%* data      data set;

%macro xdsinfo(data);
   %global _xdslib _xdsmem _xdstype _xdslab;
   %if %bquote(&data)= %then %goto exit;
   %let _xdstype=?;
   proc contents data=%unquote(&data) out=_TEMP_ noprint; run;
   %if &syserr>4 %then %goto error;
   data _null_;
      set _TEMP_(firstobs=1 obs=1);
      call symput("_xdslib",trim(libname));
      call symput("_xdsmem",trim(memname));
      call symput("_xdstype",trim(typemem));
      call symput("_xdslab",trim(memlabel));
      stop;
   run;
   %if &syserr>4 %then %goto error;
   %xxbug(XDSINFO,data _xdslib _xdsmem _xdstype _xdslab)
   %goto exit;
%error:
   %xerrset(Failure getting information for data set %qupcase(&data));
%exit:
   %xdelete(_TEMP_)
%mend xdsinfo;


%****** replace all occurrences of a substr with another string ******;
%macro xrepstr(source,replace,with);
   %local result pos len prev;
   %let result=;
   %let len=%length(&replace);
   %let pos=%index(&source,&replace);
   %do %while(&pos);
      %let prev=%xsubstr(&source,1,&pos-1);
      %let result=&result&prev&with;
      %let source=%xsubstr(&source,&pos+&len);
      %let pos=%index(&source,&replace);
   %end;
   %let result=&result&source;
   &result
%mend xrepstr;


%****** replace all occurrences of a token with another string ******;
%macro xreptok(source,replace,with);
   %local result tok;
   %let replace=%qtrim(%qleft(&replace));
   %let result=%xsubstr(&source,1,%xverify(&source,)-1);
   %do %while(%xscan(tok,source)^=B);
      %if &tok=&replace %then %let result=&result&with;
                        %else %let result=&result&tok;
      %let result=&result%xsubstr(&source,1,%xverify(&source,)-1);
   %end;
   &result
%mend xreptok;

%****** returns concatenation of &n macro variables named &_name.1 to
        &&_name&n with intervening blanks. This is a trivial thing to
        do, but if you do it the obvious way with a %LET statement in
        a loop, performance is terrible;
%macro xconcat(_cat,n);
   %local __i;
   %do __i=1 %to &n; &&&_cat&__i %end;
%mend xconcat;


%****** set global variable to indicate that xmacro has been processed;

%global _xmacro_;
%let _xmacro_=610;

 /*********************** End Utility Macros *************************/
