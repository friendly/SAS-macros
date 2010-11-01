Each new major SAS release adds new procs, features, options, etc.

This is great for some users, but creates a major migrane for people
who write macros (or programs) for public distribution
which can deal gracefully with these changes.  For example,
many of my macro programs are now sprinkled with code like

%if &sysver>7 
   %then %do; ... %end;
   %else %do; ... %end;

I've had to do this mostly to handle V7+ variable name conventions,
but also to take account of better ways to do things, while
maintaining backward compatibility.

As well, it is still hard to write macros which use other macros (as utilities), without
demanding that users edit the macro code or setup appropriate
FILENAMEs or LIBNAMEs first. 

I'm looking for a general way for a macro or program to check for its required
dependencies, including 
(a) a given SAS version (or later), 
(b) given SAS products, and 
(c) other macros which the given macro invokes, assumed to be
searchable in the autocall (SASAUTOS) library path(s).  

The idea is similar to that of the 'require' and 'use' statements
of Perl, where one can say,

require 5.004;         # only runs under perl 5.004 or later
require 'mylib.pl';    # %include(mylib.pl) if it can be found, else fail
use File::Basename;    # %include File/Basename.pm if found, else fail

there is also a a CPAN.pm module and a command-line cpan utility
to download and install required modules.

Similarly, R (http://www.r-project.org) has a require() function
that package developers can use to make sure that other packages
have been installed on the local system,

require(lattice)       # we need lattice graphics to run

or, in a script that needs some other functions or data, one can say

source("http://www.abc.com/mycode.R")

which functions like %include ... except that it can read from the
internet rather than just a local file.


By analogy, I'd like to be able to say that a given program or
macro requires SAS 8.2 or better, SAS/IML, and the %lowess macro,
as

   %require(8.2 IML macros(lowess));
or
   %require(8.2);  %require(IML); %require(macros(lowess));
or even
   %require(sasautos(lowess), url=http://euclid.psych.yorku.ca/ftp/sas/sssg/macros/lowess.sas);

Even better, a global SAS require statement or macro *should* be a builtin in SAS.
Macros are often
a simple way for developers to get around SAS deficiencies.
 
It is not too hard to implement a rudimentary 'require' as a macro 
(see below). But then (a) %require becomes another dependency, 
(b) the macro craps out gracelessly if a to-be-%included file
cannot be found, and (c) there is no general way to test for the
presence of a macro which may be found on the SASAUTOS path.
Someone correct me if I'm wrong here.

E.g., '%require(LOWESS);' will work only if lowess.sas is in the current
directory, or defined in a FILENAME statement.  One could say 
'%require(macros(LOWESS));' if there is
a LIBNAME MACROS.  But, it would be better to be able to say
%let rc = %require(sasautos(LOWESS)); 
to test if the macro can be found
on the SASAUTOS path.

From my experiments with this, it seems that a global 'require' statement
(or native %require macro function)
should be implemented as part of base SAS -- with a testable return
code. This would allow macros to take corrective action (if possible)
rather than just failing.

In the meantime, I wonder if it is possible to improve on this
require macro, for the purposes outlined above.

 /*--------------------------------------------------------------*
  *    Name: require.sas                                         *
  *   Title: Check requirements for a macro or program           *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 23 Apr 2000 11:39:16                                *
  * Revised: 17 Apr 2003 15:55:16                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The REQUIRE macro checks dependencies for a program on: a given SAS
 version, SAS products, and other macros or files which need to be
 %included.

=Usage:

 The REQUIRE macro is defined with one positional parameter, a
 blank-separated list of requirements.   It returns a null value
 on success (all requirements met), otherwise a numeric return
 code.

 For example: 
 
	%let rc = %require(6.12 IML data(baseball) macros(lowess));

 A numeric value is tested as a SAS version (&sysver); a character
 value included in the list SASPROD is tested as a SAS product;
 otherwise, the string is tested as a %include- able file.  Includ-able
 files may be specified as full-path names (not very general),
 or as members of a SAS library pre-declared in a libname statement.

=Limitations:

 - This should be a built-in macro function, perhaps with an extended
   syntax (version=, product=, etc.)
 - Fails if can't %include
 - Should be able to say 
    %require(sasautos::lowess sasautos::words)
	or (better):
    %require(sasautos(lowess) sasautos(words))
 to check if these macros are availabile via SASAUTOS
 - Should be able to say
    %require(sasautos(lowess), url=http://www.math.yorku.ca/SCS/sasmac/)
 to download (and install) these.
 
 =*/

%macro require(stuff);

%local i v;
%let rc=;      %*--return code;
%let i=1;
%let v = %upcase(%scan(&stuff,1,%str( )));
%*-- (incomplete) list of SAS products;
%let sasprod = CONNECT ETS GRAPH INSIGHT IML STAT OR;

%do %while(&v^= );
%put Checking v=&v;
	%*-- Check for proper SAS version;
	%if %datatyp(&v)=NUMERIC %then %do;
		%if &v > &sysver %then %do;
			%let rc=&rc 1;
			%put ERROR:  This program requires version &v of The SAS System;
			%end;
		%end;
	%*-- Check for SAS product installed;
	%else %if %index(&sasprod, %upcase(&v))> 0 %then %do;
		%if %sysprod(&v) = 0 %then %do;
			%let rc=&rc 2;
			%put ERROR:  This program requires the SAS &v product;
		%end;
	%*-- Check for include-able file;
	%* (This will fail if the file cannot be found)
	%else %do;
		%include &v;
		%end;

	%let i = %eval(&i+1);
	%let v = %upcase(%scan(&stuff,&i,%str( )));;
   %end;

%done:
    &rc

%mend;
