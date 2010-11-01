%macro ifn (logical, true, false, missing );

%*----------------------------------------------------------------------
	replicates the release 9 ifn() function.
	if called in release 9, it uses ifn() function. otherwise, it
	resorts to numeric-char-numeric trick to mimic what ifn() does
	note 1:
	it is strongly recommended to put the arguments within a
	pair of parentheses, e.g. female = %ifn( (sex="F"), 1, 0)
	note 2:
	as with ifn() function, when there is no fourth parameter,
	specified, then it returns the false choicee when logical
	is evaluated to a missing value
	by chang y. chung and ian whitlock on aug. 2005
	SUGI paper 042-31
	http://www2.sas.com/proceedings/sugi31/042-31.pdf
---------------------------------------------------------------------* ;

%local list choice retvalue msg ;

%*-------------------------------------------------------------------* ;
%*-- first three parameters are required --* ;
	%let retvalue = . ;

	%if ( %superq ( logical ) = %str() ) %then
		%let msg = Logical-expression ;

	%if ( %superq ( true ) = %str() ) %then
	%do ;
		%if ( %superq ( msg ) ^= %str() ) %then
		%let msg = &msg, ;
		%let msg = &msg Value-returned-when-true ;
	%end ;
	%if ( %superq ( false ) = %str() ) %then
	%do ;
		%if ( %superq ( msg ) ^= %str() ) %then
		%let msg = &msg, ;
		%let msg = &msg Value-returned-when-false ;
	%end ;
	%if ( %superq ( msg ) ^= %str() ) %then %goto retrn ;
	%if ( %superq ( missing ) = %str() ) %then
	%let missing = &false ;

%*-------------------------------------------------------------------* ;
%*-- if release 9 or later, then just call the ifn() function      --* ;

	%if %sysevalf( &sysver >= 9.0 ) %then
	%let retvalue =
	ifn ( ( &logical ) , ( &true ) , ( &false ) , ( &missing ) )
;
%*-------------------------------------------------------------------* ;
%*-- if release 8 or ealier, then use the numeric-char-numeric     --* ;
%*-- trick to mimic what ifn() does                                --* ;
	%else
	%do ;
		%let list = put ( ( &true ) , best12. )
			|| put ( ( &false ) , best12. )
			|| put ( ( &missing ) , best12. ) ;
		%let choice = 1 + 12 * ( ( &logical ) = 0 )
			+ 12 * missing ( &logical ) ;
		%let retvalue = input ( substr ( &list , &choice , 12 ) , best12. ) ;
	%end ;
%*-------------------------------------------------------------------* ;
%*-- if error, then return a missing, set _ERROR_ to 1 --* ;
%retrn:
	%if ( %superq ( msg ) ^= %str() ) %then
	%put ERROR: (ifn) Missing required parameter(s): &msg.. ;
	&retvalue


/* short version, no error checking

%macro ifn (logical, true, false, missing );
	%local list choice retvalue msg ;

	%if ( %superq ( missing ) = %str() ) %then
	%let missing = &false ;

%*-- if release 9 or later, then just call the ifn() function      --* ;

	%if %sysevalf( &sysver >= 9.0 ) %then
	%let retvalue =
	ifn ( ( &logical ) , ( &true ) , ( &false ) , ( &missing ) )

	%else
	%do ;
		%let list = put ( ( &true ) , best12. )
			|| put ( ( &false ) , best12. )
			|| put ( ( &missing ) , best12. ) ;
		%let choice = 1 + 12 * ( ( &logical ) = 0 )
			+ 12 * missing ( &logical ) ;
		%let retvalue = input ( substr ( &list , &choice , 12 ) , best12. ) ;
	%end ;
%retrn:
	&retvalue
%mend ifn ;
*/

