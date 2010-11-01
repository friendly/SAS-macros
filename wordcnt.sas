%*---------------------------------------
%*  Program:        words.sas
%*  Author:         Jack Hamilton
%*  Modifications:  Karsten M. Self (kmself@ix.netcom.com)
%*  Date:           2/26/96
%*
%*  Description:    Returns number as count of 'words' in the parameter.  
%*		    Optional - specify delimiter (default = ' ' -- space)
%*		
%*
%*  Features:
%*		
%*
%*  Bugs and lapses:
%*		
%*
%*  Additional features wish list:
%*
%*		'-' indicates wanted
%*		'x' indicates filled
%*		'o' indicates removed
%*
%*
%*
%* --------------------
%*  Revised by:     KMS	
%*  Revision Date:  
%*  Version:	    0.1
%*
%* --------------------
%*  Modification log:
%*
%*  date	pgmr	ver 	notes
%* -------	----	----	-----
%*  2/28/96		kms	0.1	Created program.  Copy to Norm.
%*
%*
%*---------------------------------------
;


%*---------------------------------------;

%macro words( string, delim= %str( ) );

    %local count;

    %let count= 0;

    %if %length( &string ) gt 0 %then
	%do;
	%let string2= %scan( &string, 1, %str(&delim) );

	%do %while( %length( &string2. ) ne 0 );
	    %let count = %eval( &count + 1 );
	    %let string2 = %scan( &string, &count+1, %str(&delim.) );
	    %end;
	%end;

    &count

    %mend;

    %*---------------------------------------;
