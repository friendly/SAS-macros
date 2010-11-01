/* Select either "standard" or "linedraw" characters for FORMCHAR */
%macro formchar ( chartype  /* STANDARD or LINEDRAW               */
                ) ;
   /* ------------------------------------------------------------
      MODULE:     FORMCHAR

      PURPOSE:    Resets the value of the FORMCHAR system option
                  to use either "linedraw" characters from the SAS
                  Monospace font or "standard" characters (+- etc.)
                  for box characters in TABULATE and other PROCs.
                  Generates an OPTIONS statement and may be used
                  wherever that is accepted.

      CLASS:      General SAS statement.

      USAGE:      %FORMCHAR(LINEDRAW)

      PARAMETERS: R chartype Must be either 
		            LINEDRAW, LINE, DRAW or 
						STANDARD, STD, ASCII

      SIDE EFFECTS: None.
      SYSTEMS:    Windows, 6.11 and higher.
      HISTORY:    25feb96  MDR
      DOCUMENT:   Here.
      SUPPORT:    Mike Rhoads <rhoadsm1@westat.com>
      ------------------------------------------------------------
   */

   %if &sysscp ^= WIN %then
     %put ERROR:  The FORMCHAR macro is only available under Windows.;
   %else %if &sysver < 6.11 %then
     %put ERROR:  The FORMCHAR macro is not available for versions prior to 6.11.;
   %else %do;
	%let chartype = %upcase(&chartype);
     %if %index(STD STANDARD ASCII,&chartype)>0 %then %do;
       OPTIONS FORMCHAR = '|----|+|---+=|-/\<>*';
     %end;
     %else %if %index(LINEDRAW,&chartype)>0 %then %do;
       OPTIONS FORMCHAR = '82838485868788898A8B8C2B3D7C2D2F5C3C3E2A'X;
     %end;
     %else
       %put ERROR:  Argument to FORMCHAR must be LINEDRAW or STANDARD.;
   %end;

%mend  FORMCHAR ;
