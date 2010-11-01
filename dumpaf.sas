/*
On Wed, 18 Oct 1995, Paul Robins wrote:

> The problem:
>
> We have a project which involves maintaining approx 300 SCL
> programs, which have already been created. We would like to
> be able to extract them to standard ascii files so that we
> can use grep/awk etc on the source. We really don't want to
> have to open each catalogue/program independantly and 'save to
> file'.
>
> Has anyone written something to do this painlessly?
>

Paul,

I had a chat about this to a colleague and they knocked up the following
code.  It seems to do the trick under SAS v6.10 under MS/Windows.  For other
operating systems you may need to make some modifications (e.g. The check for
form feed is ASCII 13, I don't know what value a form feed has under (e.g.)
EBCDIC).  I suspect that you are running SAS under UNIX; in which case just
change the file locations.

The code follows, good luck ....
e-mail : jamie_roberts@sandwich.pfizer.com
*/

***************************************************************** ;
** PROGRAM : DUMPAF                    TYPE: Base SAS          ** ;
** AUTHOR  : Darrell Edgley, Praxis plc, 20 Manvers Steet,     ** ;
**                           Bath BA1 1PX, England, UK         ** ;
** DATE    : 19OCT1995                                         ** ;
** FUNCTION: Read a SAS AF catalogue and dump its source code  ** ;
**           to ASCII files.                                   ** ;
** NOTES   : The output from this routine is sent to the       ** ;
**           directory C:\TEMP.  This directory must exist     ** ;
**           or be reassigned.                                 ** ;
**           Written and tested under SAS v6.10 under          ** ;
**           MS/Windows v3.1                                   ** ;
** ----------------------------------------------------------- ** ;
** MODIFICATION HISTORY:                                       ** ;
** =====================                                       ** ;
** NAME    :                 DATE: ddmonyy   CC No. :          ** ;
** REASON  :                                                   ** ;
***************************************************************** ;

%macro dumpaf(libname=, cat=, temp=C:\temp);

** Assign a file reference for the PROC BUILD print file ....  ** ;
FILENAME buildprt '&temp\buildprt.tmp' ;

** Run PROC BUILD, send PRINT output to the BUILDPRT file ref. ** ;
** NOTE: It is necessary to repeat the PRINT line for each     ** ;
**       entry type ....                                       ** ;

PROC BUILD CAT = &libname..&cat BATCH ;
  PRINT SOURCE ET = program PRTFILE = buildprt  ;
  PRINT SOURCE ET = scl     PRTFILE = buildprt APPEND  ;
RUN ;

** Read the external file, and create individual ASCII files   ** ;
** for each SAS/AF SCL entry encountered ....                  ** ;
DATA work.readit  ;
  INFILE buildprt LRECL = 200 PAD MISSOVER ;
  LENGTH memname memtype filetype $8 pathname $40 dataline $200 ;
  RETAIN memname memtype filetype pathname ' ' ;

  ** Check for NEW PAGE (ASCII 13), You may need to modify     ** ;
  ** this for non-ASCII (e.g. IBM Mainframes use EBCDIC)       ** ;
  INPUT @1 cc_chk $1 @ ;

  IF cc_chk <= PUT('13'X,2.) THEN DO ;
    ** Form feed character encountered.  Ignore this line cos  ** ;
    ** it only contains print time info (i.e. date and time    ** ;
    ** AND page number.  Also, skip the next line because it   ** ;
    ** will be blank ....                                      ** ;

    pathname = ' ' ;
    dataline = 'FF' ; output ;
    INPUT / ;
  END ;

  ** Read a line from the external file ....                   ** ;
  INPUT @1 dataline $200. ;

  ** .... Check this 'read' line for key elements ....         ** ;

  IF INDEX(dataline,'ENTRY:') AND
     INDEX(UPCASE(dataline),'LAST UPDATED:') THEN DO ;
    ** This line contains the program header information, i.e. ** ;
    ** the member name and type, its description and when      ** ;
    ** it was last modified.                                   ** ;
    ** ------------------------------------------------------- ** ;
    ** Get the member name and type ....                       ** ;

    pos = INDEX(dataline,'ENTRY:') ;

    pos = pos + 7 ;

    member = SCAN(SUBSTR(dataline,pos,17),1,' ') ;

    memname = SCAN(member,1) ;
    memtype = SCAN(member,2) ;

    INPUT / @1 dataline $200. ;

    SELECT(memtype) ;
      WHEN('SCL')     filetype = 'SCL' ;
      WHEN('PROGRAM') filetype = 'SAS' ;
      OTHERWISE       filetype = 'OTH' ;
    END ;

    ** Assign the name of the ASCII file to be created ....    ** ;
    pathname = "&temp" !! memname !! '.' !! filetype ;
  END ;

  IF INDEX(dataline,'**** SOURCE ****') THEN DO ;
    ** Start of a new member ....                              ** ;

    INPUT / @1 dataline $200. ;
  END ;

  OUTPUT ; ** To data set, so we can check the results ....    ** ;

  IF pathname NE ' ' THEN DO ;
    ** Output to ASCII files.  Using the FILEVAR option will   ** ;
    ** create physical files depending on the current value    ** ;
    ** of the PATHNAME variable ....                           ** ;

    FILE outfile FILEVAR = pathname NOPRINT NOTITLES ;
    PUT @1 dataline ;
  END ;
RUN ;
%mend;
