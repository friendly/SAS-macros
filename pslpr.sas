 /*--------------------------------------------------------------*
  *    Name: pslpr.sas                                           *
  *   Title: Set graphics paraemters for PostScript lpr output   *
  *                                                              *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  5 Dec 1996 14:30:47                                *
  * Revised:  5 Jan 1997 10:46:34                                *
  * Version:  1.1                                                *
  *--------------------------------------------------------------*/

%macro pslpr;
%let dev=pslpr;
%*-- Assume LPDEST is set to a PS printer;
filename gsasfile pipe 'lpr';
goptions device=pscolor gaccess=gsasfile gend='0A'x
 		 gsflen=80 gsfmode=append;
goptions colors=(black);
%mend;