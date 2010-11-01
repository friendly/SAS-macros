 /*--------------------------------------------------------------*
  *    Name: gkill                                               *
  *   Title: Delete entries in a graphics catalog                *
        Doc: http://www.datavis.ca/sasmac/gkill.html       
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 18 Apr 2003 09:31:14                                *
  * Revised: 13 Jan 2004 11:52:41                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The GKILL macro deletes entries in a graphics catalog.  This is
 useful in situations where PROC GREPLAY is used repeatedly in a
 single session or batch file, and each new sequence of plots for 
 PROC GREPLAY should start with a clean slate. 

 
==Parameters:

* CATALOG=          The name of the graphics catalog [Default: WORK.GSEG]

* ENTRIES=          Name(s) of entries to delete [Default: _ALL_]


 =*/
%macro gkill(catalog=work.gseg, entries=_all_);

%if %sysfunc(cexist(&catalog)) %then %do;
	proc greplay igout=&catalog nofs;
		delete &entries;
		run; quit;
	%end;
%else %do;
	%put ERROR: the graphics catalog &catalog does not exist;
	%end;
%mend;
