 /*--------------------------------------------------------------*
  *    Name: delgcat                                             *
  *   Title: Delete entries in a graphics catalog                *
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 18 Apr 2003 09:31:14                                *
  * Revised: 18 Apr 2003 09:31:14                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The DELGCAT macro deletes entries in a graphics catalog

 
==Parameters:

* CATALOG=          The name of the graphics catalog [Default: WORK.GSEG]

* ENTRIES=          Name(s) of entries to delete [Default: _ALL_]


 =*/
%macro delgcat(catalog=work.gseg, entries=_all_);

%if %sysfunc(cexist(&catalog)) %then %do;
	proc greplay igout=&catalog nofs;
		delete &entries;
		run; quit;
	%end;
%mend;
