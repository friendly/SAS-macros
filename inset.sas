 /*--------------------------------------------------------------*
  *    Name: inset.sas                                           *
  *   Title: Replay one graph inside another                     *
        Doc: http://www.datavis.ca/sasmac/inset.html       
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 17 Jan 2006 16:03:42                                *
  * Revised: 17 Jan 2006 16:03:42                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The INSET macro replays one graph inside another

=Usage:

 The INSET macro is defined with keyword parameters.  MAIN=, INSET=,
 AT= and SIZE= are required.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%inset(main=1, inset=2, at=70 5, size=30 30);
 
==Parameters:

* MAIN=       Name or number of the main graph in the GIN= graphics catalog

* INSET=      Name or number of inset graph in the GIN= graphics catalog

* AT=         The percentage lower left X,Y location of inset graph.  Two
              numbers; a single value is repeated.

* SIZE=       Percentage X,Y size of the inset graph.  Two
              numbers; a single value is repeated.

* GIN=        Name of input graphic catalog [Default: GIN=GSEG]

* GOUT=       The name of the output graphics catalog [Default: GOUT=GSEG]
                
* NAME=       The name of the replayed graph in the GOUT= catalog
              [Default: NAME=INSET]


 =*/
/*
Inset one graph inside another
*/
%macro inset(
   main=,                 /* name or number of main graph                 */
   inset=,                /* name or number of inset graph                */
   at=,                   /* % lower left X,Y location  of inset graph    */
   size=,                 /* % X,Y size of inset graph                    */
   gin=gseg,              /* name of input graphic catalog                */
   gout=gseg,             /* name of output graphic catalog               */
   name=inset             /* name of replayed graph in output catalog     */
    );

%let abort=0;
%if %length(&main)=0 or %length(&inset)=0 %then %do;
	%put ERROR:  The MAIN=&main and INSET=&inset graphs must both be specified.;
	%let abort=1;
	%goto done;
	%end;

%if %length(&at)=0 or %length(&size)=0 %then %do;
	%put ERROR:  The AT=&at and SIZE=&size values must both be specified.;
	%let abort=1;
	%goto done;
	%end;

%let llx = %scan(&at,1, %str( ));
%let lly = %scan(&at,2, %str( ));      %if %length(&lly)=0 %then %let lly=&llx;

%let sx = %scan(&size,1, %str( )); 
%let sy = %scan(&size,2, %str( ));     %if %length(&sy)=0 %then %let sy=&sx; 

proc greplay igout=&gin
             gout=&gout  nofs
             template=inset
			 tc = tc
             ;
	tdef inset
        1/
          ULX=0  ULY=100     URX=100 URY=100
          LLX=0  LLY=0       LRX=100 LRY=0
	    2/ clip
          ULX=&llx  ULY=%eval(&lly+&sy)  URX=%eval(&llx+&sx) URY=%eval(&lly+&sy)
          LLX=&llx  LLY=&lly             LRX=%eval(&llx+&sx) LRY=&lly
		;
	treplay 1:&main 2:&inset name="&name" des="Inset of &inset in &main";
	run; quit;

%done:
%if &abort %then %put ERROR: The INSET macro ended abnormally.;

%mend;




	
