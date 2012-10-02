 /*--------------------------------------------------------------*
  *    Name: webhelp.sas                                         *
  *   Title: Provide web browser help to a SAS session           *
        Doc: http://www.datavis.ca/sasmac/webhelp.html     
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 22 Oct 1997 09:13:38                                *
  * Revised: 20 May 2010 09:53:21                                *
  * Version: 1.4                                                 *
  * - Added SAS OnlineDoc; added VCD                             *
  * - Updated sasmac list                                        *
  *--------------------------------------------------------------*/
 /*=
=Description:
   webhelp issues a Display Manager command to display a Web document
   in the user's browser.

=Usage:
   %webhelp();
   %webhelp(shortcut);
   %webhelp(documentURL);

* doc    Is the web document to be displayed in the Web browser.
      If no argument is supplied, a default web document is displayed.
      Otherwise, if doc is a single word, help for a shortcut or SAS
      macro by that name is sought.
      Otherwise doc is taken to be a complete web URL.

=Requirements:
   webhelp requires SAS 6.12 or later

=Dependencies:
   webhelp relies on a global macro variable, webhelp, to be assigned
   a default web url.   This is usually done in the autoexec.sas file.
   e.g.,
     %let WEBHELP=http://www.psych.yorku.ca/lab/sas/;

 =*/


%*-- Default webhelp location can be set in autoexec.sas;
%global webhelp;

%macro webhelp(doc);
 %if %sysevalf(&sysver  < 6.12) %then %do;
    %put NOTE:  Webhelp is not available in SAS Version &sysver.;
    %goto done;
 %end;

 %if &sysenv = BACK %then %do;
    %put NOTE:  Webhelp is only available in the SAS Display Manager;
    %goto done;
 %end;

%*-- The following (keyword, URL) list provides shortcuts;
%let keylist=%str(
sas      http://www.sas.com/,
lab      http://www.yorku.ca/dept/psych/lab/,
sasinfo  http://www.yorku.ca/dept/psych/lab/sas/,
psy3030  http://www.yorku.ca/dept/psych/lab/psy3030/,
psy6140  http://www.yorku.ca/dept/psych/lab/psy6140/,
online   http://support.sas.com/onlinedoc/913/docMainpage.jsp
);

%*-- As do the following lists of macros;
/* %let smac=
biplot    boxanno   boxplot   contour   corresp  density
dotplot   lowess    nqplot    outlier   partial  scatmat
stars     symplot   twoway
;
 */

%* ls -1 *.html | tcgrep -vh 'webh|calis1|index|nodoc|robcovt' | perl -pe 's/\.html//' | nup -n 6 -d -w 9 -o sasmac;

%let sasmac=
addvar     contour    gask       label      points     sieveplot
agree      coplot     gbank      labels     poisplot   slice    
agreeplot  corresp    gdispla    lags       polygons   sort     
alleff     corrgram   genpat     lastword   poly       sparkline
axis       cpplot     genscat    lines      power2x2   splot    
bars       cqplot     gensym     logodds    power      sprdplot 
biplot     csmpower   gkill      lowess     powerlog   stars    
boxanno    datachk    goodfit    map2gen    powerrxc   stat2dat 
boxcox     defined    gskip      meanplot   pscale     str2ram  
boxglm     density    gtree      miplot     ram2dot    sunplot  
boxplot    distplot   halfnorm   missrc     regline    surface  
boxtid     dotplot    hecan      mosaic     resline    symbox   
bpycolors  dummy      hemat      mosaics    robcov     symplot  
brewerpal  effplot    hemreg     mosmat     robust     table    
caliscmp   ellipses   heplot     mpower     rootgram        
calisgfi   eqs2ram    heplots    multisummary  rpower     triplot  
canplot    equate     hovplot    mvnormal   rsqdelta   twoway   
catplot    expglm     inflglim   nqplot     rug        vexpand  
ccmap      expgrid    inflogis   ordplot    sas2vsta    
colorchrt  faces      inflplot   orpoly     scale               
colorramp  ffold      inset      outlier    scatmat             
combine    fourfold   interact   panels     scatter             
;
/* %let vcd=
addvar   dummy     halfnorm  mosaic    power2x2  table
agree    equate    inflglim  mosaics   powerlog  triplot
bars     fourfold  inflogis  mosmat    pscale
biplot   gdispla   interact  ordplot   robust
catplot  gensym    label     panels    rootgram
corresp  goodfit   lags      points    sieve
distplot gskip     logodds   poisplot  sort
;
 */
 %*-- No argument supplied -- use global default;
 %if %length(&doc)=0 %then %do;
    dm "wbrowse '&webhelp' continue";
    %end;

 %*-- ? or help ==> internal help;
 %else %if &doc=? or &doc=help %then %do;
    %put Web Help Usage:;
    %put %nrstr(    %webhelp;           -- Psych Lab SAS Help Guides page);
    %put %nrstr(    %webhelp(shortcut); -- see shortcut keyword list below);
    %put %nrstr(    %webhelp(URL);      -- a full web URL);
    %put %str( );
    %put Web Help is available for the following shortcuts:;
    %let count=1;
    %let item = %scan(&keylist,&count,%str(,));
    %do %while(%length(&item)>0 );
        %put %str(   )&item;
        %let count = %eval(&count+1);
        %let item = %scan(&keylist,&count,%str(,));
    %end;
    %put %str( );
    %put Shortcuts are also available for the following macro programs:;
    %put &sasmac;
    %*put &smac;
    %*put &vcd;
    %end;

 %*-- A single word? Look for a shortcut;
 %else %if %length(%scan(&doc,2,%str(:/.)))=0 %then %do;

    %*-- Search in keylist;
    %let docloc=%find(&doc, &keylist, %str(,));
    %if %length(&docloc) %then %do;
        dm "wbrowse '&docloc' continue";
        %goto done;
        %end;

    %*-- Assume user is asking for help on a macro;
    %let server=http://www.datavis.ca;
    %let source=http://www.datavis.ca/sas/macros;
	%*-- Find in sasmac first;
    %if %index(&sasmac,&doc)
        %then %let docloc=&server/sasmac/&doc..html;
/*
    %else %if %index(&smac,&doc)
        %then %let docloc=&server/sssg/&doc..html;
    %else %if %index(&vcd,&doc)
        %then %let docloc=&server/vcd/&doc..html;
*/
    %*-- Didnt find anything relevant -- go for source;
    %else %let docloc=&source/&doc..sas;

    dm "wbrowse '&docloc' continue";
 %end;

 %*-- Otherwise, anything on the Web;
 %else %do;
    %*-- Assume it is a complete web URL;
    dm "wbrowse '&doc' ";
 %end;

%done:
%mend webhelp;

%macro find(target, keylist, dlm);
   %local count item key val value;
   %let value=;
   %let count=1;
   %let item = %scan(&keylist,&count,%str(&dlm));
   %do %while(%length(&item)>0 );
       %let key = %trim(%scan(&item, 1,%str( )));
       %let val = %trim(%scan(&item, 2,%str( )));
       %if "%upcase(&target)"="%upcase(&key)" %then %do;
           %let value=&val;
           %goto done;
           %end;
       %let count = %eval(&count+1);
       %let item = %scan(&keylist,&count,%str(&dlm));
   %end;
%done:
   &value
%mend find;
