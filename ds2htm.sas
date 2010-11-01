%* modified RAD 9/15/99 to allow certain where clauses to process;

%macro ds2htm(htmlfile=,
              htmlfref=,
              openmode=replace,
              runmode=B,
              encode=Y,
              proploc=,
              bgtype=,
              bg=,
              septype=,
              seploc=,
              saspower=,
              brtitle=,
              ctext=,
              center=,
              twidth=,
              twunits=,
              talign=,
              border=,
              bwidth=,
              cpad=,
              cspace=,
              tbbgcolr=,
              obgcolr=,
              ibgcolr=,
              vbgcolr=,
              sbgcolr=,
              clbgcolr=,
              otag=,
              oface=,
              ocolor=,
              osize=,
              ohalign=,
              ovalign=,
              itag=,
              iface=,
              icolor=,
              isize=,
              ihalign=,
              ivalign=,
              iwrap=,
              vtag=,
              vface=,
              vcolor=,
              vsize=,
              vhalign=,
              vvalign=,
              vwrap=,
              stag=,
              sface=,
              scolor=,
              ssize=,
              shalign=,
              svalign=,
              cltag=,
              clface=,
              clcolor=,
              clsize=,
              clhalign=,
              clvalign=,
              clwrap=,
              ctag=,
              cface=,
              ccolor=,
              csize=,
              cvalign=,
              chalign=,
              ttag=,
              tface=,
              tcolor=,
              tsize=,
              ftag=,
              fface=,
              fcolor=,
              fsize=,
              data=,
              pw=,
              where=,
              obsnum=,
              labels=,
              formats=,
              round=,
              id=,
              var=,
              sum=,
              caption=,
              btag=,
              bface=,
              bcolor=,
              bsize=,
              bwrap=,
              by=,
              prttitle=FIRST,
              prtfoot=LAST,
              tranlist=,
              charset=,
              prtpower=LAST,
              pagepart=ALL,
              doctype=3.2,
              sstype1=,
              sstype2=,
              sstype3=,
              sstype4=,
              sstype5=,
              ssrel1=,
              ssrel2=,
              ssrel3=,
              ssrel4=,
              ssrel5=,
              ssrev1=,
              ssrev2=,
              ssrev3=,
              ssrev4=,
              ssrev5=,
              sshref1=,
              sshref2=,
              sshref3=,
              sshref4=,
              sshref5=,
              sstitle1=,
              sstitle2=,
              sstitle3=,
              sstitle4=,
              sstitle5=,
              ssmedia1=,
              ssmedia2=,
              ssmedia3=,
              ssmedia4=,
              ssmedia5=,
              ssfile1=,
              ssfile2=,
              ssfile3=,
              ssfile4=,
              ssfile5=,
              ssfref1=,
              ssfref2=,
              ssfref3=,
              ssfref4=,
              ssfref5=,
              tclass=,
              tid=,
              fclass=,
              fid=,
              tbclass=,
              tbid=,
              bclass=,
              bid=,
              cclass=,
              cid=,
              oclass=,
              oid=,
              clclass=,
              clid=,
              bdclass=,
              bdid=,
              spclass=,
              spid=,
              sepclass=,
              sepid=,
              rowssvar=,
              rowssfmt=,
              sqlview=N);

%*********************************************************************;
%*
%*  MACRO: DS2HTM
%*
%*  USAGE: %ds2htm(arg1=value, arg2=value, ... argN=valueN);
%*
%*  DESCRIPTION:
%*    This macro converts a SAS data set to an HTML table.
%*
%*  NOTES:
%*    None.
%*
%*  SUPPORT: sasvcd
%*
%*  VERSION: 1.2
%*
%*********************************************************************;

%global _htmcap _htmtitl _htmwher;

%local htmlfile htmlfref openmode proploc bgtype bg septype seploc
       ctext center twidth twunits talign border bwidth cpad cspace
       tbbgcolr obgcolr ibgcolr vbgcolr sbgcolr clbgcolr oface
       ocolor osize ohalign ovalign iface icolor isize ihalign
       ivalign iwrap vface vcolor vsize vhalign vvalign vwrap sface
       scolor ssize shalign svalign clface clcolor clsize clhalign
       clvalign clwrap cface ccolor csize cvalign chalign data
       where obsnum labels formats round id var sum caption
       cbgcolr cwrap owrap swrap eflag notes runmode
       tface tcolor tsize fface fcolor fsize
       otag itag vtag stag cltag ctag ttag ftag
       pw encode saspower brtitle
       by i comma fobs olddata last numgrps
       bface bcolor bsize bhalign bvalign bwrap btag prttitle prtfoot
       tranlist charset whecount firstby lastby prtpower pagepart
       doctype
       sstype1 sstype2 sstype3 sstype4 sstype5
       ssrel1 ssrel2 ssrel3 ssrel4 ssrel5
       ssrev1 ssrev2 ssrev3 ssrev4 ssrev5
       sshref1 sshref2 sshref3 sshref4 sshref5
       sstitle1 sstitle2 sstitle3 sstitle4 sstitle5
       ssmedia1 ssmedia2 ssmedia3 ssmedia4 ssmedia5
       ssfile1 ssfile2 ssfile3 ssfile4 ssfile5
       ssfref1 ssfref2 ssfref3 ssfref4 ssfref5
       tclass tid fclass fid tbclass tbid cclass cid oclass oid
       clclass clid bdclass bdid bclass bid
       origpp spclass spid sepclass sepid
       rowssvar rowssfmt sqlsel sqlgrp sqlord vcount sqlview;


%local var by id sum numvar numby numid numsum tempstr tempstr2
       index1 index2
       keyword numkey dsid sysrc sysmsg varnum;

%let notes=%sysfunc(getoption(notes));

%let last = &syslast;

%let cbgcolr=; %* unsupported by HTML;
%let cwrap=;   %* unsupported;
%let owrap=;   %* unsupported;
%let swrap=;   %* unsupported;
%let bhalign=; %* unsupported;
%let bvalign=; %* unsupported;

%let var=%cmpres(&var);
%let by=%cmpres(&by);
%let id=%cmpres(&id);
%let sum=%cmpres(&sum);

%let numvar=0;
%let numby=0;
%let numid=0;
%let numsum=0;

options nonotes;

%if "&runmode" eq "" %then %do;
  %put;
  %put ERROR: You must specify either I or B for execution mode.;
  %put;
  %goto exit;
%end;

%if (%upcase(&runmode) ne B) and
    (%upcase(&runmode) ne S) and
    (%upcase(&runmode) ne I)
%then %do;
  %put;
  %put ERROR: &runmode is an unknown or unsupported execution mode.;
  %put %str(       Specify either I or B.);
  %put;
  %goto exit;
%end;

%*;
%*  Check the syntax of the 4-level name. Existence will be handled
%*  in the SCL code.
%*;

%if "&proploc" ne "" %then %do;

  %if %length(%scan(&proploc, 1, '.')) gt 8 %then %do;
    %put;
    %put ERROR: &proploc is an invalid 4-level name for the SLIST entry;
    %put %str(       that contains the properties.);
    %put;
    %goto exit;
  %end;

  %if (%length(%scan(&proploc, 2, '.')) gt 8) or
      (%length(%scan(&proploc, 2, '.')) eq 0)
  %then %do;
    %put;
    %put ERROR: &proploc is an invalid 4-level name for the SLIST entry;
    %put %str(       that contains the properties.);
    %put;
    %goto exit;
  %end;

  %if (%length(%scan(&proploc, 3, '.')) gt 8) or
      (%length(%scan(&proploc, 3, '.')) eq 0)
  %then %do;
    %put;
    %put ERROR: &proploc is an invalid 4-level name for the SLIST entry;
    %put %str(       that contains the properties.);
    %put;
    %goto exit;
  %end;

  %if (%upcase(%scan(&proploc, 4, '.')) ne SLIST)
  %then %do;
    %put;
    %put ERROR: &proploc is an invalid 4-level name for the SLIST entry;
    %put %str(       that contains the properties.);
    %put;
    %goto exit;
  %end;

%end;

%if "&tranlist" ne "" %then %do;

  %if %length(%scan(&tranlist, 1, '.')) gt 8 %then %do;
    %put;
    %put ERROR: &tranlist is an invalid 4-level name for the;
    %put %str(       transcoding list.);
    %put;
    %goto exit;
  %end;

  %if (%length(%scan(&tranlist, 2, '.')) gt 8) or
      (%length(%scan(&tranlist, 2, '.')) eq 0)
  %then %do;
    %put;
    %put ERROR: &tranlist is an invalid 4-level name for the;
    %put %str(       transcoding list.);
    %put;
    %goto exit;
  %end;

  %if (%length(%scan(&tranlist, 3, '.')) gt 8) or
      (%length(%scan(&tranlist, 3, '.')) eq 0)
  %then %do;
    %put;
    %put ERROR: &tranlist is an invalid 4-level name for the;
    %put %str(       transcoding list.);
    %put;
    %goto exit;
  %end;

  %if (%upcase(%scan(&tranlist, 4, '.')) ne SLIST)
  %then %do;
    %put;
    %put ERROR: &tranlist is an invalid 4-level name for the;
    %put %str(       transcoding list.);
    %put;
    %goto exit;
  %end;

%end;

%if ("&htmlfile" eq "") and
    ("&htmlfref" eq "") and
    (%upcase(&runmode) eq B or %upcase(&runmode) eq S)
%then %do; %* no output HTML file specified;
  %put;
  %put ERROR: You must specify a file for the HTML output.;
  %put;
  %goto exit;
%end;

%if ("&htmlfile" ne "") and
    ("&htmlfref" ne "")
%then %do;
  %put;
  %put ERROR: You must specify a file OR fileref for the HTML output.;
  %put;
  %goto exit;
%end;

%if ("&openmode" eq "") and
    (%upcase(&runmode) eq B or %upcase(&runmode) eq S)
%then %do; %* no open mode for HTML file specified;
  %put;
  %put ERROR: You must specify a mode for opening the HTML output file.;
  %put;
  %goto exit;
%end;

%if (%upcase(&openmode) ne REPLACE) and
    (%upcase(&openmode) ne APPEND)
%then %do; %* invalid open mode for HTML file;
  %put;
  %put ERROR: &openmode is an unknown or unsupported open mode.;
  %put %str(       Specify either REPLACE or APPEND.);
  %put;
  %goto exit;
%end;

%*;
%*  Check the syntax of the 1 or 2-level name. Existence will be handled
%*  in the SCL code.
%*;

%if %length(%scan(&data, 1, '.')) gt 8
%then %do; %* check for libref > 8 chars;
  %put;
  %put ERROR: &data is an invalid name for the SAS data set;
  %put %str(       to be formatted.);
  %put;
  %goto exit;
%end;

%if %length(%scan(&data, 2, '.')) gt 8
%then %do; %* check for entry name > 8 chars;
  %put;
  %put ERROR: &data is an invalid name for the SAS data set;
  %put %str(       to be formatted.);
  %put;
  %goto exit;
%end;

%if %length(%scan(&data, 3, '.')) ne 0
%then %do; %* check for 3-level or higher table name;
  %put;
  %put ERROR: &data is an invalid name for the SAS data set;
  %put %str(       to be formatted.);
  %put;
  %goto exit;
%end;

%if (%upcase(&obsnum) ne N) and
    (%upcase(&obsnum) ne Y) and
    (%upcase(&obsnum) ne YES) and
    (%upcase(&obsnum) ne NO) and
    (%upcase(&obsnum) ne  )
%then %do; %* Y or N not specified for obs number flag;
  %put;
  %put ERROR: &obsnum is an unknown or unsupported value for the OBS;
  %put %str(       number flag. Specify either Y or N.);
  %put;
  %goto exit;
%end;

%if &obsnum ne
%then %let obsnum=%upcase(%substr(&obsnum, 1, 1));

%if (%upcase(&labels) ne N) and
    (%upcase(&labels) ne Y) and
    (%upcase(&labels) ne YES) and
    (%upcase(&labels) ne NO) and
    (%upcase(&labels) ne  )
%then %do; %* Y or N not specified for labels flag;
  %put;
  %put ERROR: &labels is an unknown or unsupported value for the LABELS;
  %put %str(       flag. Specify either Y or N.);
  %put;
  %goto exit;
%end;

%if &labels ne
%then %let labels=%upcase(%substr(&labels, 1, 1));

%if (%upcase(&formats) ne N) and
    (%upcase(&formats) ne Y) and
    (%upcase(&formats) ne YES) and
    (%upcase(&formats) ne NO) and
    (%upcase(&formats) ne  )
%then %do; %* Y or N not specified for formats flag;
  %put;
%put ERROR: &formats is an unknown or unsupported value for the FORMATS;
  %put %str(       flag. Specify either Y or N.);
  %put;
  %goto exit;
%end;

%if &formats ne
%then %let formats=%upcase(%substr(&formats, 1, 1));

%if &round lt 0 and "&round" ne "" and "&round" ne "."
%then %do; %* negative value entered;
  %put;
%put ERROR: &round is an invalid specification for the ROUND argument.;
  %put %str(       Specify a value of 0 or greater.);
  %put;
  %goto exit;
%end;

%if &round gt 10
%then %do; %* non-numeric value entered;
  %put;
%put ERROR: &round is an invalid specification for the ROUND argument.;
  %put %str(       Specify a value of 10 or smaller.);
  %put;
  %goto exit;
%end;

%* modified RAD 9/15/99 to allow certain where clauses to process;
%*
%if "%upcase(%scan(&where, 1, ' '))" eq "WHERE"
;
%if %upcase(%scan(%quote(&where), 1, ' ')) eq WHERE
%then %do; %* where clause starts with the word WHERE;
  %put;
  %put ERROR: Do not start WHERE clause with the word WHERE.;
  %put;
  %goto exit;
%end;

%if ("&bgtype" ne "")           and
    (%upcase(&bgtype) ne NONE)  and
    (%upcase(&bgtype) ne COLOR) and
    (%upcase(&bgtype) ne IMAGE)
%then %do;
  %put;
  %put ERROR: &bgtype is an invalid value for background type.;
  %put %str(       Specify NONE, COLOR or IMAGE.);
  %put;
  %goto exit;
%end;

%if ("&bgtype" eq "") and
    (%quote(&bg) ne )
%then %do;
  %put;
  %put ERROR: You must specify a background type.;
  %put;
  %goto exit;
%end;

%if (%upcase(&bgtype) eq IMAGE) and
    (%quote(&bg) eq )
%then %do;
  %put;
  %put ERROR: You must specify an image location for the background.;
  %put;
  %goto exit;
%end;

%if (%upcase(&bgtype) eq COLOR) and
    (%quote(&bg) eq )
%then %do;
  %put;
  %put ERROR: You must specify a color for the background.;
  %put;
  %goto exit;
%end;

%if ("&septype" ne "")           and
    (%upcase(&septype) ne NONE)  and
    (%upcase(&septype) ne RULE)  and
    (%upcase(&septype) ne IMAGE)
%then %do;
  %put;
  %put ERROR: &septype is an invalid value for page break separator.;
  %put %str(       Specify NONE, RULE or IMAGE.);
  %put;
  %goto exit;
%end;

%if (%upcase(&septype) eq IMAGE) and
    ("&seploc" eq "")
%then %do;
  %put;
  %put ERROR: You must specify an image location for the page;
  %put %str(       break separator.);
  %put;
  %goto exit;
%end;

%if ("&septype" eq "") and
    ("&seploc"  ne "")
%then %let septype =IMAGE;

%if "&encode" eq "" %then %do;
  %put;
  %put ERROR: You must specify either Y, or N for the ENCODE argument.;
  %put;
  %goto exit;
%end;

%if (%upcase(&encode) ne Y) and
    (%upcase(&encode) ne N) and
    (%upcase(&encode) ne YES) and
    (%upcase(&encode) ne NO)
%then %do;
  %put;
  %put ERROR: &encode is an unknown or unsupported ENCODE value.;
  %put %str(       Specify either Y or N.);
  %put;
  %goto exit;
%end;

%if &encode ne
%then %let encode=%upcase(%substr(&encode, 1, 1));

%if (%upcase(&center) ne Y) and
    (%upcase(&center) ne N) and
    (%upcase(&center) ne YES) and
    (%upcase(&center) ne NO) and
    (%upcase(&center) ne  )
%then %do;
  %put;
  %put ERROR: &center is an unknown or unsupported CENTER value.;
  %put %str(       Specify either Y or N.);
  %put;
  %goto exit;
%end;

%if &center ne
%then %let center=%upcase(%substr(&center, 1, 1));

%if (%upcase(&twunits) ne PERCENT) and
    (%upcase(&twunits) ne PIXELS)  and
    (%upcase(&twunits) ne  )
%then %do;
  %put;
  %put ERROR: &twunits is an unknown or unsupported table width unit.;
  %put %str(       Specify either PERCENT or PIXELS.);
  %put;
  %goto exit;
%end;

%if ("&twidth"  eq "") and
    ("&twunits" ne "")
%then %do;
  %put;
  %put ERROR: No value specified for table width.;
  %put;
  %goto exit;
%end;

%if ("&twidth"  ne "") and
    ("&twunits" eq "")
%then %let twunits=Percent;

%if (%upcase(&talign) ne LEFT)   and
    (%upcase(&talign) ne CENTER) and
    (%upcase(&talign) ne RIGHT)  and
    (%upcase(&talign) ne DEFAULT)  and
    (%upcase(&talign) ne  )
%then %do;
  %put;
  %put ERROR: &talign is an unknown or unsupported table alignment.;
  %put %str(       Specify either LEFT, CENTER or RIGHT.);
  %put;
  %goto exit;
%end;

%if (%upcase(&border) ne Y) and
    (%upcase(&border) ne N) and
    (%upcase(&border) ne YES) and
    (%upcase(&border) ne NO) and
    (%upcase(&border) ne  )
%then %do;
  %put;
  %put ERROR: &border is an unknown or unsupported BORDER value.;
  %put %str(       Specify either Y or N.);
  %put;
  %goto exit;
%end;

%if &border ne
%then %let border=%upcase(%substr(&border, 1, 1));

%if &iwrap ne
%then %let iwrap=%upcase(%substr(&iwrap, 1, 1));
%if &vwrap ne
%then %let vwrap=%upcase(%substr(&vwrap, 1, 1));
%if &clwrap ne
%then %let clwrap=%upcase(%substr(&clwrap, 1, 1));
%if &cwrap ne
%then %let cwrap=%upcase(%substr(&cwrap, 1, 1));
%if &owrap ne
%then %let owrap=%upcase(%substr(&owrap, 1, 1));
%if &swrap ne
%then %let swrap=%upcase(%substr(&swrap, 1, 1));
%if &bwrap ne
%then %let bwrap=%upcase(%substr(&bwrap, 1, 1));

%*;
%*  Perform rudimentary check for the sizes and other values entered.
%*;

%let eflag=0;

data _null_;

array names(7) $20 ('observation number'  'ID variable'  'variable'
                    'SUM variable'        'column label' 'caption'
                    'BY group');

array mvar1(7) $8  ('osize' 'isize' 'vsize' 'ssize' 'clsize' 'csize'
                    'bsize');

array mvar2(7) $8  ('ohalign' 'ihalign' 'vhalign' 'shalign'
                    'clhalign' 'chalign' 'bhalign');
array mvar3(7) $8  ('ovalign' 'ivalign' 'vvalign' 'svalign'
                    'clvalign' 'cvalign' 'bvalign');
array mvar4(7) $8  ('owrap'   'iwrap'   'vwrap'   'swrap'
                    'clwrap'   'cwrap'  'bwrap');

length tempnum 8 tempchar $200;

tempnum=0;

if symget('twidth') ne '' and
   upcase(symget('twidth')) ne 'DEFAULT'
then do;
  tempnum = symget('twidth');
  _error_ = 0;
  if tempnum lt 0 then do;
    put;
    put 'ERROR: Invalid specification for table width.';
    put;
    call symput("eflag", '1');
    STOP;
   end;
end;

if symget('bwidth') ne '' and symget('border') eq 'Y' then do;
  tempnum = symget('bwidth');
  _error_ = 0;
  if tempnum lt 0 then do;
    put;
    put 'ERROR: Invalid specification for table border width.';
    put;
    call symput("eflag", '1');
    STOP;
   end;
end;

if symget('cpad') ne '' and
   upcase(symget('cpad')) ne 'DEFAULT'
then do;
  tempnum = symget('cpad');
  _error_ = 0;
  if tempnum lt 0 then do;
    put;
    put 'ERROR: Invalid specification for cell padding.';
    put;
    call symput("eflag", '1');
    STOP;
   end;
end;

if symget('cspace') ne '' and
   upcase(symget('cspace')) ne 'DEFAULT'
then do;
  tempnum = symget('cspace');
  _error_ = 0;
  if tempnum lt 0 then do;
    put;
    put 'ERROR: Invalid specification for cell spacing.';
    put;
    call symput("eflag", '1');
    STOP;
   end;
end;

do i = 1 to dim(names);
  tempchar  = upcase(symget(mvar2(i))); * horizontal alignment;
  if tempchar not in ('CENTER' 'LEFT' 'RIGHT' 'DEFAULT' '') then do;
    put;
    put ' ERROR: Invalid specification for ' names(i)
        'horizontal alignment.';
    put;
    call symput("eflag", '1');
    STOP;
  end;
  tempchar  = upcase(symget(mvar3(i))); * vertical alignment;
  if tempchar ne '' then do;
    if names(i) eq 'caption' and
      tempchar not in ('BOTTOM' 'TOP' 'DEFAULT')
    then do;
      put;
put ' ERROR: Invalid specification for caption vertical alignment.';
      put;
      call symput("eflag", '1');
      STOP;
    end;
  end;
  if tempchar not in ('BASELINE' 'BOTTOM' 'MIDDLE' 'TOP' 'DEFAULT' '')
  then do;
    put;
put ' ERROR: Invalid specification for ' names(i) 'vertical alignment.';
    put;
    call symput("eflag", '1');
    STOP;
  end;
  tempchar  = upcase(symget(mvar4(i))); * wrap;
  if tempchar ne '' then do;
    if names(i) eq 'caption' then do;
      put;
      put ' ERROR: NOWRAP is not supported for captions.';
      put;
      call symput("eflag", '1');
      STOP;
    end;
  end;
  if tempchar not in ('Y' 'N' '') then do;
    put;
    put ' ERROR: Invalid specification for ' names(i) 'wrap argument.';
    put;
    call symput("eflag", '1');
    STOP;
  end;
end;

run;

%if &eflag eq 1 %then %goto exit;

%if (%upcase(&pagepart) ne ALL)  and
    (%upcase(&pagepart) ne HEAD) and
    (%upcase(&pagepart) ne BODY) and
    (%upcase(&pagepart) ne FOOT)
%then %do;
  %put;
  %put ERROR: &pagepart is an invalid value for the part of;
  %put %str(       page to create.);
  %put;
  %goto exit;
%end;

%if (%bquote(&var) ne ) %then %do;

  %let numvar=1;

  %local var&numvar vcls&numvar vid&numvar vcfm&numvar vifm&numvar;

  %*  Compress blanks before ( ;

  %do %while(%index(&var, %str( %()) gt 0);
     %let var=%sysfunc(tranwrd(&var, %str( %(), %str(%()));
  %end;

  %*  Compress blanks after ( ;

  %do %while(%index(&var, %str(%( )) gt 0);
     %let var=%sysfunc(tranwrd(&var, %str(%( ), %str(%()));
  %end;

  %*  Compress blanks before ) ;

  %do %while(%index(&var, %str( %))) gt 0);
     %let var=%sysfunc(tranwrd(&var, %str( %)), %str(%))));
  %end;

  %*  Compress blanks before , ;

  %do %while(%index(&var, %quote( ,)) gt 0);
     %let var=%sysfunc(tranwrd(&var, %quote( ,), %quote(,)));
  %end;

  %*  Compress blanks after , ;

  %do %while(%index(&var, %quote(, )) gt 0);
     %let var=%sysfunc(tranwrd(&var, %quote(, ), %quote(,)));
  %end;

  %*  Compress blanks before = ;

  %do %while(%index(&var, %quote( =)) gt 0);
     %let var=%sysfunc(tranwrd(&var, %quote( =), %quote(=)));
  %end;

  %*  Compress blanks after = ;

  %do %while(%index(&var, %quote(= )) gt 0);
     %let var=%sysfunc(tranwrd(&var, %quote(= ), %quote(=)));
  %end;

  %let tempstr=%scan(&var, &numvar, %str( ));

  %do %while("&tempstr" ne "");

    %let var&numvar=%scan(&tempstr, 1, %str(%());

    %* Get style sheet CLASS/CLASSFMT/ID/IDFMT values, if specified;

    %if %index(%upcase(&tempstr), CLASS=)    gt 0 or
        %index(%upcase(&tempstr), CLASSFMT=) gt 0 or
        %index(%upcase(&tempstr), IDFMT=)    gt 0 or
        %index(%upcase(&tempstr), ID=)       gt 0
    %then %do;

      %* Strip ( and );

      %let index1=%eval(%index(&tempstr, %str(%()) + 1);
      %let tempstr2=%substr(&tempstr,
                            &index1,
                            %eval(%length(&tempstr) - &index1));

      %let numkey=1;

      %let keyword=%scan(%bquote(&tempstr2), &numkey, %str(,));

      %do %while("&keyword" ne "");

      %* Get the style sheet values, if any;

      %if %index(%upcase(&keyword), CLASS=) gt 0
        %then %let vcls&numvar=%scan(&keyword, 2, %str(=));
      %else %if %index(%upcase(&keyword), CLASSFMT=) gt 0
        %then %let vcfm&numvar=%scan(&keyword, 2, %str(=));
      %else %if %index(%upcase(&keyword), ID=) gt 0
        %then %let vid&numvar=%scan(&keyword, 2, %str(=));
      %else %if %index(%upcase(&keyword), IDFMT=) gt 0
        %then %let vifm&numvar=%scan(&keyword, 2, %str(=));

        %let numkey=%eval(&numkey + 1);
        %let keyword=%scan(%bquote(&tempstr2), &numkey, %str(,));

      %end; %* KEYWORD loop;

    %end; %* CLASS/CLASSFMT/ID/IDFMT loop;

    %if (&&vcls&numvar ne) and
        (&&vcfm&numvar ne)
    %then %do;
      %put;
%put ERROR: You must specify either CLASS or CLASSFMT as a keyword;
      %put %str(       for the variable %upcase(&&var&numvar).);
      %put;
      %goto exit;
    %end;

    %if (&&vid&numvar ne) and
        (&&vifm&numvar ne)
    %then %do;
      %put;
      %put ERROR: You must specify either ID or IDFMT as a keyword;
      %put %str(       for the variable %upcase(&&var&numvar).);
      %put;
      %goto exit;
    %end;

    %let numvar=%eval(&numvar + 1);
    %local var&numvar vcls&numvar vid&numvar vcfm&numvar vifm&numvar;
    %let tempstr=%scan(&var, &numvar, %str( ));

  %end;

  %let numvar=%eval(&numvar - 1);

%end; %* Main VAR loop;

%if (%bquote(&by) ne ) %then %do;

  %let numby=1;

  %local by&numby bcls&numby bid&numby bcfm&numby bifm&numby;
  %local bso&numby bfmt&numby;

  %let bso&numby=ASCENDING;

  %*  Compress blanks before ( ;

  %do %while(%index(&by, %str( %()) gt 0);
     %let by=%sysfunc(tranwrd(&by, %str( %(), %str(%()));
  %end;

  %*  Compress blanks after ( ;

  %do %while(%index(&by, %str(%( )) gt 0);
     %let by=%sysfunc(tranwrd(&by, %str(%( ), %str(%()));
  %end;

  %*  Compress blanks before ) ;

  %do %while(%index(&by, %str( %))) gt 0);
     %let by=%sysfunc(tranwrd(&by, %str( %)), %str(%))));
  %end;

  %*  Compress blanks before , ;

  %do %while(%index(&by, %quote( ,)) gt 0);
     %let by=%sysfunc(tranwrd(&by, %quote( ,), %quote(,)));
  %end;

  %*  Compress blanks after , ;

  %do %while(%index(&by, %quote(, )) gt 0);
     %let by=%sysfunc(tranwrd(&by, %quote(, ), %quote(,)));
  %end;

  %*  Compress blanks before = ;

  %do %while(%index(&by, %quote( =)) gt 0);
     %let by=%sysfunc(tranwrd(&by, %quote( =), %quote(=)));
  %end;

  %*  Compress blanks after = ;

  %do %while(%index(&by, %quote(= )) gt 0);
     %let by=%sysfunc(tranwrd(&by, %quote(= ), %quote(=)));
  %end;

  %let tempstr=%scan(&by, &numby, %str( ));

  %do %while("&tempstr" ne "");

    %let by&numby=%scan(&tempstr, 1, %str(%());

    %if (%quote(%upcase(&&by&numby)) eq %quote(NOT)) or
        (%quote(%upcase(&&by&numby)) eq %quote(SAME))
    %then %do;
      %put;
      %put ERROR: &&by&numby is an invalid variable name for a;
      %put %str(       BY variable.);
      %put;
      %goto exit;
    %end;

    %* Get style sheet CLASS/CLASSFMT/ID/IDFMT values, if specified;
    %* Get BY processing sort order and/or format if specified;

    %if %index(%upcase(&tempstr), CLASS=)     gt 0 or
        %index(%upcase(&tempstr), CLASSFMT=)  gt 0 or
        %index(%upcase(&tempstr), IDFMT=)     gt 0 or
        %index(%upcase(&tempstr), ID=)        gt 0 or
        %index(%upcase(&tempstr), ORDER=)     gt 0 or
        %index(%upcase(&tempstr), FORMAT=)    gt 0
    %then %do;

      %* Strip ( and );

      %let index1=%eval(%index(&tempstr, %str(%()) + 1);
      %let tempstr2=%substr(&tempstr,
                            &index1,
                            %eval(%length(&tempstr) - &index1));

      %let numkey=1;

      %let keyword=%scan(%bquote(&tempstr2), &numkey, %str(,));

      %do %while("&keyword" ne "");

      %* Get the style sheet values, if any;

      %if %index(%upcase(&keyword), CLASS=) gt 0
        %then %let bcls&numby=%scan(&keyword, 2, %str(=));
      %else %if %index(%upcase(&keyword), CLASSFMT=) gt 0
        %then %let bcfm&numby=%scan(&keyword, 2, %str(=));
      %else %if %index(%upcase(&keyword), ID=) gt 0
        %then %let bid&numby=%scan(&keyword, 2, %str(=));
      %else %if %index(%upcase(&keyword), IDFMT=) gt 0
        %then %let bifm&numby=%scan(&keyword, 2, %str(=));
      %else %if %index(%upcase(&keyword), ORDER=) gt 0
        %then %let bso&numby=%scan(&keyword, 2, %str(=));
      %else %if %index(%upcase(&keyword), FORMAT=) gt 0
        %then %let bfmt&numby=%scan(&keyword, 2, %str(=));

        %let numkey=%eval(&numkey + 1);
        %let keyword=%scan(%bquote(&tempstr2), &numkey, %str(,));

      %end; %* KEYWORD loop;

    %end; %* CLASS/CLASSFMT/ID/IDFMT loop;

    %if (&&bcls&numby ne) and
        (&&bcfm&numby ne)
    %then %do;
      %put;
%put ERROR: You must specify either CLASS or CLASSFMT as a keyword;
      %put %str(       for the BY variable %upcase(&&by&numby).);
      %put;
      %goto exit;
    %end;

    %if (&&bid&numby ne) and
        (&&bifm&numby ne)
    %then %do;
      %put;
      %put ERROR: You must specify either ID or IDFMT as a keyword;
      %put %str(       for the BY variable %upcase(&&by&numby).);
      %put;
      %goto exit;
    %end;

    %if (%upcase(&&bso&numby)) ne ASCENDING  and
        (%upcase(&&bso&numby)) ne DESCENDING
    %then %do;
      %put;
%put ERROR: &&bso&numby is an invalid value for the ORDER keyword;
      %put %str(       for the BY variable %upcase(&&by&numby).);
      %put %str(       Specify either ASCENDING or DESCENDING.);
      %put;
      %goto exit;
    %end;

    %let numby=%eval(&numby + 1);
    %local by&numby bcls&numby bid&numby bcfm&numby bifm&numby;
    %local bso&numby bfmt&numby;
    %let bso&numby=ASCENDING;
    %let tempstr=%scan(&by, &numby, %str( ));

  %end;
  %let numby=%eval(&numby - 1);

%end; %* Main BY loop;


%if (%bquote(&id) ne ) %then %do;

  %let numid=1;

  %local id&numid icls&numid iid&numid icfm&numid iifm&numid;

  %*  Compress blanks before ( ;

  %do %while(%index(&id, %str( %()) gt 0);
     %let id=%sysfunc(tranwrd(&id, %str( %(), %str(%()));
  %end;

  %*  Compress blanks after ( ;

  %do %while(%index(&id, %str(%( )) gt 0);
     %let id=%sysfunc(tranwrd(&id, %str(%( ), %str(%()));
  %end;

  %*  Compress blanks before ) ;

  %do %while(%index(&id, %str( %))) gt 0);
     %let id=%sysfunc(tranwrd(&id, %str( %)), %str(%))));
  %end;

  %*  Compress blanks before , ;

  %do %while(%index(&id, %quote( ,)) gt 0);
     %let id=%sysfunc(tranwrd(&id, %quote( ,), %quote(,)));
  %end;

  %*  Compress blanks after , ;

  %do %while(%index(&id, %quote(, )) gt 0);
     %let id=%sysfunc(tranwrd(&id, %quote(, ), %quote(,)));
  %end;

  %*  Compress blanks before = ;

  %do %while(%index(&id, %quote( =)) gt 0);
     %let id=%sysfunc(tranwrd(&id, %quote( =), %quote(=)));
  %end;

  %*  Compress blanks after = ;

  %do %while(%index(&id, %quote(= )) gt 0);
     %let id=%sysfunc(tranwrd(&id, %quote(= ), %quote(=)));
  %end;

  %let tempstr=%scan(&id, &numid, %str( ));

  %do %while("&tempstr" ne "");

    %let id&numid=%scan(&tempstr, 1, %str(%());

    %* Get style sheet CLASS/CLASSFMT/ID/IDFMT values, if specified;

    %if %index(%upcase(&tempstr), CLASS=)    gt 0 or
        %index(%upcase(&tempstr), CLASSFMT=) gt 0 or
        %index(%upcase(&tempstr), IDFMT=)    gt 0 or
        %index(%upcase(&tempstr), ID=)       gt 0
    %then %do;

      %* Strip ( and );

      %let index1=%eval(%index(&tempstr, %str(%()) + 1);
      %let tempstr2=%substr(&tempstr,
                            &index1,
                            %eval(%length(&tempstr) - &index1));

      %let numkey=1;

      %let keyword=%scan(%bquote(&tempstr2), &numkey, %str(,));

      %do %while("&keyword" ne "");

      %* Get the style sheet values, if any;

      %if %index(%upcase(&keyword), CLASS=) gt 0
        %then %let icls&numid=%scan(&keyword, 2, %str(=));
      %else %if %index(%upcase(&keyword), CLASSFMT=) gt 0
        %then %let icfm&numid=%scan(&keyword, 2, %str(=));
      %else %if %index(%upcase(&keyword), ID=) gt 0
        %then %let iid&numid=%scan(&keyword, 2, %str(=));
      %else %if %index(%upcase(&keyword), IDFMT=) gt 0
        %then %let iifm&numid=%scan(&keyword, 2, %str(=));

        %let numkey=%eval(&numkey + 1);
        %let keyword=%scan(%bquote(&tempstr2), &numkey, %str(,));

      %end; %* KEYWORD loop;

    %end; %* CLASS/CLASSFMT/ID/IDFMT loop;

    %if (&&icls&numid ne) and
        (&&icfm&numid ne)
    %then %do;
      %put;
%put ERROR: You must specify either CLASS or CLASSFMT as a keyword;
      %put %str(       for the ID variable %upcase(&&id&numid).);
      %put;
      %goto exit;
    %end;

    %if (&&iid&numid ne) and
        (&&iifm&numid ne)
    %then %do;
      %put;
      %put ERROR: You must specify either ID or IDFMT as a keyword;
      %put %str(       for the ID variable %upcase(&&id&numid).);
      %put;
      %goto exit;
    %end;

    %let numid=%eval(&numid + 1);
    %local id&numid icls&numid iid&numid icfm&numid iifm&numid;
    %let tempstr=%scan(&id, &numid, %str( ));

  %end;

  %let numid=%eval(&numid - 1);

%end; %* Main ID loop;


%if (%bquote(&sum) ne ) %then %do;

  %let numsum=1;

  %local sum&numsum scls&numsum sid&numsum scfm&numsum sifm&numsum;

  %*  Compress blanks before ( ;

  %do %while(%index(&sum, %str( %()) gt 0);
     %let sum=%sysfunc(tranwrd(&sum, %str( %(), %str(%()));
  %end;

  %*  Compress blanks after ( ;

  %do %while(%index(&sum, %str(%( )) gt 0);
     %let sum=%sysfunc(tranwrd(&sum, %str(%( ), %str(%()));
  %end;

  %*  Compress blanks before ) ;

  %do %while(%index(&sum, %str( %))) gt 0);
     %let sum=%sysfunc(tranwrd(&sum, %str( %)), %str(%))));
  %end;

  %*  Compress blanks before , ;

  %do %while(%index(&sum, %quote( ,)) gt 0);
     %let sum=%sysfunc(tranwrd(&sum, %quote( ,), %quote(,)));
  %end;

  %*  Compress blanks after , ;

  %do %while(%index(&sum, %quote(, )) gt 0);
     %let sum=%sysfunc(tranwrd(&sum, %quote(, ), %quote(,)));
  %end;

  %*  Compress blanks before = ;

  %do %while(%index(&sum, %quote( =)) gt 0);
     %let sum=%sysfunc(tranwrd(&sum, %quote( =), %quote(=)));
  %end;

  %*  Compress blanks after = ;

  %do %while(%index(&sum, %quote(= )) gt 0);
     %let sum=%sysfunc(tranwrd(&sum, %quote(= ), %quote(=)));
  %end;

  %let tempstr=%scan(&sum, &numsum, %str( ));

  %do %while("&tempstr" ne "");

    %let sum&numsum=%scan(&tempstr, 1, %str(%());

    %* Get style sheet CLASS/CLASSFMT/ID/IDFMT values, if specified;

    %if %index(%upcase(&tempstr), CLASS=)    gt 0 or
        %index(%upcase(&tempstr), CLASSFMT=) gt 0 or
        %index(%upcase(&tempstr), IDFMT=)    gt 0 or
        %index(%upcase(&tempstr), ID=)       gt 0
    %then %do;

      %* Strip ( and );

      %let index1=%eval(%index(&tempstr, %str(%()) + 1);
      %let tempstr2=%substr(&tempstr,
                            &index1,
                            %eval(%length(&tempstr) - &index1));

      %let numkey=1;

      %let keyword=%scan(%bquote(&tempstr2), &numkey, %str(,));

      %do %while("&keyword" ne "");

      %* Get the style sheet values, if any;

      %if %index(%upcase(&keyword), CLASS=) gt 0
        %then %let scls&numsum=%scan(&keyword, 2, %str(=));
      %else %if %index(%upcase(&keyword), CLASSFMT=) gt 0
        %then %let scfm&numsum=%scan(&keyword, 2, %str(=));
      %else %if %index(%upcase(&keyword), ID=) gt 0
        %then %let sid&numsum=%scan(&keyword, 2, %str(=));
      %else %if %index(%upcase(&keyword), IDFMT=) gt 0
        %then %let sifm&numsum=%scan(&keyword, 2, %str(=));

        %let numkey=%eval(&numkey + 1);
        %let keyword=%scan(%bquote(&tempstr2), &numkey, %str(,));

      %end; %* KEYWORD loop;

    %end; %* CLASS/CLASSFMT/ID/IDFMT loop;

    %if (&&scls&numsum ne) and
        (&&scfm&numsum ne)
    %then %do;
      %put;
%put ERROR: You must specify either CLASS or CLASSFMT as a keyword;
      %put %str(       for the SUM variable %upcase(&&sum&numsum).);
      %put;
      %goto exit;
    %end;

    %if (&&sid&numsum ne) and
        (&&sifm&numsum ne)
    %then %do;
      %put;
      %put ERROR: You must specify either ID or IDFMT as a keyword;
      %put %str(       for the SUM variable %upcase(&&sum&numsum).);
      %put;
      %goto exit;
data one;x=1;run;
    %end;

    %let numsum=%eval(&numsum + 1);
    %local sum&numsum scls&numsum sid&numsum scfm&numsum sifm&numsum;
    %let tempstr=%scan(&sum, &numsum, %str( ));

  %end;

  %let numsum=%eval(&numsum - 1);

%end; %* Main SUM loop;

proc display c=sashelp.htmlgen.ckvar.scl; run;

%if &eflag eq 1 %then %goto exit;

%*;
%*  Check validity of sytle sheet arguments
%*;

%do i = 1 %to 5;

  %if (%bquote(&&sshref&i) ne ) and
      (%bquote(&&ssfile&i) ne or %bquote(&&ssfref&i) ne )
  %then %do;
    %put;
    %put ERROR: You must specify only one of the following for style;
    %put %str(       sheet &i.: SSHREF&i., SSFILE&i or SSFREF&i..);
    %put;
    %goto exit;
  %end;

  %if (%bquote(&&ssfile&i) ne ) and
      (%bquote(&&ssfref&i) ne )
  %then %do;
    %put;
    %put ERROR: You must specify a file OR fileref for style sheet &i..;
    %put;
    %goto exit;
  %end;

%end;

%if (%cmpres(&rowssfmt) ne ) and
    (%cmpres(&rowssvar) eq )
%then %do;
  %put;
  %put ERROR: You must specify a value for ROWSSVAR when;
  %put %str(       ROWSSFMT is specified.);
  %put;
  %goto exit;
%end;


%if (%upcase(&runmode) eq B or %upcase(&runmode) eq S) %then %do;

  %if (%bquote(&by) ne ) %then %do;

    %if "&data" eq "" %then %let data=&syslast;

    %if %upcase(&data) eq _NULL_ %then %do;
      %put;
      %put ERROR: Cannot process data set _NULL_.;
      %put;
      %goto exit;
    %end;

    %if (%upcase(&prttitle) ne FIRST) and
        (%upcase(&prttitle) ne ALL)
    %then %do;
      %put;
      %put ERROR: &prttitle is an unknown or unsupported value for the;
      %put %str(       title printing control.);
      %put %str(       Specify either ALL or FIRST.);
      %put;
      %goto exit;
    %end;

    %if (%upcase(&prtfoot) ne LAST) and
        (%upcase(&prtfoot) ne ALL)
    %then %do;
      %put;
      %put ERROR: &prtfoot is an unknown or unsupported value for the;
      %put %str(       footnote printing control.);
      %put %str(       Specify either ALL or LAST.);
      %put;
      %goto exit;
    %end;

    %if (%upcase(&prtpower) ne LAST) and
        (%upcase(&prtpower) ne ALL)
    %then %do;
      %put;
      %put ERROR: &prtpower is an unknown or unsupported value for the;
      %put %str(       SASPOWERED logo printing control.);
      %put %str(       Specify either ALL or LAST.);
      %put;
      %goto exit;
    %end;

    %if (&pw ne )
      %then %let tempstr=(pwreq=no pw=&pw);
      %else %let tempstr=(pwreq=no);

    %let dsid   = %sysfunc(open(&data&tempstr));
    %let sysrc  = %sysfunc(sysrc());
    %let sysmsg = %sysfunc(sysmsg());

    %if (&dsid le 0)
    %then %do;
      %put;
      %put &sysmsg;
      %put;
      %goto exit;
    %end;

    %let i=1;
    %let comma=;

    %let sqlsel=;
    %let sqlgrp=;
    %let sqlord=;

    %do i = 1 %to &numby;
      %let varnum = %sysfunc(varnum(&dsid,  &&by&i));

      %if (&varnum le 0)
      %then %do;
        %let sysrc = %sysfunc(close(&dsid));
        %put;
        %put ERROR: Variable %upcase(&&by&i) not found on;
        %put %str(       data set %upcase(&data).);
        %put;
        %goto exit;
      %end;

      %local btyp&i;
      %let btyp&i = %sysfunc(vartype(&dsid,
                                     %sysfunc(varnum(&dsid, &&by&i))));

      %if (%bquote(&&bfmt&i) eq )
        %then %let bfmt&i = %sysfunc(varfmt(&dsid,
                            %sysfunc(varnum(&dsid, &&by&i))));

    %end;

    %let sysrc = %sysfunc(close(&dsid));

    proc display c=sashelp.htmlgen.ckfmt.scl; run;

    %if &eflag eq 1 %then %goto exit;

    %let vcount=0;

    %do i = 1 %to &numby;

      %local sqlsel&i sqlgrp&i sqlord&i;

      %if (%bquote(&&bfmt&i) ne ) %then %do;
        %let vcount=%eval(&vcount + 2);
        %let sqlsel&i=%bquote(put(bydata.&&by&i.,&&bfmt&i.)
                      as &&by&i., min(bydata.&&by&i)  );
        %let sqlgrp&i=%bquote(calculated &&by&i);
        %let sqlord&i=%bquote(&vcount &&bso&i);
      %end;
      %else %do;
        %let vcount=%eval(&vcount + 1);
        %let sqlsel&i=%bquote(min(bydata.&&by&i) as &&by&i );
        %let sqlgrp&i=%bquote(&&by&i);
        %let sqlord&i=%bquote(bydata.&&by&i &&bso&i);
      %end;

      %let sqlsel&i=%cmpres(&&sqlsel&i);

    %end;

    %do i = 1 %to &numby;
      %let sqlsel=&sqlsel.&comma &&sqlsel&i;
      %let sqlgrp=&sqlgrp.&comma &&sqlgrp&i;
      %let sqlord=&sqlord.&comma &&sqlord&i;
      %let comma=,;
    %end;

    %if (%upcase(&sqlview) ne Y) and
        (%upcase(&sqlview) ne N) and
        (%upcase(&sqlview) ne YES) and
        (%upcase(&sqlview) ne NO) and
        (%upcase(&sqlview) ne  )
    %then %do;
      %put;
      %put ERROR: &sqlview is an unknown or unsupported SQL View flag.;
      %put %str(       Specify either Y or N.);
      %put;
      %goto exit;
    %end;

    %if &sqlview ne
    %then %let sqlview=%upcase(%substr(&sqlview, 1, 1));

    %if (&pw ne )
      %then %do;
        %if (&sqlview eq Y)
          %then %let tempstr=(pw=&pw);
          %else %let tempstr=(pw=&pw sortedby=_null_);
      %end;
      %else %do;
        %if (&sqlview eq Y)
          %then %let tempstr=;
          %else %let tempstr=(sortedby=_null_);
      %end;

    proc sql noprint;
      create table _bygrps as
        select &sqlsel
        from &data&tempstr as bydata
        group by &sqlgrp
        order by &sqlord
        ;

    quit;

    %if &syserr gt 4 %then %goto exit;

    proc sql noprint;
      select count(*)
      into :numgrps
      from _bygrps;
    quit;

    %if &syserr gt 4 %then %goto exit;

    %let fobs=1;

    %let whecount = 0;
    %let firstby  = 0;
    %let lastby   = 0;
    %let origpp   = &pagepart;

    %do i = 1 %to &numgrps;

      %let syslast = &last; %* reset SYSLAST;

      proc display c=sashelp.htmlgen.tabgenb.scl; run;

      %if &eflag eq 1 %then %goto exit;

    %end;

  %end; %* by group processing;
  %else %do;
    proc display c=sashelp.htmlgen.tabgenb.scl; run;
  %end;
%end;
%else %do;
  %if "&sysver" lt "6.12" %then %do;
    %put;
    %put ERROR: Interactive mode not supported prior to release 6.12.;
    %put;
    %goto exit;
  %end;
  proc display c=sashelp.htmlgen.tabgeni.frame; run;
%end;

%exit:
options &notes;

%mend ds2htm;
