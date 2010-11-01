
TITLE 'uListMac: list of autocall macros'
      ' to check for duplicate names';
;/*uListMac: utility: List Macros
RJF2 00Sep11
notes on autocall usage
1. to use system macros only: no change
   see config: v6: config.sas, SASv7.cfg, SASv8.cfg
2. to use your own macros AND system macros:
   add path(s) to top of list of -SET SASAUTOS in config
3. to use only your own macros:
   filename SASAUTOS "<fileref";%*single;
   or for multiple filerefs:
      note parens and comma delimiters
   filename SASAUTOS ("<fileref-1"
                     ,"<fileref-2"
                     );
;/*..............................................*/
%MACRO MEMLIST(DIR1,DIR2,XPATH);
%*get list of members in directory;
DATA &DIR1._&DIR2.(keep = Dir1 Dir2 FileName);
length Dir1 Dir2 FileName $ 8;
retain Dir1 "&DIR1."
       Dir2 "&DIR2.";
Rc   = filename('dir', "&XPATH.");
DsId =    dopen('dir');
if DsId then do;          dim_DS=dnum(DsId);
 do MemNum = 1 to dim_DS; MemName = lowcase(dread(DsId, MemNum));

                          *put memname=;
  if scan(MemName,-1,'.') eq 'sas' then do;
                          FileName=scan(MemName,-2,'.\');
                          output;
                                           %*if scan;   end;
                                           %*do memnum; end;
                                           %*if DsId;   end;
Rc=dclose(DsId);
run;

%*user names may be in upper & lower case
  SI macros are all lower case
  note FileName has been lowcased;
proc SORT data = &DIR1._&DIR2.
          nodups;
          by     FileName;
run;%*.........................................; %MEND;

%*read list of External Files(FileRef=SASAUTOS)
  call MEMLIST for each;

DATA DSN(keep = Dir1 Dir2);
length Dir1 Dir2 $ 8;
set SASHELP.VEXTFL
   (where=(FileRef eq 'SASAUTOS'));
DIR2= scan(trim(xPath),-1,'\');%*trim is a kludge;
DIR1= scan(trim(xPath),-2,'\');
call execute('%MEMLIST(' !!       Dir1
                         !! ','!! Dir2
                         !! ','!! xPath
                         !! ');');
%*concatenate;
DATA _NULL_;
call execute('data ALL;set');
do until(EndoFile);
 set DSN end = EndoFile;
 call execute( trim(Dir1) !! '_' !! Dir2 );                    end;
call execute('; by FileName Dir1 Dir2;');
call execute(
"if not(first.FileName and last.FileName) then Note='duplicate';");
stop;

proc PRINT data = ALL;
run;
