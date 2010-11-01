;/* ARGS: ARGUMENTS of a macro                                   01Jun26
          prints text of macro, from beginning of file
          till semicolon ending macro statement
USAGE: %ARGS(<filename>);
       %ARGS(<filename>,PRINT=LOG);
RJF2 99Nov19 written to list leading documentation
                             and arguments of a macro
             modeled after S-Plus method for viewing function arguments
;/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . */
%MACRO ARGS(
 FILENAME       /* NOTE: NO EXTENSION                                 */
,PRINT   =PRINT /* in (PRINT,LOG)                                     */
);
data _NULL_;
 file &PRINT.;
 retain OpenCmnt 1 MacParms 0;
 do until(EndoFile);%*-------------------------------------------------;
  infile SASAUTOS("&FILENAME..SAS")
   end = EndoFile
   pad lrecl=72;
  input @1 Line $char72.;
  if OpenCmnt then
  if index(upcase(Line),'%MACRO') then do; MacParms = 1;
                                           OpenCmnt = 0;            end;
  if OpenCmnt or MacParms         then     put Line $char72.;
  if MacParms and index(Line,';') then     MacParms = 0;
                                      %*do until(EndoFile);         end;
stop;
run;%*...........................................................;%MEND;
%*ARGS(ARGS);
