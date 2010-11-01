%Macro sasfile;
%* sasfile.SAS;
%*
  961127, SB: Design changed to use of %getopt
;
%* sasfile  - small case
   sasfileU - Upper case
   sasfileL - Long name, ie the path
;
%Global sasfile sasfileu sasfilel;
%Let sasfileu=%upcase(%getopt(SYSIN));
%Let sasfile=%lowcase(%getopt(SYSIN));
%Let sasfilel=%sysget(PWD)/&sasfile;
%Mend sasfile;
