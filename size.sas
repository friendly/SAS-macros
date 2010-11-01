/*------------------------------------------------------------------*
 | Macro to set goptions for figure sizes                           |
 | Use:                                                             |
 |  %size(s,b)                                                      |
 |     s=1..5  - set size explicitly, in SAS file or in GOPTIONS    |
 |     s=0     - goptions are set in SAS file (special cases)       |
 |     b=0     - no border                                          |
 |     b=1     - border (default)                                   |
 *------------------------------------------------------------------*/
%macro size(s,b);
 
       /* Set border & footnotes for space on sides & bottom */
       /* Note: Border and footnotes set if b=""             */
     %if &b ^= 0 %then %do;
         footnote h=.5  ' '
                  a=90  ' '
                  a=-90 ' ';
         goptions border;
         %end;
     %else %do;
         goptions noborder;
         %end;
 
       /* text width, square            */
     %if &s=1 %then %do;
     goptions hsize=5.16in vsize=5.16 vpos=55;
     %end;
%else  /* text width, rectangular, tall */
     %if &s=2 %then %do;
     goptions hsize=5.16in vsize=6.9in  vpos=60;
     %end;
%else  /* full width, square             */
     %if &s=3 %then %do;
     goptions hsize=6.5in  vsize=6.5in horigin=1in vpos=45;
     %end;
%else  /* full width, rectangular (tall) */
     %if &s=4 %then %do;
     goptions hsize=6.5in  vsize=8.6in  vpos=60;
     %end;
%else  /* full width, rectangular, short */
     %if &s=5 %then %do;
     goptions hsize=6.5in  vsize=4.0in  hpos=60 vpos=66;
     %end;
%else  /* square, SUGI 2-col format @65% */
     %if &s=6 or &s=SUGI65 %then %do;
     goptions vsize=12.9 cm hsize=13.1 cm
              horigin=1.75 in vorigin=2 in;
     %end;
%else  /* square, offset for foils       */
     %if &s=7 %then %do;
     goptions vsize=6.5 in hsize=6.2 in
              horigin=1 in vorigin=1.5 in;
     %end;
%else  /* square, SUGI 2-col format @65% */
     %if &s=8 or &s=SUGI %then %do;
     goptions vsize=8.5 cm hsize=8.5 cm
          /*    horigin=0.2cm vorigin=0.2cm */;
     %end;
%mend size;
