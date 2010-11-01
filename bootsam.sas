%MACRO BOOTSAM (DATA=&SYSDSN,SAMPLE=100,FREQ=NIDOBS,NOGO=0);* / STMT;
 
     /* I generally find it better to use &SYSDSN rather than
        _LAST_ in case I need to refer to the data set later */
 
%IF &DATA=&SYSDSN %THEN %DO;
     %LET DATA1=%SCAN(&DATA,1);
     %LET DATA2=%SCAN(&DATA,2);
     %LET DATA=&DATA1..&DATA2;
%END;
 
    /* if there is a FREQ variable on original data, need to create
        individual observations for each one */
 
%IF &FREQ ^= NIDOBS %THEN %DO;
            data bootdata(drop=&FREQ);
              drop bob;
              set &DATA;
              bob=&FREQ;
              do until (bob<=0);
                   if bob ^=0 then output; bob=bob-1;
              end;
            run;
     %LET DATA=BOOTDATA;
%END;
 
     /* this draws &SAMPLE number of samples of size n from
        the original data.  Obviously, many samples of a large
        dataset may be a pain in the butt */
 
            data bootdata;
              drop i;
              do j=1 to &SAMPLE;
                   do i=1 to n;
                        iobs=int(ranuni(0)*n)+1;
                        idobs=iobs;
                        set &DATA point=iobs nobs=n;
                        output;
                   end;
              end;
              stop;
            run;
 
   /* this section takes the individual level bootstrap data
      and groups it together by original ID number.  That is,
      if the first obs of the original dataset appears three
      times in sample 1, NIDOBS (or whatever the user set &FREQ
      equal to) will be three for IDOBS=1.
      &NOGO is not needed for anything I sent you, but is needed
      for other bootstrap macros I'm working on which do not process
      data using FREQ variables. */
 
%IF &NOGO^=1 %THEN %DO;
            proc sort data=bootdata;
              by j idobs;
            run;
            data bootdata;
              set bootdata;
              by j idobs;
              retain &FREQ 0;
              &FREQ=&FREQ+1;
              if last.idobs then do;
                   output;
                   &FREQ=0;
              end;
            run;
%END;
 
%MEND BOOTSAM;
