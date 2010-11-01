%MACRO vexpand(data,var);

%LOCAL newvar;

%LET data=%UPCASE(&data);
%LET var=%SYSFUNC(COMPRESS(%UPCASE(&var)));
%IF %INDEX(&data,.)=0 %THEN %LET data=WORK.&data;

%IF %INDEX(%STR(_ALL_ _NUMERIC_ _CHARACTER_),&var) %THEN %DO;

    DATA _vnames_;
      SET &data(KEEP=&var obs=0);
    RUN;

    PROC sql NOPRINT;
      SELECT name INTO :newvar SEPARATED BY ' '
      FROM DICTIONARY.COLUMNS
      WHERE libname='WORK' AND memname='_VNAMES_' AND memtype='DATA';
    QUIT;

    PROC datasets LIB=work MT=data NOLIST;
      DELETE _vnames_;
    RUN;
    QUIT;

%END;

%ELSE %IF %INDEX(&var,--) OR
          %INDEX(&var,-NUMERIC-) OR
          %INDEX(&var,-CHARACTER-) %THEN %DO;

          %LOCAL var1 var_last num1 num_last typestr;
          %LET var1=%SCAN(&var,1,-);

          %IF %INDEX(&var,--) %THEN %LET var_last=%SCAN(&var,2,-);
          %ELSE %LET var_last=%SCAN(&var,3,-);


          %IF %INDEX(&var,-NUMERIC-) %THEN %LET typestr=%STR( and
type='num' );
          %ELSE %IF %INDEX(&var,-CHARACTER-) %THEN %LET
             typestr=%STR( and type='char' ) ;

          PROC sql NOPRINT;
            SELECT varnum INTO :num1
            FROM DICTIONARY.COLUMNS
            WHERE libname="%SCAN(&data,1,%STR( .))" AND
                  memname="%SCAN(&data,2,%STR(  .))" AND
                  memtype='DATA' AND
                  name="&var1" &typestr;

          %IF &num1 NE %STR() %THEN %DO;
            SELECT varnum INTO :num_last
            FROM DICTIONARY.COLUMNS
            WHERE libname="%SCAN(&data,1,%STR( .))" AND
                  memname="%SCAN(&data,2,%STR(  .))" AND
                  memtype='DATA' AND
                  name="&var_last" &typestr;
          %END;


          %IF &num1 NE %STR() AND
              &num_last NE %STR() AND
              %EVAL(&num1<=&num_last) %THEN %DO;

              SELECT name INTO :newvar SEPARATED BY ' '
              FROM DICTIONARY.COLUMNS
              WHERE libname="%SCAN(&data,1,%STR( .))" AND
                    memname="%SCAN(&data,2,%STR(  .))" AND
                    memtype='DATA' AND
                    varnum >= &num1 AND
                    varnum <= &num_last &typestr;

          %END;

          QUIT;

%END;

%ELSE %DO;

          %LOCAL vlist;

          DATA _vnames_(KEEP=vname);
            ARRAY vars[*] &var;
            LENGTH vname $8;
            DO i=1 TO DIM(vars);
              CALL VNAME(vars[i],vname);
              OUTPUT;
            END;
          RUN;

          PROC sql NOPRINT;
            SELECT QUOTE(vname) INTO: vlist SEPARATED BY ','
            FROM _vnames_;

            SELECT name INTO: newvar SEPARATED BY ' '
            FROM DICTIONARY.COLUMNS
            WHERE libname="%SCAN(&data,1,%STR( .))" AND
                  memname="%SCAN(&data,2,%STR(  .))" AND
                  memtype='DATA' AND
                  name IN (&vlist);
          QUIT;

          PROC datasets LIB=work MT=data NOLIST;
            DELETE _vnames_;
          RUN;
          QUIT;

%END;

%put &newvar ;

%MEND vexpand;
