/*
Hi Rao,

The following tested, recursive macro code might help you generating all combinations of given variables:

OPTIONS PS=63 LS=80;
*/

%macro combine / PARMBUFF;
  %LOCAL Comma NoFirst Number Argument VarList Element;

  %LET Comma = %INDEX(&SYSPBUFF,%STR(,));
  %IF (&Comma EQ 0) %THEN %LET NoFirst = ;
  %ELSE %LET NoFirst = %SYSFUNC (COMPRESS (%BQUOTE
                      (%SUBSTR (&SYSPBUFF, &Comma+1) ), %STR(%(%)) ) );

%* 1. return first argument only;
  %SCAN(&SYSPBUFF,1) %STR(,) %* returned;

  %IF (&Comma) %THEN
  %DO;
    %LET CombList =  %Combine(&NoFirst);

%* 2. return first argument and combination of remaining ones;

    %LET VarList = ;
    %LET Number=1;
    %LET Element=%SCAN(%STR(&CombList),&Number,%STR(,));
    %DO %WHILE(&Element NE);
      %LET VarList = &VarList %SCAN(&SYSPBUFF,1) &Element %STR(,);
      %LET Number=%EVAL(&Number+1);
      %LET Element=%SCAN(%STR(&CombList),&Number,%STR(,));
    %END;

    &VarList %* returned;

%* 3. return combination of remaining ones only;
    &CombList %* returned;
  %END;

%MEND Combine;

%macro process (List);
  %LET Counter=1;
  %LET Element=%SCAN(&List,&Counter,%STR(,));
  %LET NoFirst = ;
  %DO %WHILE(&Element NE);
/*========= this may be your application code =========
    MODEL DepVar=&Element;
=========*/
    %PUT Element=<<&Element>>;
    %LET Counter=%EVAL(&Counter+1);
    %LET Element=%SCAN(&List,&Counter,%STR(,));
  %END;
%MEND Process;

%LET CombList=%Combine(Var1,Var2,Var3,Var4,Var5); %* VARIABLE LIST;

/*========= this may be your application code =========
  PROC REG;
=========*/
%Process (&CombList);
/*========= this may be your application code =========
  RUN;
=========*/

/*
Regards - Jim.
--
.  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .

Jim Groeneveld, MSc.
Biostatistician
Science Team
Vitatron B.V.
Meander 1051
6825 MJ  Arnhem
Tel: +31/0 26 376 7365
Fax: +31/0 26 376 7305
Jim.Groeneveld@Vitatron.com
www.vitatron.com

My computer remains home, but I will attend SUGI 2004.

[common disclaimer]


-----Original Message-----
From: Nageswara Rao Punnani [mailto:npunnani@MEMPHIS.EDU]
Sent: Sunday, March 14, 2004 20:47
To: SAS-L@LISTSERV.UGA.EDU
Subject: proc regression macro ..


hi all ,

can someone please help me out in solving the following problem ..i am
trying to write a macro for this proc reg step ..the macro should be able
to take in  the variables as parameters  and generate the combinations of
all the models possible ..the macro should generate 2 to the power of n
minus 1 no. of models .
eg. %xyz(x1,x2,x3,x4) macro should generate 15 model statements inside the
proc reg statement .
the following proc step actually takes in 3 variables and generates 7
statements ..

proc reg data=z;
       M1: model y=x1;
    M2: model y=x2;
    M3: model y=x3;
    M4: model y=x1 x2;
    M5: model y=x1 x3;
    M6: model y=x2 x3;
    M7: model y=x1 x2 x3;
                             run;
i hope i was able to put across my problem properly ..

i would really appreciate if someone can help me out in this

thanks
rao.
*/
