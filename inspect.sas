/*
Inspect a data set - from Dana Quade at UNC Biostatistics

Output from sample program:                                                     
                                                                                
INSPECTION OF DATASET UBRARY.SAMDAT                                             
25 OBSERVATIONS                                                                 
                                                                                
CHARACTER VARIABLES                                                             
                                                                                
              -------LENGTH---(0 IF MISSING)-------   -TRUNCATED TO AT MOST 18  
 CHARACTERS-                                                                    
VARIABLE    #=0   #=1   #=2   #=3   #=4   #>4   MAX   ALPHA              OMEGA  
                 LABEL                                                          
                                                                                
NAME          0     0     0     5    14     6     5   Alice              Zelda  
            FIRST NAME                                                          
SCHOOL        0    25     0     0     0     0     1   A                  D      
            SCHOOL                                                              
                                                                                
NUMERIC VARIABLES                                                               
                                                                                
VARIABLE    #=.   #<0   #=0   #=1   #=2   #=3   #=4  #OTH        MIN       MEAN 
       MAX       LABEL                                                          
                                                                                
SEX           0     0    12    13     0     0     0     0          0       0.52 
         1  0(FEMALE) / 1(MALE)                                                 
HEIGHT        1     0     0     0     0     0     0    24         49  54.083333 
        60  HEIGHT IN INCHES                                                    
WEIGHT        1     0     0     0     0     0     0    24         50  65.916667 
        98  WEIGHT IN POUNDS                                                    
IQ            3     0     0     0     0     0     0    22         83  107.45455 
       140  INTELLIGENCE QUOTIENT                                               
DAYS          0     0     4     5     3     1     2    10          0          5 
        24  DAYS ABSENT DURING SCHOOL YEAR                                      
SCORE         0     0     3     4     9     5     4     0          0       2.12 
         4  4(A) / 3(B) / 2(C) / 1(D) / 0(F)                                    
                                                                                
NOTES:                                                                          
1) MAKE SURE YOUR LINESIZE IS THE FULL 132 COLUMNS (SAS DEFAULT).               
   (The output above was produced with LS=132 but printed with                  
   LS=80 so the last part of each line is missing.)                             
                                                                                
2) If "DATA=" is omitted (type "%INSPECT( )", INSPECT will use                  
   DATA=_LAST_.                                                                 
                                                                                
3) Character variables:                                                         
0              Number of observations with missing values ("length 0")          
#=0, ..., #=4  Number of observations with length 1, ..., 4                     
#>4            Number of observations with length > 4                           
MAX            Length of longest value                                          
ALPHA          First non-missing value in alphabetic order  |  (truncated to    
OMEGA          Last  non-missing value in alphabetic order  |   18 characters)  
4) Numeric variables:                                                           
#=.            Number of observations with missing values (of all kinds)        
#<0            Number of observations with negative values                      
#=0, ..., #=4  Number of observations with values equal to 0, ..., 4            
#OTH           Number of observations with other values                         
MIN (MAX)      Minimum (maximum) non-missing value                              
MEAN           Mean of non-missing values                                       
                                                                                
5) If your dataset does not have both character and numeric                     
   variables, you will get a warning about an array of dimension                
   zero, but this can be ignored .                                              
*/                                                                                
                                                                                
* INSPECTION OF DATASET;                                                        
%MACRO INSPECT(DATA=_LAST_);                                                    
options ls=132;

%MACRO I1; @15 "-------LENGTH---(0 IF MISSING)-------"                          
       @55 "-Truncated to at most 18 characters-" /                             
       @1 "Variable    #=0   #=1   #=2   #=3   #=4   #>4   MAX"                 
       @55 "ALPHA" @74 "OMEGA" @108 "LABEL"; 
		 %MEND I1;                          

%MACRO I2; @1 "Variable    #=.   #<0   #=0   #=1   #=2   #=3   #=4  #OTH"       
       @66 "MIN" @76 "MEAN" @88 "MAX" @108 "LABEL" /; 
		 %MEND I2;                 

 DATA _NULL_; SET &DATA NOBS=_N;                                                
    ARRAY _1 (*) _CHARACTER_; CALL SYMPUT("MC",LEFT(PUT(DIM(_1),8.)));          
    ARRAY _2 (*) _NUMERIC_;   CALL SYMPUT("MN",LEFT(PUT(DIM(_2),8.)));          
    CALL SYMPUT("N",TRIM(LEFT(PUT(_N,8.)))); 
	 RUN;                               

%IF &MC NE 0 %THEN %DO;                                                         
    PROC TRANSPOSE DATA=&DATA OUT=FC PREFIX=X; VAR _CHARACTER_; 
	 %END;           
%IF &MN NE 0 %THEN %DO;                                                         
    PROC TRANSPOSE DATA=&DATA OUT=FN PREFIX=Y; VAR _NUMERIC_;   
	 %END;           

DATA _NULL_;                                                                    
     FILE PRINT LINESLEFT = LL; ARRAY S{0:5} S0-S5;                             
     IF _N_ = 1 THEN PUT                                                        
        / "Inspection report for dataset &DATA" // "&N Observations";                   
     RETAIN Q QC QN 0; 
	  LENGTH _LABEL_ $ 40; _LABEL_ = " ";                      
IF Q = 0 THEN DO; 
	IF QC = 0 THEN PUT // "CHARACTER VARIABLES" /;               

   %IF &MC = 0 %THEN %DO; 
	PUT "NONE"; Q = 1; %END;                              
   %ELSE %DO;
		SET FC END=LAST; 
		ARRAY X (&N) $ X1-X&N;                           
      ALPHA = "                  "; OMEGA = ALPHA;                              
      IF QC = 0 THEN PUT %I1; QC = 1;                                           

   DO J = 0 TO 5; 
	S(J) = 0; 
	END; MAX = 0;                                       
   DO I = 1 TO &N; 
		XX = X(I); 
		IF XX = " " THEN S0 + 1;                          
      ELSE DO; 
			L = LENGTH(XX); MAX = MAX(MAX,L);                                
			IF L < 5 THEN S(L) + 1; ELSE S5 + 1;                                      
			IF XX < ALPHA OR ALPHA = " " THEN ALPHA = XX;                             
			IF XX > OMEGA THEN OMEGA = XX;
		END; 
	END;                                  
   IF LL < 2 THEN PUT _PAGE_ %I1;                                               
   PUT _NAME_ @10 (S0-S5 MAX) (6.0) @55 ALPHA @74 OMEGA @93 _LABEL_;            
   IF LAST THEN Q = 1; 
	%END; 
	END;                                               
IF Q = 1 THEN DO; IF QN = 0 THEN PUT /// "NUMERIC VARIABLES" /;                 
   %IF &MN = 0 %THEN %DO; 
	PUT "NONE"; STOP; %END;                               
   %ELSE %DO; SET FN; ARRAY Y (&N) Y1-Y&N; IF QN = 0 THEN PUT %I2; QN = 1;      

   DO J = 0 TO 4; 
	S(J) = 0; END; 
	SM = 0; SN = 0; MIN = .; MEAN = .; MAX = .;    

   DO I = 1 TO &N; YY = Y(I);                                                   
      IF YY LE .Z THEN SM + 1; ELSE DO; SN + (YY<0);                            
      S0 + (YY=0); S1 + (YY=1); S2 + (YY=2); S3 + (YY=3); S4 + (YY=4);          
      MIN = MIN(MIN,YY); MEAN = SUM(MEAN,YY); MAX = MAX(MAX,YY); END; END;      

   IF LL < 2 THEN PUT _PAGE_ %I1;                                               
   N = &N - SM; 
	MEAN = MEAN / N; OTH = N - SN - S0 - S1 - S2 - S3 - S4;         
   PUT _NAME_ @10 (SM SN S0-S4 OTH) (6.0)                                       
       (MIN MEAN MAX) (+2,BEST9.) @93 _LABEL_; 
	%END; END;                       

PROC DATASETS NOLIST; 
	TITLE; 
	DELETE                                             
     %IF &MC NE 0 %THEN FC; %IF &MN NE 0 %THEN FN; ; 
%MEND INSPECT;             
