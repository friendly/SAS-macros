/* SAS2EXCL.sas, example from SUGI 17 proceedings page 117           */        
/* will send via dde from sas to excel and retain column headings    */        
                                                                                   
%macro sas2excl (
   data=_last_,   /* name of sas dataset */               
   colhead=NAME,  /* change to LABEL to have columns use labels */ 
   exdirin=,      /* directory of excel sheet */                                                  
   excelin=SHEET1,      /* name of excel sheet */                 
   excelout=
);                                                   
                                                                                   
%if %upcase(&colhead) ne NAME and                                               
	%upcase(&colhead) ne LABEL %then %do;                                       
		data _null_;                                                                
		put 'Parameter error: COLHEAD='"&colhead";                               
		abort;                                                                    
	run;                                                                        
%end;                                                                           

options noxwait noxsync ;                                                 
                                                                                   
filename cmdexcel dde 'excel|system'; 
                                          
/* Start Excel if it is not already open */
data _null_;
	fid = fopen('cmdexcel', 'S');		*-- check if Excel is open;

	if fid le 0 then do;
		rc = system("Start Excel");	*-- DOS cmd to open Excel;
		start = datetime();				*-- note start time;
		stop = start+10;					*-- max time to try;
		
		do while(fid le 0);
			fid = fopen('cmdexcel', 'S');		*-- check if Excel is open;
			time = datetime();					*-- reset current time;
			if time ge stop then
				fid = time;							*-- terminate loop;
			end;	/* while fid le 0 */
		end; /* not open, open Excel via Windows registry */
	rc = fclose(fid);		*-- close fileopen on Excel;
run;
*-- do not clear the CMDEXCEL fileref;

                                                                                   
   %if %upcase(&excelin) ne SHEET1 %then %do;                                      
       data _null_;                                                                
         file cmdexcel;                                                            
         put "[open(%bquote(&exdirin&excelin))]";                                  
run;                                                                        
  
%end;                                                                           
                                                                                   
proc contents data=&data noprint                                               
					out=conts(keep=nobs NAME label);                                 
run;                                                                            
                                                                                   
   data _null_;                                                                    
     set conts end=eof;                                                            
      if label eq '' then                                                          
        label=NAME;                                                               
      call symput('col'||left(_n_),trim(NAME));                                    
      call symput('lab'||left(_n_),trim(LABEL));                                   
                                                                                   
      if eof then do;                                                              
      call symput('columns',trim(left(_n_)));                                      
      call symput('rows',trim(left(nobs)));                                        
	end;                                                                         
  
run;                                                                            
                                                                                   
   filename excel dde "excel| &excelin.!r1c1:r1c&columns" notab;                   
                                                                                   
   data _null_;                                                                    
     file excel;                                                                   
    
	hextab='09'x;                                                                 
     %if %upcase(&colhead) eq NAME %then %do;                                      
       put %do i=1 %to &columns;                                                   
       "%trim(&&col&i)" hextab                                                     
      %end;                                                                       
      ;                                                                           
    %end;                                                                         
                                                                                   
     %else %if %upcase(&colhead) eq LABEL %then %do;                               
       put %do i=1 %to &columns;                                                   
       "%trim(&&lab&i)" hextab                                                     
      %end;                                                                       
      ;                                                                           
    %end;                                                                         
  
run;                                                                            
                                                                                   
filename excel dde "excel|&excelin.!r2c1:R%eval(&rows+1)C&columns." notab;      
                                                                                   
data _null_;                                                                    
	set &data;                                                                   
	hextab='09'x;                                                                 
	file excel;                                                                   
	put %do i=1 %to &columns;                                                     
		&&col&i hextab                                                              
	%end;                                                                       
	;                                                                           
run;                                                                            
                                                                                   
%mend sas2excl;                                                                 
