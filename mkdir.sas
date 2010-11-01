/*
 Subject: SOLUTION: Create a directory from SAS
 Date:    05/22/2000
 Author:  Benjamin Guralnik <guralnik@BEZEQINT.NET>Thanks, Jim.
    
 That would be very interesting. Meanwhile, here's a 100% SAS solution
that I found. I'm quite surprised that it can even handle long filenames
as well -- 

  
 %mkdir(c:\testdir\anothertest\and_yet_another_one);

*/  
  
 %macro mkdir(path,dlm=\);
  
   %let msg = The path &path was successfully created.;
  
   %let pos = 1;
   %let token = %scan(&path, &pos, &dlm);
   %let segment =;
   %let delim =;
   %let op =;
  
  
   %do %while (&token ne);
  
     %let segment = &segment.&delim.&token;
     %let delim = \;
     %* put &segment;
  
     %let pos = %eval(&pos + 1);
     %let token = %scan(&path, &pos, &dlm);
  
     %if %sysfunc(fileexist(&segment)) eq 0 %then %do;
       %sysexec md &segment;
       %* put ERRORCODE &sysrc;
       %let op = 1;
  
         /* if the operation failed, display approptiate message
           and abort the program */
         %if %sysfunc(fileexist(&segment)) eq 0 %then %do;
         %let token =;
         %let msg = Invalid path name, directory will not be
created.;         %end;
  
     %end;
   %end;
  
   %if &op = 1 %then %put MKDIR: &msg;
	/*
   %else %put MKDIR: The path &path does already exist;
   */
 %mend;
  
