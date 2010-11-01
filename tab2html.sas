%macro tab2html(infile=TEMP, outfile=table.html);                                                 
*macro t2h(outfile=table.html);                                                 
***********************************************************************         
*  t2h.sasmacro - Convert PROC TABULATE output to HTML 3.0 Tables
*  Version:  0.1alpha
*
*  Copyright (C) 1996 Thom Kunselman, vaatek@ukcc.uky.edu
*
*  Please e-mail your questions and comments to me.  Im not really a
*  technical type, as you can probably tell by the code, but Im willing
*  to fix bugs and incorporate your suggestions as my time permits.
*
*  I have received much useful information from comp.soft-sys.sas and
*  since I dont usually know enough to contribute, please accept this
*  as partial payment for all the help everyone has given me.
*
*************************************************************************
*  This package should include tabulate.sas and t2h.sasmacro(this file)
*                                                                               
* Current limitations:                                                          
*   Maximum of 99 columns                                                       
*   Maximum of 9,999 rows                                                       
*   Maximum row length of 200 characters                                        
*   
*  See tabulate.sas for additional limitations   
*
*  You should probably take a look at tabulate.sas for an example of
*  how to invoke this macro.
*                                                                               
*  COPYRIGHT
*  NOTICE:
*  This program is free software. You can redistribute it and/or
*  modify it under the terms of the GNU General Public License
*  as published by the Free Software Foundation, either version 2
*  of the License, or (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY, without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program, if not, write to the Free Software
*  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*  You can find a copy of the GNU General Public License in t2h.license
***********************************************************************;        
%if "&outfile" eq "table.html" %then %put NOTE: The OUTFILE parameter         
was not specified, using output filename: &outfile;                             
                                                                                
***********************************************************************         
* Read the tabulate into the dataset RAW, and split the line up                 
* into indiridual characters for determining rows and columns.                  
* Create a dataset, TABLE, which includes only rows of the table.               
*                                                                               
***********************************************************************;        
data raw;                                                                       
  length line $200;                                                             
  infile &INFILE TRUNCOVER;                                                             
  array columns(200) $ x1-x200;                                                 
  input  (x1-x200) ($CHAR1.) 
	 @1 line $CHAR200.;                                           
                                                                                
data table;                                                                     
  set raw;                                                                      
    * remove lines that are not part of the table;                              
  if not (x1 in ('|')) then delete;                                             
                                                                                
                                                                                
***********************************************************************         
* Determine each column starting point.                                         
* Then transpose this dataset and make 9,999 observations each                  
* containing cols variables set to each column starting point.                  
* Never know how many rows are going to be in a table.                          
*                                                                               
***********************************************************************;        
data cols(keep=j);                                                              
  set table;                                                                    
  array columns(200) $ x1-x200;                                                 
  do j = 1 to 200;                                                              
   if columns(j) eq '|' then output cols;                                       
  end;                                                                          
                                                                                
proc sort data=cols nodupkey; by j;                                             
                                                                                
proc transpose data=cols out=outcols prefix=col; var j;                         
                                                                                
data outcols;                                                                   
  set outcols;                                                                  
  do j = 1 to 9999;                                                             
     output;                                                                    
  end;                                                                          
                                                                                
data table;                                                                     
  merge outcols table(in=a);                                                    
  if x1 eq '' then delete;                                                      
                                                                                
                                                                                
***********************************************************************         
* Next split the text up into separate column cells.                  *         
* If a cell spans more than two columns, then concatenate the text    *         
* into the cell.                                                      *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************;        
  data colsplit;                                                                
    array columns(200) $ x1-x200;                                               
    array colspn{99} colspn1-colspn99;                                          
    array col{99} col1-col99;                                                   
    set table;                                                                  
    array coltxt{99} $99 coltxt1-coltxt99;                                      
    maxcol = 1;                                                                 
    do while(col{maxcol+1} ne .);                                               
      coltxt{maxcol}=substr(line,col{maxcol},col{maxcol+1}-col{maxcol});        
      maxcol = maxcol + 1;                                                      
    end;                                                                        
    maxcol = maxcol -1;  *reset back one cause of ending |;                     
    do j = 1 to maxcol;                                                         
       colspn{j} = 1;                                                           
    end;                                                                        
    do j = maxcol to 2 by -1;                                                   
      if substr(coltxt{j},1,1) ne '|' then do;                                  
              coltxt{j-1} = trim(coltxt{j-1})|| trim(left(coltxt{j}));          
              coltxt{j} = '';                                                   
              colspn{j-1} = colspn{j-1} + colspn{j};                            
              colspn{j} = .;                                                    
      end;                                                                      
    end;                                                                        
    do j =  1 to maxcol;                                                        
      if substr(coltxt{j},1,1) eq '|'                                           
        then coltxt{j} = trimn(left(substr(coltxt{j},2)));                      
    end;                                                                        
                                                                                
***********************************************************************         
* Begin section to calculate rowspans for each cell.                  *         
*                                                                     *         
* Fist, reverse the table using rowid to keep track of the rows.      *         
*                                                                     *         
* Next, go from the bottom of the table to the top and check to see   *         
* whether there is a separator line or not. All tables will have a    *         
* seperator line at the bottom.  WARNING: DO NOT USE NOSEPS.          *         
*                                                                     *         
* If a seperator line exists, reset rowspn  to 1                      *         
* If a seperator line does not exist, then add 1 to rowspn            *         
*                                                                     *         
* Do not worry about spanned cells, rowspn set for these will be      *         
* checked for and dropped when writing the html.                      *         
*                                                                     *         
***********************************************************************;        
data revtable colsplit;                                                         
  set colsplit;                                                                 
 *array rowspn{99} rowspn1-rowspn99;                                            
  rowid = _N_;                                                                  
                                                                                
proc sort data=revtable; by descending rowid;                                   
                                                                                
data rowspans;                                                                  
  set revtable;                                                                 
  array coltxt{99} $99 coltxt1-coltxt99;                                        
  array tmptxt{99} $99 tmptxt1-tmptxt99;                                        
  array columns{200} $ x1-x200;                                                 
  array col{100} col1-col100;                                                   
  array rowspn{99} rowspn1-rowspn99;                                            
  retain rowspn1-rowspn99 tmptxt1-tmptxt99;                                     
  do j = 1 to 99;                                                               
    if rowspn{j} eq . then rowspn{j} = 0;                                       
  end;                                                                          
  do j = 1 to maxcol;                                                           
    if columns{col{j}+1} ne '_' then do;                                        
      rowspn{j} = rowspn{j} + 1;                                                
      if substr(coltxt{j},length(trim(coltxt{j})),1) eq '-' then       
      tmptxt{j} = substr(trim(coltxt{j}),1,length(trim(coltxt{j}))-1) 
		  || trim(tmptxt{j});
      else tmptxt{j} =  trim(coltxt{j}) || ' ' || trim(tmptxt{j});           
      coltxt{j} = left(tmptxt{j});                                              
    end;                                                                        
    else do;                                                                    
      rowspn{j} = 0;                                                            
      tmptxt{j} = '';                                                           
    end;                                                                        
  end;                                                                          
                                                                                
proc sort data=rowspans; by rowid;                                              
                                                                                
data revspans;                                                                  
  set rowspans;                                                                 
  array coltxt{99} $99 coltxt1-coltxt99;                                        
  array tmptxt{99} $99 tmptxt1-tmptxt99;                                        
  array columns{200} $ x1-x200;                                                 
  array col{100} col1-col100;                                                   
  array rowspn{99} rowspn1-rowspn99;                                            
  do j = 1 to maxcol;                                                           
    if rowspn{j} lt lag(rowspn{j}) then coltxt{j} = '';                         
  end;                                                                          
  if rowspn1 eq 0 or rowspn2 eq 0 then delete;                                  
  drop rowspn1-rowspn99;                                                        
                                                                                
***********************************************************************         
* After deleting out the seperator lines as in above, it now appears  *         
* that there are some boxes, ie, the upper left hand box, that no     *         
* longer has the correct ROWSPN information, so this needs to be      *         
* recalculated.                                                       *         
*                                                                     *         
* So after doing what we did before, all over again without the       *         
* seperator lines, we finally arrive at a useable dataset called      *         
* rowsout.                                                            *         
*                                                                     *         
* NOTE:  May need to delete more than just rowspn1 and rowspn2 eq 0.  *         
*                                                                     *         
*                                                                     *         
***********************************************************************;        
                                                                                
proc sort data=revspans; by descending rowid;                                   
                                                                                
data rowspans;                                                                  
  set revspans;                                                                 
  array coltxt{99} $99 coltxt1-coltxt99;                                        
  array columns{200} $ x1-x200;                                                 
  array col{100} col1-col100;                                                   
  array rowspn{99} rowspn1-rowspn99;                                            
  retain rowspn1-rowspn99;                                                      
  do j = 1 to 99;                                                               
    if rowspn{j} eq . then rowspn{j} = 0;                                       
  end;                                                                          
  do j = 1 to maxcol;                                                           
    if coltxt{j} eq '' then do;                                                 
      rowspn{j} = rowspn{j} + 1;                                                
    end;                                                                        
    else do;                                                                    
      rowspn{j} = 0;                                                            
    end;                                                                        
    if rowspn{j} eq . then rowspn{j} = 0;                                       
  end;                                                                          
                                                                                
data rowsout;                                                                   
  set rowspans;                                                                 
  array coltxt{99} $99 coltxt1-coltxt99;                                        array tmptxt{99} $99 tmptxt1-tmptxt99;                                        
  array columns{200} $ x1-x200;                                                 
  array col{100} col1-col100;                                                   
  array rowspn{99} rowspn1-rowspn99;                                            
  do j = 1 to maxcol;                                                           
    temprow = lag(rowspn{j});                                                   
    if rowspn{j} lt temprow then rowspn{j} = temprow + 1;                       
  end;                                                                          
                                                                                
proc sort data=rowsout; by rowid;                                               
                                                                                
***********************************************************************         
* PROCESS title statements.                                           *         
***********************************************************************;        
data pretable(keep=line header);                                                
   retain test;                                                                 
   set raw;                                                                     
   if substr(line,1,1) eq '_' then test = 'nothead';                            
   if test eq 'nothead' then delete;                                            
   if line eq '' then delete;                                                   
   header = 1;                                                                  
                                                                                
***********************************************************************         
* PROCESS footnote statements.                                        *         
***********************************************************************;        
data postable(keep=line footer);                                                
   retain test;                                                                 
   set raw;                                                                     
   if substr(line,1,1) eq '_' and test eq 'nothead'                             
      then test = 'footer';                                                     
   if substr(line,1,1) eq '_' and test ne 'footer'                              
      then test = 'nothead';                                                    
   if substr(line,1,1) ne '' then                                               
   line = '<P>' || trim(line) || '<P>';                                         
   if test eq 'footer';                                                         
   if substr(line,4,1) eq '_' then delete;                                      
   if line eq '' then delete;                                                   
   footer = 1;                                                                  
                                                                                
***********************************************************************         
* Start processing the output for the HTML to the specified file.     *         
*                                                                     *         
* OUTFILE should be specified on the command line invoking this macro *         
*                                                                     *         
***********************************************************************;        
                                                                                
FILENAME OUTFILE "&OUTFILE" ;
                                                                                
***********************************************************************         
* First output header type information.                               *         
***********************************************************************;        
data _NULL_;                                                                    
  set pretable;                                                                 
  file OUTFILE linesize=80;                                                     
  by header;                                                                    
  if first.header then                                                          
  PUT '<HTML> <HEAD>'                                                           
    / '<TITLE>' line '</TITLE>'                                                 
    / "<!--Created by: &PROGNAME    -->"                                        
    / "<!--System: &sysdir  -->"                                                
    / "<!--Author: %trim(&author) -->"                                          
    / "<!--Contact: &contact  -->"                                              
    / '<BODY>'                                                                  
    / '<BASEFONT SIZE=2>'                                                       
    / '<CENTER>'                                                                
      ;                                                                         
   else PUT '<H2>' line '</H2>';                                                
   if last.header then                                                          
   PUT  / '</CENTER>'                                                           
        / '<TABLE BORDER=2 CELLSPACING=0>'                                      
          ;                                                                     
  return;                                                                       
                                                                                
***********************************************************************         
* Output the html table now.                                          *         
*                                                                     *         
* For the first line of the table, print the header information.      *         
* Set the count of header rows, HEADCNT, to be the rowspan of the     *         
* first cell, ie., the BOX from the PROC TABULATE.                    *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************;        
data _NULL_;                                                                    
  array coltxt{99} $80 coltxt1-coltxt99;                                        
  array colspn{99} colspn1-colspn99;                                            
  array rowspn{99} rowspn1-rowspn99;                                            
  retain headcnt;                                                               
  SET rowsout;                                                                  
  by rowid;                                                                     
  file OUTFILE MOD LINESIZE=80;                                                 
  if _N_ eq 1 then do;                                                       
    headcnt = rowspn{1};                                                        
  end;                                                                          
***********************************************************************         
* THE FOLLOWING SECTION DOES THE HEADER ROWS                          *         
***********************************************************************;        
  if headcnt gt 1 then do;                                                      
    PUT '<TR CLASS=HEADER ALIGN=CENTER>';                                       
    do j = 1 to maxcol;                                                         
      if coltxt{j} ne '' and colspn{j} ne . then do;                            
        if colspn{j} gt 1 then do;                                              
          if rowspn{j} gt 1 then                                                
PUT '<TH COLSPAN=' colspn{j} ' ROWSPAN=' rowspn{j} '>' coltxt{j} '</TH>';       
          else                                                                  
PUT '<TH COLSPAN=' colspn{j} '>' coltxt{j} '</TH>';                             
        end;                                                                    
        else do;                                                                
          if rowspn{j} gt 1 then                                                
PUT '<TH ROWSPAN=' rowspn{j} '>' coltxt{j} '</TH>';                             
          else                                                                  
PUT '<TH>' coltxt{j} '</TH>';                                                   
        end;                                                                    
      end;                                                                      
    end;                                                                        
    PUT '</TR>';                                                                
    headcnt = headcnt - 1;                                                      
  end;                                                                          
***********************************************************************         
* THIS CODE PUTS OUT THE BODY ROWS OF THE TABLE                       *         
*    If a cell starts alpha, left align, otherwise right align.       *         
*    Default to right align, cause most will probably be numbers.     *         
***********************************************************************;        
  else do;                                                                      
    PUT '<TR CLASS=BODY ALIGN=RIGHT>';                                        
    do j = 1 to maxcol;                                                         
      if coltxt{j} ne '' and colspn{j} ne . then do;                            
        if colspn{j} gt 1 then do;                                              
          if rowspn{j} gt 1 then                                                
PUT '<TD COLSPAN=' colspn{j} ' ROWSPAN=' rowspn{j} '>' coltxt{j} '</TD>';       
          else                                                                  
PUT '<TD COLSPAN=' colspn{j} '>' coltxt{j} '</TD>';                             
        end;                                                                    
        else do;                                                                
          if rowspn{j} gt 1 then                                                
            if trim(left(coltxt{j})) ne '.' then
PUT '<TD ROWSPAN=' rowspn{j} '>' coltxt{j} '</TD>';                             
            else  PUT '<TD ROWSPAN=' rowspn{j} '><BR></TD>';                    
          else                                                                  
            if trim(left(coltxt{j})) ne '.' then
PUT '<TD>' coltxt{j} '</TD>';                                                   
            else PUT '<TD><BR></TD>';                                           
        end;                                                                    
      end;                                                                      
    end;                                                                        
    PUT '</TR>';                                                                
  end;                                                                          
return;                                                                         
                                                                                
***********************************************************************         
* End the Table since couldnt figure out why rowid didnt work above.  *         
***********************************************************************;        
data _NULL_;                                                                    
  set postable;                                                                 
  file OUTFILE MOD LINESIZE=80;                                                 
  by footer;                                                                    
  if first.footer then PUT '</TABLE>';                                          
  PUT line;                                                                     
  if last.footer then PUT  / '</BODY> </HTML>';                                 
                                                                                                                                        
%mend; * 2th;                                                                   
