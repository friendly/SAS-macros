%*=========================================================================

 SAS2PDF v0.02

   A SAS macro for converting text files to PDF documents

 Copyright (C) 2000  Industrial Softworks

 This program is free software, you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation, either version 2
 of the License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY, without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

 For information, contact:
   Industrial Softworks
   2181 Stonebridge Drive
   Ann Arbor, MI 48108

   or email:  sas2pdf@industrialsoftworks.com
 
 You should have received a copy of the GNU General Public License
 along with this program if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

---------------------------------------------------------------------------
v0.02 - P Wehr
   Brown Paper Bag bug fix--removed substring that trimmed off the first
character for all OSs except MVS, and it doesnt even print the first page
on most platforms...

===========================================================================;	
%macro sas2pdf(in=,infile=,out=,file=,ls=132,ps=60,maxlines=100,maxpages=10000,
               charset=,crlf=);
  run;  * force previous step to finish;

  %local lb rb pages i;
  %let charset=%upcase(&charset);
  %if %bquote(&in)=%bquote() %then %let in=&infile;
  %if %bquote(&out)=%bquote() %then %let out=&file;
  %if &sysscp=WIN %then %do;
    %if &crlf= %then %let crlf=2;
    %if &charset= %then %let charset=ASCII;
    %let pagechar='0C'x;
  %end;
  %else %if &sysscp=OS %then %do;
    %if &crlf= %then %let crlf=2;
    %if &charset= %then %let charset=OS;
    %let pagechar='1';
  %end;
  %else %do;
    %if &crlf= %then %let crlf=1;
    %if &charset= %then %let charset=ASCII;
    %let pagechar='0C'x;
  %end;

  data _null_;
    length char $ 1;
    infile &in end=eof;
    input char $1.;
    if char=&pagechar or _n_=1 then pages+1;
    if eof then call symput('pages',compress(put(pages,8.)));
    %if &charset=EBCDIC %then %do;
      call symput('lb','ad'x);
      call symput('rb','bd'x);
    %end;
    %else %if &charset=ANSI %then %do;
      call symput('lb','41'x);
      call symput('rb','42'x);
    %end;
    %else %do;
      call symput('lb','5b'x);
      call symput('rb','5d'x);
    %end;
  run;

  data _null_;
    file &out noprint;
    infile &in length=lg end=eof;
    length string instring setup follow $ 200;
    retain bytecnt obj lines 0;
    array objbyte {&maxpages} 8 _temporary_;
    array line {&maxlines} $ 200 _temporary_;
    array header {%eval(&pages+8)} $ 80 _temporary_ (
      '%PDF-1.0'
      '1 0 obj << /Type /Catalog /Pages 5 0 R /Outlines 2 0 R >> endobj'
      '2 0 obj << /Type /Outlines /Count 0 >> endobj'
      '3 0 obj << /Type /Font /Subtype /Type1 /Name /F1'
      '/BaseFont /Courier /Encoding /StandardEncoding >> endobj'
      "4 0 obj &lb /PDF /Text &rb endobj"
      "5 0 obj << /Type /Pages /Count &pages /Kids &lb"
      %do i=1 %to &pages;
        "%eval(&i*2+4) 0 R"
      %end;
      "&rb >> endobj"
      ) ;
    if _n_=1 then do;
      do i=1 to %eval(&pages+8);
        string=header{i};
        link output;
      end;
    end;
    input instring $varying. lg;
    if substr(instring,1,1)=&pagechar or _n_=1 then do;
      if obj>5 then link endpage;
      string=compress(put(obj+1,8.))!!' 0 obj << /Type /Page '!!
             '/Parent 5 0 R /Resources';
      link output;
      string='<< /Font << /F1 3 0 R >> /ProcSet 4 0 R >>';
      link output;
      string="/MediaBox &lb 0 0 792 612 &rb /Contents " !!
             compress(put(obj+1,8.)) !! ' 0 R >> endobj';
      link output;
      lines=0;
    end;
    lines+1;
    instring=translate(instring,'FEFF'x,'()');
    do while(index(instring,'FE'x) ne 0);
      if index(instring,'FE'x)=1 then
        instring='\(' !! substr(instring!!' ',2);
      else instring=
        substr(instring,1,index(instring,'FE'x)-1)
        !! '\(' !! substr(instring,index(instring,'FE'x)+1);
    end;
    do while(index(instring,'FF'x) ne 0);
      if index(instring,'FF'x)=1 then
        instring='\)' !! substr(instring!!' ',2);
      else instring=
        substr(instring,1,index(instring,'FF'x)-1)
        !! '\)' !! substr(instring,index(instring,'FF'x)+1);
    end;
    %if &sysscp=OS %then 
      line{lines}='('!!trim(substr(instring,2))!!")'";
    %else
      line{lines}='('!!trim(instring)!!")'";
    ;
    pagesize+length(line{lines})+&crlf;
    if eof then do;
      link endpage;
      strtxref=bytecnt;
      string='xref'; link output;
      string='0 '!!compress(put(obj+1,8.)); link output;
      string='0000000000 65535 f'; link output;
      do i=1 to obj;
        string=put(objbyte{i},z10.)!!' 00000 n';
        link output;
      end;
      string='trailer << /size '!!compress(put(obj+1,8.))!!
             ' /Root 1 0 R >>';
      link output;
      string=compress(put(strtxref,12.)); link output;
      string='%%EOF'; link output;
    end;
  return;

  endpage:
    setup='BT /F1 8 Tf '!!compress(put(6.5*72/&ps,8.))!!
           ' TL 90 522 Td';
    follow='ET';
    string=compress(put(obj+1,8.)) !! ' 0 obj << /Length '!!
      compress(put(pagesize+length(setup)+length(follow)+&crlf,8.))!!
           ' >>';
    link output;
    string='stream';
    link output;
    string=setup;
    link output;
    do i=1 to lines;
      string=line{i};
      link output;
    end;
    string=follow;
    link output;
    string='endstream';
    link output;
    string='endobj';
    link output;
    pagesize=0;
  return;

  output:
    lg=length(string);
    if index(' '!!string!!' ',' obj ') then do;
      obj+1;
      objbyte{obj}=bytecnt;
    end;
    bytecnt+lg+&crlf;
    put string $varying. lg;
  return;

  run;
%mend;
