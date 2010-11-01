/*
Some while ago I discovered that when you let the ODS create a .html file,
and name it with an .xls extension, then Excel will automatically open this
file and convert it to a 'real' sheet.

This is quite useful to export SAS data to Excel.

Unfortunately the ODS creates very inefficient html code (at least in v8.1
that I'm using), so the export/import of large datasets runs for hours.
Because of this I wrote a little macro that creates plain html without
formatting tags so that the file is much (say 5-10 times) smaller.
Also, the file is automatically named with an xls dimension so it looks like
a real sheet is created ;-)

You find the code of this macro below to copy/paste into SAS.
It is called like: %ds2xls (data=sashelp.class, file=c:\qqqq\class.xls).
I tested it in 8.1 but I think it works in other versions, including v6 as
well...

You are free to use the macro. I would appreciate a short email if you like
it though.

Victor Bos



%*****

  DS2XLS.SAS

  Macro to convert a dataset to an Excel .xls file.
  This is done by creating from the dataset a simple
  html file, but with an xls extension. Excel will
  automatically open this file, and convert it to
  a real sheet.

  Parameters:
     data -    name of the dataset.
     file -    name of the xls file that is created.
               if not present, the macro will add the
               .xls extension.

  Notes:
     - the macro will write a header with variable-labels
       if these are present, or variable-names if not.
     - the macro will use existing formats to write the
       values. This might cause problems in Excel, depending
       on language-settings. If no format is defined
       for a numeric variable, it will default to best.

  Example call:
    %ds2xls (data=sashelp.class, file=c:\qqqq\class.xls) ;


  Author:      Victor Bos
  Email:       news@victorbos.com
  Website:     www.victorbos.com


*****;
*/

%macro ds2xls (data=, file=ds2xls.xls) ;
  %*      Force a .xls extension to the output file ;
  %if %upcase (%scan (%sysfunc (reverse (&file)), 1, .)) ^= SLX %then
    %let file = &file..xls ;

  %let id = %sysfunc (open (&data)) ;
  %if ^&id %then %do ;
    ERROR: DS2XLS - Unable to open dataset &data. ;
    %goto endmac ;
    %end ;

  %*      Read variable names, labels, formats and types ;
  %let nvar = %sysfunc (attrn (&id, nvar)) ;
  %do i = 1 %to &nvar ;
    %let vn&i = %sysfunc (varname  (&id, &i)) ;
    %let vl&i = %sysfunc (varlabel (&id, &i)) ;
    %if "&&&vl&i" = "" %then
      %let vl&i = &&&vn&i ;
    %let vf&i = %sysfunc (varfmt   (&id, &i)) ;

    %let vt&i = %sysfunc (varlen   (&id, &i)) ;
    %let vy&i = %sysfunc (vartype  (&id, &i)) ;

    %end ;

  %let rc = %sysfunc (close (&id)) ;

  %*      Create the html file ;
  %put NOTE: DS2XLS - Creating file &file ;
  data _null_ ;
    set &data end=end;
    file "&file" recfm=n;

    %*  to put a numeric var as a character string ;
    length putnum $200 ;

    if _n_ = 1 then do;
      %*       Write the html header ;
      put '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 3.2//EN">' ;
      put '<html><head>' ;
      put "<meta name='Author' content='DS2XLS by Victor Bos -
www.victorbos.com'>";
      put "<title>Created with DS2XLS - &data</title></head>" ;
      put '<body>' ;

      %*       Write the column headers ;
      put '<table><tr>' ;
      %do i = 1 %to &nvar ;
        put "<td><b>&&&vl&i</b></td>" ;
        %end ;
      put '</tr></table>' ;

      %*       Table for data values ;
      put '<table>' ;
      end ;

    %*    Write the values ;
    put '<tr>' ;
    %do i = 1 %to &nvar ;
      put '<td>' ;
      %if &&&vy&i = C %then %do ;
        if &&&vn&i ^= '' then do ;
          l = length (trim (&&&vn&i)) ;
          put &&&vn&i $varying200. l ;
          end ;
        %end ;
      %else %do ;
        if &&&vn&i ^= . then do ;
          %if &&&vf&i ^= %str () %then
            putnum = put (&&&vn&i, &&&vf&i..) ;
          %else
            putnum = put (&&&vn&i, best.) ;
          ;
          put putnum @ ;
          end ;
        %end ;
      put '</td>' ;
      %end ;

    put '</tr>' ;

    if end then
      put '</table></body></html>' ;

  run ;
%endmac:
%mend ;
