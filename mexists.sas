/*
Determine if a macro variable exists

In v9.1+ this can be replaced by %symexist(mvar);

*/

%macro mexists(mvar);
    %global m_exists;

    %* load macro var m_exists with 1 or 0 ;
    proc sql noprint;
        select count(*) into :m_exists
        from sashelp.vmacro
        where upcase(name) eq "%upcase(&mvar)";
    quit;

    %* eval() used to get rid of spaces ;
    %let m_exists=%eval(&m_exists);
%mend;

/*
%let aaa=hello;

%mexists(aaa);
%put m_exists is >&m_exists<;


%mexists(bbb);
%put m_exists is >&m_exists<;



The &m_exists can then be tested for 1 or 0
in macro code or in following datastep code.
*/

