%************************** xvlist ******************************;
%* process a variable list, allowing abbreviated lists, and return
   information about each variable and/or the entire list.

 * Create a data set named according to the value of the _name
   argument prefixed by an underscore that contains information
   about the variables from PROC CONTENTS.

 * Optionally create macros containing:
      name
      type (1=numeric 2=char)
      format (optionally specify default format and length)
   for each variable.

 * Optionally remove variables from the list that have been
   specified in one or more other variable lists.

 * Optionally return the number of variables.

 * Optionally return the list type (0=empty 1=numeric 2=char 3=mixed)
   and/or validate the list type and issue an error for an invalid
   list type.

 * Optionally replace abbreviated variable lists by an expanded list
   containing each individual variable name.

 * BY lists are allowed if the list name _list=by.
   ;

%macro xvlist(data=,_list=,_name=,_type=,_fmt=,dnf=BEST,dnl=8,dcf=CHAR,
              _count=,_ltype=,valid=,remove=,replace=0,format=);

%global _xdebug_;
%global _maxobs_;
%let _maxobs_=20000000;
%if &_xdebug_ %then %do;

   %put xvlist: data=&data!;     %* data set name;

   %put xvlist: _list=&_list!;   %* name of variable list. The list is
                                    updated if replace=1 is specified;
   %put xvlist: list value=&&&_list!;

   %put xvlist: _name=&_name!;   %* prefix for returned macro vars with
                                    numerical suffixes for individual
                                    variable names. if blank, no macro
                                    vars for names are returned.

   %put xvlist: _type=&_type!;   %* prefix for returned macro vars with
                                    numerical suffixes for individual
                                    variable types. if blank, no macro
                                    vars for types are returned;

   %put xvlist: _fmt=&_fmt!;     %* prefix for returned macro vars with
                                    numerical suffixes for individual
                                    variable formats. if blank, no macro
                                    vars for formats are returned;

   %put xvlist: dnf=&dnf!;       %* default numeric format;

   %put xvlist: dnl=&dnl!;       %* default numeric format length;

   %put xvlist: dcf=&dcf!;       %* default character format;
                                 %* to detect variables for which no
                                    format has been specified, use:
                                       dnf=,dnl=0,dcf=
                                    and check for a format of 0.;

   %put xvlist: _count=&_count!; %* returned: number of variables.
                                    if blank, no count is returned;

   %put xvlist: _ltype=&_ltype!; %* returned: list type:
                                              0=empty list
                                              1=all numeric
                                              2=all character
                                              3=mixed
                                    if blank, no list type is returned;

   %put xvlist: valid=&valid!;   %* string of digits 0-3 specifying
                                    valid list types (_ltype). If the
                                    list type is not in this list, an
                                    error is issued. If blank, no
                                    validation is done, but some proc
                                    steps may issue warnings if the
                                    variable list is empty. Hence,
                                    valid= should always be used for a
                                    list that is allowed to be empty,
                                    such as a BY list, for which use
                                    valid=0123;

   %put xvlist: remove=&remove!; %* list of _names of other variable
                                    lists to be removed from this list.
                                    These _name values must have been
                                    processed in previous calls to
                                    xvlist. the _name values should be
                                    separated by blanks. This argument
                                    may be blank if no variables need
                                    to be removed;

   %put xvlist: replace=&replace!; %* if replace=1, the value of _list
                                    is replaced by an expanded list
                                    containing individual variable
                                    names instead of abbreviated lists.
                                    For example, X1-X3 is replaced by
                                    X1 X2 X3. However, if you use this
                                    with a BY list, the DESCENDING and
                                    NOTSORTED keywords will be lost;

   %put xvlist: format=&format!; %* format list to be used in a FORMAT
                                    statement. If blank, then no FORMAT
                                    statement is used;

   %put ;

%end;

   %global &_count &_ltype;
   %local __count __j __token;

   %************ see if empty list is ok;
   %if %bquote(&&&_list)= %then %if %index(&valid,0) %then %do;
      %if &_count^= %then %let &_count=0;
      %goto exit;
   %end;

   %************ if the _list is to be replaced, _name must
      be a valid prefix;
   %if &replace %then %if %bquote(&_name)= %then %let _name=_;

   %************ if valid= is specified, have to get list type;
   %if %bquote(&valid)^= %then %if &_ltype= %then %let _ltype=_xtype;

   ************ make a small data set so summary wont take a lot of
     time --cant use obs= data set option on the real data set ******;
   %if %bquote(&data)= %then %goto exit;
         data _XTMP0;
            set %unquote(&data);
            %if %bquote(&format)^= %then %do;
               format %unquote(&format);
            %end;
            stop;
         run;
   %if &syserr>4 %then %goto fail;

   ************ use summary to reorder the variables;
         proc summary data=_XTMP0(firstobs=1 obs=1);
            output out=_XTMP1(drop=_TYPE_ _FREQ_);
   %if %qupcase(&_list)=BY %then %do;
            by %unquote(&&&_list);
   %end;
   %else %do;
            id %unquote(&&&_list);
   %end;
         run;
   %if &syserr>4 %then %do;
      %put ERROR: "%upcase(&_list)=&&&_list" is probably invalid.;
      %goto fail;
   %end;

   ************ make data set with variable names;
         proc contents data=_XTMP1(firstobs=1)
            %if &_fmt^= & &sysver>=6.08 %then fmtlen;
            noprint out=_&_name;
         run;
   %if &syserr>4 %then %goto fail;
   %if &_xdebug_>=2 %then %do;
         proc print data=_&_name(firstobs=1 obs=&_maxobs_); run;
   %end;

   ************ remove variables from other lists;
   %if &remove^= %then %do;
      %let __j=1;
      %let __token=%scan(&remove,&__j);
      %do %while(&__token^=);
         proc sql;
            delete from _&_name(firstobs=1 obs=&_maxobs_)
               where name in
                  (select name
                     from _&__token(firstobs=1 obs=&_maxobs_));
         run;
         %if &syserr>4 %then %goto fail;
         %let __j=%eval(&__j+1);
         %let __token=%scan(&remove,&__j);
      %end;
   %end;

   ************ order by position in data set, not alphabetically;
   %xnobs(_xvntmp,_&_name(firstobs=1 obs=&_maxobs_))
   %if &_xvntmp %then %do; %* avoid spurious warning from SORT;
         proc sort force
            data=_&_name(firstobs=1 obs=&_maxobs_);
            by npos; run;
      %if &syserr>4 %then %goto fail;
   %end;

   ************ find out how many variables and what types;
   %let __count=0;
   %if &_ltype^= %then %let &_ltype=0;
         data _null_;
   %if &_ltype^= %then %do;
            retain ltype 0;
   %end;
            set _&_name(firstobs=1 obs=&_maxobs_) nobs=count end=end;
            if _n_=1 then do;
   %if &_xdebug_ %then %do;
               put 'xvlist: count=' count;
   %end;
               call symput("__count",trim(left(put(count,5.))));
   %if &_ltype^= %then %do;
                  ltype=type;
   %end;
            end;
   %if &_ltype^= %then %do;
            else if ltype^=type then ltype=3;
            if end then call symput("&_ltype",left(put(ltype,5.)));
   %end;
   %else %do;
            stop;
   %end;
         run;
   %if &syserr>4 %then %goto fail;

   %if &_xdebug_ %then %do;
      %put xvlist: count=&__count!;
      %if &_ltype^= %then %put xvlist: ltype=&&&_ltype;
   %end;

   %************ validate list type;
   %if %bquote(&valid)^= %then %if %index(&valid,&&&_ltype)=0 %then %do;
      %let _xrc_=The %qupcase(&_list)= variable list must;
      %if &&&_ltype=0 %then %let _xrc_=%qcmpres(&_xrc_
         contain at least one variable);
      %else %if %verify(&valid,02)=0 %then %let _xrc_=%qcmpres(&_xrc_
         contain only character variables);
      %else %if %verify(&valid,01)=0 %then %let _xrc_=%qcmpres(&_xrc_
         contain only numeric variables);
      %else %if %verify(&valid,012)=0 %then %let _xrc_=%qcmpres(&_xrc_
         not contain both numeric and character variables);
      %else %let _xrc_=%qcmpres(&_xrc_ ???);
      %put ERROR: &_xrc_..;
      %goto finish;
   %end;

   %if &_count^= %then %let &_count=&__count;

   ************ make macro variables global before defining them;
   %if &_name^= %then %do __j=1 %to &__count;
      %global &_name&__j;
   %end;
   %if &_type^= %then %do __j=1 %to &__count;
      %global &_type&__j;
   %end;
   %if &_fmt^=  %then %do __j=1 %to &__count;
      %global &_fmt&__j;
    %end;

   ************ assign variable names and formats to macro variables;
   %if &_name^= | &_type ^= | &_fmt^= %then %do;
         data _null_;
            set _&_name(firstobs=1 obs=&_maxobs_);
      %if &_name^= %then %do;
            call symput("&_name"||left(put(_n_,5.)),trim(name));
      %end;
      %if &_type^= %then %do;
            call symput("&_type"||left(put(_n_,5.)),put(type,1.));
      %end;
      %if &_fmt^= %then %do;
            length fmt $20;
            if format=' ' then do;
               if type=1 then do;
                  if formatl=0 then format="&dnf";
               end;
               else format="&dcf";
            end;
            if formatl=0 then if format^=' ' then do;
               if type=1 then formatl=&dnl;
                         else formatl=length;
            end;
            fmt=trim(format)||
                trim(left(put(formatl,3.)))||'.';
            if type=1 then
               fmt=trim(fmt)||trim(left(put(formatd,3.)));
            call symput("&_fmt"||left(put(_n_,5.)),trim(fmt));
      %end;
         run;
      %if &syserr>4 %then %goto fail;
   %end;

   %************ replace _list with expanded list;
   %if &replace %then %do;
      %let &_list=%xconcat(&_name,&__count);
      %if &_xdebug_ %then %do;
         %put xvlist: replaced list=&&&_list;
      %end;
   %end;

   %goto finish;

%fail:
   %xerrset(Processing %qupcase(&_list) variable list failed)

%finish:
   %xdelete(_XTMP0 _XTMP1);

%exit:
%mend xvlist;


%************************** xdelete *****************************;
%* delete work data sets;
%* list      list of data sets to delete;

%macro xdelete(list);
   %if %bquote(&list)^= %then %do;
      %if &_xdebug_ %then %put XDELETE: &list;
      proc datasets nolist nowarn;
         delete %unquote(&list) / memtype=data;
      quit;
   %end;
%mend xdelete;

%************************** xnobs *****************************;
%* find number of observations in a data set;
%* _xn       name of macro variable to assign number of observations;
%* data      data set;

%macro xnobs(_xn,data);
   %if &_xdebug_ %then %put XNOBS: _xn=&_xn data=&data;
   %global &_xn;
   %let &_xn=0;
   data _null_;
      if 0 then set %unquote(&data) nobs=_xn;
      call symput("&_xn",trim(left(put(_xn,12.))));
      stop;
   run;
   %if &syserr>4 %then %xerrset(DATA step to find number of
      observations in data set %qupcase(&data) failed);
   %else %if &_xdebug_ %then %put XNOBS: &_xn=&&&_xn;
%mend xnobs;

%************************** xerrset ******************************;
%* set error return code and print message;

%macro xerrset(msg);
   %let _xrc_=%qcmpres(&msg);
   %put ERROR: &_xrc_..;
%mend;

%************************** xconcat ******************************;
%*      returns concatenation of &n macro variables named &_name.1 to
        &&_name&n with intervening blanks. This is a trivial thing to
        do, but if you do it the obvious way with a %LET statement in
        a loop, performance is terrible;
%macro xconcat(_cat,n);
   %local __i;
   %do __i=1 %to &n; &&&_cat&__i %end;
%mend xconcat;


