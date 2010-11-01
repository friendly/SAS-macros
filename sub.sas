%*-- sub.sas -------------------------------------------------------*;
%*--                                                              --*;
%*-- macros simplify doing (recursive) subroutines in a data step --*;
%*--                                                              --*;
%*-- version 0.17 by chang y. chung on 2004-07-29                 --*;
%*--                                                              --*;
%*-- These macros are continuously improved. They are offered with--*;
%*-- no guarantees what so ever. Use at your own risk. Comments   --*;
%*-- are welcome. The latest version will be available for        --*;
%*-- download at changchung.com.                                  --*;


%*-- utilities --*;
%macro iif(cond, true, false);
  %if %unquote(&cond.) %then %do;%unquote(&true.)%end; 
  %else %do;%unquote(&false.)%end;
%mend;
%macro increase(mvar, by=1); 
  %let &mvar. = %eval(&&&mvar. + (&by.));
%mend;
%macro decrease(mvar, by=1);
  %increase(&mvar., by=-&by.)
%mend;
%macro globalLet(mvar, value);
  %global &mvar.;
  %let &mvar.=&value.;
%mend;
%macro uniqueName(length=8, seed=0);
  %*;_%sysfunc(ceil(1e%eval(&length.-1)*%sysfunc(ranuni(&seed.))))
%mend;


%*-- NameTypeLen                                                 --*;
%*-- a numeric var should be name only:                          --*;
%*-- a char var should be followed by a dollar sign and length   --*;
%macro ntl_name(nameTypeLen);
  %*;%scan(&nameTypeLen., 1, $)
%mend;
%macro ntl_type(nameTypeLen, character=$, numeric=%str());
  %*;%iif(%nrstr(%index(&nameTypeLen.,$)), &character., &numeric.)
%mend;
%macro ntl_len(nameTypeLen, numeric=%str()); 
  %*;%iif(%nrstr(%ntl_type(&nameTypeLen., character=$)=$)
        , %nrstr(%scan(&nameTypeLen., 2, $))
        , &numeric.)
%mend;   
%macro ntl_missing(nameTypeLen);
  %*;%iif(%nrstr(%ntl_type(&nameTypeLen.,character=$)=$),%str(" "),.)
%mend;
  

%*-- stack --*;
%macro stack_init(nameTypeLen, max=1000);
  %local name dollar len;
  %let name   =%ntl_name(&nameTypeLen.);
  %let dollar =%ntl_type(&nameTypeLen.);
  %let len    =%ntl_len (&nameTypeLen.);
  array &name._stack [0:&max.] &dollar.&len. _temporary_;
  &name._stack[0]  = %ntl_missing(&nameTypeLen.); 
  retain &name._stack_top 0;
%mend;
%macro stack_push(name, item);
  do;
    &name._stack_top + 1;
    if &name._stack_top > hbound(&name._stack) then do;
      put "ERROR: stack &name. over-flow.";
      stop;
    end;
    &name._stack[&name._stack_top]=&item.;
  end;
%mend;
%macro stack_pop(name);
  %*;&name._stack[&name._stack_top];
  do;
    &name._stack_top + (-1);
    if &name._stack_top < 0 then do;
      &name._stack_top = 0;
      put "WARNING: stack &name. is empty already.";
    end;
  end /* semicolon intentionally left out */
%mend;


%*-- forEach (limited version) by chang y chung on 2004-01-03    --*;
%*-- inspired by Peter Crawford^s, %mLoopsX (sas-l 2002-11-20)   --*;
%macro forEach(unit, in=, dlm=%str( ,), from=, to=, by=1, 
  invoke=%nrstr(%put ***&&&unit.***;));

  %local &unit. units idx start finish before after inf;
  %let inf    = 32767; 
  %let before = 0;
  %let after  = 0; 
  %let units  = item|number;
  %let unit   = %lowcase(&unit.);

%branch:  /* Richard DeVenezia^s "Dynamic Branching" */
  %if %index(|&units.|, |&unit.|) %then %goto &unit.;
  %else %do;
    %put ERROR: "&unit." is not supported; 
    %goto out;
  %end;

%item:
  %if &in.= %then %do;
    %put WARNING: empty list in IN=; %goto out;
  %end;
  %let start  = 1;
  %let finish = &inf.;
  %let before = %nrstr(
    %let &unit.  = %scan(&in., &idx., &dlm.);
    "&&&unit.." = "" /* true when %scan returns nothing */
  );
  %goto loop;

%number: 
  %if &from.= or &to.= %then %do;
    %put ERROR: needs a number in both FROM= and TO=; %goto out;
  %end;
  %let start  = &from.;
  %let finish = &to.;
  %let before = %nrstr(
    %let &unit. = &idx.;
    0               /* always false */
  );
  %goto loop;

%loop:
  %do idx = %unquote(&start.) %to %unquote(&finish.);
    %if  %unquote(&before.) %then %goto out;
    %*;%unquote(&invoke.)
    %if  %unquote(&after.) %then %goto out;
    %let idx = %eval(&idx. - 1 + (&by.));
 %end;

%out:
%mend  forEach;


%*-- subroutine macros by chang y chung on 2004-01-03            --*;
%macro sub_init(name, args, heap=1000, dlm=%str( ));
  %globalLet(&name._args_dlm, &dlm.)
  %forEach(item, in=&args., dlm=&dlm., invoke=%nrstr(
    %stack_init(&name._&item., max=&heap.)
    length %ntl_name(&item.) 
      %ntl_type(&item.)%ntl_len(&item.,numeric=8);
    %ntl_name(&item.) = %ntl_missing(&item.);
  ))
  %globalLet(&name._args, 
    %forEach(item, in=&args., dlm=&dlm., invoke=%nrstr(
    %*;%ntl_name(&item.)&dlm.
  )))
  %stack_init(&name._rp, max=&heap.)
  %globalLet(&name._rp, 0)            
  retain &name._rp 0;                      
%mend;
%macro sub_call(name, values, dlm=%str( ));
  %increase(&name._rp)
  %globalLet(&name._call&&&name._rp._values, %nrbquote(&values.))
  %globalLet(&name._call&&&name._rp._values_dlm, &dlm.)
  do;
    &name._rp  = &&&name._rp.;
    goto &name.;
    &name._rp&&&name._rp.:;
  end;
%mend; 
%macro sub_do(name);
  &name.:
  %stack_push(&name._rp , &name._rp )
  %forEach(item, in=&&&name._args, dlm=&&&name._args_dlm., invoke=%nrstr(
    %stack_push(&name._&item., &item.)
  ))
  goto &name._values;
  &name._do:;
%mend;
%macro sub_end(name);
  &name._rp = %stack_pop(&name._rp);
  %forEach(item, in=&&&name._args., invoke=%nrstr(
    &item. = %stack_pop(&name._&item.);
  ))
  select(&name._rp);
    %forEach(number, from=1, to=&&&name._rp., invoke=%nrstr(
      when (&number.) goto &name._rp&number.;
    )) 
  end;
  &name._values:;
  select (&name._rp);
    %forEach(number, from=1, to=&&&name._rp., invoke=%nrstr(
      when (&number.)
      do; 
        %forEach(item, in=&&&name._call&number._values., 
          dlm=&&&name._call&number._values_dlm., invoke=%nrstr(
          %scan(&&&name._args., &idx., &&&name._args_dlm.) = &item;
        ))
      end;
    ))
  end;
  goto &name._do;  
%mend;


%macro sub_test(what);

%let what=%upcase(&what.);
%if &what.= %then %let what=;

%*-- test1 -- calc factorial by multiplication                   --*;
%if &what.=_ALL_ or %index(&what.,1) %then %do;
data _null_;
  %sub_init(foo, m level)
  f = 1;
  %sub_call(foo, 12 1)
  put "12!=" f;
  stop;

  %sub_do(foo)
    put level=;
    if m>1 then do;
      f = m*f; 
      m + (-1);
      %sub_call(foo, m level+1)
    end;
  %sub_end(foo)
run;
/* on log
  12!=479001600
*/
%end;

%*-- test2 translated from macro permute                         --*;
%*-- by Dale McLerran (sas-l 2003-12-15)                         --*;
%if &what.=_ALL_ or %index(&what.,2) %then %do;
data _null_;
  %sub_init(permute, c$6 chars$6 word$6 len i)

  counter = 0;
  %sub_call(permute, "" "DGJLOU" "" 6 1)
  put counter=;
  stop;

  %sub_do(permute)
    if missing(chars) then len = 0;
    else len = length(chars);
    if len > 0 then do;
      i = 1;
      do while (i <= len); /* you cannot call a sub within a    */
        /* do i=## to ## since to-expression cannot be altered. */
        %sub_call(permute, %str(
          substr(chars,i,1)         #  /* c     */
          trimn(compress(chars, c)) #  /* chars */
          trimn(trimn(word) || c)   #  /* word  */
          len                       #  /* len   */
          i                         #  /* i     */
        ), dlm=#)
        i = i + 1;
      end;
     end; else do;
       counter ++ 1;
       if counter = 194 then put "The 194th word is " word;
    end;
  %sub_end(permute)
run;
%end;

%*-- test3 a silly one, involving two sub-routines               --*;
%if &what.=_ALL_ or %index(&what.,3) %then %do;
data _null_;
  %sub_init(addTwo,      a)
  %sub_init(subtractOne, b) /* you cannot use a again */

  x = 0;
  do while (x <= 10);
    put x=;
    %sub_call(addTwo, x)
    %sub_call(subtractOne, x);
  end;

  stop;

  %sub_do(addTwo)
    %sub_call(subtractOne, %str(x+3)) 
  %sub_end(addTwo)
  %sub_do(subtractOne)
    x = b - 1;    /* you cannot call addTwo here, since 
                    %sub_end(addTwo) would not know 
                    anything about the call. See test 7 in the
                    below for an workaround when two sub-routines
                    call each other
                  */
  %sub_end(subtractOne)
run;
%end;

%*-- test4. "it would be difficult to do this with IF-THEN"      --*;
%*-- Jack Hamilton (sas-l 2002-07-24)                            --*;
%if &what.=_ALL_ or %index(&what.,2) %then %do;
data one;
  do i = 1 to 10; output; end;
run;
data two;
  %sub_init(getIt)

  if not end then do;
    %sub_call(getIt)
    put "just got one obs. " i=;
  end; 

  put / "...doing something else..." /;
  
  if not end then do;
    %sub_call(getIt)
    put "got another obs.  " i=;
  end; 

  if end then stop; else return;

  %sub_do(getIt)
    set one end=end;
  %sub_end(getIt)
run;
%end;

%*-- test5. Benjamin Jr^s Observations 18 article                --*;
%*-- "pseudo-recursive sas macro"                                --*;
%*-- http://www.sas.com/service/library/periodicals/obs/         --*;
%*--   obswww18/index.html                                       --*;
%*-- a little bit modified so that input is a dataset, also      --*;
%*-- uses different variable names.                              --*;
%if &what.=_ALL_ or %index(&what.,5) %then %do;
data master(index=(var));
  array cards[13] $30 (
    "AA 6 BB CC DD EE FF GG"
    "BB 2 GG HH"
    "CC 3 EE FF HH"
    "DD 3 FF GG HH"
    "EE 0"
    "FF 1 MM"
    "GG 0"
    "HH 1 II"
    "II 1 JJ"
    "JJ 1 KK"
    "KK 1 LL"
    "LL 1 MM"
    "MM 0"
  ); 
  do _n_ = lbound(cards) to hbound(cards);
    var = scan(cards[_n_], 1);
    count = input(scan(cards[_n_], 2), best.);
    array d[1:6] $2 d1-d6;
    do i = 3 to 8;
      d[i-2] = scan(cards[_n_], i);
    end;  
    output;
    keep var count d1-d6;
  end;
run;

data varlist; /* this is the input dataset */
  do v = "BB", "EE", "FF"; output; end;
run;

%macro c2i(cc );index("ABCDEFGHIJKLM",substr(&cc.,1,1))%mend;

data dep_list; 

  /* init --------------------------------------------------- */
  %sub_init(depend, var$2 count d1$6 d2$6 d3$6 d4$6 d5$6 d6$6 i);
  %sub_init(writeModules);
  
  array modules[1:13] $2 _temporary_;
  array d[1:6]        $2 d1-d6;

  /* main --------------------------------------------------- */
  set varlist end=end;
  %sub_call(depend, v 0 "" "" "" "" "" "" 0)
  if end then do;
    put "Requires: " @;
    %sub_call(writeModules);
  end;
  return;
  
  /* subs --------------------------------------------------- */
  %sub_do(depend)
    modules[%c2i(v)] = v;
    %sub_call(writeModules)
    set master key=var/unique; /* aka get_mstr */
    i = 1;
    do while (i <= count); 
      modules[%c2i(d[i])]=d[i];
      %sub_call(depend, d[i] count d1 d2 d3 d4 d5 d6 i)
      i + 1;
    end;  
  %sub_end(depend)

  %sub_do(writeModules)
    do i = 1 to 13;
      if not missing(modules[i]) then put modules[i] $3. @;
    end;
    put;
  %sub_end(writeModules)
run;

/* we are looking for an output: 
   Requires: BB EE FF GG HH II JJ KK LL MM
*/
%end;

%*-- test6. Euclid Algorithm to calculated GCD                   --*;
%*-- translated from Paul Dorfman^ macro gcd (sas-l 1999-12-20)  --*;
%if &what.=_ALL_ or %index(&what.,6) %then %do;
  
data _null_;

  %sub_init(getGcd, gcd ref)
  
  x = 2**15;
  y = 2**12;
  z = 2**10;
  put x= y= z=;
  %sub_call(getGcd, x y) 
  gcd_xy = r; 
  put "GCD(x,y)=" gcd_xy;
  %sub_call(getGcd, gcd_xy z)
  gcd_xyz = r; 
  put "GCD(x,y,z)=" gcd_xyz;
  stop;

  %sub_do(getGcd)
    do while (ref > 0);
      res = mod(gcd, ref);
      gcd = ref;
      ref = res;
    end;
    r = gcd;
  %sub_end(getGcd)
run;
/* on log
x=32768 y=4096 z=1024
GCD(x,y)=4096
GCD(x,y,z)=1024
*/
%end;

%*-- test7. Geographical Adjacency problem                       --*;
%*-- Robert Stratton^s problem (sas-l 2003-08-05)                --*;
%*-- Richard DeVenezia^s solution (sas-l 2003-08-05)             --*;
%*-- NOTE: Dorfman fully endorses Richard^s original solution:   --*;
%*--   "Efficient, well structured, clear SAS code. Classic.     --*;
%*--    very aesthetic, too. (Dorfman. sas-l 2003-08-05)"        --*;

%if &what.=_ALL_ or %index(&what.,7) %then %do;
/* Stratton^s data */
data groups;
  cards = "1 A 1 B 1 C 2 D 3 A 3 F 4 H 4 I 4 J 4 K";
  keep set id;
  do i = 1 to 10;
    set = input(scan(cards, 2*(i-1) + 1),1.);
    id  = scan(cards, 2*i);
    output; 
  end;
run;
proc print data=groups(obs=10);
run; 

/* the solution translated with %sub_ macros */
proc sql;
  reset noprint;
  select count (distinct set) into :nSet from groups;
quit;

data _null_;

  /* init --------------------------------------------------- */
  %sub_init(putS)
  %sub_init(claimRow   , ri rj)
  %sub_init(claimColumn, ci cj)

  /* main --------------------------------------------------- */
  array S[&nSet.,26] _temporary_;
 
  do while (not eog);
    set groups end=eog;
    S[set, rank(id)-64] = -1; /* for ascii box only -- cyc */
  end;

  put "before: ";
  %sub_call(putS)
  ss = 0;
  do i = 1 to &nSet.;
    do j = 1 to 26;
      if S[i,j] = -1 then do;
        ss + 1;
        %sub_call(claimRow, i 0)
      end;
    end;
  end;
  put "after: ";
  %sub_call(putS)

  stop;

  /* subs --------------------------------------------------- */
  %sub_do(claimRow)
    rj = 1;
    do until (rj > 26);
      if S[ri,rj] = -1 then do;
        s[ri,rj] = ss;
        %sub_call(claimColumn, 0 rj); 
      end;  
      rj + 1;
    end;  
    if 0 then do; /* a hack by Dorfman. it is required since
                     %sub_end(claimRow) has to come after
                     the last call to it, which happens to be
                     the inside of the claimColumn
                  */
      %sub_do(claimColumn)
        ci = 1;
        do until (ci > &nSet.);
          if S[ci,cj] = -1 then do;
            S[ci,cj] = ss;
            %sub_call(claimRow, ci cj)
          end;
          ci + 1;
        end;  
      %sub_end(claimColumn)
    end;
  %sub_end(claimRow)

  %sub_do(putS)
    put +5 @;  /* column header -- cyc */
    do jj = 1 to 26;
      letter = byte(64 + jj); /* for ascii box only -- cyc */
      put letter $2. +1 @;
    end;
    put;       /* end column header */
    do ii = 1 to &nSet.;
      put ii 2. '. ' @;
      do jj = 1 to 26;
        put S[ii,jj] 2. + 1 @;
      end;
      put;
    end;
    put;
  %sub_end(putS)
run;
/* on log
before:
     A  B  C  D  E  F  G  H  I  J  K  L  M  N  
 1. -1 -1 -1  .  .  .  .  .  .  .  .  .  .  .  
 2.  .  .  . -1  .  .  .  .  .  .  .  .  .  .  
 3. -1  .  .  .  . -1  .  .  .  .  .  .  .  .  
 4.  .  .  .  .  .  .  . -1 -1 -1 -1  .  .  .  

after:
     A  B  C  D  E  F  G  H  I  J  K  L  M  N  
 1.  1  1  1  .  .  .  .  .  .  .  .  .  .  .  
 2.  .  .  .  2  .  .  .  .  .  .  .  .  .  .  
 3.  1  .  .  .  .  1  .  .  .  .  .  .  .  .  
 4.  .  .  .  .  .  .  .  3  3  3  3  .  .  .  
*/
%end;

%*-- test8. "Summing combinations of n objects taken m by m,     --*;
%*--   where for each combination we consider the product of     --*;
%*--   elements"                                                 --*;
%*--                                                             --*;
%*-- Translated from Ian Whitlock^s macro %B (sas-l 1998-07-09), --*;
%*-- where Ian says:                                             --*;
%*--                                                             --*;
%*--  "Now Fabrizio actually wanted the call                   " --*;
%*--  "                                                        " --*;
%*--  "   %put %b(2,60)                                        " --*;
%*--  "                                                        " --*;
%*--  "When I tried this my system ran out of resources.       " --*;
%*--  "So yes you can make recursive macro calls, but one      " --*;
%*--  "should remember that even a rather innocent looking     " --*;
%*--  "recursion can demand an awful lot of resources from     " --*;
%*--  "the system.                                             " --*;
%*--  "[...]                                                   " --*;
%*--  "I once was almost thrown out of a COBOL training program" --*;
%*--  "on my first programming job when I innocently asked if  " --*;
%*--  "COBOL could make recursive calls to perform.            " --*;
%*--                                                             --*;
%*--   [a semicolon ending %put statement in the origial posting --*;
%*--    is not shown in the above quote]                         --*;
%if &what.=_ALL_ or %index(&what.,8) %then %do;
data _null_;
  %sub_init(b, m n temp) /* m and n are parameters temp is local */

  x = .;
  %sub_call(b, 2 40 .)
  put "B(2,40)=" x;
 
  x = .;
  %sub_call(b, 2 60 .)
  put "B(2,60)=" x;
  stop;

  %sub_do(b)
    if      m = 0 and n >= 0 then x = 1;
    else if m > n            then x = 0; 
    else do;
      %sub_call(b, m   n-1 .)
      temp = x;
      %sub_call(b, m-1 n-1 .)
      temp = temp + n * x;
      x    = temp;
    end;
    *put "returning b(" m "," n ") is " x;
  %sub_end(b) 
run;
/* on log
B(2,40)=325130
B(2,60)=1637545
NOTE: DATA statement used:
      real time           0.22 seconds
      cpu time            0.21 seconds
*/
%end;
%mend sub_test;
options mprint nosymbolgen;
%*sub_test(_ALL_); /* uncomment this line to run the tests */

%*-- end ----------------------------------------------------------*;
