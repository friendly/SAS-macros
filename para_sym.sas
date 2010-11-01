/*-----
 * group: Miscellaneous
 * purpose: Generate symbol statements for a parallel plot
 * notes: Sample use to follow someday
 */

* Richard A. DeVenezia, 93/06/30 ;
*
* macro to set-up symbols used in parallel coordinate plotting method;
* a plot y*x=z, where z is ranked into N groups requires N symbols;
*;

data _null_;
  infile cards eof=end;
  length sym $200;
  input value $char8. +1 font $char8. +1 color $char8.;
  x=_n_;
  y=_n_;
  z=_n_;
  output;
  x+1;
  output;

  if value='"' then value="'"||'"'||"'";

  sym="symbol"||trim(left(put(_n_,2.)))||" value="||value;
  if font ^= "" then sym=trim(sym)||' font='||font;
  if color^= "" then sym=trim(sym)||' color='||color;
                else sym=trim(sym)||' color=black';

* put sym=;

  call symput ('symbol'||left(put(_n_,2.)), trim(sym));
  return;

End:
  call symput ('Nsymbol', left(put(_n_-1,8.)));
  delete;

*2345678 01234567 90123456 89012345 78901234;
  cards;
dot               black    filled-circle
U        marker   blue     filled-square
P        marker   orange   filled-diamond
C        marker   brown    filled-triangle up
V        marker   red      filled-star
M        marker   purple   filled-club
N        marker   black    filled-heart
I        music    blue     flat
O        marker   orange   filled-spade
D        marker   brown    filled-triangle down
circle            black    empty-circle
square            blue     empty-square
diamond           orange   empty-diamond
triangle          brown    empty-triangle up
=                 red      empty-star
%                 purple   empty-club
#                          empty-heart
"                          empty-spade
A        marker            filled-triangle left
B        marker            filled-triangle right
G        music             sharp
H        music             natural
  ;
run;

* call this macro to execute required symbol statements;
%macro symbols (groups);
  %if (&groups > &Nsymbol) %then %let groups=&Nsymbol;
  goptions reset=symbol;
  %do i=1 %to &groups;
    &&symbol&i;
  %end;
%mend;
