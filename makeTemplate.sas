/*
"Arto Raiskio" <arto-nospam@raiskio.com> wrote in message
news:buc6qn$ft0a2$1@ID-102906.news.uni-berlin.de...

>> is there a simple way to produce a side-by-side as opposed to stacked (one
>> on top, other on bottom) when doing a proc gchart-> proc greplay?
>>
>> pseudocode:
>>
>> %let path = <webserver path>;
>> ods html path = &path file="serverfile.html";
>> goptions reset=all dev=gif;
>> proc gchart gout=plots;
>> hbar3d h1 / discrete legend name="v1";
>> hbar3d h2 / discrete legend name="v2";
>> run;
>> quit;
>> proc greplay igout=plots nofs;
>>  tc sashelp.templt;
>>  template l2r2;
>>  tplay 1: v1 2: v2;
>> quit;
>>


One often finds the need to layout several plots in a grid.
The following macro %makeTemplate, allows you to dynamically create a
template to do so.
It allows you to specify the 'ordering' of the panels
Ordering is L1L2 where L1 and L2 are looping specifiers of form LR RL TB BT
meaning left-to-right, right-to-left, top-to-bottom and bottom-to-top.  Thus
LRTB means panels are layed out like words on a western page.

The code includes eight replays to demonstrate all eight possible orderings.
In your case you might want to do
%makeTemplate (name=art, across=2, down=1);
template art;
treplay 1:v1 2:v2;
*/



%macro makeTemplate (

  name     = template
, across   = 1
, down     = 1
, hGap     = 1
, vGap     = 1
, gapT     = 1
, gapB     = 1
, gapL     = 1
, gapR     = 1
, ordering = LRTB
, borderColor = gray

);

/*
 * ordering
 * LRTB
 *  1  2  3  4
 *  5  6  7  8
 *  9 10 11 12
 *
 * LRBT
 *  9 10 11 12
 *  5  6  7  8
 *  1  2  3  4
 *
 * RLTB
 *  4  3  2  1
 *  8  7  6  5
 * 12 11 10  9
 *
 * RLBT
 * 12 11 10  9
 *  8  7  6  5
 *  4  3  2  1
 *
 * TBLR
 *  1  4  7 10
 *  2  5  8 11
 *  3  6  9 12
 *
 * TBRL
 * 10  7  4  1
 * 11  8  5  2
 * 12  9  6  3
 *
 * BTLR
 *  3  6  9 12
 *  2  5  8 11
 *  1  4  7 10
 *
 * BTRL
 * 12  9  6  3
 * 11  8  5  2
 * 10  7  4  1
 *
 */


  %local this;

  %let this = makeTemplate;

  %if (%index(|LRTB|
              |LRBT|
              |RLTB|
              |RLBT|
              |TBLR|
              |TBRL|
              |BTLR|
              |BTRL|, |&ordering.|) = 0) %then %do;
    %put ERROR: &this: Ordering=&ordering is out of order;
    %goto EndMacro;
  %end;

  %let ncol = &across;
  %let nrow = &down;

  %let rowfuncLRTB = %nrstr (%eval(                      &count/&ncol));
  %let colfuncLRTB = %nrstr (              %sysfunc (mod(&count,&ncol)));

  %let rowfuncLRBT = %nrstr (%eval(&nrow-1-              &count/&ncol));
  %let colfuncLRBT = %nrstr (              %sysfunc (mod(&count,&ncol)));

  %let rowfuncRLTB = %nrstr (%eval(                      &count/&ncol));
  %let colfuncRLTB = %nrstr (%eval(&ncol-1-%sysfunc (mod(&count,&ncol))));

  %let rowfuncRLBT = %nrstr (%eval(&nrow-1-              &count/&ncol));
  %let colfuncRLBT = %nrstr (%eval(&ncol-1-%sysfunc (mod(&count,&ncol))));

  %let rowfuncTBLR = %nrstr (              %sysfunc (mod(&count,&nrow)));
  %let colfuncTBLR = %nrstr (%eval(                      &count/&nrow));

  %let rowfuncTBRL = %nrstr (              %sysfunc (mod(&count,&nrow)));
  %let colfuncTBRL = %nrstr (%eval(&ncol-1-              &count/&nrow));

  %let rowfuncBTLR = %nrstr (%eval(&nrow-1-%sysfunc (mod(&count,&nrow))));
  %let colfuncBTLR = %nrstr (%eval(                      &count/&nrow));

  %let rowfuncBTRL = %nrstr (%eval(&nrow-1-%sysfunc (mod(&count,&nrow))));
  %let colfuncBTRL = %nrstr (%eval(&ncol-1-              &count/&nrow));

  %let npanel = %eval (&ncol * &nrow);

  %let rowfunc = &&rowfunc&ordering;
  %let colfunc = &&colfunc&ordering;

  %put %qsysfunc(compress(&rowfunc));
  %put %qsysfunc(compress(&colfunc));

  %let xpct = %sysevalf ((100-&gapL-&gapR-&hGap*(&ncol-1)) / &ncol);
  %let ypct = %sysevalf ((100-&gapB-&gapT-&vGap*(&nrow-1)) / &nrow);

  %put xpct = &xpct;
  %put ypct = &ypct;


  tdef &name des="&across across &down down, panel order &ordering"


  %do count = 0 %to %eval(&npanel-1);
    %let col = %unquote(&colfunc);
    %let row = %unquote(&rowfunc);

    %let x = &col;
    %let y = %eval(&nrow-1-&row);

    %put count=&count row=&row col=&col x=&x y=&y;

    %let panel = %eval (&count+1);

    %let lowerX = %sysevalf ( &gapL + &x*(&xpct+&hgap) );
    %let lowerY = %sysevalf ( &gapB + &y*(&ypct+&hgap) );
    %let upperX = %sysevalf ( &lowerX + &xpct );
    %let upperY = %sysevalf ( &lowerY + &ypct );

    &panel
    / llx=&lowerX lly=&lowerY
      ulx=&lowerX uly=&upperY
      urx=&upperX ury=&upperY
      lrx=&upperX lry=&lowerY
      color=&borderColor

  %end;

  0 / DEF

  ;

%EndMacro:

%mend;


data foo;
  do x = 0 to 10 by 0.1;
    y1 = x;
    y2 = x**2;
    y3 = x**3;
    y4 = x**x;
    y5 = x**0.5;
    y6 = x**(1/x);
    output;
  end;
run;

symbol v=point i=join;

/*
goptions device=win target=pdf nodisplay;
goptions goutmode = replace;
*/

title;
footnote;

%macro slide (text);
  proc gslide gout=work.slides name="&text";
  goptions reset=note;
  note h=25pct f='Arial' "&text";
  run;
%mend;

%macro slides (n=12);
  %do slide = 1 %to &n;
  proc gslide gout=work.slides name="slide&slide";
  goptions reset=note;
  note move=(45pct,45pct) h=25pct f='Arial' "&slide";
  run;
  goptions goutmode=append;
  %end;
  %slide (TBLR)
  %slide (TBRL)
  %slide (BTLR)
  %slide (BTRL)
  %slide (LRTB)
  %slide (LRBT)
  %slide (RLTB)
  %slide (RLBT)
  quit;
%mend;
%slides;

options mprint;

goptions display goutmode=replace;

proc greplay nofs igout=work.slides tc=work.templates gout=work.test;
  %makeTemplate (name=LRTB, across=4, down=3, ordering=LRTB)
  %makeTemplate (name=LRBT, across=4, down=3, ordering=LRBT)
  %makeTemplate (name=RLTB, across=4, down=3, ordering=RLTB)
  %makeTemplate (name=RLBT, across=4, down=3, ordering=RLBT)
  %makeTemplate (name=TBLR, across=4, down=3, ordering=TBLR)
  %makeTemplate (name=TBRL, across=4, down=3, ordering=TBRL)
  %makeTemplate (name=BTLR, across=4, down=3, ordering=BTLR)
  %makeTemplate (name=BTRL, across=4, down=3, ordering=BTRL)
  ;
  template LRTB; treplay 1:1 2:2 3:3 4:4 5:5 6:6 7:7 8:8 9:9 10:10 11:11
12:12 0:LRTB;
  template LRBT; treplay 1:1 2:2 3:3 4:4 5:5 6:6 7:7 8:8 9:9 10:10 11:11
12:12 0:LRBT;
  template RLTB; treplay 1:1 2:2 3:3 4:4 5:5 6:6 7:7 8:8 9:9 10:10 11:11
12:12 0:RLTB;
  template RLBT; treplay 1:1 2:2 3:3 4:4 5:5 6:6 7:7 8:8 9:9 10:10 11:11
12:12 0:RLBT;
  template TBLR; treplay 1:1 2:2 3:3 4:4 5:5 6:6 7:7 8:8 9:9 10:10 11:11
12:12 0:TBLR;
  template TBRL; treplay 1:1 2:2 3:3 4:4 5:5 6:6 7:7 8:8 9:9 10:10 11:11
12:12 0:TBRL;
  template BTLR; treplay 1:1 2:2 3:3 4:4 5:5 6:6 7:7 8:8 9:9 10:10 11:11
12:12 0:BTLR;
  template BTRL; treplay 1:1 2:2 3:3 4:4 5:5 6:6 7:7 8:8 9:9 10:10 11:11
12:12 0:BTRL;
run;
quit;

/*
-- Richard A. DeVenezia http://www.devenezia.com/downloads/sas/macros/ 
*/
