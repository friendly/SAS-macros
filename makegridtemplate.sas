/* Richard A. DeVenezia
 * http://www.devenezia.com
 * 1/18/2003, posted to SAS-L
 *
 * Context:
 * This macro should only be called from within a GREPLAY Proc Step
 *
 * Purpose:
 * Define a template to display SAS/Graph content in a grid of
 * ordered panels.
 *
 * Notes:
 * If content seems squeezed or squashed when replayed through the
 * template, you need to ensure the aspect at creation time matches
 * the aspect of the panel at replay time
 */

/*-----
 * group: Data presentation
 * purpose: Create a GREPLAY template that is a grid of ordered panels
 */

%macro makeGridTemplate (

  name     = template
, across   = 1        %* number of panels across;
, down     = 1        %* number of panels down;
, hGap     = 1        %* percentage gap between panels horizontally;
, vGap     = 1        %* percentage gap between panels vertically;
, gapT     = 1        %* percentage gap above top panel;
, gapB     = 1        %* percentage gap below bottom panel;
, gapL     = 1        %* percentage gap left of left panel;
, gapR     = 1        %* percentage gap right of right panel;
, ordering = LRTB     %* layout ordering (see below);
, borderColor = gray  %* border color of panels;

);

/*
 * ordering
 *
 * LRTB              RLTB
 *  1  2  3  4        4  3  2  1
 *  5  6  7  8        8  7  6  5
 *  9 10 11 12       12 11 10  9
 *
 * LRBT              RLBT
 *  9 10 11 12       12 11 10  9
 *  5  6  7  8        8  7  6  5
 *  1  2  3  4        4  3  2  1
 *
 *
 * TBLR              TBRL
 *  1  4  7 10       10  7  4  1
 *  2  5  8 11       11  8  5  2
 *  3  6  9 12       12  9  6  3
 *
 * BTLR              BTRL
 *  3  6  9 12       12  9  6  3
 *  2  5  8 11       11  8  5  2
 *  1  4  7 10       10  7  4  1
 *
 */


  %local this;

  %let this = makeGridTemplate;

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

  %*put %qsysfunc(compress(&rowfunc));
  %*put %qsysfunc(compress(&colfunc));

  %let xpct = %sysevalf ((100-&gapL-&gapR-&hGap*(&ncol-1)) / &ncol);
  %let ypct = %sysevalf ((100-&gapB-&gapT-&vGap*(&nrow-1)) / &nrow);

  %*put xpct = &xpct;
  %*put ypct = &ypct;


  tdef &name des="&across across &down down, panel order &ordering"


  %do count = 0 %to %eval(&npanel-1);
    %let col = %unquote(&colfunc);
    %let row = %unquote(&rowfunc);

    %let x = &col;
    %let y = %eval(&nrow-1-&row);

    %*put count=&count row=&row col=&col x=&x y=&y;

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

/**html
 * <p>Sample code</p>
 */

*/*;

goptions reset=all device=win nodisplay;
goptions target=png rotate=landscape hsize=2.25in vsize=2.25in;

goptions goutmode = replace;

title;
footnote;

%macro slide (text);
  proc gslide gout=work.slides name="&text";
  title h=10pct f='Arial' "&text";
  goptions reset=note;
  note " ";
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

%macro play(name);
  filename slide "\\Extreme\macros\&name..png";
  template &name;
  treplay 1:1 2:2 3:3 4:4 5:5 6:6 7:7 8:8 9:9 10:10 11:11 12:12 0:&name;
%mend;

options mprint;

goptions display goutmode=replace;

goptions device=png gsfname=slide;

proc greplay nofs igout=work.slides tc=work.templates gout=work.test;
  %makeGridTemplate (name=LRTB, across=4, down=3, ordering=LRTB, gapT=10)
  %makeGridTemplate (name=LRBT, across=4, down=3, ordering=LRBT, gapT=10)
  %makeGridTemplate (name=RLTB, across=4, down=3, ordering=RLTB, gapT=10)
  %makeGridTemplate (name=RLBT, across=4, down=3, ordering=RLBT, gapT=10)
  %makeGridTemplate (name=TBLR, across=4, down=3, ordering=TBLR, gapT=10)
  %makeGridTemplate (name=TBRL, across=4, down=3, ordering=TBRL, gapT=10)
  %makeGridTemplate (name=BTLR, across=4, down=3, ordering=BTLR, gapT=10)
  %makeGridTemplate (name=BTRL, across=4, down=3, ordering=BTRL, gapT=10)

  %play (LRTB)
  %play (LRBT)
  %play (RLTB)
  %play (RLBT)
  %play (TBLR)
  %play (TBRL)
  %play (BTLR)
  %play (BTRL)
quit;
*/;

/**html
 * <img style="margin:0.5em" src="LRTB.png">
 * <img style="margin:0.5em" src="RLTB.png">
 * <br>
 * <img style="margin:0.5em" src="LRBT.png">
 * <img style="margin:0.5em" src="RLBT.png">
 * <hr>
 * <img style="margin:0.5em" src="TBLR.png">
 * <img style="margin:0.5em" src="TBRL.png">
 * <br>
 * <img style="margin:0.5em" src="BTLR.png">
 * <img style="margin:0.5em" src="BTRL.png">
 */

