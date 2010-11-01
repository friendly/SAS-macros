 /****************************************************************/
 /*          S A S   S A M P L E   L I B R A R Y                 */
 /*                                                              */
 /*    NAME: CRSPPLOT (labelplt)                                 */
 /*   TITLE: Graphical scatter plots with optimal label placement*/
 /* PRODUCT: STAT                                                */
 /*  SYSTEM: ALL                                                 */
 /*    KEYS: SCATTERPLOTS, GRAPHS                                */
 /*   PROCS: PLOT GANNO PRINTTO CORRESP FORMAT FACTOR PRINQUAL   */
 /*          TRANSREG                                            */
 /*    DATA:                                                     */
 /*                                                              */
 /* SUPPORT: SASWFK                      UPDATE:  18AUG92        */
 /*     REF:                                                     */
 /*    MISC: SAS/GRAPH SOFTWARE MUST BE LICENSED.                */
 /*          THE DEFAULT DEVICE IS XCOLOR.                       */
 /*                                                              */
 /*          THE MACRO CREATES FILES WITH HOST SPECIFIC NAMES.   */
 /*          SEE THE FILEPREF= PLOTFILE=, AND POST= OPTIONS.     */
 /*          IT IS A GOOD IDEA TO SPECIFY DEFAULTS               */
 /*          FOR THESE OPTIONS TO AVOID MAKING THE MACRO         */
 /*          GENERATE NAMES.                                     */
 /*                                                              */
 /*          BEFORE YOU RUN THE MACRO, MAKE SURE THESE FILE      */
 /*          NAMES DO NOT CONFLICT WITH EXISTING FILES!!!        */
 /*                                                              */
 /****************************************************************/
 
 /*---------------------------------------------------------------------
 
 Purpose: This file contains a macro to create graphical scatterplots
          with point labels, vectors, and circles.
 Paper:   Marketing Research: Uncovering Competitive Advantages,
          SUGI 17 Proceedings.
 Author:  Warren F. Kuhfeld, SAS Institute Inc.
 Section: Statistics and Statistical Graphics.
 Software Release: 6.07 for MVS, CMS, VMS.
 
 -----------------------------------------------------------------------
 
 DISCLAIMER:
 
       THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE
 TO ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
 EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
 PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
 CONTAINED HEREIN.
 
 -----------------------------------------------------------------------
 
 This macro uses PROC PLOT to create a plot and PROC PRINTTO to write
 the plot to a file.  Then the macro reads the plot and creates an
 annotate data set, which is displayed.  The macro replaces the printer
 vertical and horizontal lines with graphical lines.  Circles can
 be drawn around certain locations, and vectors can be drawn from the
 origin to other locations.
 
 Typical usage:
 
 * Specify METHOD=gplot2 (the default) to create from scratch the
   graphical scatterplot.  Iteratively change options until you are
   satisfied with the plot.  (Note: specify DATA= since the macro
   creates a data set that cannot be used as input again to the macro.)
 
 Editing the scatterplot:
 
 * Specify METHOD=plot to create a printer plot.
   Iterate on this until you are satisfied with the printer plot.
 
 * Specify METHOD=gplot to see the graphics version of the plot.
   You must create a printer plot before you can create a graphical
   plot with METHOD=gplot.  From now on, you will be using as input
   a printer plot, stored on disk, created by the METHOD=plot or
   METHOD=gplot2 step.
 
 * Edit PLOTFILE= raw plot to change any undesirable label placements.
   Rerun the macro with METHOD=gplot.  Iterate until you are satisfied
   with the plot.
 
 Post-processing the scatterplot:
 
 * You can post-process the annotate data step to change
   undesirable placements.  See the samples for examples.  When you
   specify METHOD=none or METHOD=none2, you create an annotate data
   set without displaying it.  The data set name is by default
   WORK.ANNO.  You can then manipulate it further with a data step
   or PROC FSEDIT to change colors, fonts, or sizes for some labels;
   move some labels; and so on.  If the final result is a new data
   set called anno2, display it by running:
 
   PROC GANNO ANNOTATE=ANNO2;
      RUN;
 
 Alternative output:
 
 * To create a file to route to a plotter or printer, specify
   METHOD=print.
 
 * Use GOUT= to write the plot to a catalogue.
 
 Problem solving:
 
 * When you have problems, try DEBUG=yes to see what the macro
   thinks you specified.  You can also print the final annotate
   data set:
 
   OPTIONS LS=180;
   PROC PRINT DATA=ANNO UNIFORM;
      RUN;
 
 -----------------------------------------------------------------------
 
 The FORMAT= and COLORS= options allow you to edit the file that
 contains the raw plot, specified by PLOTFILE=, and add markup commands
 to override the default formatting.  By default, SYMBOLS=* means
 the macro tries to use a proportional font.  It will output to the
 annotate file strings that it thinks go together (strings of nonblanks
 separated by &NBLANKS or more blanks).  It will look for symbols and
 decide whether to center, left justify, or right justify the string.
 The second through sixth markup characters in &FORMAT are only valid
 when &SYMBOLS is not null.
 
 Consider a plot that contains:
 
   * House       Sea *   * This That *
     Mouse      Lion
 
 By default, House will be left justified and Mouse will be centered.
 By default, Sea will be right justified and Lion will be centered.
 By default, 'This That' will be right justified.
 
 With FORMAT=%str(~<>=\/), if the plot contains:
 
   * House       Sea *   * This\That *
    >Mouse     <Lion
 
 The '>' ensures that both House and Mouse are left justified.
 The '<' ensures that both Sea and Lion will be right justified.
 The '\' ensures that This is left justified and That is right
 justified.  The macro assumes a label consists of nonblank
 characters separated by at most one blank.  Use '~'
 in places where you do not want two or more blanks meaning
 start a new label.  For example: A~~Label~~With~~Extra~~Blanks.
 Alternatively, you may be able to use the NBLANKS= option.
 
 Specify /1, /2, ..., /9, /a, /b, ..., /z
 to change the colors.  If COLORS=red white blue cyan, then
 the following words will appear in their correct colors:
 
 /4 * cyan    /1red *     /2white  /3blue
 
 Once a color is changed it stays in effect as long as is specified by
 the COLRESET= option.  A color may remain in effect until it is
 changed, until the next label, or until the next line.  The color
 markups only affect colors inside the plot.  Use the other color
 parameters to affect the labels, ticks, and so on.  A color markup
 can appear at the beginning or end of a label and it will affect that
 label.  So for example ' /1red ' and ' red/1 ' have the same effect.
 
 -----------------------------------------------------------------------
 
 Samples:
 
 *------Simple Correspondence Analysis------;
 proc corresp all data=cars outc=coor;
    tables marital, origin;
    run;
 
 %labelplt(data=coor,datatype=corresp);
 
 *------Multiple Correspondence Analysis------;
 proc corresp mca observed data=cars outc=coor;
    tables origin size type income home marital sex;
    run;
 
 %labelplt(data=coor,datatype=mca);
 
 *------MDPREF------;
 proc prinqual data=carpref out=results n=2
               replace standard scores correlations;
    id model;
    transform ide(judge1-judge25);
    title2 'Multidimensional Preference (MDPREF) Analysis';
    run;
 
 %labelplt(data=results,datatype=mdpref 2.5);
 
 *------PREFMAP, Vector Model------;
 proc transreg data=presults;
    model ide(mpg reliable ride)=identity(prin1 prin2);
    output tstandard=center coefficients replace out=tresult1;
    id model;
    title2 'Preference Mapping (PREFMAP) Analysis';
    run;
 
 %labelplt(method=none2,data=tresult1,datatype=vector 2.5);
 
 data anno2; * Some plot specific post-processing;
    set anno;
    if text =: 'Reliab' then do;
       y = y + 1;
       x = x + 5;
       end;
    if text =: 'Miles' then do;
       y = y - 2;
       end;
    run;
 
 proc ganno anno=anno2;
    run;
 
 *------PREFMAP, Ideal Point------;
 proc transreg data=presults;
    model identity(mpg reliable ride)=point(prin1 prin2);
    output tstandard=center coordinates replace out=tresult1;
    id model;
    title2 'Preference Mapping (PREFMAP) Analysis';
    run;
 
 %labelplt(data=tresult1,datatype=ideal,antiidea=1);
 
 -----------------------------------------------------------------------
 
 WARNINGS:
 
 The macro creates files.  See the FILEPREF= PLOTFILE=, and POST=
 options and make sure these file names do not conflict with
 existing names.
 
 It is not feasible with a macro to provide the full range of error
 checking that is provided with a procedure.  Some error checking is
 provided, but not all errors will be diagnosed.
 
 Not all options will work with all other options.  Some combinations
 of options may produce macro errors or annotate errors.
 
 This macro may not be fully portable.  When you switch operating
 systems or graphics devices, some changes may be necessary to get the
 macro to run successfully again.  Possible problem areas include host
 differences in the file creation and reading from PROC PRINTTO.
 
 SAS/GRAPH device differences may also be a problem.  This macro has
 run successfully on an HP700 with DEVICE=XCOLOR, and MVS with
 DEVICE=IBM3179, and in WINDOWS.  We do not know of any portability
 problems, but the macro has not been tested on all supported devices.
 
 This macro tries to create a plot with equated axes, where a
 centimeter on one axis represents the same data range as a centimeter
 on the other axis.  The only way to accomplish this in SAS/GRAPH is by
 explicitly and jointly controlling the HSIZE=, VSIZE=, HPOS=, and
 VPOS= GOPTIONS.  By default, the macro tries to ensure that all of the
 values work for the specific device.  See MAKEFIT=, XMAX=, and YMAX=.
 By default the macro uses GASK to determine XMAX and YMAX.
 If you change any of these options, your axes may not be equated.
 
                            vsize * hpos
 Axes are equated when:    --------------  =  vtoh
                            hsize * vpos
 
 By default, the macro iteratively creates and recreates the plot,
 increasing the line size and the flexibility in the PLACEMENT= list
 until there are no penalties.  By default, iteration starts at LS=65
 and PLACE=1.  When there is a large number of long labels, the first
 plot attempt could fail with a warning:
 
 WARNING: Not enough plot positions for label plotting.
          Some heuristics were disabled.
 
 When this happens, increase the starting line size.  For example
 specify: LS=100 search.
 
 The SAS system option OVP (overprint) is not supported by this macro.
 
 --------------------------------------------------------------------*/
 
%macro labelplt(          /*-----------------------------------------*/
                          /* The default device is XCOLOR.  Specify  */
                          /* GOPPLOT= to change it.                  */
                          /*                                         */
                          /* Note that you may not specify a         */
                          /* GOPTIONS statement.  If you do, it will */
                          /* be overridden.  All GOPTIONS must be    */
                          /* specified in the GOPPLOT= or the        */
                          /* GOPPRINT= options.                      */
                          /*                                         */
                          /* Note that for many analyses, the only   */
                          /* options you need to specify are DATA=,  */
                          /* DATATYPE=, and sometimes METHOD=.       */
                          /*-----------------------------------------*/
method=gplot2,            /* plot - Printer plot only.               */
                          /*                                         */
                          /* gplot - Uses GOPTIONS in &GOPPLOT.      */
                          /* Generates graphics plot using printer   */
                          /* plot on disk.                           */
                          /*                                         */
                          /* gplot2 - Writes printer plot to file    */
                          /* and immediately creates graphics plot   */
                          /* using GOPTIONS &GOPPLOT (default).      */
                          /*                                         */
                          /* print - Uses GOPTIONS in &GOPPRINT.  It */
                          /* creates a graphic stream file using the */
                          /* printer plot on disk.                   */
                          /*                                         */
                          /* print2 - Creates the printer plot and   */
                          /* generates a graphic stream file from    */
                          /* it.  Uses GOPTIONS in &GOPPRINT.        */
                          /*                                         */
                          /* none - Just creates the annotate data   */
                          /* set.  Sets up GOPTIONS using &GOPPLOT.  */
                          /* It uses the printer plot on disk.       */
                          /*                                         */
                          /* none2 - Creates the printer plot on     */
                          /* disk and creates the annotate data set  */
                          /* from it.  Sets up GOPTIONS using        */
                          /* &GOPPLOT.                               */
                          /*-----------------------------------------*/
                          /* The next three options specify GOPTIONS */
                          /* for printing (creating a graphics       */
                          /* stream file), &GOPPRINT, and directly   */
                          /* plotting on the screen, &GOPPLOT.  The  */
                          /* &GOPTS options provides goptions that   */
                          /* are always used, regardless of whether  */
                          /* &GOPPLOT or &GOPPRINT is used.          */
                          /*                                         */
                          /* A few device names.                     */
                          /* qmscolor - color plotter.               */
                          /* psepsf   - encapsulated postscript.     */
                          /* psl      - postscript.                  */
                          /* xcolor   - X windows, color.            */
                          /*-----------------------------------------*/
 
gopprint=device=pscolor gsfmode=replace gaccess=sasgaedt gprolog='2521'x gsfname=gsasfile,
 
gopplot=,	/*device=nxtcolor, */
 
gopts=,		/*reset=goptions erase,*/
 
                          /*-----------------------------------------*/
data=_last_,              /* Raw data for PROC PLOT.  When creating  */
                          /* the plot with METHOD=plot,              */
                          /* METHOD=gplot2, METHOD=print2, or        */
                          /* METHOD=none2, you should specify DATA=  */
                          /* since the macro creates data sets.      */
                          /*                                         */
out=anno,                 /* Output annotate data set.               */
                          /*                                         */
gout=,                    /* PROC ANNO GOUT= specification.  This    */
                          /* option puts a plot in a catalogue.      */
                          /* With GOUT=GC.SLIDES, specify:           */
                          /* LIBNAME GC '.';                         */
                          /*-----------------------------------------*/
datatype=,                /* Specifies the type of data analysis     */
                          /* that generated the data set.  This      */
                          /* option is used to set defaults for      */
                          /* other options and to do some            */
                          /* preprocessing of the data.  Whenever a  */
                          /* non-null value is specified,            */
                          /* LABEL=typical is implied when LABEL= is */
                          /* not otherwise specified.  The default   */
                          /* point label variable is the last        */
                          /* character variable in the data set.     */
                          /*                                         */
                          /* null - (default) no special processing. */
                          /* Default plotting variables are the      */
                          /* first two numeric variables in the data */
                          /* set.                                    */
                          /*                                         */
                          /* corresp, mds, mca, row, column - makes  */
                          /* default &PLOTVARS DIM2 and DIM1.        */
                          /*                                         */
                          /* otherwise when a non-null value is      */
                          /* specified, the default &PLOTVARS are    */
                          /* PRIN2 and PRIN1.                        */
                          /*                                         */
                          /* mdpref - specifies VECTOR=#,            */
                          /* VECLAB=blank, and VECHEAD=typical when  */
                          /* nothing else is specified for these     */
                          /* options.                                */
                          /*                                         */
                          /* vector - specifies a prefmap vector     */
                          /* model.  It also specifies VECTOR=#,     */
                          /* VECLAB=typical, and VECHEAD=typical     */
                          /* when nothing else is specified for      */
                          /* these options.  DATATYPE=mds vector,    */
                          /* may be specified if a PREFMAP analysis  */
                          /* was performed after the MDS analysis.   */
                          /*                                         */
                          /* ideal - specifies a PREFMAP ideal point */
                          /* model.  It also specifies CIRCLE=+,     */
                          /* CIRSYM=+, and VECLAB=typical when       */
                          /* nothing else is specified for these     */
                          /* options.  DATATYPE=mds ideal may be     */
                          /* specified if a PREFMAP analysis was     */
                          /* performed after the MDS analysis.       */
                          /*                                         */
                          /* row - specifies a PROC CORRESP          */
                          /* PROFILE=ROW analysis.  Column points    */
                          /* are plotted as vectors.  It sets        */
                          /* SCORES='VAR', VECTOR=#,                 */
                          /* VECHEAD=typical, and VECLAB=typical.    */
                          /*                                         */
                          /* column - specifies a PROC CORRESP       */
                          /* PROFILE=COLUMN analysis.  Row points    */
                          /* are plotted as vectors.  It sets        */
                          /* SCORES='OBS', VECTOR=#,                 */
                          /* VECHEAD=typical, and VECLAB=typical.    */
                          /*                                         */
                          /* A number may be specified after the     */
                          /* name.  This is primarily useful for     */
                          /* biplot data sets produced by PRINQUAL   */
                          /* and PREFMAP data sets produced by PROC  */
                          /* TRANSREG.  This number specifies that   */
                          /* the lengths of vectors should be        */
                          /* changed by this amount.  The number     */
                          /* must be specified last.  Examples:      */
                          /* DATATYPE=mdpref 2,                      */
                          /* DATATYPE=mds vector 1.5.                */
                          /*                                         */
                          /* DATATYPE=corresp provides pre- and      */
                          /* post- processing of data sets that come */
                          /* from PROC CORRESP.  It is used to make  */
                          /* the row and column points different     */
                          /* colors.  After the DATATYPE=corresp,    */
                          /* eight values may be provided:           */
                          /* row color, column color,                */
                          /* final row symbol, final column symbol,  */
                          /* row symbol, column symbol,              */
                          /* row marker, column marker.              */
                          /* All values are separated by spaces.     */
                          /* DATATYPE=corresp, is equivalent to      */
                          /* DATATYPE=%str(corresp red blue * * * +  */
                          /* ( ) ).  The SYMBOLS= option is          */
                          /* redefined to use the specified symbols. */
                          /* This option creates a symbol variable   */
                          /* &SYMVAR that must be used in the plot   */
                          /* request.  You do not have to specify    */
                          /* all eight values.  For example,         */
                          /* DATATYPE=corresp green pink, can be     */
                          /* specified to just change the colors.    */
                          /* Defaults will be used for the other     */
                          /* options.                                */
                          /*-----------------------------------------*/
filepref=labelplt,        /* File name prefix.                       */
                          /*                                         */
plotfile=,                /* Name of file to store raw plot.  The    */
                          /* default name is constructed from the    */
                          /* FILEPREF= value and "plt" in a host     */
                          /* specific way.                           */
                          /*                                         */
post=,                    /* Graphics stream file name.  The default */
                          /* name is constructed from the FILEPREF=  */
                          /* value and "ps" in a host specific way.  */
                          /*                                         */
tempplot=tempplot,        /* Data set used to hold intermediate      */
                          /* results.                                */
                          /*-----------------------------------------*/
procopts=nolegend,        /* PROC PLOT statement options.            */
                          /*                                         */
plotreq=,                 /* PLOT statement plot request.  BOX will  */
                          /* be specified, even if you do not        */
                          /* specify it.  When null, the macro will  */
                          /* try to provide a default based on       */
                          /* PLOTVARS=, LABELVAR=, DATATYPE=, and so */
                          /* on.  There is no guarantee that this    */
                          /* will really do what you want.           */
                          /*                                         */
plotopts=,                /* When you are letting the macro generate */
                          /* a default plot request, you can specify */
                          /* additional options with PLOTOPTS=.      */
                          /* Otherwise this option is ignored.       */
                          /*                                         */
label=,                   /* PROC PLOT LABEL statement.  Note that   */
                          /* this option can be used to include any  */
                          /* other statements with PROC PLOT such as */
                          /* FORMAT statements.  Just specify the    */
                          /* full statement.  Note that the keyword  */
                          /* LABEL or FORMAT must be specified.  To  */
                          /* specify LABEL and FORMAT, put a         */
                          /* semicolon between them.  Abbreviations: */
                          /* LABEL=typical specifies that a label    */
                          /* statement be constructed with           */
                          /* 'Dimension' and the numeric suffix,     */
                          /* such as LABEL DIM1 = 'Dimension 1'      */
                          /* DIM2 = 'Dimension 2'  if PLOTVARS=DIM2  */
                          /* DIM1.  LABEL=typical can only be used   */
                          /* with variable names that consist of a   */
                          /* prefix and a numeric suffix.            */
                          /*                                         */
place=1 search,           /* Generates a PLACEMENT= option for the   */
                          /* plot request.  Specify a positive       */
                          /* integer.  Values greater than 15 are    */
                          /* set to 15.  As the value gets larger,   */
                          /* the procedure is given more freedom to  */
                          /* move the labels farther from the        */
                          /* symbols.  The generated placement list  */
                          /* will be printed in the log.  You can    */
                          /* still specify PLACEMENT= directly on    */
                          /* the plot request.  This option just     */
                          /* gives you a shorthand notation.         */
                          /*                                         */
                          /* The PLACE= option, along with the LS=   */
                          /* option can be used to search for an     */
                          /* optimum placement list and an optimum   */
                          /* line size.  By default, when PLACE=1    */
                          /* search, the macro will create and       */
                          /* recreate the plot until it avoids all   */
                          /* collisions.  The search is turned off   */
                          /* when a PLACEMENT= option is detected in */
                          /* the plot request or plot options.       */
                          /*                                         */
                          /* If search is not specified with PLACE=  */
                          /* or LS=, the specified value is fixed.   */
                          /* If search is specified with the other   */
                          /* option, only that option's value is     */
                          /* incremented in the search.              */
                          /*                                         */
                          /* By default, when both PLACE=1 search    */
                          /* and LS=65 search are used: on the first */
                          /* pass, PLACE=1 is used with LS=65, on    */
                          /* the second pass, PLACE=2 is used with   */
                          /* LS=75, and so on.  With PLACE=5 search  */
                          /* and LS=90 search: on the first pass,    */
                          /* PLACE=5 is used with LS=90, on the      */
                          /* second pass, PLACE=6 is used with       */
                          /* LS=100, and so on.                      */
                          /*                                         */
ls=65 search,             /* Line size.  Specify an integer.  By     */
                          /* default when LS=65 search, the macro    */
                          /* searches for an optimal line size       */
                          /* starting at LS=65.  See the PLACE=      */
                          /* option for more information on          */
                          /* searches.                               */
                          /*                                         */
maxiter=15,               /* Maximum number of iterations.           */
                          /*                                         */
lsinc=10,                 /* Increment to line size in iterations.   */
                          /*                                         */
restore=ls=78 ps=60,      /* Restore line size and page size to      */
                          /* these values when the macro is done     */
                          /* processing, when a plot is made.        */
                          /*-----------------------------------------*/
font=swiss,               /* SAS/GRAPH font.                         */
                          /*                                         */
tsize=1,                  /* Annotate size for text.                 */
                          /*                                         */
lsize=3,                  /* Annotate size for lines.                */
                          /*                                         */
ticklen=1.5,              /* Length of tick mark in horizontal       */
                          /* cells.                                  */
                          /*                                         */
tickaxes=l r t b,         /* Axes to draw tick marks.  Default:      */
                          /* TICKAXES=l r t b means left, right,     */
                          /* top, bottom.  To just have tick marks   */
                          /* on the left and bottom axes, specify    */
                          /* TICKAXES=l b.  Order and spacing do not */
                          /* matter.                                 */
                          /*                                         */
rotate=,                  /* Specifies rotate the vertical axis      */
                          /* variable label.                         */
                          /* null - (default) macro guesses which    */
                          /* column the label is in.                 */
                          /* 0  - do not rotate label                */
                          /* >0 - specifies a column number,         */
                          /* relative to the first column with       */
                          /* input.  So ROTATE=1 means capture the   */
                          /* label from the first column that has    */
                          /* input in it.  The label is captured and */
                          /* rotated so that it would be read        */
                          /* normally when the plot is rotated 90    */
                          /* degrees clock-wise.                     */
                          /*                                         */
remnote=yes,              /* yes - Remove lines after the plot that  */
                          /* are not a part of the plot.  The first  */
                          /* line removed contains 'NOTE:', or the   */
                          /* title, or the title for LIST= output.   */
                          /* Otherwise, do not remove these lines.   */
                          /*                                         */
center=yes,               /* yes - Center plot in window.            */
                          /* Otherwise, do not center.               */
                          /* When CENTER=yes is specified, then      */
                          /* VSIZE= and HSIZE= are set to their      */
                          /* maximum values, and the VPOS= and HPOS= */
                          /* values are increased accordingly.  The  */
                          /* X and Y coordinates are increased to    */
                          /* position the plot in the center of the  */
                          /* full graphics area.                     */
                          /*-----------------------------------------*/
color=black,              /* Default SAS/GRAPH color, used when no   */
                          /* other color is set.                     */
                          /*                                         */
framecol=,                /* Color of frame, defaults to &COLOR.     */
                          /*                                         */
titlecol=,                /* Color of title, defaults to &COLOR.     */
                          /*                                         */
labelcol=,                /* Color of label, defaults to &COLOR.     */
                          /*                                         */
tickcol=,                 /* Color of ticks, defaults to &COLOR.     */
                          /*                                         */
textcol=blue,             /* Color of text.                          */
                          /*                                         */
veccol=red,               /* Color of vectors.                       */
                          /*                                         */
circol=red,               /* Color of circles.                       */
                          /*                                         */
symcol=green,             /* Color of symbols.  A list can be        */
                          /* specified for SYMCOL=.  In that case,   */
                          /* the first SYMBOLS= symbol uses the      */
                          /* first color, and so on.                 */
                          /*-----------------------------------------*/
symcola=,                 /* Color of all symbols.  The default is   */
                          /* null.  When specified, all symbols in   */
                          /* the plot specified in SYMBOLS= are set  */
                          /* to this color.  This option is not      */
                          /* overridden by markups.                  */
                          /*                                         */
monochro=,                /* Overrides all other specified colors.   */
                          /* This option is useful when you have     */
                          /* specified colors and you want to        */
                          /* temporarily override them to send the   */
                          /* plot to a monochrome device.  By        */
                          /* default, this option has no effect.     */
                          /* Typical usage: MONOCHRO=BLACK.          */
                          /*-----------------------------------------*/
format=,                  /* Markup characters are in the raw plot   */
                          /* file when FORMAT= is not blank.         */
                          /* Specify a six character string.  Blank  */
                          /* turns off that formatting capability.   */
                          /* The six characters are:                 */
                          /* 1) Substitute space for this character. */
                          /* 2) Right justify string after this      */
                          /*    character.                           */
                          /* 3) Left justify string after this char. */
                          /* 4) Center string after this char.       */
                          /* 5) Break between labels.                */
                          /* 6) Next character is color code.        */
                          /* For no formatting: FORMAT=,             */
                          /* Typical usage: FORMAT=%str(~<>=\/).     */
                          /* Equivalent to above: FORMAT=typical.    */
                          /*                                         */
symbols=*,                /* If you specify a list of plot symbols,  */
                          /* the macro will try to figure out which  */
                          /* symbol goes with which label and use    */
                          /* proportional fonts.  For uniform        */
                          /* spacing: SYMBOLS=,.  When you do not    */
                          /* specify a list here, you will need to   */
                          /* use hardware characters or decrease the */
                          /* text size to a smaller value such as    */
                          /* TSIZE=0.7.  Tick and label colors will  */
                          /* generally not be correct with a null    */
                          /* symbols.  Note that the symbols list is */
                          /* augmented automatically by the VECTOR=, */
                          /* CIRCLE=, CIRSYM=, and DATATYPE=         */
                          /* options.                                */
                          /*                                         */
nblanks=2,                /* Number of blanks that separate labels.  */
                          /* 35th.                                   */
                          /*                                         */
colreset=none,            /* label - reset for the next label.       */
                          /*                                         */
                          /* line  - reset for the next line.        */
                          /*                                         */
                          /* otherwise, do not reset colors once     */
                          /* they are set.                           */
                          /*                                         */
                          /*-----------------------------------------*/
                          /* Color codes: A markup of /1 with        */
                          /* FORMAT=%str(~<>=\/) specifies use the   */
                          /* first color in the COLORS= list, by     */
                          /* default red.  /2 uses the second, ...,  */
                          /* /a uses the tenth, ..., /z uses the     */
                          /* 35th.                                   */
                          /*-----------------------------------------*/
 
colors=red white blue cyan brown magenta gold green pink olive orange
yellow purple violet gray black,
 
                          /*-----------------------------------------*/
inc=,                     /* Default HAXIS=BY &INC and VAXIS=BY &INC */
                          /* value.                                  */
sametick=no,              /* no - do not try to trick PROC PLOT into */
                          /* using the same ticks for both axes.     */
                          /* Otherwise, overlay the transpose of the */
                          /* desired plot with blank symbols to      */
                          /* trick PROC PLOT into using the same     */
                          /* ticks for both axes.                    */
                          /*-----------------------------------------*/
                          /* These next options create a copy of the */
                          /* input data set: &TEMPPLOT.  They are    */
                          /* designed for post-processing TRANSREG,  */
                          /* PRINQUAL, MDS, and CORRESP output.      */
                          /* These options only operate when         */
                          /* METHOD=plot, METHOD=gplot2,             */
                          /* METHOD=print2, or METHOD=none2.         */
                          /*                                         */
                          /* Many of these options operate on two    */
                          /* variables such as PRIN1 and PRIN2, DIM1 */
                          /* and DIM2, PRIN2 and PRIN3, and so on.   */
                          /* Set these names with PLOTVARS=.         */
                          /*                                         */
plotvars=,                /* Specify the y variable then the x       */
                          /* variable.  To work with say DIM2 and    */
                          /* DIM3, specify PLOTVARS=DIM2 DIM3.  The  */
                          /* DATATYPE= option controls the default.  */
                          /*                                         */
labelvar=,                /* Variable that contains the point        */
                          /* labels.  The DATATYPE= and VECLAB=blank */
                          /* options use this variable.  The default */
                          /* is the last character variable in the   */
                          /* data set.                               */
                          /*                                         */
symvar=,                  /* Variable or string that contains the    */
                          /* plotting symbol.  The DATATYPE= options */
                          /* can create this variable.               */
                          /*                                         */
antiidea=,                /* Eliminate PREFMAP anti-ideal points.    */
                          /*                                         */
                          /* null - do nothing.                      */
                          /*                                         */
                          /* 1 - reverse in obs whose _TYPE_         */
                          /* contains 'POINT' when _ISSQ_ > 0.       */
                          /*                                         */
                          /* -1 - reverse in obs whose _TYPE_        */
                          /* contains 'POINT' when _ISSQ_ < 0.       */
                          /*-----------------------------------------*/
vector=,                  /* Symbol indicates end points of vectors. */
                          /* Typical specification: VECTOR=#.        */
                          /*                                         */
vechead=,                 /* Symbol to use in plot for vector head.  */
                          /* If two integers are specified, an arrow */
                          /* head is drawn.  VECHEAD=2 1, specifies  */
                          /* draw a head consisting of two           */
                          /* hypotenuses from triangles with sides 2 */
                          /* cell units long along the vector and 1  */
                          /* cell unit on the side perpendicular to  */
                          /* the vector.  VECHEAD=typical means      */
                          /* VECHEAD=2.0 0.5.                        */
                          /*                                         */
veclab=,                  /* Typical usage: VECLAB=typical and       */
                          /* VECLAB=blank.  Both create the          */
                          /* &TEMPPLOT data set.  This option allows */
                          /* some labels to be post-processed as a   */
                          /* group in a manner that is different     */
                          /* from the other labels.  A typical way   */
                          /* that this option might be used is with  */
                          /* labels for vectors with a PREFMAP       */
                          /* analysis.  If the vectors are red, for  */
                          /* example, this option can be used to     */
                          /* make the labels for the vectors red,    */
                          /* make them bigger, and change their      */
                          /* fonts.                                  */
                          /*                                         */
                          /* VECLAB=typical, is the same as          */
                          /* VECLAB=%bquote(red 1.25 swissi ( ).     */
                          /* The values are the color, size, font,   */
                          /* and marker.  Later values may be        */
                          /* omitted.  Defaults will be used.  Say   */
                          /* PROC TRANSREG was used for a PREFMAP    */
                          /* and you want to change the way the      */
                          /* vector labels appear.  Observations     */
                          /* whose _TYPE_ value is not &SCORES are   */
                          /* automatically marked with the left      */
                          /* parenthesis to designate that they are  */
                          /* the ones to change.  This assumes that  */
                          /* no ordinary point labels contain a left */
                          /* parenthesis.  The parenthesis markers   */
                          /* will be removed from the labels before  */
                          /* the final graphical scatterplot is      */
                          /* produced.                               */
                          /*                                         */
                          /* VECLAB=blank, sets to blank labels for  */
                          /* observations whose _TYPE_ value is not  */
                          /* in &SCORES.                             */
                          /*                                         */
scores=,                  /* _TYPE_ values for score (not vector)    */
                          /* observations.  This option generally    */
                          /* set by VECLAB=.                         */
                          /*-----------------------------------------*/
circle=,                  /* Symbol indicates center of circles.     */
                          /* Typical specification: CIRCLE==.        */
                          /*                                         */
cirsym=,                  /* Replace circle symbol with this symbol. */
                          /* Typical specification: CIRCLE=+.        */
                          /*                                         */
radii=%str(10, 20),       /* Radii of circles (data step do list).   */
                          /*                                         */
nsegs=60,                 /* The the number of circle segments in    */
                          /* each circle is:                         */
                          /* ceil(log10(25 + radius) * &NSEGS)       */
                          /*-----------------------------------------*/
debug=no,                 /* no - do not print debugging info.       */
                          /* Otherwise, print macro options and      */
                          /* macro variables for debugging.          */
                          /* print - print intermediate data sets.   */
                          /*                                         */
notes=no,                 /* no - specify OPTIONS NONOTES during     */
                          /* most of the macro.  Otherwise, do not   */
                          /* specify OPTIONS NONOTES.                */
                          /*-----------------------------------------*/
                          /* Normally, you should not specify any    */
                          /* options from this section.  The macro   */
                          /* has or chooses suitable defaults.       */
                          /*                                         */
vtoh=2,                   /* PROC PLOT VTOH= option.  Do not specify */
                          /* values much different than 2,           */
                          /* especially by default when you are      */
                          /* using proportional fonts.  There is no  */
                          /* one-to-one correspondence between       */
                          /* characters and cells and character      */
                          /* widths vary, but characters tend to be  */
                          /* approximately half as wide as they are  */
                          /* high.  When you specify VTOH= values    */
                          /* larger than 2, near-by labels may       */
                          /* overlap, even when they do not collide  */
                          /* in the printer plot.                    */
                          /*                                         */
                          /* Smaller values give you more lines and  */
                          /* smaller labels.  VTOH=1.75 is a good    */
                          /* alternative to VTOH=2 when you need     */
                          /* more lines to avoid collisions.         */
                          /* VTOH=1.75 means 7 columns for each 4    */
                          /* rows between ticks (7 / 4 = 1.75).      */
                          /* VTOH=2 means the plot will have 8       */
                          /* columns for each 4 rows between ticks.  */
                          /* Only specify values that are ratios of  */
                          /* small integers.                         */
                          /*                                         */
makefit=0.95,             /* Proportion of graphics window to use.   */
                          /* When non-null, the macro uses GASK to   */
                          /* determine the minimum and maximum       */
                          /* graphics window sizes and makes sure    */
                          /* the plot can fit in them.  The macro    */
                          /* uses GOPPRINT= or GOPPLOT= to determine */
                          /* the device.                             */
                          /*                                         */
                          /* To make the plot fill up the entire     */
                          /* graphics area (like normal PROC GPLOT   */
                          /* behavior) specify MAKEFIT=.  Sometimes, */
                          /* you must also specify HSIZE= and VSIZE= */
                          /* values larger than the maximum allowed. */
                          /*                                         */
                          /* If you specify just the HSIZE= or the   */
                          /* VSIZE= but not both, you can control    */
                          /* the absolute size of one axis and the   */
                          /* size of the other axis will be scaled   */
                          /* accordingly.                            */
                          /*                                         */
unit=in,                  /* HSIZE=, VSIZE=, unit: in or cm.         */
                          /*                                         */
hsize=,                   /* Horizontal graphics area size in UNIT=  */
                          /* units.  The default is the maximum size */
                          /* for the device.  By default when        */
                          /* CENTER=yes, HSIZE= affects the size of  */
                          /* the plot but not the HSIZE= GOPTION.    */
                          /* For other CENTER= values, HSIZE=        */
                          /* affects both the plot size and the      */
                          /* HSIZE= GOPTION.                         */
                          /*                                         */
vsize=,                   /* Vertical graphics area size in UNIT=    */
                          /* units.  The default is the maximum size */
                          /* for the device.  By default when        */
                          /* CENTER=yes, VSIZE= affects the size of  */
                          /* the plot but not the VSIZE= GOPTION.    */
                          /* For other CENTER= values, VSIZE=        */
                          /* affects both the plot size and the      */
                          /* VSIZE= GOPTION.                         */
                          /*                                         */
xmax=,                    /* Maximum horizontal size of the graphics */
                          /* area.                                   */
                          /*                                         */
ymax=,                    /* Maximum vertical size of the graphics   */
                          /* area.                                   */
                          /*                                         */
hpos=,                    /* Horizontal positions in graphics area.  */
                          /*                                         */
vpos=,                    /* Vertical positions in graphics area.    */
                          /*                                         */
ps=,                      /* Page size.                              */
                          /*                                         */
);                        /*-----------------------------------------*/
 
%if %bquote(&notes) = no %then options nonotes;;
 
*------initialize some macro variables------;
%let ok       = yes;                             /* no errors so far */
%let y0       = 0;                               /* cell of y = 0.0  */
%let x0       = 0;                               /* cell of x = 0.0  */
%let noteline = 0;                               /* line with note   */
%let singular = 1e-8;                            /* approximate zero */
%let biplot   = ;                                /* vector stretch   */
%let symtype  = ;                                /* symbol type      */
%let search   = no;                              /* ls, place search */
%let labely   = ;                                /* y-axis label     */
%let corresp  = ;                                /* pre-processing   */
%let pi       = 3.1415926536;                    /* pi               */
%let typfor   = %str(~<>=\/);                    /* typical markups  */
%let typcor   = %bquote( red blue * * * + ( ) ); /* typical corresp  */
%let typvec   = %bquote( red 1.25 swissi ( );    /* typical vectors  */
%let blanks   = %str( )%str( )%str( )%str( )%str( )%str( );/* kludge */
 
%let listtitl = List of Point Locations, Penalties, and Placement;
 
*------check options------;
%if %bquote(&method) = both %then %let method = gplot2;
 
%if not (%bquote(&method) = gplot or %bquote(&method) = gplot2 or
         %bquote(&method) = print or %bquote(&method) = print2 or
         %bquote(&method) = none  or %bquote(&method) = none2  or
         %bquote(&method) = plot) %then %do;
   %put ERROR: METHOD=&method is not valid.;
   %let ok = no;
   %goto endit;
   %end;
 
%let i = %scan(%bquote(&datatype),1,%str( ));
%if %length(%bquote(&i)) and not
   (%bquote(&i) = corresp or %bquote(&i) = mds    or
    %bquote(&i) = mca     or %bquote(&i) = mdpref or
    %bquote(&i) = vector  or %bquote(&i) = ideal  or
    %bquote(&i) = row     or %bquote(&i) = column) %then %do;
   %put ERROR: DATATYPE=%bquote(&datatype) is not valid.;
   %let ok = no;
   %goto endit;
   %end;
 
%if %bquote(&unit) ne in and %bquote(&unit) ne cm %then %do;
   %put ERROR: UNIT=&unit is not valid.;
   %let ok = no;
   %goto endit;
   %end;
 
%if %bquote(&circle) = %bquote(+) %then %do;
   %put ERROR: CIRCLE=+ is not allowed.;
   %let ok = no;
   %goto endit;
   %end;
 
%if %bquote(&vector) = %bquote(+) %then %do;
   %put ERROR: VECTOR=+ is not allowed.;
   %let ok = no;
   %goto endit;
   %end;
 
%if %length(%bquote(&plotreq)) and (%length(%bquote(&plotvars)) or
    %length(%bquote(&symvar)) or %length(%bquote(&labelvar))) %then %do;
   %let i = may not be specified with PLOTREQ=.;
   %put ERROR: PLOTVARS=, LABELVAR= and SYMVAR= &i;
   %let ok = no;
   %goto endit;
   %end;
 
*------some options cannot have null values------;
%if %bquote(&data)     = %then %let data = _last_;
%if %bquote(&data)     = %bquote(_last_) %then %let data = &syslast;
%if %bquote(&filepref) = %then %let filepref = sasplot;
%if %bquote(&out)      = %then %let out      = anno;
%if %bquote(&filepref) = %then %let filepref = sasplot;
%if %bquote(&tempplot) = %then %let tempplot = tempplot;
%if %bquote(&radii)    = %then %let radii    = 10, 20;
%if %bquote(&nsegs)    = %then %let nsegs    = 60;
%if %bquote(&vtoh)     = %then %let vtoh     = 2;
%if %bquote(&color)    = %then %let color    = white;
%if %bquote(&colors)   = %then %let colors   = white;
%if %bquote(&maxiter)  = %then %let maxiter  = 15;
%if %bquote(&lsinc)    = %then %let lsinc    = 10;
 
*------set default colors------;
%if %length(%bquote(&framecol)) = 0 %then %let framecol = &color;
%if %length(%bquote(&titlecol)) = 0 %then %let titlecol = &color;
%if %length(%bquote(&labelcol)) = 0 %then %let labelcol = &color;
%if %length(%bquote(&tickcol))  = 0 %then %let tickcol  = &color;
%if %length(%bquote(&textcol))  = 0 %then %let textcol  = &color;
%if %length(%bquote(&veccol))   = 0 %then %let veccol   = &color;
%if %length(%bquote(&circol))   = 0 %then %let circol   = &color;
%if %length(%bquote(&symcol))   = 0 %then %let symcol   = &textcol;
 
*------fix up a few options------;
%let procopts = &procopts formchar='|----|+|---';
 
%if %bquote(&format) = typical %then %let format = %bquote(&typfor);
%else %if %length(%bquote(&format)) < 6
   %then %let format = %bquote(&format&blanks);
 
%if %bquote(&symbols) = %then %let symbols = ;
%if %length(%bquote(&symbols)) = 0 %then %do;
   %let format = %substr(%bquote(&format),1,1)&blanks;
   %let rotate = 0;
   %end;
 
*------are we creating a printer plot on disk?------;
%if %bquote(&method) = plot   or %bquote(&method) = gplot2 or
    %bquote(&method) = print2 or %bquote(&method) = none2
   %then %let makeplot = yes;
   %else %let makeplot = no;
 
*------check data set names for conflicts------;
%if %qupcase(&tempplot)      = %qupcase(&out)       or
    WORK.%qupcase(&tempplot) = %qupcase(&out)       or
    %qupcase(&tempplot)      = WORK.%qupcase(&out)  %then %do;
   %let i = Names must be different.;
   %put ERROR: TEMPPLOT=&tempplot, and OUT=&out..  &i;
   %let ok = no;
   %goto endit;
   %end;
 
%if &makeplot = yes and
   (%qupcase(&tempplot)      = %qupcase(&data)      or
    WORK.%qupcase(&tempplot) = %qupcase(&data)      or
    %qupcase(&tempplot)      = %qupcase(&out)       or
    WORK.%qupcase(&out)      = %qupcase(&data)      or
    %qupcase(&out)           = WORK.%qupcase(&data) or
    %qupcase(&out)           = %qupcase(&data)) %then %do;
   %let i = Names must be different.;
   %put ERROR: DATA=&data, TEMPPLOT=&tempplot, and OUT=&out..  &i;
   %let ok = no;
   %goto endit;
   %end;
 
*------set default file names------;
%if %length(%bquote(&plotfile)) = 0 %then
   %let plotfile = %fname(&filepref,plt);
%if %length(&post) = 0 %then
   %let post = %fname(&filepref,ps);
 
%put %str( );
%put NOTE: The plot file name is %bquote(&plotfile) .;
%if &method = print or &method = print2 %then
   %put NOTE: The graphics stream file name is %bquote(&post) .;
%put %str( );
 
*------break up format string------;
%let fspace  = %substr(%bquote(&format),1,1);
%let fleft   = %substr(%bquote(&format),2,1);
%let fright  = %substr(%bquote(&format),3,1);
%let fcenter = %substr(%bquote(&format),4,1);
%let fbreak  = %substr(%bquote(&format),5,1);
%let fcolor  = %substr(%bquote(&format),6,1);
 
*---process datatype= option------;
%if %length(%bquote(&datatype)) and %length(%bquote(&label)) = 0
   %then %let label = typical;
 
%if %qscan(%bquote(&datatype),1,%str( )) = corresp %then %do;
   %if %length(%bquote(&scores)) = 0 %then %let scores = 'OBS' 'SUPOBS';
   %if %length(%qscan(%bquote(&datatype),2,%str( )))
      %then %let corresp = %substr(%bquote(&datatype),9);
      %else %let corresp = typical;
   %end;
 
%else %do;
   %do i = 5 %to 2 %by -1;
      %if %length(%bquote(&biplot)) = 0
         %then %let biplot = %scan(%bquote(&datatype),&i,%str( ));
      %end;
   %if %bquote(&biplot) = vector or %bquote(&biplot) = ideal  or
       %bquote(&biplot) = mdpref or %bquote(&biplot) = row    or
       %bquote(&biplot) = column or %bquote(&biplot) = mds
      %then %let biplot = 1;
   %end;
 
%if %index(%bquote(&datatype),mdpref) %then %do;
   %if %length(%bquote(&vector))   = 0 %then %let vector  = #;
   %if %length(%bquote(&veclab))   = 0 %then %let veclab  = blank;
   %if %length(%bquote(&scores))   = 0 %then %let scores  = 'SCORE' ' ';
   %if %length(%bquote(&vechead))  = 0 %then %let vechead = typical;
   %if %length(%bquote(&biplot))   = 0 %then %let biplot  = 1;
   %end;
 
%if %index(%bquote(&datatype),vector) %then %do;
   %if %length(%bquote(&vector))   = 0 %then %let vector  = #;
   %if %length(%bquote(&veclab))   = 0 %then %let veclab  = typical;
   %if %length(%bquote(&scores))   = 0 %then %let scores  = 'SCORE' ' ';
   %if %length(%bquote(&vechead))  = 0 %then %let vechead = typical;
   %if %length(%bquote(&biplot))   = 0 %then %let biplot  = 1;
   %end;
 
%if %index(%bquote(&datatype),ideal) %then %do;
   %if %length(%bquote(&circle))   = 0 %then %let circle  = =;
   %if %length(%bquote(&veclab))   = 0 %then %let veclab  = typical;
   %if %length(%bquote(&scores))   = 0 %then %let scores  = 'SCORE' ' ';
   %if %length(%bquote(&cirsym))   = 0 %then %let cirsym  = +;
   %if %length(%bquote(&biplot))   = 0 %then %let biplot  = 1;
   %if %length(%bquote(&antiidea)) = 0 and &makeplot = yes %then
      %put NOTE: Specify antiidea= to eliminate anti-ideal points.;
   %end;
 
%if %index(%bquote(&datatype),row) %then %do;
   %if %length(%bquote(&vector)) = 0 %then %let vector = #;
   %if %length(%bquote(&veclab)) = 0 %then %let veclab = typical;
   %if %length(%bquote(&biplot)) = 0 %then %let biplot = 1;
   %if %length(%bquote(&scores)) = 0 %then %let scores = 'OBS' 'SUPOBS';
   %if %length(%bquote(&vechead)) = 0 %then %let vechead = typical;
   %end;
 
%if %index(%bquote(&datatype),column) %then %do;
   %if %length(%bquote(&vector)) = 0 %then %let vector = #;
   %if %length(%bquote(&veclab)) = 0 %then %let veclab = typical;
   %if %length(%bquote(&biplot)) = 0 %then %let biplot = 1;
   %if %length(%bquote(&scores)) = 0 %then %let scores = 'VAR' 'SUPVAR';
   %if %length(%bquote(&vechead)) = 0 %then %let vechead = typical;
   %end;
 
%if %length(%bquote(&scores)) = 0 %then %let scores = 'SCORE';
 
*------set up vector heads------;
%if %bquote(&vechead) = typical %then %let vechead = 2.0 0.5;
%let headr = %scan(%bquote(&vechead),1,%str( ));
%let headw = %scan(%bquote(&vechead),2,%str( ));
%if %length(%bquote(&headw)) = 0 %then %let headr = ;
%if %length(%bquote(&vechead)) = 0 or %length(%bquote(&headw))
   %then %let vechead = %str( );
 
*------break up corresp string------;
%if %bquote(&corresp) = typical %then %let corresp = %bquote(&typcor);
 
%if %length(%bquote(&corresp)) %then %do;
 
   %let crowcol = %scan(%bquote(&corresp),1,%str( ));
   %let ccolcol = %scan(%bquote(&corresp),2,%str( ));
   %let crowfsy = %scan(%bquote(&corresp),3,%str( ));
   %let ccolfsy = %scan(%bquote(&corresp),4,%str( ));
   %let crowsym = %scan(%bquote(&corresp),5,%str( ));
   %let ccolsym = %scan(%bquote(&corresp),6,%str( ));
   %let crowmar = %scan(%bquote(&corresp),7,%str( ));
   %let ccolmar = %scan(%bquote(&corresp),8,%str( ));
 
   %if %length(%bquote(&crowcol)) = 0
       %then %let crowcol = %scan(%bquote(&typcor),1,%str( ));
   %if %length(%bquote(&ccolcol)) = 0
       %then %let ccolcol = %scan(%bquote(&typcor),2,%str( ));
   %if %length(%bquote(&crowfsy)) = 0
       %then %let crowfsy = %scan(%bquote(&typcor),3,%str( ));
   %if %length(%bquote(&ccolfsy)) = 0
       %then %let ccolfsy = %scan(%bquote(&typcor),4,%str( ));
   %if %length(%bquote(&crowsym)) = 0
       %then %let crowsym = %scan(%bquote(&typcor),5,%str( ));
   %if %length(%bquote(&ccolsym)) = 0
       %then %let ccolsym = %scan(%bquote(&typcor),6,%str( ));
   %if %length(%bquote(&crowmar)) = 0
       %then %let crowmar = %scan(%bquote(&typcor),7,%str( ));
   %if %length(%bquote(&ccolmar)) = 0
       %then %let ccolmar = %scan(%bquote(&typcor),8,%str( ));
 
   %let symbols = %bquote(&crowsym&ccolsym&symbols);
 
   %end;
 
%else %do;
   %let crowsym = ;   %let ccolsym = ;   %let crowmar = ;
   %let ccolmar = ;   %let crowcol = ;   %let ccolcol = ;
   %let crowfsy = ;   %let ccolfsy = ;
   %end;
 
*------augment symbols with all known symbols------;
%if %length(%bquote(&symbols)) %then %do;
   %let symbols = %bquote(&symbols&vector&circle&cirsym);
   %if %length(%bquote(&vector)) %then
      %let symcol = %bquote(&symcol &veccol);
   %if %length(%bquote(&circle)) %then
      %let symcol = %bquote(&symcol &circol &circol);
   %end;
 
*------break up veclab string------;
%if %bquote(&veclab) = typical %then %let veclab = %bquote(&typvec);
 
%if %length(%bquote(&veclab)) and
    %bquote(&veclab) ne blank %then %do;
 
   %let veclcol  = %scan(%bquote(&veclab),1,%str( ));
   %let veclsize = %scan(%bquote(&veclab),2,%str( ));
   %let veclfont = %scan(%bquote(&veclab),3,%str( ));
   %let veclmark = %scan(%bquote(&veclab),4,%str( ));
 
   %if %length(%bquote(&veclcol))  = 0
       %then %let veclcol  = %scan(%bquote(&typvec),1,%str( ));
   %if %length(%bquote(&veclsize)) = 0
       %then %let veclsize = %scan(%bquote(&typvec),2,%str( ));
   %if %length(%bquote(&veclfont)) = 0
       %then %let veclfont = %scan(%bquote(&typvec),3,%str( ));
   %if %length(%bquote(&veclmark)) = 0
       %then %let veclmark = %scan(%bquote(&typvec),4,%str( ));
 
   %end;
 
%else %do;
   %let veclmark = ;   %let veclcol  = ;
   %let veclfont = ;   %let veclsize = ;
   %end;
 
*------make sure symcol is long enough------;
%do i = 1 %to %length(%bquote(&symbols));
   %if %length(%qscan(&symcol,&i,%str( ))) = 0 %then
      %let symcol = &symcol %qscan(&symcol,%eval(&i - 1),%str( ));
   %end;
 
*------are we marking up labels?------;
%if %length(%bquote(&crowmar&ccolmar&veclmark))
   %then %let marklab = yes;
   %else %let marklab = no;
 
*-----set up plot request information------;
%if &makeplot = yes %then %do;
 
   *------set default plotvars------;
   %if %length(&plotvars) = 0 %then %do;
 
      %if %length(&plotreq) %then %do;
          %let plotvars = %scan(&plotreq,1,%str( *:$=/));
          %let plotvars = &plotvars %qscan(&plotreq,2,%str( *:$=/));
          %end;
 
      %else %if %index(%bquote(&datatype),mds) or
         %index(%bquote(&datatype),mca) or
         %index(%bquote(&datatype),row) or
         %index(%bquote(&datatype),column) or
         %length(%bquote(&corresp))
         %then %let plotvars = dim2 dim1;
         %else %if %length(%bquote(&datatype)) %then
            %let plotvars = prin2 prin1;
 
      %end;
 
   *------set default label variable------;
   %if %length(&labelvar) = 0 and %length(&plotreq) %then %do;
 
      %let i = %index(&plotreq,%str($));
      %if &i > 0 %then
         %let labelvar = %scan(%qsubstr(&plotreq,&i + 1),
                                1,%str( *:$=/));
      %else %do;
         %put ERROR: Label variable missing from plot request.;
         %let ok = no;
         %goto endit;
         %end;
 
      %end;
 
   *------set up default symbol------;
   %if %length(&symvar) = 0 %then %do;
      %if %length(&biplot) or %length(%bquote(&corresp))
         %then %let symvar = _SYMBOL_;
         %else %if %length(%bquote(&symbols))
         %then %let symvar = "%qsubstr(%bquote(&symbols),1,1)";
         %else %let symvar = "*";
      %end;
 
   %if %index(%bquote(&symvar),%str(%')) or
       %index(%bquote(&symvar),%str(%"))
      %then %let symtype = constant;
      %else %let symtype = variable;
 
   *------find first two numeric and last char var in data set------;
   %if %length(&plotreq) = 0 and
       (%length(&plotvars) = 0 or %length(&labelvar) = 0) %then %do;
 
      proc contents data=&data noprint out=&tempplot;
         run;
 
      %if &syserr > 4 %then %do;
         %let ok = no;
         %goto endit;
         %end;
 
      proc sort data=&tempplot(keep=varnum type name);
         by varnum;
         run;
 
      %if &syserr > 4 %then %do;
         %let ok = no;
         %goto endit;
         %end;
 
      data _null_;
         set &tempplot end=eof;
         retain set1 set2 setl '        ' nf;
         if name = '_name_' then nf = 1;
 
        if type = 1 then do;
            if set1 = ' ' then set1 = name;
            else if set2 = ' ' then set2 = name;
            end;
 
         else if type = 2 then do;
            if setl = ' ' or name ne '_TYPE_' then setl = name;
            end;
 
         if eof then do;
 
            %if %length(%bquote(&plotvars)) = 0 %then %do;
               if set2 = ' ' and set1 ne ' ' then set2 = set1;
               call symput('plotvars',trim(set2) || ' ' || trim(set1));
               %end;
 
            %if %length(%bquote(&labelvar)) = 0 %then %do;
               if setl = ' ' then setl = name;
               call symput('labelvar',trim(setl));
               %end;
 
            end;
 
         run;
 
      %if &syserr > 4 or &ok = no %then %do;
         %let ok = no;
         %goto endit;
         %end;
 
      %end;
 
   %if %length(%bquote(&plotvars)) = 0
      %then %let plotvars = prin2 prin1;
 
   %if (%length(%bquote(&corresp)) or
       %length(&biplot) or %length(&antiidea) or
       %length(%bquote(&veclab))) and
       %qupcase(&symvar) = %qupcase(&labelvar) %then %do;
      %let symtype = label;
      %let symvar  = _SYMBOL_;
      %put NOTE: Constructing temporary symbol variable _SYMBOL_.;
      %end;
 
   %end;
 
%let plotvar1 = %scan(%bquote(&plotvars),1,%str( ));
%let plotvar2 = %scan(%bquote(&plotvars),2,%str( ));
 
*------construct typical label------;
%if &makeplot = yes and %bquote(&label) = typical %then %do;
 
   %let ind1 = ;   %let ind2 = ;
 
   %let i = %length(%bquote(&plotvar1));
   %if &i < 2 %then %let ok = no;
   %if &ok = yes %then %do;
      %let x = %substr(&plotvar1,&i,1);
      %if %eval(not (1 <= &x and &x <= 9)) %then %let ok = no;
      %end;
 
   %let i = %length(%bquote(&plotvar2));
   %if &i < 2 %then %let ok = no;
   %if &ok = yes %then %do;
      %let x = %substr(%bquote(&plotvar2),&i,1);
      %if %eval(not (1 <= &x and &x <= 9)) %then %let ok = no;
      %end;
 
   %if &ok = no %then %do;
       %put ERROR: LABEL=typical requires a numeric suffix PLOTVAR=.;
       %goto endit;
       %end;
 
   %do i = %length(%bquote(&plotvar1)) %to 1 %by -1;
       %let x = %substr(%bquote(&plotvar1),&i,1);
       %if %eval(not (1 <= &x and &x <= 9)) %then %do;
          %let ind1 = %substr(%bquote(&plotvar1), &i + 1);
          %let i = 0;
          %end;
      %end;
 
   %do i = %length(%bquote(&plotvar2)) %to 1 %by -1;
       %let x = %substr(%bquote(&plotvar2),&i,1);
       %if %eval(not (1 <= &x and &x <= 9)) %then %do;
          %let ind2 = %substr(%bquote(&plotvar2), &i + 1);
          %let i = 0;
          %end;
      %end;
 
   *------for correspondence analysis,     ------;
   *------put inertia percentages in labels------;
   %let pin1 = .; %let pin2 = .;
   %if %length(%bquote(&corresp)) %then %do;
 
      data _null_;
         set &data;
         if _type_ = 'INERTIA' then do;
            contr&ind1 = 100.0 * contr&ind1 / inertia;
            contr&ind2 = 100.0 * contr&ind2 / inertia;
            %putvar('pin1',contr&ind1,6.2); /* percent intertia for y */
            %putvar('pin2',contr&ind2,6.2); /* percent intertia for x */
            end;
         stop;
         run;
 
      %if &syserr > 4 %then %do;
         %let ok = no;
         %goto endit;
         %end;
 
      %end;
 
   %if %length(%bquote(&corresp)) and
      &pin1 ne . and &pin2 ne . %then %do;
      %let label = label &plotvar1 = "Dimension &ind1 (&pin1%str(%%))";
      %let label = &label &plotvar2 = "Dimension &ind2 (&pin2%str(%%))";
      %let labely = "Dimension &ind1 (&pin1%str(%%))";
      %end;
 
   %else %do;
      %let label = label  &plotvar1 = "Dimension &ind1";
      %let label = &label &plotvar2 = "Dimension &ind2";
      %let labely = "Dimension &ind1";
      %end;
 
   %end;
 
*------set up search parameters------;
%if &makeplot = yes %then %do;
 
   %if %index(&place,search) and %index(&ls,search) %then %do;
      %let search = both;
      %let ls     = %scan(&ls,1,%str( ));
      %let place  = %scan(&place,1,%str( ));
      %end;
 
   %else %if %index(&ls,search) %then %do;
      %let search = ls;
      %let ls     = %scan(&ls,1,%str( ));
      %end;
 
   %else %if %index(&place,search) %then %do;
      %let search = place;
      %let place  = %scan(&place,1,%str( ));
      %end;
 
   %if %length(&ls) < 2 %then %let ls = 65;
   %if %length(&place) = 0 %then %let place = 1;
 
   %end;
 
%else %do;
   %if %index(&ls,search) %then %let ls = 200;
   %let place = %scan(&place,1,%str( ));
   %end;
 
*------check some parameters for valid values------;
data _null_;
   %check(tsize   ,      0 ,   100 );
   %check(lsize   ,      0 ,   100 );
   %check(ticklen ,      0 ,    10 );
   %check(rotate  ,      0 ,   100 );
   %check(nblanks ,      1 ,    10 );
   %check(inc     ,   1e-8 ,   1e9 );
   %check(antiidea,     -2 ,     2 );
   %check(nsegs   ,      1 ,   1e9 );
   %check(vtoh    ,      1 ,     3 );
   %check(makefit ,    0.1 ,    10 );
   %check(hsize   ,    0.1 ,   1e5 );
   %check(vsize   ,    0.1 ,   1e5 );
   %check(xmax    ,    0.1 ,   1e5 );
   %check(ymax    ,    0.1 ,   1e5 );
   %check(hpos    ,    0.1 ,   1e5 );
   %check(vpos    ,    0.1 ,   1e5 );
   %check(ls      ,     64 ,   200 );
   %check(ps      ,     15 ,   200 );
   %check(biplot  ,   1e-8 ,   100 );
   %check(headr   ,      0 ,    10 );
   %check(headw   ,      0 ,    10 );
   %check(place   ,      1 ,    15 );
   %check(maxiter ,      1 ,    50 );
   %check(lsinc   ,      1 ,   200 );
   run;
 
%if &ok = no %then %goto endit;
 
*------determine graphics area------;
goptions &gopts
   %if &method = print or &method = print2 %then %do;
      &gopprint;
      %let device = &gopprint;
      %end;
   %else %if &method=plot or &method=plot2 %then %do;
      &gopplot;
      %let device = &gopplot;
      %end;
	%else %do;
	  %str(;);
	  %end;
 
data _null_;
 
      rc1 = ginit();
      call gask('maxdisp',units,xmax,ymax,xpix,ypix,rc2);
      rc3 = gterm();
 
      if rc1 or rc2 or rc3 then do;
         put 'ERROR: GASK call for XMAX= and YMAX= failed.';
         call symput('ok','no');
         end;
 
      xmax = xmax * 100;
      ymax = ymax * 100;
 
      %if &unit = in %then %do;
         xmax = xmax / 2.54;
         ymax = ymax / 2.54;
         %end;
 
      %if %length(&xmax) = 0 %then %do;
         %putvar('xmax',xmax,9.2); /* max horizontal graph size */
         %end;
 
      %if %length(&ymax) = 0 %then %do;
         %putvar('ymax',ymax,9.2); /* max vertical graph size */
         %end;
 
   run;
 
%if &syserr > 4 or &ok = no %then %do;
   %let ok = no;
   %goto endit;
   %end;
 
*------debugging output------;
%if &debug ne no %then %do;
   %put method=&method..;         %put makeplot=&makeplot..;
   %put data=&data..;             %put out=&out..;
   %put gout=&gout..;             %put filepref=&filepref..;
   %put plotfile=&plotfile..;     %put post=&post..;
   %put procopts=&procopts..;     %put plotreq=&plotreq..;
   %put plotopts=&plotopts..;     %put place=&place..;
   %put label=&label..;           %put labely=&labely..;
   %put search=&search..;         %put vtoh=&vtoh..;
   %put ls=&ls..;                 %put ps=&ps..;
   %put restore=&restore..;       %put hsize=&hsize..;
   %put vsize=&vsize..;           %put unit=&unit..;
   %put makefit=&makefit..;       %put xmax=&xmax..;
   %put ymax=&ymax..;             %put hpos=&hpos..;
   %put vpos=&vpos..;             %put font=&font..;
   %put tsize=&tsize..;           %put lsize=&lsize..;
   %put ticklen=&ticklen..;       %put tickaxes=&tickaxes..;
   %put rotate=&rotate..;         %put remnote=&remnote..;
   %put center=&center..;         %put color=&color..;
   %put framecol=&framecol..;     %put titlecol=&titlecol..;
   %put labelcol=&labelcol..;     %put textcol=&textcol..;
   %put tickcol=&tickcol..;       %put veccol=&veccol..;
   %put circol=&circol..;         %put symcol=&symcol..;
   %put symcola=&symcola..;       %put format=&format..;
   %put symbols=&symbols..;       %put monochro=&monochro..;
   %put nblanks=&nblanks..;       %put colors=&colors..;
   %put inc=&inc..;               %put colreset=&colreset..;
   %put plotvars=&plotvars..;     %put plotvar1=&plotvar1..;
   %put plotvar2=&plotvar2..;     %put labelvar=&labelvar..;
   %put symvar=&symvar..;         %put symtype=&symtype..;
   %put marklab=&marklab..;       %put datatype=&datatype..;
   %put biplot=&biplot..;         %put corresp=&corresp..;
   %put crowsym=&crowsym..;       %put ccolsym=&ccolsym..;
   %put crowmar=&crowmar..;       %put ccolmar=&ccolmar..;
   %put crowcol=&crowcol..;       %put ccolcol=&ccolcol..;
   %put crowfsy=&crowfsy..;       %put ccolfsy=&ccolfsy..;
   %put antiidea=&antiidea..;     %put vector=&vector..;
   %put vechead=&vechead..;       %put veclab=&veclab..;
   %put veclmark=&veclmark..;     %put veclcol=&veclcol..;
   %put veclfont=&veclfont..;     %put veclsize=&veclsize..;
   %put headr=&headr..;           %put headw=&headw..;
   %put circle=&circle..;         %put cirsym=&cirsym..;
   %put radii=&radii..;           %put nsegs=&nsegs..;
   %put debug=&debug..;           %put singular=&singular..;
   %put gopprint=&gopprint..;     %put gopplot=&gopplot..;
   %put gopts=&gopts..;           %put tempplot=&tempplot..;
   %put y0=&y0..;                 %put x0=&x0..;
   %put fspace=&fspace..;
   %put fleft=&fleft..;           %put fright=&fright..;
   %put fcenter=&fcenter..;       %put fbreak=&fbreak..;
   %put fcolor=&fcolor..;         %put noteline=&noteline..;
   %put device=&device..;         %put ok=&ok..;
   %put blanks=&blanks..;         %put typfor=&typfor..;
   %put typcor=&typcor..;         %put typvec=&typvec..;
   %put notes=&notes..;           %put maxiter=&maxiter..;
   %put lsinc=&lsinc..;           %put sametick=&sametick..;
   %end;
 
*------pre-processing of data------;
%if &makeplot = yes %then %do;
 
   *------look at data to determine default increments------;
   %if %length(&inc) = 0 %then %do;
 
      proc means data=&data noprint;
         output out=&tempplot max=max1 max2 min=min1 min2;
         var &plotvar1 &plotvar2;
         run;
 
      %if &syserr > 4 %then %do;
         %let ok = no;
         %goto endit;
         %end;
 
      data _null_;
         set &tempplot;
 
         range = %if &sametick = no
                    %then max(max1 - min1, max2 - min2);
                    %else max(max1,max2) - min(min1,min2);;
 
         if range < &singular then range = &singular;
         inc = 10 ** ceil(log10(range) - 1.0);
         if range / inc >= 7.5 then inc = inc * 2;
         if range / inc <= 2.5 then inc = inc / 2;
         if range / inc <= 2.5 then inc = inc / 2;
         %putvar('inc',inc,best12.);
         run;
 
      %if &syserr > 4 %then %do;
         %let ok = no;
         %goto endit;
         %end;
 
      %if %length(&inc) = 0 %then %do;
         %put NOTE: Input data set is empty or corrupt.;
         %goto endit;
         %end;
 
      %end;
 
   *------pre-process the input data set, prinqual & transreg------;
   %if %length(&biplot) or %length(&antiidea) or
       %length(%bquote(&veclab)) %then %do;
 
      data &tempplot;
 
         %if &symtype ne constant %then length &symvar $ 1;;
         length _type_ $ 8;
 
         set &data;
 
         %if &symtype = label %then &symvar = &labelvar;;
 
         %if %length(%bquote(&veclmark)) %then %do;
            if index(&labelvar,"&veclmark") then do;
               put "WARNING: &labelvar= " &labelvar "conflicts "
                   "with label markup character: &veclmark..  "
                   "This character will be removed.";
               &labelvar = translate(&labelvar,' ',"&veclmark");
               end;
            %end;
 
         if _type_ in (&scores) then do;
            %if &symtype ne constant %then %do;
               if &symvar = ' ' then &symvar =
                  %if %length(%bquote(&symbols))
                     %then "%qsubstr(%bquote(&symbols),1,1)";
                     %else "*";;
               %end;
               end;
 
         else do;
 
            if index(_type_,'POINT') then do;
 
               %if %length(&antiidea) %then %do;
                  if _issq_ ne . and
                     ((&antiidea >  0 and _issq_ > 0) or
                      (&antiidea <= 0 and _issq_ < 0)) then do;
                     &plotvar1 = -(&plotvar1);
                     &plotvar2 = -(&plotvar2);
                     end;
                  %end;
 
               %if &symtype ne constant %then %do;
                  if &symvar = ' ' then &symvar =
                     %if %length(%bquote(&circle))
                        %then "&circle";
                        %else "=";;
                  %end;
 
               end;
 
            else do;
 
               %if %length(&biplot) %then %do;
                  &plotvar1 = &plotvar1 * &biplot;
                  &plotvar2 = &plotvar2 * &biplot;
                  %end;
 
               %if &symtype ne constant %then %do;
                  if &symvar = ' ' then &symvar =
                     %if %length(%bquote(&vector)) %then "&vector";
                                                   %else "#";;
                  %end;
 
               end;
 
            %if %length(%bquote(&veclmark)) %then
               &labelvar = translate(trim(&labelvar) || "&veclmark",
                                     "&veclmark", " ");
 
            %if %bquote(&veclab) = blank %then &labelvar = ' ';;
 
            end;
 
         run;
 
      %if &syserr > 4 %then %do;
         %let ok = no;
         %goto endit;
         %end;
 
      %let data = &tempplot;
 
      %if &debug = print %then %do;
         proc print;
            run;
         %end;
 
      %end;
 
   *------pre-process the input data set, corresp------;
   %if %length(%bquote(&corresp)) %then %do;
 
      data &tempplot;
 
         length &labelvar $ 40;
         %if &symtype ne constant %then length &symvar $ 1;;
 
         set &data;
         %if &symtype = label %then &symvar = &labelvar;;
 
         if indexc(&labelvar,"&crowmar&ccolmar") then do;
            put "WARNING: &labelvar= " &labelvar "conflicts "
                "with correspondence analysis row or column "
                "markup characters: &crowmar &ccolmar..  "
                "These characters will be removed.";
            &labelvar = translate(&labelvar,'  ',"&crowmar&ccolmar");
            end;
 
         if _type_ in (&scores) then do;
            &labelvar = translate(trim(&labelvar) || "&crowmar",
                                  "&crowmar"," ");
            %if &symtype ne constant %then
               if &symvar = ' ' then &symvar = "&crowsym";;
            end;
 
         else do;
            &labelvar = translate(trim(&labelvar) || "&ccolmar",
                                  "&ccolmar", " ");
            %if &symtype ne constant %then
               if &symvar = ' ' then &symvar = "&ccolsym";;
            end;
         run;
 
      %if &syserr > 4 %then %do;
         %let ok = no;
         %goto endit;
         %end;
 
      %let data = &tempplot;
 
      %if &debug = print %then %do;
         proc print;
            run;
         %end;
 
      %end;
 
   %end;
 
*------debugging output------;
%if &debug ne no %then %do;
   %put inc=&inc..;
   %end;
 
*------printer plot file------;
filename rawplot "&plotfile" lrecl=204 blksize=6124;
 
*------set up iteration parameters------;
%let done    = 0;
%let iternum = 0;
 
*------create printer plot------;
%if &makeplot = yes %then %do %while(&done = 0);
 
   %let iternum = %eval(&iternum + 1);
   %if &ls > 200 %then %let ls = 200;
   %if %length(&ps) %then %let actualps = &ps;
   %else %let actualps = %eval(10 + ((5 * (&ls)) / 8));
   %if &actualps > 200 %then %let actualps = 200;
 
   options ls=&ls ps=&actualps;
 
   %let holdreq = &plotreq;
 
   *------construct default plot request------;
   %if %length(&plotreq) = 0 %then
      %let plotreq = &plotvar1 * &plotvar2 $ &labelvar = &symvar;
 
   *------find slash, insert slash if necessary------;
   %let l = %index(%bquote(&plotreq),'/');
   %if &l = 0 %then %let l = %index(%bquote(&plotreq),"/");
 
   %if &l > 0 %then %do;
      %if %length(&plotreq) > %eval(&l + 3)
         %then %let l = %index(%qsubstr(%bquote(&plotreq),
                               %eval(&l + 3)),/);
         %else %let l = 0;
      %end;
 
   %else %let l = %index(%bquote(&plotreq),/);
 
   %if &l = 0 %then %do;
      %let plotreq = &plotreq /;
      %let l = %length(&plotreq);
      %end;
 
   *------sametick specified, overlay transpose with null symbols------;
   %if &sametick ne no %then %do;
      %let i       = %substr(&plotreq,&l) overlay;
      %let plotreq = %substr(&plotreq, 1, &l - 1);
      %let plotreq = &plotreq &plotvar2 * &plotvar1 = ' ' &i;
      %let l = %eval(&l - 1 + %index(%qsubstr(&plotreq,&l),/));
      %end;
 
   *------add BOX option, plot request options------;
   %if %length(&plotopts) %then %let plotreq = &plotreq &plotopts;
   %let plotreq = &plotreq box;
 
   *------see if we need to generate a placement list------;
   %if %index(%qsubstr(%bquote(&plotreq),&l),%str(place=)) or
       %index(%qsubstr(%bquote(&plotreq),&l),%str(placement=))
       %then %do;
      %let place = ;
      %if &search = both %then %let search = ls;
      %else %if &search = place %then %let search = no;
      %end;
 
   *------construct placement list------;
   %let plotreq = &plotreq %place(&place);
 
   *------add tick increments------;
   %if %index(%qsubstr(%bquote(&plotreq),&l),haxis) = 0
      %then %let plotreq = &plotreq haxis=by &inc;
   %if %index(%qsubstr(%bquote(&plotreq),&l),vaxis) = 0
      %then %let plotreq = &plotreq vaxis=by &inc;
 
   *------add list= option------;
   %if %index(%qsubstr(%bquote(&plotreq),&l),list) = 0 %then %do;
      %if &iternum < &maxiter and &search ne no
         %then %let plotreq = &plotreq list=1;
      %end;
   %else %if %index(%qsubstr(%bquote(&plotreq),&l),%str(list=0)) or
             %index(%qsubstr(%bquote(&plotreq),&l),%str(list ))
      %then %let search = no;
 
   *------print iteration history line------;
   %put NOTE: Iteration=&iternum, place=&place, ls=&ls, ps=&actualps..;
 
   *------debugging output------;
   %if &debug ne no %then %put plotreq=&plotreq..;
 
   *------write plot to file------;
   options nonumber;
 
   proc printto print=rawplot new; run;
 
   %if &syserr > 4 %then %do;
      %let ok = no;
      %goto endit;
      %end;
 
   proc plot &procopts data=&data vtoh=&vtoh;
      plot &plotreq;
      &label;
      run; quit;
 
   %let i = &syserr;
 
   options number;
 
   proc printto print=print; run;
 
   %if &i > 4 or &syserr > 4 %then %do;
      %let ok = no;
      %goto endit;
      %end;
 
   %if &method = plot %then %do;
 
      *------write plot to screen------;
      proc plot &procopts data=&data vtoh=&vtoh;
         plot &plotreq;
         &label;
         run; quit;
 
      %if &syserr > 4 %then %do;
         %let ok = no;
         %goto endit;
         %end;
 
      %end;
 
   *------if we are iterating, see if we are done------;
   %let done = 1;
   %if &search ne no %then %do;
 
      *------search for header that means nonoptimal placements------;
      data _null_;
         infile rawplot end=last pad lrecl=204 blksize=6124;
         input line $char&ls..;
         if index(line,"&listtitl") then do;
            call symput('done','0');
            stop;
            end;
         run;
 
      %if &syserr > 4 %then %do;
         %let ok = no;
         %goto endit;
         %end;
 
      %if &iternum >= &maxiter %then %let done = 1;
 
      *------get set up for the next iteration------;
      %if &done = 0 %then %do;
         %let plotreq  = &holdreq;
 
         *------adjust placement list------;
         %if &search = both or &search = place %then
            %let place = %eval(&place + 1);
 
         *------adjust line size------;
         %if &search = both or &search = ls %then %do;
            %let ls = %eval(&ls + &lsinc);
            %if &ls > 200 %then %let ls = 200;
            %end;
 
         *------last iteration if we cannot increment------;
         *------the parameters any more.             ------;
         %if (&search = both and &ls >= 200 and &place >= 15) or
             (&search = ls and &ls >= 200) or
             (&search = place and &place >= 15)
             %then %let maxiter = &iternum;
 
         %end;
 
      %end;
 
   *------print final plot request------;
   %if &done = 1 %then %do;
 
      options &restore;
 
      %put %str ( );
      %put NOTE: The following plot request was used:;
      %put %str( );
      %put &plotreq;
      %put %str ( );
 
      %end;
 
   *------debugging output------;
   %if &debug ne no %then %do;
      %put plotreq=&plotreq..;    %put place=&place..;
      %put search=&search..;      %put iternum=&iternum..;
      %put ls=&ls..;
      %end;
 
   *------skip graphics if just making printer plot------;
   %if &method = plot and &done = 1 %then %goto endit;
 
   %end;
 
*------post-process plot, step 1------;
data &out(keep=xtick ytick comment);
   length line hrefs title $ &ls comment $ 25;
   retain coln top bottom afterbot noteline nlines x0  0
          nvrefs                                      -2
          right left col1 rotate                     &ls
          hrefs title                                ' ';
   infile rawplot end=last pad lrecl=204 blksize=6124;
 
   if _n_ = 1 then input @2 line $char&ls..; /* skip form feed */
   else            input    line $char&ls..;
 
   length = length(line);
   coln   = max(coln,length); /* last nonblank column */
 
   *------find vertical axis (horizontal) reference lines------;
   if index(line,'--') then do;
      bottom = _n_;           /* last line will be bottom line */
      nvrefs = nvrefs + 1;    /* n of horizontal ref lines     */
      if top = 0 then do;     /* top box line of plot          */
         top = _n_;
         left   = index(line,'-');  /* left axis coordinate    */
         right  = length;           /* right axis coordinate   */
         end;
      end;
 
   *------flaglines after the bottom line of the plot------;
   if _n_ > top and afterbot = 0 and
      substr(line,left,1) = '-' and substr(line,right,1) = '-'
      then afterbot = 1;
   else if afterbot then afterbot = afterbot + 1;
 
   *------find horizontal axis ticks------;
   if afterbot = 1 then do;
      comment = 'frame, tick';
      ytick   = .;
      do i = left to right;     /* output tick mark coords. */
         if substr(line,i,1) = '+' then do;
            xtick = i;
            output;
            end;
         end;
      end;
 
   *------find first line to exclude------;
   if afterbot > 2 and noteline = 0 and
      (index(line,'NOTE:') or index(line,"&listtitl"))
      then noteline = _n_;
 
   *------remove form feeds from titles------;
   if afterbot > 2 and substr(line,2) = title then do;
      line = substr(line,2);
      if noteline = 0 then noteline = _n_;
      end;
 
   *------remove note line------;
   if "&remnote" = "yes" and noteline > 0 and
      _n_ >= noteline then line = ' ';
 
   *------find last line------;
   if line ne ' ' then nlines = _n_;
 
   *------find horizontal axis (vertical) reference lines------;
   do i = 1 to &ls;
      c  = substr(line,i,1);
 
      if c = '|' then do;
         substr(hrefs,i,1) = '|';
         end;
 
      if c ne ' ' then do;
         col1 = min(col1,i); /* first nonblank col */
         if top and i < left and i < rotate and
            substr(line,i+1,3) = '   ' and indexc(line,'|+')
            then rotate = i;
         end;
 
      end;
 
   *------output vertical axis tick mark coordinates------;
   if top and substr(line,left,1) = '+' then do;
      xtick   = .;
      ytick   = _n_;
      comment = 'frame, tick';
      output;
      end;
 
   *------output coordinates of circles------;
   %if %length(%bquote(&circle)) %then %do;
      comment = 'circle';
      if top then do i = left + 1 to right - 1;
         if substr(line,i,1) = "&circle" then do;
            ytick = _n_;
            xtick = i;
            output;
            end;
         end;
      %end;
 
   *------output coordinates of vectors------;
   %if %length(%bquote(&vector)) %then %do;
      comment = 'vector';
      if top then do i = left + 1 to right - 1;
         if substr(line,i,1) = "&vector" then do;
            ytick = _n_;
            xtick = i;
            output;
            end;
         end;
 
      *------find (approximate) zero point on x-axis------;
      if afterbot = 2 then do;
         i = 1;
         do while(scan(line,i,' ') ne ' ');
            if input(scan(line,i,' '),?? 12.) = 0 then do;
               x0 = index(line, ' ' || scan(line,i,' ') || ' ');
               i  = right;
               end;
            i = i + 1;
            end;
         end;
 
      %end;
 
   *------store first title line------;
   if _n_ = 1 then title = line;
 
   *------on last observation, output results to macro variables------;
   if last then do;
 
      *------horizontal axis reference lines------;
      nhrefs = 0;
      do i = 1 to &ls;
         if i ne left and i ne right and
            substr(hrefs,i,1) = '|' then do;
            nhrefs = nhrefs + 1;
            %putvar(compress('href'||put(nhrefs,5.0)),i - col1 + 1,5.0);
            end;
         end;
 
      *------size of plot parameters------;
      x0     = max(x0 - col1 + 1, 0);
      ls     = coln   - col1 + 1;
      left   = left   - col1 + 1;
      right  = right  - col1 + 1;
      rotate = rotate - col1 + 1;
      hpos   = ls + 0.5 * (1.0 + (&ticklen));
      vpos   = nlines;
      y0     = rotate;
      if y0 >= left then y0 = 0;
 
      %if %length(&hpos) %then %do;
         if &hpos < hpos then do;
            m1 = -1;
            put "ERROR: HPOS=&hpos is to small.  "
                "The default minimum for this plot is " hpos +m1 ".";
            call symput('ok','no');
            end;
         else hpos = &hpos;
         %end;
 
      %if %length(&vpos) %then %do;
         if &vpos < vpos then do;
            m1 = -1;
            put "ERROR: VPOS=&vpos is to small.  "
                "The default minimum for this plot is " vpos +m1 ".";
            call symput('ok','no');
            end;
         else vpos = &vpos;
         %end;
 
      *------determine hsize and vsize------;
      %if %length(&vsize) and %length(&hsize) = 0 %then %do;
         vsize = &vsize;
         hsize = hpos * vsize / (vpos * (&vtoh));
         %end;
 
      %else %do;
         hsize = %if %length(&hsize) = 0
                     %then &xmax;
                     %else &hsize;;
         vsize = %if %length(&vsize) = 0
                     %then (vpos / hpos) * hsize * (&vtoh);
                     %else &vsize;;
         %end;
 
      *-------scale sizes so plot fits in specified area------;
      %if %length(&makefit) %then %do;
 
         scale = max(hsize / (&makefit * (&xmax)),
                     vsize / (&makefit * (&ymax)));
 
         if scale > 1.0 then do;
            hsize = hsize / scale;
            vsize = vsize / scale;
            end;
 
         %end;
 
      *------add extra positions to center plot------;
      %if &center = yes %then %do;
         hposoff = max(0.5 * ((&xmax) - hsize) / (hsize / hpos), 0.0);
         vposoff = max(0.5 * ((&ymax) - vsize) / (vsize / vpos), 0.0);
         hsize   = &xmax;
         vsize   = &ymax;
         %end;
 
      %else %do;
         vposoff = 0;
         hposoff = 0;
         %end;
 
      *------override default positions------;
      %if %length(&hpos) = 0 %then hpos = hpos + 2.0 * hposoff%str(;);
                             %else hposoff = 0;;
      %if %length(&vpos) = 0 %then vpos = vpos + 2.0 * vposoff%str(;);
                             %else vposoff = 0;;
 
      *------column from which y axis label is extracted------;
      %if %length(&rotate) %then rotate = &rotate;;
      if rotate >= left then rotate = 0;
 
      *------make noteline first line to ignore in plot------;
      if noteline = 0 then noteline = nlines + 1;
 
      *------output plot size parameters------;
      %putvar('x0'      ,x0      ,5.0);   /* column of the origin   */
      %putvar('y0'      ,y0      ,5.0);   /* label column           */
      %putvar('col1'    ,col1    ,5.0);   /* first column with data */
      %putvar('ls'      ,ls      ,5.0);   /* actual line size       */
      %putvar('nlines'  ,nlines  ,5.0);   /* number of lines        */
      %putvar('nvrefs'  ,nvrefs  ,5.0);   /* n of ver. ref lines    */
      %putvar('nhrefs'  ,nhrefs  ,5.0);   /* n of hor. ref lines    */
      %putvar('top'     ,top     ,5.0);   /* top boundary index     */
      %putvar('bottom'  ,bottom  ,5.0);   /* bottom boundary index  */
      %putvar('left'    ,left    ,5.0);   /* left boundary index    */
      %putvar('right'   ,right   ,5.0);   /* right boundary index   */
      %putvar('rotate'  ,rotate  ,5.0);   /* label column guess     */
      %putvar('noteline',noteline,5.0);   /* line with hidden note  */
      %putvar('hposoff' ,hposoff ,7.1);   /* hor. offset to center  */
      %putvar('vposoff' ,vposoff ,7.1);   /* ver. offset to center  */
      %putvar('hpos'    ,hpos    ,9.2);   /* goptions hpos= value   */
      %putvar('vpos'    ,vpos    ,9.2);   /* goptions vpos= value   */
      %putvar('hsize'   ,hsize   ,9.2);   /* horizontal graph size  */
      %putvar('vsize'   ,vsize   ,9.2);   /* vertical graph size    */
      end;
 
   return;
   run;
 
%if &syserr > 4 or &ok = no %then %do;
   %let ok = no;
   %goto endit;
   %end;
 
%if &debug = print %then %do;
   proc print;
      run;
   %end;
 
*------debugging output------;
%if &debug ne no %then %do;
   %put noteline=&noteline..;     %put col1=&col1..;
   %put ls=&ls..;                 %put nlines=&nlines..;
   %put hpos=&hpos..;             %put vpos=&vpos..;
   %put hposoff=&hposoff..;       %put vposoff=&vposoff..;
   %put hsize=&hsize..;           %put vsize=&vsize..;
   %put nvrefs=&nvrefs..;         %put nhrefs=&nhrefs..;
   %put top=&top..;               %put bottom=&bottom..;
   %put left=&left..;             %put right=&right..;
   %put rotate=&rotate..;         %put x0=&x0..;
   %end;
 
%if &ls <= 0 %then %do;
   %put ERROR: Plot is corrupt.;
   %let ok = no;
   %goto endit;
   %end;
 
*------post-process plot, step 2: create text annotate data set------;
data &tempplot(keep=function x y text angle position
               comment symbol color);
 
   length text line $ %eval(&ls+2);
   length function color $ 8 symbol position $ 1 comment $ 25;
   retain nvrefs 0 color '        ';
 
   infile rawplot end=last pad lrecl=204 blksize=6124;
 
   comment  = 'text';
   function = 'LABEL';
 
   *------read data, skip form feed on first line------;
   if _n_ = 1 then input @%eval(&col1 + 1) line $char&ls..;
   else            input @&col1            line $char&ls..;
 
   *------is this an axis or reference line?------;
   istop = (_n_ = &top);
   isbot = (_n_ = &bottom);
   isref = (not istop and not isbot and index(line,'--'));
 
   *------remove note line------;
   if "&remnote" = "yes" and _n_ >= &noteline then line = ' ';
 
   %if %length(%bquote(&vector)) %then %do;
 
      *------find (exact) zero point on x-axis------;
      if isbot and &x0 > 0 then do;
         do i = &x0 to &right until(substr(line,i,1) = '+');
            end;
         if i >= &right then i = 0;
         %putvar('x0',i,5.0);   /* column of the origin   */
         end;
 
      *------find zero point on y-axis------;
      j = 1 %if &y0 ne 0 %then %str(+ &y0);;
      if (&top < _n_ < &bottom) and
         input(left(substr(line,j,&left - j)),?? 12.) = 0 then do;
         %putvar('y0',_n_,5.0); /* row of origin */
         end;
 
      %end;
 
   *------rotate the y axis label------;
   %if &rotate > 0 %then %do;
      length ylabel $ &noteline;
      retain ylabel ' ';
 
      if &top <= _n_ < &noteline then do;
         substr(ylabel,_n_,1)   = substr(line,&rotate,1);
         substr(line,&rotate,1) = ' ';
         end;
 
      if last then do;
         y     = ((2 * &nlines) - &top - &bottom) / 2;
         x     = &rotate;
         text  = %if %length(&labely) %then
                 %then &labely;
                 %else translate(left(ylabel),'  ',"&fbreak&fspace");;
         angle = 90;
         output;
         end;
      %end;
 
   angle = 0;
 
   *------remove axis and reference lines------;
   if &top <= _n_ <= &bottom then do;
      substr(line,&left ,1) =
            translate(substr(line,&left ,1),'  ','-+');
      substr(line,&right,1) =
            translate(substr(line,&right,1),'  ','-+');
      end;
 
   if isbot or isref then
      substr(line,&left,&right - &left + 1) =
            translate(substr(line,&left,&right - &left + 1),'  ','-+');
   if istop then substr(line,&left,&right - &left + 1) = ' ';
   line = translate(line,' ','|');
 
   *------output coordinates of vertical axis reference lines------;
   if isref then do;
      nvrefs = nvrefs + 1;
      %putvar(compress('vref'||put(nvrefs,5.0)),&nlines - _n_,5.0);
      end;
 
   *------reset colors for this line?------;
   %if &colreset = line %then color = ' ';;
 
   *------output labels, symbols, tick values, and so on------;
   y = &nlines - _n_;
   i = 1;
   do while(i <= &ls);
 
      text = substr(line,i,1);
      len  = 1;
      skip = 0;
 
      *------isolate and process one label------;
      if not (text = ' ' or text = "&fbreak") then do;
 
         position = ' ';
         symbol   = ' ';
 
         *------reset colors for this label?------;
         %if &colreset = label %then color = ' ';;
 
         *------check for format strings------;
         %if %length(%bquote(&fleft&fright&fcenter)) %then %do;
 
            if indexc(text,"&fleft&fright&fcenter") then do;
 
               *------set label position------;
               i = i + 1;
               if      text = "&fright" then position = '>';
               else if text = "&fleft"  then position = '<';
               else                          position = '+';
               end;
 
            %end;
 
         *------try to guess how to position label------;
         %if %length(%bquote(&symbols)) %then %do;
 
            %let labend1 = %bquote(&symbols&fleft&fright);
            %let labend1 = %bquote(&labend1&fcenter&fbreak);
            %let labend2 = %bquote(&crowmar&ccolmar&veclmark);
 
            if &top < _n_ < &bottom then nblanks = &nblanks;
            else                         nblanks = 2;
 
            *------find full label                  ------;
            *------len is index, not true length yet------;
 
            *------handle label inside the plot------;
            if i >= &left and (&top < _n_ < &bottom) then do;
 
               len    = i;
               isasym = indexc("&symbols",text);
 
               if isasym = 0 then do;
                  do len = i + 1 to &ls until(done);
 
                     *------done when we find nblanks blanks in a------;
                     *------row.  Also done if we find one of the------;
                     *------corresp or vector markups, unless it ------;
                     *------is both followed and preceded by a   ------;
                     *------non-blank.                           ------;
                     j    = min(&ls - len + 1, nblanks);
                     done = substr(line,len,j) = ' ' or
                            indexc("&labend1&labend2",
                                   substr(line,len,1));
 
                     %if %length(%bquote(&labend2)) %then %do;
                        if done and
                           index("&labend2",substr(line,len,1)) and
                           substr(line,len - 1, 1) ne ' ' and
                           substr(line,len + 1, 1) ne ' ' then done = 0;
                        %end;
 
                     end;
 
                  end;
 
               *------check for trailing symbol, markup------;
               if len > i and indexc("&labend1",substr(line,len,1))
                  then len = len - 1;
 
               *------check to see if we got a vector or------;
               *------corresp marker from the next label------;
               %if %length(%bquote(&labend2)) %then %do;
                  if (len - i) > 1 and
                     index("&labend2",substr(line,len,1)) and
                     substr(line,len - 1, 1) = ' '
                     then len = len - 2;
                  %end;
 
               *------isolate label, compute length------;
               text = substr(line,i, len - i + 1);
               len  = length(text);
 
               *------make sure we do not have two pieces------;
               %if %length(%bquote(&labend2)) %then %do;
                  j = index(reverse(trim(text)),' ');
                  k = index("&labend2",substr(text,len,1));
 
                  *------imbedded blank and markup means------;
                  *------break at the last blank        ------;
                  if j and k then do;
                     len  = len - j;
                     text = substr(text,1,len);
                     end;
 
                  *------if the text contains both a corresp------;
                  *------row and column marker, break up the------;
                  *------the string until just one kind is  ------;
                  *------left in the string.                ------;
                  %if %length(%bquote(&corresp)) %then %do;
 
                     else if k then do;
 
                        k = index(text,"&crowmar");
                        j = index(text,"&ccolmar");
 
                        do while(j and k);
                           len  = len - indexc(reverse(
                                  substr(text,1,len-1)),"&labend2");
                           text = substr(text,1,len);
                           k    = index(text,"&crowmar");
                           j    = index(text,"&ccolmar");
                           end;
 
                        end;
 
                     %end;
                  %end;
 
               * put i= len= text= position=;
 
               *------check labels for right, left justification------;
               if isasym = 0 then do;
 
                  *------check for symbol before label------;
                  if position = ' ' and (i - 2) > &left and
                     indexc(substr(line,i - 2, 2),"&symbols") then do;
                     position  = '>';
                     symbol    = substr(line,i - 2,1);
                     if symbol = ' ' then symbol = substr(line,i - 1,1);
                     end;
 
                  *------check for symbol after label------;
                  if position = ' ' and (i + len + 2) <= &right and
                     indexc(substr(line,i + len, 2),"&symbols") then do;
                     position  = '<';
                     symbol    = substr(line,i + len,1);
                     if symbol = ' ' then
                        symbol = substr(line,i + len + 1, 1);
                     end;
 
                  *------check to see if we are near left edge------;
                  if position = ' ' and ((i - &left) = 1 or
                     ((i - &left) = 2 and
                     substr(line,&left + 1, 1) = ' '))
                     then position = '>';
 
                  *------check to see if we are near right edge------;
                  if position = ' ' and (&right = (i + len) or
                     ((&right - (i + len)) = 1 and
                     substr(line,&right - 1,1) = ' '))
                     then position = '<';
 
                  end;
 
               else do;
 
                  *------replace symbols for vectors------;
                  %if %length(%bquote(&vector)) %then %do;
                     text = translate(text,"&vechead","&vector");
                     %end;
 
                  *------replace symbols for circles------;
                  %if %length(%bquote(&circle)) %then %do;
                     text = translate(text,"&cirsym","&circle");
                     %end;
 
                  end;
 
               end;
 
            *------handle title lines------;
            else if _n_ < &top then do;
               text = left(line);
               len  = length(text);
               i    = &ls + 1;
               end;
 
            *------handle label outside the plot, not title------;
            else do;
 
               do len = i + 1 to &ls until(done);
                  j    = min(&ls - len + 1, nblanks);
                  done = substr(line,len,j) = ' ' or
                         indexc("&labend1",substr(line,len,1));
                  end;
 
               text = substr(line,i,len - i + 1);
               len  = length(text);
 
               end;
 
            * put i= len= text= position=;
 
            %end;
 
         %else %do;
            *------locate one character at a time------;
            if position = ' ' then position = '+';
            %end;
 
         *------handle color markups------;
         %if %length(%bquote(&fcolor)) %then %do;
 
            l = index(text,"&fcolor");
            do while(l);
 
               if l = 1 then do;
                  i = i + 2;
                  len = len - 2;
                  end;
 
               color = substr(text,l+1,1);
               colnm = index('123456789abcdefghijklmnopqrstuvwxyz',
                             compress(color));
 
               if colnm then do;
                  substr(text,l,2) = '  ';
                  do until(color ne ' ' or colnm < 1);
                     color = scan("&colors",colnm,' ');
                     colnm = colnm - 1;
                     end;
                  end;
 
               else do;
                  substr(text,l,1) = ' ';
                  color = ' ';
                  i = i + 1;
                  end;
 
               text = left(text);
               skip = len - length(text);
               len  = len - skip;
               l    = index(text,"&fcolor");
 
               end;
 
           %end;
 
         *------get rid of breaks and hard coded blanks------;
         text = translate(text,'  ',"&fspace&fbreak");
 
         *------compute x coordinate------;
         if      position = '>' then x = i;
         else if position = '<' then x = i +  len - 1;
         else                        x = i + (len - 1) / 2;
 
         * put i= len= text= position=;
 
         *------output label------;
         if text ne ' ' then output;
 
         end;
 
      * put i= len= text= skip=;
 
      i = i + len + skip;
 
      end;
 
   run;
 
%if &syserr > 4 %then %do;
   %let ok = no;
   %goto endit;
   %end;
 
%if &debug = print %then %do;
   proc print;
      run;
   %end;
 
*------switch top and bottom to annotate coordinates------;
%let top    = %eval(&nlines - &top);
%let bottom = %eval(&nlines - &bottom);
 
*------make goptions statement------;
%let gopts = &gopts hpos=&hpos vpos=&vpos;
%let gopts = &gopts hsize=&hsize&unit vsize=&vsize&unit;
%if &method = print or &method = print2
   %then %let gopts = &gopts &gopprint;
   %else  %if &method=plot or &method=plot2 %then %let gopts = &gopts &gopplot;
%let comlen = %length(&gopts);
 
*------debugging output------;
%if &debug ne no %then %do;
   %put y0=&y0..;                 %put x0=&x0..;
   %put top=&top..;               %put bottom=&bottom..;
   %put gopts=&gopts..;           %put comlen=&comlen..;
   %end;
 
*------make sure the origin was found------;
%if %length(%bquote(&vector))
    and not (&x0 > &left   and &x0 < &right and
             &y0 > &bottom and &y0 < &top) %then %do;
   %put ERROR: (0,0) coordinate not found.;
   %let ok = no;
   %goto endit;
   %end;
 
*------create annotate data set with lines and circles------;
data &out(keep=function x y comment);
 
   length comment $ &comlen function $ 8;
 
   if _n_ = 1 then do;
 
      *------draw frame------;
      comment = 'frame, box';
      %line(&left,&top,&right,&top);
      x = &right; y = &bottom; function = 'DRAW '; output;
      x = &left;  y = &bottom; function = 'DRAW '; output;
      x = &left;  y = &top;    function = 'DRAW '; output;
 
      *------draw vertical reference lines------;
      comment = 'frame, vertical ref';
      %do i = 1 %to &nvrefs;
          %line(&left, &&vref&i, &right, &&vref&i);
          %end;
 
      *------draw horizontal reference lines------;
      comment = 'frame, horizontal ref';
      %do i = 1 %to &nhrefs;
          %line(&&href&i, &top, &&href&i, &bottom);
          %end;
 
      end;
 
   set &out;
 
   *------adjust for blank cols------;
   if xtick ne . then xtick = xtick - &col1 + 1;
 
   halftick = 0.5 * (&ticklen);
 
   *------draw y-axis tick marks------;
   if xtick = . then do;
 
      %if %index(&tickaxes,l) %then %do;
         %line(&left  - halftick, &nlines - ytick,
               &left  + halftick, &nlines - ytick);
         %end;
 
      %if %index(&tickaxes,r) %then %do;
         %line(&right - halftick, &nlines - ytick,
               &right + halftick, &nlines - ytick);
         %end;
 
      end;
 
   *------draw x-axis tick marks------;
   else if ytick = . then do;
      halftick = halftick / &vtoh;
 
      %if %index(&tickaxes,t) %then %do;
         %line(xtick, &top    + halftick, xtick, &top    - halftick);
         %end;
 
      %if %index(&tickaxes,b) %then %do;
         %line(xtick, &bottom + halftick, xtick, &bottom - halftick);
         %end;
 
      end;
 
   *------draw vectors------;
   else if comment = 'vector' then do;
      %line(&x0, &nlines - &y0, xtick, &nlines - ytick);
 
      %if %length(&headw) %then %do;
         comment  = 'vector, head';
 
         *------compute slope of vector------;
         vecslope = xtick - &x0;
         if abs(vecslope) > &singular then
            vecslope = (&y0 - ytick) / (vecslope / (&vtoh));
         else vecslope = .;
 
         *------find point on vector headr distance from end------;
         %linept(xtick,&nlines - ytick,headx,heady,vecslope,-&headr);
 
         *------slope of line perpendicular to vector------;
         if vecslope = . then vecslope = 0;
         else if abs(vecslope) > &singular
            then vecslope = -1.0 / vecslope;
         else vecslope = .;
 
         *------draw vector head------;
         %linept(headx,heady,xonvec,yonvec,vecslope,-&headw);
         x = xonvec; y = yonvec; function = 'DRAW '; output;
         %linept(headx,heady,xonvec,yonvec,vecslope,&headw);
         %line(xtick,&nlines - ytick,xonvec,yonvec);
         %end;
 
      end;
 
   *------draw circles------;
   else if comment = 'circle' then do;
 
      ytick  = &nlines - ytick;
 
      do radius = &radii;
 
         comment  = 'circle, ' || put(radius,best5.)  ||
                    ', (' || put(xtick,best5.) || ',' ||
                    put(ytick,best3.) || ')';
         inc      = 2.0 * (&pi) / ceil(log10(25 + radius) * (&nsegs));
         justdraw = 0; /* 1 means DRAW without MOVE */
         seg0     = inc * &singular;
 
         do i = -&pi + inc to &pi by inc;
 
            xtop    = .;
            xbottom = .;
            yleft   = .;
            yright  = .;
 
            *------coordinates for a line segment------;
            x1 = xtick + sin(i - inc) * radius;
            y1 = ytick + cos(i - inc) * radius / (&vtoh);
            x2 = xtick + sin(i      ) * radius;
            y2 = ytick + cos(i      ) * radius / (&vtoh);
 
            *------find where line crosses edges------;
            b  = (x2 - x1);
            bi = (y2 - y1);
 
            if abs(b) > &singular then do;
               b       = (y2 - y1) / b;
               a       = y1 - b * x1;
               yleft   = b * (&left) + a;
               yright  = b * (&right) + a;
               end;
 
            if abs(bi) > &singular then do;
               bi      = (x2 - x1) / bi;
               xtop    = ((&top) - y1) * bi + x1;
               xbottom = ((&bottom) - y1) * bi + x1;
               end;
 
            *------adjust coordinates when line crosses edges------;
            if x1 < &left then do;
               x1 = &left; y1 = yleft; justdraw = 0;
               end;
 
            if x1 > &right then do;
               x1 = &right; y1 = yright; justdraw = 0;
               end;
 
            if y1 < &bottom then do;
               y1 = &bottom; x1 = xbottom; justdraw = 0;
               end;
 
            if y1 > &top then do;
               y1 = &top; x1 = xtop; justdraw = 0;
               end;
 
            if x2 < &left then do;
               x2 = &left; y2 = yleft;
               end;
 
            if x2 > &right then do;
               x2 = &right; y2 = yright;
               end;
 
            if y2 < &bottom then do;
               y2 = &bottom; x2 = xbottom;
               end;
 
            if y2 > &top then do;
               y2 = &top; x2 = xtop;
               end;
 
            *------see if segment should be output------;
            if x1 ne . and x2 ne . and y1 ne . and y2 ne . and
               sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2) > seg0 then do;
 
               *------draw from previous location?------;
               if justdraw then do;
                  x = x2; y = y2;
                  function = 'DRAW '; output;
                  end;
 
               *------move and draw------;
               else do;
                  %line(x1,y1,x2,y2);
                  justdraw = 1;
                  end;
               end;
 
            else justdraw = 0;
 
            end;
 
         end;
 
      end; /* circle generation */
 
   run;
 
%if &syserr > 4 %then %do;
   %let ok = no;
   %goto endit;
   %end;
 
%if &debug = print %then %do;
   proc print;
      run;
   %end;
 
options notes;
 
*------combine annotate data sets------;
data &out(drop=symbol);
 
   drop j;
   length color style function $ 8;
 
   *------store goptions with annotate data set------;
   if _n_ = 1 then do;
      comment = "&gopts";
      output;
      end;
 
   set &out &tempplot;
 
   xsys  = '4';
   ysys  = '4';
   line  = 1;
   if angle = . then angle = 0;
 
   *------distinguish between text types, set colors------;
   if comment = 'text' then do;
 
      *------handle title------;
      if y > &top then do;
         comment = 'text, title';
         color   = "&titlecol";
         %if %length(%bquote(&symbols)) %then %do;
            if position = ' ' then x = (&left + &right) / 2.0;
            %end;
         end;
 
      *------handle x-axis label and x-ticks------;
      else if y < &bottom then do;
         if input(trim(text),?? 10.) = .
            then comment = 'text, x label';
            else comment = 'text, x tick';
         end;
 
      *------handle y-axis label and y-ticks------;
      else if x < &left then do;
         if input(trim(text),?? 10.) = .
            then comment = 'text, y label';
            else comment = 'text, y tick';
         end;
 
      *------handle text, inside the plot------;
      else do;
 
         *------vector label post-processing------;
         %if %length(%bquote(&veclab)) and
             %bquote(&veclab) ne blank %then %do;
            if index(text,"&veclmark") then do;
               if color = ' ' then color = "&veclcol";
               text  = left(translate(text,' ',"&veclmark"));
               style = "&veclfont";
               size  = &veclsize;
               end;
            %end;
 
         *------corresp post-processing------;
         %if %length(%bquote(&corresp)) %then %do;
 
            if text = "&crowsym" then do;
               if color = ' ' then color = "&crowcol";
               text = "&crowfsy";
               end;
 
            else if text = "&ccolsym" then do;
               if color = ' ' then color = "&ccolcol";
               text = "&ccolfsy";
               end;
 
            else if index(text,"&crowmar") then do;
               if color = ' ' then color = "&crowcol";
               text = left(translate(text,' ',"&crowmar"));
               end;
 
            else if index(text,"&ccolmar") then do;
               if color = ' ' then color = "&ccolcol";
               text = left(translate(text,' ',"&ccolmar"));
               end;
 
            %end;
 
         *------symbol colors------;
         if length(text) = 1 then do;
            j = indexc("&symbols",substr(text,1,1));
            if j then do;
               if color = ' ' then color = scan("&symcol",j,' ');
               %if %length(&symcola) %then color = "&symcola";;
               end;
            end;
 
         comment = 'text, inside plot';
         if color = ' ' then color = "&textcol";
         end;
 
      *------set label colors------;
      if index(comment,"label") then color = "&labelcol";
 
      *------set tick colors, right justify ticks------;
      else if index(comment,"tick")  then do;
         color = "&tickcol";
         if position = ' ' then do;
            position = '<';
            x = x + (length(text) - 1) / 2;
            end;
         end;
 
      *------add to comment symbol we think goes with the label------;
      if symbol ne ' ' then comment = trim(comment) || ', ' || symbol;
 
      end;
 
   *------set frame, circle, vector colors------;
   else if index(comment,'frame' ) then color = "&framecol";
   else if index(comment,'circle') then color = "&circol";
   else if comment = 'vector'      then color = "&veccol";
 
   *------set position if not yet set, adjust justifications------;
   if      position = ' ' then position = '+';
   else if position = '<' then x = x + 0.5;
   else if position = '>' then x = x - 0.5;
 
   *------set line, text sizes------;
   if size = . then do;
      if function  = 'LABEL'
         then size = &tsize;
         else size = &lsize;
      end;
 
   *------set font------;
   %if %length(&font) %then if style = ' ' then style = "&font";;
 
   *------if color is still not set, set it------;
   if color = ' ' then color = "&color";
 
   *------override colors for monochrome device------;
   %if %length(&monochro) %then %do;
      color = "&monochro";
      %end;
 
   *------add offsets to center plot                ------;
   *------adjust y so bottom line has coordinate 0.5------;
   x = x + (&hposoff);
   y = y + (&vposoff) + 0.5;
 
   output;
 
   run;
 
%if &syserr > 4 %then %do;
   %let ok = no;
   %goto endit;
   %end;
 
*------get set up for sas/graph------;
%if &gopts ne asis %then %do;                              /* MF */
goptions &gopts;
filename gsasfile "&post";
%end;                                                      /* MF */
%if &notes = no and
    (&method = gplot or &method = gplot2) %then options nonotes;;
 
%put %str( );
%put NOTE: The plot was created with the following goptions:;
%put %str( );
%put goptions &gopts%str(;);
%put %str( );
 
*------produce the plot------;
%if &method = gplot or &method = gplot2 or
    &method = print or &method = print2 %then %do;
 
   proc ganno annotate=&out %if %length(&gout) %then gout=&gout;;
      run;
 
   %if &syserr > 4 %then %let ok = no;
 
   %end;
 
%endit: options notes;
 
%if &ok = no %then %put ERROR: Macro ended abnormally.;
 
%mend labelplt;
 
*------define (subroutine) macros------;
 
*------create a macro variable from a numeric variable------;
%macro putvar(name,val,fmt);
 
   call symput(&name,compress(put(&val,&fmt)));
 
   %mend putvar;
 
*------check a numeric macro parameter------;
%macro check(param,min,max);
 
   %if %length(%bquote(&&&param)) %then %do;
      param = input("&&&param",?? best12.);
      if param = . or not ((&min) <= param <= (&max)) then do;
         call symput("ok","no");
         put "ERROR: %qupcase(&param)=%bquote(&&&param) is not valid.";
         end;
      %end;
 
   %mend check;
 
*------generate a host dependent file name------;
%macro fname(piece1,piece2);
 
   %if &sysscp = CMS           %then &piece1 &piece2;
   %else %if &sysscp = VMS or
             &sysscp = WIN or
             &sysscp = OS2     %then &piece1..&piece2;
                               %else .&piece1..&piece2;
 
   %mend fname;
 
*------draw a line------;
%macro line(x1,y1,x2,y2);
 
   x = &x1; y = &y1; function = 'MOVE '; output;
   x = &x2; y = &y2; function = 'DRAW '; output;
 
   %mend line;
 
*------find a point (px,py) r distance from (x,y) on a  ------;
*------line with slope m, negative r means toward origin------;
%macro linept(x,y,px,py,m,r);
 
   vecdir = -sign(&r);
 
   if &m ne . then do;
      vecm2 = sqrt((&r) * (&r) / (1.0 + (&m) * (&m)));
      if (&x) > (&x0) then vecm2 = -vecm2;
      &px = vecdir * vecm2 + (&x);
      &py = vecdir * (&m) * vecm2 / (&vtoh) + (&y);
      end;
 
   else do;
      &px = (&x);
      &py = abs(&r);
      if (&y) > ((&nlines) - (&y0)) then vecdir = -vecdir;
      &py = vecdir * (&py) / (&vtoh) + (&y);
      end;
 
   %mend linept;
 
 
*------construct placement states------;
%macro place(place);
 
   %if %length(&place) %then %do;
 
      %if &place > 15 %then %let place = 15;
      %let pl = (h=2 -2 : s=right left);
 
      %if &place = 1 %then
         %let pl = &pl (v=1 * h=0 -1 to -2 by alt);
 
      %else %if &place = 2 %then
         %let pl = &pl (v=1 -1 * h=0 -1 to -5 by alt);
 
      %else %if &place = 3 %then
         %let pl = &pl (v=1 to 2 by alt * h=0 -1 to -5 by alt);
 
      %else %if &place = 4 %then
         %let pl = &pl (v=1 to 2 by alt * h=0 -1 to -10 by alt);
 
      %else %if &place = 5 %then %do;
         %let pl = &pl (s=center right left *;
         %let pl = &pl v=0 1 to 2 by alt * h=0 -1 to -10 by alt);
         %end;
 
      %else %do;
         %let pl = &pl (l=1 2 * s=center right left *;
         %let pl = &pl v=0 1 to %eval(2 * (&place - 5)) by alt *;
         %let pl = &pl h=0 -1 to %eval(-5 * (&place - 5)) by alt);
         %end;
 
      %let pl = placement=(&pl);
 
      %if &place > 5 %then %do;
         %let pl = &pl penalty(7) = %eval((3 * (&place)) / 2);
         %end;
 
      &pl
      %end;
 
   %mend;
