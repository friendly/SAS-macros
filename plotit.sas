 /****************************************************************/
 /*          S A S   A U T O C A L L   L I B R A R Y             */
 /*                                                              */
 /*    NAME: PLOTIT                                              */
 /*   TITLE: Graphical Scatter Plots With Iteratively Derived    */
 /*          Optimal Label Placement                             */
 /* PRODUCT: STAT                                                */
 /*  SYSTEM: ALL                                                 */
 /*    KEYS: graphs                                              */
 /*   PROCS: PLOT GANNO                                          */
 /*    DATA:                                                     */
 /*                                                              */
 /* SUPPORT: saswfk                      UPDATE:  17Jan2000      */
 /*     REF:                                                     */
 /*    MISC: SAS/GRAPH software must be licensed.                */
 /*                                                              */
 /*          !!!!! IMPORTANT * IMPORTANT * IMPORTANT !!!!!       */
 /*                                                              */
 /* Carefully read the documentation that precedes the macro in  */
 /* this file.  Look at "Typical usage:" to see how to create    */
 /* a plot on your screen, a postscript file, and hard copy.     */
 /* Pay particular attention to the METHOD=, GOPPRINT=,          */
 /* GOPPLOT=, GOPTS2=, and GOPTS= options.                       */
 /*                                                              */
 /* You can modify the GOPPRINT= and GOPPLOT= options, setting   */
 /* default devices so that you will not be prompted for a       */
 /* device.                                                      */
 /*                                                              */
 /* The macro creates files with host specific names.  See the   */
 /* FILEPREF= and POST= options.  It is a good idea              */
 /* to specify defaults for these options to avoid making the    */
 /* macro generate names.  Before you run the macro, make sure   */
 /* these file names do not conflict with existing files!!!      */
 /* Search for the string '-set default file names-' to see how  */
 /* these names are created.                                     */
 /*                                                              */
 /* This macro has been updated for release 8.1 to handle        */
 /* titles and footnotes differently than in previous releases   */
 /* due to an ODS change.                                        */
 /*                                                              */
 /****************************************************************/

 /*---------------------------------------------------------------------

 Purpose: This file contains a macro to create graphical scatter plots
          with point labels, vectors, circles, contours, functions.
 Paper:   Graphical Scatter Plots of Labeled Points, Observations,
          Fourth Quarter, 1994.
 Author:  Warren F. Kuhfeld, SAS Institute Inc.
 Software Release: 8.1 or later releases.

 -----------------------------------------------------------------------

 DISCLAIMER:

       THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE
 TO ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
 EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
 PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
 CONTAINED HEREIN.

 -----------------------------------------------------------------------

 Typical usage:

 The default METHOD=gplot creates the graphical scatter plot.
 Here is a typical specification for a correspondence analysis,
 which by default will display the plot on your screen.

 %plotit(data=coor,datatype=corresp)

 The macro will by default use the last data set created, however
 DATA= was explicitly specified in case the macro is run a second
 time.  The macro creates an output Annotate data set that cannot be
 used as input to the macro.  If no graphics device has been previously
 specified, you will be prompted for a device as follows:

    No device name has been given--please enter device name:

 Enter the device name for your system.  This name will be remembered
 for the duration of your SAS session or until you change the device.
 You can modify the GOPPRINT= and GOPPLOT= options to set default
 devices so that you will not be prompted.  Note that all graphics
 options specified on a GOPTIONS statement (except DEVICE=) are
 ignored by default.  Use the macro options GOPPRINT=, GOPPLOT=,
 GOPTS2=, and GOPTS= to set GOPTIONS.

 You exit the graph window in the same way you would any other graph
 window, for example by clicking with the mouse, hitting return,
 typing END or CANCEL, and so on.

 To create a color postscript file named "myplot.ps", suitable for
 printing on a CLJPS device, specify:

 %plotit(data=coor, datatype=corresp,
         method=print, post=myplot.ps, gopts=device=cljps)

 Alternatively, change the default for GOPPRINT= below to name your
 typical device, for example from

   gopprint=gsfmode=replace gaccess=gsasfile,

 to

   gopprint=gsfmode=replace gaccess=gsasfile device=cljps,

 Then to create a postscript file, just specify:

 %plotit(data=coor, datatype=corresp, method=print, post=myplot.ps)

 Then the file may be previewed and printed.

 Another alternative is to send the plot directly to the printer.
 In this example, CHPLJR51 is a printer that prints on ordinary
 8 1/2 x 11 paper.

 %plotit(data=coor, datatype=corresp, gopts=device=chpljr51 cback=white)

 To just see the printer plot, specify METHOD=plot.

 Use GOUT= to write the plot to a catalogue.

 -----------------------------------------------------------------------

 Colors:

 If you do not like the default background color this, specify
 GOPPLOT=cback=some-color, (substituting your favorite color for
 some-color) or GOPPLOT=, to use the default background.
 Use CBACK= to set the background color. You can also permanently
 change the default by modifying the macro.  Similarly, you can change
 COLOR=cyan, and COLORS=blue red green cyan magenta orange gold lilac
 olive purple brown gray rose violet salmon yellow, as you see fit.

 -----------------------------------------------------------------------

 Samples:

 *------Simple Correspondence Analysis------;
 proc corresp all data=cars outc=coor;
    tables marital, origin;
    title 'Simple Correspondence Analysis';
    run;

 %plotit(data=coor,datatype=corresp)

 This example performs a simple correspondence analysis.
 Often, only the DATA= and DATTYPE= option need to be specified.

 -----------------------------------------------------------------------

 *------Multiple Correspondence Analysis------;
 proc corresp mca observed data=cars outc=coor;
    tables origin size type income home marital sex;
    title 'Multiple Correspondence Analysis';
    run;

 %plotit(data=coor,datatype=mca)

 This example performs multiple correspondence analysis.

 -----------------------------------------------------------------------

 *------MDPREF------;
 proc prinqual data=carpref out=results n=2
               replace standard scores correlations;
    id model mpg reliable ride;
    transform ide(judge1-judge25);
    title 'Multidimensional Preference (MDPREF) Analysis';
    run;

 %plotit(data=results,datatype=mdpref 2.5)

 This example performs multidimensional preference analysis.  The
 vector lengths are increased by a factor of 2.5 to make a better
 graphical display.

 -----------------------------------------------------------------------

 *------PREFMAP, Vector Model------;
 proc transreg data=results(where=(_type_ = 'SCORE'));
    model ide(mpg reliable ride)=identity(prin1 prin2);
    output tstandard=center coefficients replace out=tresult1;
    id model;
    title 'Preference Mapping (PREFMAP) Analysis - Vector';
    run;

 %plotit(data=tresult1,datatype=vector 2.5)

 This example performs a preference mapping, vector model.
 Again, the vector lengths are increased by a factor of 2.5 to
 make a better graphical display.

 -----------------------------------------------------------------------

 *------PREFMAP, Ideal Point------;
 proc transreg data=results(where=(_type_ = 'SCORE'));
    model identity(mpg reliable ride)=point(prin1 prin2);
    output tstandard=center coordinates replace out=tresult1;
    id model;
    title 'Preference Mapping (PREFMAP) Analysis - Ideal';
    run;

 %plotit(data=tresult1,datatype=ideal,antiidea=1)

 This example performs a preference mapping, ideal point model.
 ANTIIDEA=1 is specified to handle anti-ideal points when large
 data values are positive or ideal.

 -----------------------------------------------------------------------

 *------MDPREF, labeled vector end points------;
 proc prinqual cor data=recreate out=rec score std rep;
    transform identity(sub:);
    id activity active relaxing spectato;
    title 'MDPREF of Recreational Activities';
    run;

 %plotit(data=rec,datatype=mdpref2 3,
         symlen=2,vechead=,adjust1=%str(
         if _type_ = 'CORR' then do;
            __symbol = substr(activity,4);
            __ssize  = 0.7;
            activity = ' ';
            end;))

 This example performs multidimensional preference analysis.  The
 "mdpref2" means MDPREF and label the vectors *too*.  The
 vector lengths are increased by a factor of 3 to make a better
 graphical display.  SYMLEN=2 specifies two-character symbols.
 VECHEAD=, (a null value) means no vector heads since there are
 labels.  ADJUST1= is used to add full SAS data step statements to the
 preprocessed data set.  This example takes for _TYPE_ = 'CORR'
 observations (those that contain vector coordinates) the activity
 values (SUB1, SUB2, SUB3, ...) which are the variable names from the
 preference data and creates symbol values (1, 2, 3, ...) of size 0.7.
 The result is a plot with each vector labeled at the end with the
 subject number.

 -----------------------------------------------------------------------

 *------Bivariate Normal Density Function------;
 proc iml;
    title 'Bivariate Normal Density Function';
    s = inv({1 0.25 , 0.25 1});
    m = -2.5; n = 2.5; inc = 0.05; k = 0;
    x = j((1 + (n - m) / inc) ** 2, 3, 0);
    c = sqrt(det(s)) / (2 * 3.1415);
    do i = m to n by inc;
       do j = m to n by inc;
          v = i || j; z = c * exp(-0.5 * v * s * v`);
          k = k + 1; x[k,] = v || z;
          end;
       end;
    create x from x[colname={'x' 'y' 'z'}]; 
	append from x;
    quit;

 %plotit(datatype=contour, data=x, excolors=CX000000,
          paint=z black blue magenta red, gopts=cback=black)

 This example creates a contour plot, displaying density with color.
 PAINT=z black blue magenta red, specifies that color interpolation is
 based on the variable z, going from black (zero density) through blue,
 magenta, and to red (maximum density).  Observations with a computed
 color of black (CX000000) are excluded for efficiency.  This color
 list assumes cback=black.

 -----------------------------------------------------------------------

 *------Discriminant Analysis------;
 data plotdata;  * Create a grid over which DISCRIM outputs densities.;
    do sepallen = 30 to 90 by 0.6;
       h + 1; * Number of horizontal cells;
       do petallen = 0 to 80 by 0.6;
          n + 1; * Total number of cells;
          output;
          end;
       end;
    call symput('hnobs', compress(put(h    , best12.))); * H grid size;
    call symput('vnobs', compress(put(n / h, best12.))); * V grid size;
    drop n h;
    run;

 proc discrim data=iris testdata=plotdata testoutd=plotd
              method=normal pool=no short noclassify;
    class species;
    var petallen sepallen;
    title 'Discriminant Analysis of Fisher (1936) Iris Data';
    title2 'Using Normal Density Estimates with POOL=NO';
    run;

 data all;
    * Set the density observations first so the scatter plot points
      will be on top of the contour plot.  Otherwise the contour plot
      points will hide the scatter plot points.;
    set plotd iris(in=iris);
    if iris then do;
       _type_ = s; * unformatted species number 1, 2, 3;
       output;
       end;
    else do;
       _type_ = 4; * density observations;
       density = max(setosa,versicol,virginic);
       output;
       end;
    run;

 %plotit(data=all,plotvars=petallen sepallen,labelvar=_blank_,
         symlen=10,exttypes=symbol contour,ls=100,
         paint=density black yellow orange red,rgbtypes=contour,
         hnobs=&hnobs,vnobs=&vnobs,excolors=CX000000,
         types  =1      2          3          4,
         symtype=symbol symbol     symbol     contour,
         symbols=Setosa Versicolor Virginica  '',
         symsize=0.7    0.7        0.7        1,
         symfont=swiss  swiss      swiss      solid,
         colors =blue   magenta    cyan
         )

 The goal of this example is to create a plot with each observation
 identified by its species.  Species name is centered at each point's
 location, and each species name is plotted in a different color.  This
 scatter plot is overlaid on the densities used by PROC DISCRIM to
 classify the observations.  There are three densities, one for each
 species.  Density is portrayed by a color contour plot with black
 (the assumed background color) indicating a density of essentially
 zero.  Yellow, orange, and red indicate successively increasing
 density.

 DATA= names the input SAS data set.  PLOTVARS= names the y-axis and
 x-axis variables.  LABELVAR=_blank_ specifies that all labels are
 blank.  This example does not use any of PROC PLOT's label collision
 avoidance code.  It simply uses PROC PLOT to figure out how big to
 make the plot, and then the macro puts everything inside the plot
 independently of PROC PLOT, so the printer plot is blank.  SYMLEN=10
 specifies that the maximum length of a symbol value is 10 characters.
 This is because the full species names are used as symbols.
 EXTTYPES=symbol contour, explicitly specifies that PROC PLOT will know
 nothing about the symbols or the contours.  They are external types
 that will be added to the graphical plot by the macro after PROC PLOT
 has finished.  LS=100 specifies a constant line size.  Since no label
 avoidance is done, there can be no collisions, and the macro will not
 iteratively determine the plot size.  The default line size of 65 is
 too small for this example, whereas LS=100 makes a better display.
 PAINT= specifies that based on values the variable DENSITY, colors
 should be interpolated ranging from black (minimum DENSITY) to yellow
 to orange to red (maximum DENSITY).  RGBTYPES=contour specifies that
 the PAINT= option should apply to contour type observations.

 The grid (created with the loops: DO SEPALLEN = 30 TO 90 BY 0.6; and
 DO PETALLEN = 0 TO 80 BY 0.6;) is not square, so for optimal results
 the macro must be told the number of horizontal and vertical
 positions.  The PLOTDATA data step creates these values and stores
 them in macro variables &hnobs and &vnobs, so the specification
 HNOBS=&hnobs, VNOBS=&vnobs, specifies the grid size.  Of course these
 values could have been specified directly instead of through symbolic
 variables.  The excolors=CX000000 option is included for efficiency.
 The input data consist of a large grid for the contour plot.  Most of
 the densities are essentially zero, so many of the colors will be
 CX000000, which is black, computed by PAINT=, which is the same color
 as the background.  Excluding them from processing makes the macro run
 faster and creates smaller datasets.

 This example shows how to manually do the kinds of things that the
 DATATYPE= option does for you with standard types of data sets.  The
 macro expects the data set to contain observations of one or more
 types.  Each type is designated by a variable, often named _TYPE_.  In
 this example, there are four types of observations, designated by the
 _TYPE_'s four values, 1, 2, 3, 4, which are specified in the TYPES=
 option.  SYMTYPE= specifies the symbol types for these four
 observation types.  The first three types of observations are "symbol"
 and the last type, _TYPE_ = 4, designates the contour observations.
 The first three symbols are the species names (SYMBOLS= values)
 printed in SYMFONT=swiss font.  The last symbol is null because
 contours do not use symbols.  The first three symbols, since they are
 words as opposed to a single character, are given a small size
 (SYMSIZE=0.7).  A value of 1 is specified for the symbol size for
 contour type observations.  The macro determines the optimal size for
 each color rectangle of the contour plot.  Constant colors are only
 specified for the noncontour observations since a variable color is
 computed for contour observations.

 -----------------------------------------------------------------------

 You create a data set either with a data step or with a procedure.
 Then you run the macro to create a graphical scatter plot.  This macro
 is not a SAS/GRAPH procedure and does not behave like a typical
 SAS/GRAPH procedure.  For example, by default, you do not specify a
 GOPTIONS statement.  GOPTIONS are specified in macro options (the macro
 options that begin with GOP).  The METHOD= and GOP options determine
 whether the plot is displayed on the screen or routed to a file.

 The PLOTIT macro performs the following steps.

 1) It reads an input data set and preprocesses it.  The preprocessed
 data set contains information such as the axis variables, the
 point-symbol and point-label variables, and symbol and label types,
 sizes, fonts, and colors.  The nature of the preprocessing depends on
 the type of data analysis that generated the input data set.  For
 example, if the option DATATYPE=MDPREF was specified with an input
 data set created by PROC PRINQUAL for a multidimensional preference
 analysis, then the PLOTIT macro creates blue points for _TYPE_ =
 'SCORE' observations and red vectors for _TYPE_ = 'CORR'
 observations.

 2) A DATA step, using the DATA Step Graphics Interface, determines how
 big to make the graphical plot.

 3) PROC PLOT determines where to position the point labels.
 By default, if some of the point label characters are
 hidden, the PLOTIT macro recreates the printer plot with a larger line
 and page size, and hence creates more cells and more room for the
 labels.  Note that when there are no point labels, the printer plot
 may be empty.  All of the information that is in the graphical scatter
 plot may be stored in the EXTRAOBS= data set. All results from PROC PLOT
 are written to data sets with ODS.  The macro will clear existing
 ODS SELECT and ODS EXCLUDE statements.

 4) The printer plot is read and information from it, the preprocessed
 data set, and the extra observations data set are combined to create
 an Annotate data set.  The label position information is read from the
 PROC PLOT output, and all of the symbol, size, font, and color
 information is extracted from the preprocessed (or extra observations)
 data set.  The Annotate data set contains all of the instructions for
 drawing the axes, ticks, tick marks, titles, point symbols, point
 labels, axis labels, and so on.  Circles can be drawn around certain
 locations, and vectors can be drawn from the origin to other
 locations.

 5) The Annotate data set is displayed with the GANNO procedure.  The
 PLOTIT macro does not use PROC GPLOT.

 -----------------------------------------------------------------------

 Problem solving:

 When you have problems, try DEBUG=vars to see what the macro
 thinks you specified.  It is also helpful to specify:
 DEBUG=mprint notes.  You can also print the final Annotate
 data set and the preprocessing data set:

 options ls=180;
 proc print data=anno uniform;
    format text $20. comment $40.;
    run;

 proc print data=preproc uniform;
    run;

 -----------------------------------------------------------------------

 Post-processing the scatter plot:

 You can post-process the Annotate data step to change colors,
 fonts, undesirable placements, and so on.  Sometimes, this can be
 done with the ADJUST4= option.  Alternatively, when you specify
 METHOD=none, you create an Annotate data set without displaying it.
 The data set name is by default WORK.ANNO.  You can then manipulate
 it further with a data step or PROC FSEDIT to change colors, fonts,
 or sizes for some labels; move some labels; and so on.  If the final
 result is a new data set called anno2, display it by running:

 proc ganno annotate=anno2;
    run;

 -----------------------------------------------------------------------

 WARNINGS:

 With method=print, the macro creates a file.  See the FILEPREF= and
 POST= options and make sure that the file name does not conflict with
 existing names.

 This macro creates variable names that begin with two underscores and
 assumes that these names will not conflict with any input data set
 variable names.

 It is not feasible with a macro to provide the full range of error
 checking that is provided with a procedure.  Some error checking is
 provided, but not all errors will be diagnosed.

 Not all options will work with all other options.  Some combinations
 of options may produce macro errors or Annotate errors.

 This macro may not be fully portable.  When you switch operating
 systems or graphics devices, some changes may be necessary to get the
 macro to run successfully again.

 Graphics device differences may also be a problem.  We do not know of
 any portability problems, but the macro has not been tested on all
 supported devices.

 This macro tries to create a plot with equated axes, where a
 centimeter on one axis represents the same data range as a centimeter
 on the other axis.  The only way to accomplish this is by
 explicitly and jointly controlling the HSIZE=, VSIZE=, HPOS=, and
 VPOS= GOPTIONS.  By default, the macro tries to ensure that all of the
 values work for the specific device.  See MAKEFIT=, XMAX=, and YMAX=.
 By default the macro uses GASK to determine XMAX and YMAX.
 If you change any of these options, your axes may not be equated.

                            vsize * hpos
 Axes are equated when:    --------------  =  vtoh
                            hsize * vpos

 When you are plotting variables that have very different scales, you
 may need to specify appropriate tick increments for both axes to get
 a reasonable plot.  Here is an example:
 PLOTOPTS=haxis=by 20 vaxis=by 5000.
 Alternatively, just specifying the smaller increment is often
 sufficient: PLOTOPTS=haxis=by 20.
 Alternatively, specify VTOH=, (null value) to get a plot like PROC
 GPLOT's, with the window filled.

 By default, the macro iteratively creates and recreates the plot,
 increasing the line size and the flexibility in the PLACEMENT= list
 until there are no penalties.

 The SAS system option OVP (overprint) is not supported by this macro.

 --------------------------------------------------------------------*/

%macro plotit(            /*-----------------------------------------*/
                          /* Note that by default you may not        */
                          /* specify a GOPTIONS statement.  If you   */
                          /* do, it will be overridden.              */
                          /* All GOPTIONS (except DEVICE=) must be   */
                          /* specified with the four GOP options.    */
                          /*                                         */
                          /* Modify the GOPPRINT= and GOPPLOT=       */
                          /* options to set default devices so that  */
                          /* you will not be prompted.               */
                          /*                                         */
                          /* Note that for many analyses, the only   */
                          /* options you need to specify are DATA=,  */
                          /* DATATYPE=, and sometimes METHOD=.  To   */
                          /* specify variables to plot, specify      */
                          /* PLOTVARS=, LABELVAR=, and SYMVAR=.      */
                          /*                                         */
                          /* This macro looks for a special global   */
                          /* macro variable named PLOTITOP.  If it   */
                          /* exists, its values are used to override */
                          /* the macro options.  Say you have a      */
                          /* series of calls to the plotting macro   */
                          /* and you want to route them all to a     */
                          /* postscript file, you can specify once,  */
                          /*                                         */
                          /*   %let plotitop = gopts=                */
                          /*                   gsfmode=append        */
                          /*                   gaccess=gsasfile      */
                          /*                   device=qmscolor;      */
                          /*                                         */
                          /* and run the macro calls without change. */
                          /*                                         */
                          /* The value of the macro variable must    */
                          /* begin with a name, followed by an equal */
                          /* sign, followed by a value.  Optionally, */
                          /* it may continue with a comma, followed  */
                          /* by a name, followed by an equal sign,   */
                          /* followed by a value, and so on.  Values */
                          /* must not contain commas.  Example:      */
                          /*                                         */
                          /*   %let plotitop = color=black,          */
                          /*                   gopts=cback=cyan;     */
                          /*-----------------------------------------*/
method=gplot,             /* gplot - displays a graphical            */
                          /*         scatter plot on your screen     */
                          /*         using the GOPTIONS from         */
                          /*         GOPPLOT=.  GOPPLOT= should      */
                          /*         contain the GOPTIONS that only  */
                          /*         apply to plots displayed on the */
                          /*         screen.                         */
                          /*                                         */
                          /* plot  - creates a printer plot only.    */
                          /*                                         */
                          /* print - routes the plot to a graphics   */
                          /*         stream file, such as a post     */
                          /*         script file, using the GOPTIONS */
                          /*         from GOPPRINT=.  GOPPRINT=      */
                          /*         should contain the GOPTIONS     */
                          /*         that only apply to hard-copy    */
                          /*         plots.  Specify the file name   */
                          /*         with POST=.                     */
                          /*                                         */
                          /* none  - Just creates the Annotate data  */
                          /*         set and sets up GOPTIONS using  */
                          /*         GOPPLOT=.                       */
                          /*-----------------------------------------*/
                          /* The next four options specify GOPTIONS: */
                          /* gopprint - specifies the GOPTIONS for   */
                          /*            printing (creating a         */
                          /*            graphics stream file).       */
                          /* gopplot  - specifies the GOPTIONS for   */
                          /*            directly plotting on the     */
                          /*            screen.                      */
                          /* gopts2   - specifies the GOPTIONS that  */
                          /*            are always used, no matter   */
                          /*            which METHOD= is specified.  */
                          /* gopts    - provides a way to specify    */
                          /*            additional GOPTIONS that are */
                          /*            always used.                 */
                          /*                                         */
                          /* Modify the GOPPRINT= and GOPPLOT=       */
                          /* option defaults to set default devices  */
                          /* so that you will not be prompted.       */
                          /* Example:                                */
                          /* GOPPRINT=gsfmode=replace                */
                          /*          gaccess=gsasfile               */
                          /*          device=qmscolor,               */
                          /* GOPPLOT=cback=black device=xcolor,      */
                          /*                                         */
                          /* If you would prefer to specify your own */
                          /* GOPTIONS statement and have the macro   */
                          /* use it, just specify or change the      */
                          /* default for these four options to null: */
                          /* GOPPLOT=, GOPPRINT=, GOPTS2=, GOPTS=.   */
                          /*                                         */
                          /* A few device names:                     */
                          /* qmscolor - color plotter.               */
                          /* psepsf   - encapsulated postscript.     */
                          /* psl      - postscript.                  */
                          /* xcolor   - X windows, color.            */
                          /*                                         */
                          /* Useful additional goptions for GOPTS=   */
                          /* include:                                */
                          /* rotate      - landscape orientation     */
                          /* cback=color - background color          */
                          /*-----------------------------------------*/

gopplot=,

gopprint=gsfmode=replace gaccess=gsasfile,

gopts2=/*reset=goptions erase*/,

gopts=,

                          /*-----------------------------------------*/
data=_last_,              /* Input data set.  You should always      */
                          /* specify DATA= since the macro creates   */
                          /* data sets that are not suitable for use */
                          /* as input.                               */
                          /*                                         */
out=anno,                 /* Output Annotate data set.               */
                          /*                                         */
gout=,                    /* PROC ANNO GOUT= specification.  This    */
                          /* option puts a plot in a catalogue.      */
                          /* With GOUT=gc.slides, first specify:     */
                          /* LIBNAME GC '.';                         */
                          /* Then to replay, run:                    */
                          /* PROC GREPLAY IGOUT=GC.SLIDES; RUN;      */
                          /*                                         */
                          /* Note that replayed plots will not in    */
                          /* general have the correct aspect ratio.  */
                          /*                                         */
gname=,                   /* Name of catalogue entry,                */
                          /* optionally used with PROC ANNO GOUT=,   */
                          /* provides NAME=.                         */
                          /*                                         */
gdesc=,                   /* Name of catalogue description,          */
                          /* optionally used with PROC ANNO GOUT=,   */
                          /* provides DESCRIPTION=.                  */
                          /*-----------------------------------------*/
plotvars=,                /* Specify the y variable then the x       */
                          /* variable.  To work with say DIM2 and    */
                          /* DIM3, specify PLOTVARS=DIM2 DIM3.  The  */
                          /* DATATYPE= option controls the default.  */
                          /*                                         */
labelvar=,                /* Variable that contains the point        */
                          /* labels.  The default is the last        */
                          /* character variable in the data set.     */
                          /* If LABELVAR=_blank_ is specified, the   */
                          /* macro will create a blank label         */
                          /* variable.                               */
                          /*                                         */
symvar=_symbol_,          /* Variable that contains the plotting     */
                          /* symbol for input to PROC PLOT.  When    */
                          /* _symbol_ is specified, this variable is */
                          /* created, typically from the SYMBOLS=    */
                          /* list, which may be constructed inside   */
                          /* the macro.  (Note that the variable     */
                          /* __symbol is created to contain the      */
                          /* symbol for the graphical scatter plot.  */
                          /* The variables _symbol_ and __symbol may */
                          /* or may not contain the same values.)    */
                          /* Variables can be specified, and the     */
                          /* first SYMLEN= characters are used for   */
                          /* the symbol.  When a null (SYMVAR=,) or  */
                          /* constant value is specified, the symbol */
                          /* from the printer plot will be used      */
                          /* (which is always length one, no matter  */
                          /* what is specified for SYMLEN=).  To get */
                          /* PROC PLOT pointer symbols, specify      */
                          /* SYMVAR='00'x, (hex null constant).  To  */
                          /* center labels with no symbols, specify: */
                          /* SYMVAR=,PLACE=0.                        */
                          /*                                         */
symlen=1,                 /* Length of symbols.  Normally, symbols   */
                          /* are single characters, but the macro    */
                          /* can center longer strings at the symbol */
                          /* location.                               */
                          /*-----------------------------------------*/
datatype=,                /* Specifies the type of data analysis     */
                          /* that generated the data set.  This      */
                          /* option is used to set defaults for      */
                          /* other options and to do some            */
                          /* preprocessing of the data.              */
                          /*                                         */
                          /* When the data type is corresp, mds,     */
                          /* mca, row, column, mdpref, mdpref2,      */
                          /* vector, or ideal, LABEL=typical is      */
                          /* implied when LABEL= is not otherwise    */
                          /* specified.  The default point label     */
                          /* variable is the last character variable */
                          /* in the data set.                        */
                          /*                                         */
                          /* Some data types (mdpref, vector, ideal, */
                          /* corresp, row, mca, column, mds) expect  */
                          /* certain observation types and set the   */
                          /* TYPES= list accordingly.  For example,  */
                          /* mdpref expects _TYPE_ = 'SCORE' and     */
                          /* _TYPE_ = 'CORR' observations.  The      */
                          /* remaining data types do not expect any  */
                          /* specific value of the TYPEVAR=          */
                          /* variable.  So if you do not get the     */
                          /* right data types associated with the    */
                          /* right observation types, specify        */
                          /* TYPES=, and specify the TYPES= values   */
                          /* in an order that corresponds to the     */
                          /* order of the symbol types in the "Types */
                          /* Legend" table.  Unlike SYMTYPE=, the    */
                          /* order in which you specify DATATYPE=    */
                          /* values is irrelevant.                   */
                          /*                                         */
                          /* null value - (DATATYPE=, the default)   */
                          /* no special processing.  The default     */
                          /* plotting variables are the first two    */
                          /* numeric variables in the data set.      */
                          /*                                         */
                          /* corresp, mds, mca, row, column:         */
                          /* sets the default PLOTVARS to DIM2 and   */
                          /* DIM1.                                   */
                          /*                                         */
                          /* otherwise when a nonnull value is       */
                          /* specified, the default PLOTVARS are     */
                          /* PRIN2 and PRIN1.                        */
                          /*                                         */
                          /* DATATYPE=symbol,                        */
                          /* ordinary scatter plot.                  */
                          /*                                         */
                          /* DATATYPE=mdpref,                        */
                          /* specifies multidimensional preference   */
                          /* analysis with vectors and blank labels. */
                          /* Note that DATATYPE=mdpref can also be   */
                          /* used for ordinary principal component   */
                          /* analysis.                               */
                          /*                                         */
                          /* DATATYPE=mdpref2,                       */
                          /* specifies MDPREF with vector labels     */
                          /* (MDPREF and labels *too*).              */
                          /*                                         */
                          /* DATATYPE=vector,                        */
                          /* specifies a PREFMAP vector model.       */
                          /*                                         */
                          /* DATATYPE=mds vector,                    */
                          /* specifies PREFMAP after MDS.            */
                          /*                                         */
                          /* DATATYPE=ideal,                         */
                          /* specifies a PREFMAP ideal point model.  */
                          /* See the ANTIIDEA=, RADII=, and CIRSEGS= */
                          /* options.                                */
                          /*                                         */
                          /* DATATYPE=mds ideal,                     */
                          /* specifies PREFMAP ideal point after     */
                          /* the MDS.                                */
                          /*                                         */
                          /* DATATYPE=corresp,                       */
                          /* specifies an ordinary correspondence    */
                          /* analysis.                               */
                          /*                                         */
                          /* DATATYPE=row,                           */
                          /* specifies a PROC CORRESP PROFILE=ROW    */
                          /* analysis.  Column points are plotted as */
                          /* vectors.                                */
                          /*                                         */
                          /* DATATYPE=column,                        */
                          /* specifies a PROC CORRESP PROFILE=COLUMN */
                          /* analysis.  Row points are plotted as    */
                          /* vectors.                                */
                          /*                                         */
                          /* DATATYPE=mca,                           */
                          /* specifies a multiple correspondence     */
                          /* analysis.                               */
                          /*                                         */
                          /* DATATYPE=vector ideal,                  */
                          /* both PREFMAP vectors and ideal points.  */
                          /*                                         */
                          /* DATATYPE=curve,                         */
                          /* fit a curve through the scatter plot.   */
                          /*                                         */
                          /* DATATYPE=curve2,                        */
                          /* fit a curve through the scatter plot    */
                          /* and try to make the labels avoid        */
                          /* overprinting the curve too.             */
                          /*                                         */
                          /* DATATYPE=function,                      */
                          /* draws functions.  Typically, no labels  */
                          /* or symbols are drawn.  This option has  */
                          /* a similar effect to the PROC GPLOT      */
                          /* SYMBOL statement options I=JOIN V=NONE. */
                          /*                                         */
                          /* DATATYPE=contour,                       */
                          /* draws solid color contour plots.  See   */
                          /* the sample code above.  When the number */
                          /* of row points is not the same as the    */
                          /* number of column points in the grid,    */
                          /* use HNOBS= and VNOBS= to specify the    */
                          /* number of points.  This method creates  */
                          /* an HNOBS= by VNOBS= grid of colored     */
                          /* rectangles.  Each of the rectangles     */
                          /* should touch all adjacent rectangles.   */
                          /* This method works well with a regular   */
                          /* grid of points.  METHOD=square is a     */
                          /* good alternative when the data do not   */
                          /* fall in a regular grid.                 */
                          /*                                         */
                          /* DATATYPE=square,                        */
                          /* plots each point as a solid square.     */
                          /* DATATYPE=square is useful as a form of  */
                          /* contour plotting when the data do not   */
                          /* form a regular grid.  DATATYPE=square   */
                          /* unlike DATATYPE=contour does not try to */
                          /* scale the size of the square so that    */
                          /* each square will touch another square.  */
                          /*                                         */
                          /* For some DATATYPE= values, a number may */
                          /* be specified after the name.  This is   */
                          /* primarily useful for biplot data sets   */
                          /* produced by PRINQUAL and PREFMAP data   */
                          /* sets produced by PROC TRANSREG.  This   */
                          /* number specifies that the lengths of    */
                          /* vectors should be changed by this       */
                          /* amount.  The number must be specified   */
                          /* last.  Examples:                        */
                          /* DATATYPE=mdpref 2,                      */
                          /* DATATYPE=mds vector 1.5.                */
                          /*                                         */
                          /* The primary purpose of the DATATYPE=    */
                          /* option is to provide an easy mechanism  */
                          /* for specifying defaults for the options */
                          /* in the next section (TYPEVAR= through   */
                          /* OUTWARD=).                              */
                          /*-----------------------------------------*/
options=,                 /* Binary options.  Specify zero, one, or  */
                          /* more in any order.  For example:        */
                          /* OPTIONS=nocenter nolegend,              */
                          /*                                         */
                          /* nocenter - do not center.  By default,  */
                          /* when nocenter is not specified, VSIZE=  */
                          /* and HSIZE= are set to their maximum     */
                          /* values, and the VPOS= and HPOS= values  */
                          /* are increased accordingly.  The X and Y */
                          /* coordinates are increased to position   */
                          /* the plot in the center of the full      */
                          /* graphics area.                          */
                          /*                                         */
                          /* noclip - do not clip.  By default, when */
                          /* noclip is not specified, labels that    */
                          /* extend past the edges of the plot are   */
                          /* clipped.  Otherwise, do not clip.  This */
                          /* option will not absolutely prevent      */
                          /* labels from extending beyond the plot,  */
                          /* particularly when sizes are greater     */
                          /* than 1.                                 */
                          /*                                         */
                          /* textline - put text in the data set,    */
                          /* followed by lines, so lines overwrite   */
                          /* text.  Otherwise text overwrites lines. */
                          /*                                         */
                          /* square - uses the same ticks for both   */
                          /* axes and tries to make the plot square  */
                          /* by tinkering with the EXTEND= option.   */
                          /* Otherwise, ticks may be different.      */
                          /*                                         */
                          /* nolegend - suppresses the printing of   */
                          /* the legends.                            */
                          /*                                         */
                          /* nocode - suppresses the printing of the */
                          /* proc plot and goptions statements.      */
                          /*                                         */
                          /* nohistory - suppresses the printing of  */
                          /* the iteration history table.            */
                          /*                                         */
                          /* noprint - equivalent to nolegend,       */
                          /* nocode, and nohistory.                  */
                          /*                                         */
                          /* nodelete - do not delete intermediate   */
                          /* data sets.                              */
                          /*                                         */
                          /* border - draw a border box around the   */
                          /* outside of the graphics area, like the  */
                          /* border goption.                         */
                          /*                                         */
                          /* close - if a border is being drawn      */
                          /* perform the same adjustments on the     */
                          /* border that are performed on the axes.  */
                          /* This option is most useful with contour */
                          /* plots.                                  */
                          /*                                         */
                          /* expand - specifies annotate data set    */
                          /* post processing, typically for use      */
                          /* with extend=close and contour plots.    */
                          /* Makes the plot bigger to fill up more   */
                          /* of the window.                          */
                          /*                                         */
                          /* diag - draw diagonal reference line     */
                          /*-----------------------------------------*/
                          /* Defaults for these options are set by   */
                          /* the DATATYPE= option.  When you can,    */
                          /* you should use DATATYPE= instead of any */
                          /* of these options.  If you do use these  */
                          /* options, specify a variable, in         */
                          /* TYPEVAR=, whose values distinguish the  */
                          /* observation types.  Specify the list of */
                          /* valid types in TYPES=.  Then specify    */
                          /* colors, fonts, sizes, and so on for the */
                          /* various observation types.              */
                          /* Alternatively, you can use these        */
                          /* options with DATATYPE=.  Specify lists  */
                          /* for just those label or symbol          */
                          /* characteristics you want to change, for */
                          /* example colors, fonts or sizes.         */
                          /*                                         */
typevar=,                 /* Variable that is looked at for the      */
                          /* observation types.  By default, this    */
                          /* will be _TYPE_ if it is in the input    */
                          /* data set.                               */
                          /*                                         */
                          /* These next options specify lists that   */
                          /* are all related.  They specify how to   */
                          /* process the plot.  Observations are     */
                          /* distinguished by their values on the    */
                          /* TYPEVAR= variable, and each type of     */
                          /* observation can have a different color, */
                          /* size, symbol and so on.  Be aware that  */
                          /* your lists are added to internally.     */
                          /*                                         */
                          /* These next options consist of lists of  */
                          /* strings.  The lists do not all have to  */
                          /* have the same number of elements.  The  */
                          /* number of elements in TYPES= determines */
                          /* how many of the other list elements are */
                          /* used.  When an observation type does    */
                          /* not match one of the TYPE= values, the  */
                          /* results are the same as if the first    */
                          /* type were matched.  If one of the other */
                          /* lists is shorter than the TYPES= list,  */
                          /* the shorter list is extended by adding  */
                          /* copies of the last element to the end.  */
                          /* Individual list values may be quoted,   */
                          /* but quotes are not required.            */
                          /*                                         */
                          /* ***EMBEDDED BLANKS ARE NOT PERMITTED*** */
                          /*                                         */
                          /* IF YOU EMBED BLANKS, YOU WILL NOT GET   */
                          /* THE RIGHT RESULTS.                      */
                          /*                                         */
                          /* Values of the TYPEVAR= variable are     */
                          /* compressed before they are used, so for */
                          /* example, an _TYPE_ value of 'M COEFFI'  */
                          /* must be specified as 'MCOEFFI'.         */
                          /*                                         */
types=,                   /* Observation types are usually values of */
                          /* a variable like _TYPE_.                 */
                          /*                                         */
                          /* ***EMBEDDED BLANKS ARE NOT PERMITTED*** */
                          /*                                         */
                          /* Examples:                               */
                          /* TYPES='SCORE'                           */
                          /* TYPES='OBS' 'SUPOBS' 'VAR' 'SUPVAR'     */
                          /* TYPES='SCORE'                           */
                          /* TYPES='SCORE' 'MCOEFFI'                 */
                          /*                                         */
                          /* The order in which values are specified */
                          /* for the subsequent options depends on   */
                          /* the order of the types.  The default    */
                          /* types for various DATATYPE= values are  */
                          /* given next:                             */
                          /*                                         */
                          /* corresp: 'VAR' 'OBS' 'SUPVAR' 'SUPOBS'  */
                          /* row:     'VAR' 'OBS' 'SUPVAR' 'SUPOBS'  */
                          /* mca:     'VAR' 'OBS' 'SUPVAR' 'SUPOBS'  */
                          /* column:  'VAR' 'OBS' 'SUPVAR' 'SUPOBS'  */
                          /* mdpref:  'SCORE' 'CORR'                 */
                          /* vector:  'SCORE' 'MCOEFFI'              */
                          /* ideal:   'SCORE' 'MPOINT'               */
                          /* mds:     'SCORE' 'CONFIG'               */
                          /*                                         */
                          /* For combinations, these lists are       */
                          /* combined in order, but without          */
                          /* repeating 'SCORE', for example with     */
                          /* DATATYPE=mdpref vector ideal, the       */
                          /* default TYPES= list is:                 */
                          /* 'SCORE' 'CORR' 'MCOEFFI' 'MPOINT'.      */
                          /*                                         */
symbols=,                 /* Plotting symbols.  Symbols may be more  */
                          /* than a single character.  You must      */
                          /* specify SYMLEN=n for longer lengths.    */
                          /* Blank symbols *MUST* be specified as '' */
                          /* with no embedded blanks.                */
                          /* Examples:                               */
                          /* SYMBOLS='*',                            */
                          /* SYMBOLS='**'                            */
                          /* SYMBOLS='*' '+' '*' ''                  */
                          /* SYMBOLS='NC' 'OH' 'NJ' 'NY'             */
                          /*                                         */
symtype=,                 /* Types of symbols.  Valid values are     */
                          /* symbol, vector, circle, contour,        */
                          /* square.  Examples:                      */
                          /* SYMTYPE='symbol'                        */
                          /* SYMTYPE='symbol' 'vector'               */
                          /* SYMTYPE='symbol' 'circle'               */
                          /*                                         */
symcol=,                  /* Colors of symbols.  The default list    */
                          /* is constructed from the COLORS= option. */
                          /* Examples:                               */
                          /* SYMCOL='red'                            */
                          /* SYMCOL='red' 'white' 'blue'             */
                          /*                                         */
symsize=,                 /* Sizes of symbol.                        */
                          /* Examples:                               */
                          /* SYMSIZE=1                               */
                          /* SYMSIZE=1 1.5                           */
                          /*                                         */
symfont=,                 /* Symbol fonts.  The font is ignored for  */
                          /* vectors with no symbols.  Examples:     */
                          /* SYMFONT='swiss'                         */
                          /* SYMFONT='swiss' 'swissi'                */
                          /*                                         */
labcol=,                  /* Colors for the label associated with    */
                          /* the symbol.  The default list is        */
                          /* constructed from the COLORS= option.    */
                          /* Examples:                               */
                          /* LABCOL='red'                            */
                          /* LABCOL='red' 'white' 'blue'             */
                          /*                                         */
labsize=,                 /* Sizes for the label associated with the */
                          /* symbol.  Examples:                      */
                          /* LABSIZE=1                               */
                          /* LABSIZE=1 1.5                           */
                          /* LABSIZE=1 0                             */
                          /*                                         */
labfont=,                 /* Fonts for the label associated with the */
                          /* symbol.  Examples:                      */
                          /* LABFONT='swiss'                         */
                          /* LABFONT='swiss' 'swissi'                */
                          /*-----------------------------------------*/
britypes=symbol,          /* Types to which BRIGHT= applies.         */
                          /*                                         */
rgbtypes=symbol,          /* Types to which PAINT=, RED=, GREEN=,    */
                          /* and BLUE= apply.                        */
                          /*                                         */
exttypes=vector,          /* Types to always put in the EXTRAOBS=    */
                          /* data set when they have blank labels.   */
                          /*-----------------------------------------*/
filepref=sasplot,         /* File name prefix.                       */
                          /*                                         */
post=,                    /* Graphics stream file name.  The default */
                          /* name is constructed from the FILEPREF=  */
                          /* value and "ps" in a host specific way.  */
                          /*                                         */
tempdat1=tempdat1,        /* Data sets used to hold intermediate     */
tempdat2=tempdat2,        /* results.                                */
tempdat3=tempdat3,        /*                                         */
tempdat4=tempdat4,        /*                                         */
tempdat5=tempdat5,        /*                                         */
tempdat6=tempdat6,        /*                                         */
                          /*                                         */
preproc=preproc,          /* Data set to contain the preprocessed    */
                          /* DATA= data set.                         */
                          /*                                         */
extraobs=extraobs,        /* Data set to contain the extra           */
                          /* observations that do not go through     */
                          /* PROC PLOT.                              */
                          /*                                         */
regdat=regdat,            /* Data set used to contain intermediate   */
                          /* regression results for curve fitting.   */
                          /*-----------------------------------------*/
procopts=nolegend,        /* PROC PLOT statement options.            */
                          /*                                         */
plotopts=,                /* PLOT statement options.  BOX will be    */
                          /* specified, even if you do not specify   */
                          /* it.  Reference lines should not be      */
                          /* specified using the PROC PLOT HREF= and */
                          /* VREF= options.  Instead, directly use   */
                          /* the macro options.                      */
                          /*                                         */
href=,                    /* Horizontal reference lines.  Specify a  */
                          /* DATA step DO list.                      */
                          /*                                         */
vref=,                    /* Vertical reference lines.  Specify a    */
                          /* DATA step DO list.                      */
                          /*                                         */
hminor=,                  /* Number of horizontal axis minor tick    */
                          /* marks between major tick marks.  A      */
                          /* typical value is 9.  The number cannot  */
                          /* be specified when HAXIS= is specified   */
                          /* with PLOTOPTS=.  Alternatively, specify */
                          /* a data step DO list.  Note that with    */
                          /* log scaling, specify log10's of the     */
                          /* data values.  For example, HMINOR=0.25  */
                          /* to 5 by 0.25, with data ranging up to   */
                          /* 10**5.                                  */
                          /*                                         */
vminor=,                  /* Number of vertical axis minor tick      */
                          /* marks between major tick marks.  A      */
                          /* typical value is 9.  The number cannot  */
                          /* be specified when VAXIS= is specified   */
                          /* with PLOTOPTS=.  Alternatively, specify */
                          /* a data step DO list.  Note that with    */
                          /* log scaling, specify log10's of the     */
                          /* data values.  For example, VMINOR=0.25  */
                          /* to 5 by 0.25, with data ranging up to   */
                          /* 10**5.                                  */
                          /*                                         */
label=,                   /* LABEL statement.  Note that specifying  */
                          /* the keyword LABEL is optional.          */
                          /* Abbreviation: LABEL=typical specifies   */
                          /* that a label statement be constructed   */
                          /* with 'Dimension' and the numeric suffix */
                          /* of the variable name, for example,      */
                          /* LABEL DIM1 = 'Dimension 1'              */
                          /*       DIM2 = 'Dimension 2';             */
                          /* when PLOTVARS=DIM2 DIM1.  LABEL=typical */
                          /* can only be used with variable names    */
                          /* that consist of a prefix and a numeric  */
                          /* suffix.                                 */
                          /*                                         */
place=2 search,           /* Generates a PLACEMENT= option for the   */
                          /* plot request.  Specify a nonnegative    */
                          /* integer.  Values greater than 13 are    */
                          /* set to 13.  As the value gets larger,   */
                          /* the procedure is given more freedom to  */
                          /* move the labels farther from the        */
                          /* symbols.  The generated placement list  */
                          /* will be printed in the log.  You can    */
                          /* still specify PLACEMENT= directly on    */
                          /* the PLOTOPTS= option.  This option just */
                          /* gives you a shorthand notation.         */
                          /* For example:                            */
                          /* PLACE=0 - placement=((s=center))        */
                          /* PLACE=1 - placement=(h=2 -2 : s=right   */
                          /* left) (v=1 * h=0 -1 to -2 by alt))      */
                          /* PLACE=2 - placement=(h=2 -2 : s=right   */
                          /* left) (v=1 -1 * h=0 -1 to -5 by alt))   */
                          /* PLACE=3 - placement=(h=2 -2 : s=right   */
                          /* left) (v=1 to 2 by alt * h=0 -1 to -10  */
                          /* by alt))                                */
                          /* PLACE=4 - placement=((h=2 -2 : s=right  */
                          /* left) (v=1 to 2 by alt * h=0 -1 to -10  */
                          /* by alt) (s=center right left * v=0 1 to */
                          /* 2 by alt * h=0 -1 to -6 by alt * l= 1   */
                          /* to 2))                                  */
                          /* and so on.                              */
                          /*                                         */
                          /* The PLACE= option, along with the LS=   */
                          /* option can be used to search for an     */
                          /* optimal placement list and an optimal   */
                          /* line size.  By default, the macro will  */
                          /* create and recreate the plot until it   */
                          /* avoids all collisions.  The search is   */
                          /* turned off when a PLACEMENT= option is  */
                          /* detected in the plot request or plot    */
                          /* options.                                */
                          /*                                         */
                          /* If search is not specified with PLACE=  */
                          /* or LS=, the specified value is fixed.   */
                          /* If search is specified with the other   */
                          /* option, only that option's value is     */
                          /* incremented in the search.              */
                          /*                                         */
ls=compute search,        /* Specifies how line sizes are generated. */
                          /* When the second word is "search", the   */
                          /* macro searches for an optimal line      */
                          /* size.  See the PLACE= option for more   */
                          /* information on searches.  When the      */
                          /* first word is "compute", the line size  */
                          /* is computed from the iteration number   */
                          /* so that the line sizes are:             */
                          /* 65 80 100 125 150 175 200.              */
                          /* Otherwise the first word is the first   */
                          /* linesize and with each iteration the    */
                          /* linesize is incremented by the LSINC=   */
                          /* amount.  Example: LS=65 search.         */
                          /*                                         */
maxiter=15,               /* Maximum number of iterations.           */
                          /*                                         */
maxokpen=0,               /* Maximum acceptable penalty sum.         */
                          /*                                         */
lsinc=15,                 /* Increment to line size in iterations    */
                          /* when line size is not computed.         */
                          /*-----------------------------------------*/
font=swiss,               /* Default font.                           */
                          /*                                         */
lsizes=1 1 1 1 1,         /* Line sizes (thicknesses) for frame,     */
                          /* ticks, vectors, circles, curves.        */
                          /*                                         */
tsize=1,                  /* Default text size.                      */
                          /*                                         */
ticklen=1.5,              /* Length of tick mark in horizontal       */
                          /* cells.  A negative value can be         */
                          /* specified to indicate that only half    */
                          /* ticks should be used, which means the   */
                          /* ticks run to but not across the axes.   */
                          /*                                         */
tickaxes=LRTBFlb,         /* Axes to draw tick marks.  Default:      */
                          /* TICKAXES=LRTBFlb, means major ticks on  */
                          /* left (L), right (R), top (T), and       */
                          /* bottom (B), and the full frame (F) is   */
                          /* to be drawn, and potentially minor tick */
                          /* marks on the left (l) and bottom (b).   */
                          /* Minor ticks on the right (r) and top    */
                          /* (t) can also be requested.  To just     */
                          /* have major tick marks on the left and   */
                          /* bottom axes, and no full frame, specify */
                          /* TICKAXES=LB.  Order and spacing do not  */
                          /* matter.  HMINOR= and VMINOR= must also  */
                          /* be specified to get minor ticks.        */
                          /*                                         */
extend=,                  /* This option is used to extend the X and */
                          /* Y axes beyond their default lengths.    */
                          /* Specify four values, for left, right,   */
                          /* top, and bottom.  Missing values are    */
                          /* set by the macro.  If the word 'close'  */
                          /* is specified somewhere in the string,   */
                          /* then macro moves the axes in close to   */
                          /* the extreme data values, and the        */
                          /* computed values are added to the        */
                          /* specified values (if any).  Sample      */
                          /* specifications:                         */
                          /* EXTEND=2 2, or EXTEND=3 3 -0.5 0.5.     */
                          /* Specifying a positive value n extends   */
                          /* the axis n positions in the indicated   */
                          /* direction.  Specifying a negative value */
                          /* shrinks the axis.  The defaults for     */
                          /* missing values are in the range -2 to   */
                          /* 2, and are chosen in an attempt to add  */
                          /* a little extra horizontal space and     */
                          /* make equal the extra space next to each */
                          /* of the four extreme ticks.  When there  */
                          /* is enough space, the horizontal axis is */
                          /* slightly extended by default to         */
                          /* decrease the chance of a label slightly */
                          /* extending outside the plot.  PROC PLOT  */
                          /* usually puts one or two more lines on   */
                          /* the top of the plot than in the bottom. */
                          /* The macro tries to eliminate this       */
                          /* discrepancy.  This option does not add  */
                          /* any tick marks; it just extends or      */
                          /* shrinks the ends of the axis lines.  So */
                          /* typically, only small values should be  */
                          /* specified.  Be careful with this option */
                          /* and a positive MAKEFIT= value.          */
                          /*                                         */
offset=0.25,              /* Move symbols for coincident points      */
                          /* OFFSET= spaces up/down and left/right.  */
                          /* This helps with the resolution of       */
                          /* coincident symbols.  Specify a null     */
                          /* value (OFFSET=,) to turn off            */
                          /* offsetting.                             */
                          /*                                         */
interpol=yes,             /* Specifies the interpolation method.     */
                          /* The default, unless ls or no is         */
                          /* specified, is to interpolate symbol     */
                          /* locations, starting with least squares  */
                          /* but replacing them with tick-based      */
                          /* estimates when they are available.      */
                          /* ls - use the least-squares method only. */
                          /* Compute the mapping between data and    */
                          /* positions using OLS linear regression.  */
                          /* Usually, you should not specify         */
                          /* INTERPOL=ls because slight inaccuracies */
                          /* may result, producing aesthetically     */
                          /* unappealing plots.                      */
                          /* tick - use tick mark method.  Compute   */
                          /* the slope and intercept using tick      */
                          /* marks and their values.  Tick marks are */
                          /* read using the TICKFOR= format.         */
                          /* no - do not interpolate.                */
                          /* hlog - x-axis is on a log scale.        */
                          /* vlog - y-axis is on a log scale.        */
                          /* This option makes the symbols, vectors, */
                          /* and circles map to the location they    */
                          /* would in a true graphical scatter plot, */
                          /* not the cell locations from PROC PLOT.  */
                          /* This option has no effect on labels,    */
                          /* frame, reference lines, titles, or      */
                          /* ticks.  INTERPOL=no plots tend to look  */
                          /* nicer while INTERPOL=yes plots are      */
                          /* slightly more accurate.  Note that the  */
                          /* strategy used to interpolate can be     */
                          /* defeated in certain cases.  If the      */
                          /* horizontal axis tick values print       */
                          /* vertically, specify INTERPOL=ls.  The   */
                          /* hlog and vlog values are specified in   */
                          /* addition to the method.  For example,   */
                          /* INTERPOL=yes vlog hlog.                 */
                          /*                                         */
tickfor=32.,              /* Tick format used by INTERPOL=tick.      */
                          /* Change this if the tick values in the   */
                          /* PROC PLOT output cannot be read with    */
                          /* the default format, for example,        */
                          /* specify TICKFOR=date7., with dates.     */
                          /*                                         */
inc=,                     /* HAXIS=BY INC and VAXIS=BY INC values.   */
                          /* The specified increments apply to both  */
                          /* axes.  To individually control the      */
                          /* increments, you must specify the PROC   */
                          /* PLOT PLOT statement HAXIS= and VAXIS=   */
                          /* options on the PLOTOPTS= option.  When  */
                          /* you are plotting variables that have    */
                          /* very different scales, you may need to  */
                          /* independently specify appropriate tick  */
                          /* increments for both axes to get a       */
                          /* reasonable plot.  Here is an example:   */
                          /* PLOTOPTS=haxis=by 20 vaxis=by 5000.     */
                          /*-----------------------------------------*/
                          /* Note that the symbol and point label    */
                          /* colors are set by the LABCOL= and       */
                          /* SYMCOL= options.                        */
                          /*                                         */
color=black,               /* Default color, used when no other color */
                          /* is set.                                 */
                          /*                                         */
framecol=,                /* Color of frame, defaults to COLOR=.     */
                          /*                                         */
titlecol=,                /* Color of title, defaults to COLOR=.     */
                          /*                                         */
labelcol=,                /* Color of variable labels, defaults to   */
                          /* COLOR=.                                 */
                          /*                                         */
tickcol=,                 /* Color of ticks, defaults to COLOR=.     */
                          /*                                         */
curvecol=,                /* Color of curve, defaults to COLOR=.     */
                          /*                                         */
cframe=,                  /* Color of background within the frame,   */
                          /* analagous to cframe= option.            */
                          /* By default, when CFRAME= is null,       */
                          /* this option has no effect.              */
                          /*                                         */
monochro=,                /* Overrides all other specified colors.   */
                          /* This option is useful when you have     */
                          /* specified colors and you want to        */
                          /* temporarily override them to send the   */
                          /* plot to a monochrome device.  By        */
                          /* default when MONOCHRO= is null, this    */
                          /* option has no effect.  Typical usage:   */
                          /* MONOCHRO=black.                         */
                          /*                                         */
bright=,                  /* Generate random label colors for        */
                          /* BRITYPES= values.  Normalize so         */
                          /* mean(red,green,blue) equals BRIGHT=.    */
                          /* The valid range is 5 <= BRIGHT <= 250.  */
                          /* 128 is a good value.  Small values will */
                          /* produce essentially black labels and    */
                          /* large values will produce essentially   */
                          /* white labels, and so should be avoided. */
                          /* null value - (BRIGHT=, the default)     */
                          /* means no random labels.  If you get a   */
                          /* "color table full" error message, you   */
                          /* need to specify larger values for the   */
                          /* RGBROUND= option                        */
                          /*                                         */
                          /* COLORS= specifies the default color     */
                          /* list for SYMCOL= and LABCOL=, which are */
                          /* other color options described in a      */
                          /* different section.                      */
                          /*-----------------------------------------*/
colors=blue red green cyan magenta orange gold lilac olive
purple brown gray rose violet salmon yellow,
                          /*-----------------------------------------*/
excolors=,                /* Exclude observations from the annotate  */
                          /* data set with colors in this list.  For */
                          /* example with a black background, to     */
                          /* exclude all observations that have a    */
                          /* color set to "black" and those with a   */
                          /* computed black value, for example from  */
                          /* BRIGHT= or PAINT=, specify              */
                          /* EXCOLORS=black CX000000.  This is       */
                          /* simply done for efficiency, to make the */
                          /* annotate data set smaller.              */
                          /*-----------------------------------------*/
                          /* These next options are used to create   */
                          /* label and symbol colors using some      */
                          /* function of input data set variables.   */
                          /* PAINT= gives you a simple and fairly    */
                          /* general way to interpolate.  RED=,      */
                          /* GREEN=, and BLUE= are used together for */
                          /* many other types of interpolations, but */
                          /* are much harder to use.  These options  */
                          /* apply to RGBTYPES= observations.  If    */
                          /* RED=, GREEN=, and BLUE= are not         */
                          /* flexible enough, for example if you     */
                          /* need full statements, specify RED=128   */
                          /* (so later code will know you are        */
                          /* computing colors) then insert the full  */
                          /* statements you need to generate the     */
                          /* colors using ADJUST1=.                  */
                          /*                                         */
paint=,                   /* Used to interpolate between colors      */
                          /* based on the values of a variable.  The */
                          /* simplest specification is               */
                          /* PAINT=variable.  More generally,        */
                          /* specify: PAINT=variable                 */
                          /* optional-color-list                     */
                          /* optional-data-value-list.               */
                          /* The colors must be selected from: red,  */
                          /* green, blue, yellow, magenta, cyan,     */
                          /* black, white, orange, brown, gray,      */
                          /* olive, pink, purple, violet.  For other */
                          /* colors, specify the RGB color name      */
                          /* (CXrrggbb where rr is the red value, gg */
                          /* is the green, and bb is blue, all three */
                          /* specified in hex).  When a variable     */
                          /* name Z is specified with no other       */
                          /* arguments, the default is               */
                          /* PAINT=z blue magenta red.               */
                          /* PAINT=z red green 1 10, interpolates    */
                          /* between red and green, based on the     */
                          /* values of the variable z, where values  */
                          /* of 1 or less map to red, values of 10   */
                          /* or more map to green, and values in     */
                          /* between map to colors in between.       */
                          /* PAINT=z red yellow green 1 5 10,        */
                          /* interpolates between red at Z=1, yellow */
                          /* at Z=5, and green at Z=10.  If the data */
                          /* value list is omitted it is computed    */
                          /* from the data.                          */
                          /*                                         */
rgbround=-240 1 1 1,      /* Rounding factors used for the PAINT=    */
                          /* variable and RGB values.  The first     */
                          /* value is used to round the PAINT=var    */
                          /* variable.  Specify a positive value to  */
                          /* have the variable rounded to multiples  */
                          /* of that value.  Specify a negative      */
                          /* value n to have a maximum of abs(n)     */
                          /* colors.  For the other three values,    */
                          /* specify positive values.  The last      */
                          /* three are rounding factors for the red, */
                          /* green, and blue component of the color. */
                          /* If more than 256 colors are generated,  */
                          /* you will get the error that a color was */
                          /* not added because the color table is    */
                          /* full.  By default, when a value is      */
                          /* missing, there is no rounding.          */
                          /* Rounding the PAINT= variable is useful  */
                          /* with contour plots.                     */
                          /*                                         */
red=,                     /* Specify for RED=, GREEN=, and BLUE=     */
green=,                   /* arithmetic expressions that produce     */
blue=,                    /* integers in the range 0 to 255.  Colors */
                          /* will be created as follows:             */
                          /* __color =                               */
                          /* 'CX' || put(%if &red ne %then           */
                          /* round(&red,__roured); %else 128;        */
                          /* ,hex2.) || put(%if &green ne %then      */
                          /* round(&green,__rougre); %else 128;,     */
                          /* hex2.) || put(%if &blue ne %then        */
                          /* round(&blue,__roublu); %else 128;       */
                          /* ,hex2.);                                */
                          /* Where the __rou variables are extracted */
                          /* from RGBROUND=.  Example:               */
                          /* RED = min(100 + (z - 10) * 3, 255),     */
                          /* BLUE=50, GREEN=50.                      */
                          /* Then all labels are various shades of   */
                          /* red, depending on the value of z.       */
                          /* Be aware that light colors (small red-  */
                          /* green-blue values) do not show up well  */
                          /* on white backgrounds and dark colors do */
                          /* not show up well on dark backgrounds,   */
                          /* so typically, you will not want to use  */
                          /* the full range of possible              */
                          /* red-green-blue values.  The computed    */
                          /* value was minned with 255 so that if a  */
                          /* larger value is computed, it will not   */
                          /* go over 255 and effectively become      */
                          /* mod(red,255).                           */
                          /*-----------------------------------------*/
                          /* Use these options with contour plots.   */
                          /* For example if the grid for a contour   */
                          /* plot was generated as follows           */
                          /*                                         */
                          /* do x = -4 to 4 by 0.1;                  */
                          /*    do y = -2 to 2 by 0.1;               */
                          /*                                         */
                          /* then specify HNOBS=81, VNOBS=41.  By    */
                          /* default, the square root of the number  */
                          /* of contour type observations is used    */
                          /* for both (which assumes a square grid). */
                          /*                                         */
hnobs=,                   /* Number of horizontal observations in    */
                          /* the grid for contour plots.             */
                          /*                                         */
vnobs=,                   /* Number of vertical observations in the  */
                          /* grid for contour plots.                 */
                          /*-----------------------------------------*/
                          /* You can use the adjust options to add   */
                          /* full SAS data step statements to        */
                          /* strategic places in the macro, such as  */
                          /* the PROC PLOT step, the end of the      */
                          /* preprocessing and last full data steps  */
                          /* to do minor adjustments before the      */
                          /* final plot is produced.                 */
                          /*                                         */
adjust1=,                 /* The following variables are created in  */
                          /* the preprocessing data set:             */
                          /* __lsize  - label size                   */
                          /* __lfont  - label font                   */
                          /* __lcolor - label color                  */
                          /* __ssize  - symbol size                  */
                          /* __sfont  - symbol font                  */
                          /* __scolor - symbol color                 */
                          /* __stype  - symbol type                  */
                          /* __symbol - symbol value                 */
                          /* __otype  - observation type             */
                          /*                                         */
                          /* Use ADJUST1= to adjust these variables  */
                          /* in the preprocessing data set from      */
                          /* their usual values.  You must specify   */
                          /* complete statements with semicolons.    */
                          /* Examples:                               */
                          /* ADJUST1=%str(__lsize = mysize;          */
                          /* __lcolor = mycolor;)                    */
                          /*                                         */
                          /* adjust1=%str(if z > 20 then do;         */
                          /* __scolor = 'green'; __lcolor = 'green'; */
                          /* end;))                                  */
                          /*                                         */
adjust2=,                 /* This option can be used to include      */
                          /* statements with PROC PLOT such as       */
                          /* FORMAT statements.  Just specify the    */
                          /* full statement.                         */
                          /*                                         */
                          /* Use ADJUST3= and ADJUST4= to adjust the */
                          /* final Annotate data set.  For example,  */
                          /* in swiss fonts asterisks are not        */
                          /* vertically centered when printed, so    */
                          /* ADJUST3= converts to use the SYMBOL     */
                          /* function.                               */
                          /*                                         */
adjust3=%str(if text = '*' and function = 'LABEL' then do;
                style = ' '; text = 'star'; function = 'SYMBOL'; end;),
                          /*                                         */
adjust4=,                 /* ADJUST4 can add additional adjustments. */
                          /* If you add new variables to the data    */
                          /* set, you must also include a KEEP       */
                          /* statement.  Here is an example of using */
                          /* ADJUST4= to vertically print the y-axis */
                          /* label, like it would be in PROC PLOT.   */
                          /* adjust4=%str(if angle = 90 then do;     */
                          /*                 angle = 270;            */
                          /*                 rotate = 90;            */
                          /*                 keep rotate;            */
                          /*                 end;)                   */
                          /* This example changes the size of title  */
                          /* lines. adjust4=%str(if index(comment,   */
                          /* 'title') then size = 2;)                */
                          /*                                         */
adjust5=,                 /* Adds extra statements to the final      */
                          /* data step that is used only for         */
                          /* DATATYPE=function.   For example, to    */
                          /* periodically mark the function with     */
                          /* plusses, specify: ADJUST5=%str(         */
                          /* if mod(_n_,30) = 0 then do; size=0.25;  */
                          /* function = 'LABEL'; text = '+'; output; */
                          /* end;)                                   */
                          /*-----------------------------------------*/
antiidea=,                /* Eliminate PREFMAP anti-ideal points.    */
                          /*                                         */
                          /* null value - (ANTIIDEA=, the default) - */
                          /* do nothing.                             */
                          /*                                         */
                          /* 1 - reverse in obs whose _TYPE_         */
                          /* contains 'POINT' when _ISSQ_ > 0.       */
                          /* Specify ANTIIDEA=1 with DATATYPE=ideal  */
                          /* when large data values are positive     */
                          /* or ideal.                               */
                          /*                                         */
                          /* -1 - reverse in obs whose _TYPE_        */
                          /* contains 'POINT' when _ISSQ_ < 0.       */
                          /* Specify ANTIIDEA=-1 with DATATYPE=ideal */
                          /* when small data values are positive     */
                          /* or ideal.                               */
                          /*                                         */
radii=%str(1, 2),         /* Radii of circles (data step do list).   */
                          /* The unit corresponds to the horizontal  */
                          /* axis variable.  The RADII= option can   */
                          /* also specify a variable in the input    */
                          /* data set when radii vary for each       */
                          /* point.                                  */
                          /*                                         */
cirsegs=.1,               /* A circle smoothness parameter used in   */
                          /* determining the number of line segments */
                          /* in each circle.  Smaller values create  */
                          /* smoother circles.  The CIRSEGS= value   */
                          /* is approximately related to the length  */
                          /* of the line segments that compose the   */
                          /* circle.                                 */
                          /*                                         */
vechead=0.2 0.05,         /* How to draw vector heads.  For example, */
                          /* VECHEAD=0.2 0.05, specifies draw a head */
                          /* consisting of two hypotenuses from      */
                          /* triangles with sides 0.2 units long     */
                          /* along the vector and 0.05 units on the  */
                          /* side perpendicular to the vector.       */
                          /*                                         */
outward=,                 /* String for the PLOT statement OUTWARD=  */
                          /* option.  Normally, this option's value  */
                          /* is constructed from the symbol that     */
                          /* holds the place for vectors.  Specify   */
                          /* OUTWARD=none if you want to not have    */
                          /* OUTWARD= specified for vectors:         */
                          /*-----------------------------------------*/
regopts=,                 /* TRANSREG options for curve fitting.     */
                          /* These are specified after the slash in  */
                          /* the functional specification for the    */
                          /* independent variable.  Example:         */
                          /* REGOPTS=nkn=10 evenly.                  */
                          /*                                         */
nknots=,                  /* TRANSREG number of knots option.        */
                          /*                                         */
regprint=noprint,         /* PROC TRANSREG PROC statement options,   */
                          /* typically printing options such as:     */
                          /* NOPRINT - no regression printed output, */
                          /* SS2     - regression results,           */
                          /* SHORT   - suppress iteration histories. */
                          /* To see the regression table, specify:   */
                          /* REGPRINT=SS2 SHORT.                     */
                          /*                                         */
regfun=spline,            /* Function for curve fitting.  Possible   */
                          /* values include LINEAR, SPLINE, MSPLINE, */
                          /* MONOTONE.                               */
                          /*                                         */
cursegs=200,              /* Number of segments in a curve.          */
                          /*-----------------------------------------*/
debug=time,               /* vars   - print macro options and macro  */
                          /*          variables for debugging.       */
                          /* dprint - print intermediate data sets.  */
                          /* notes  - do not specify OPTIONS NONOTES */
                          /*          during most of the macro.      */
                          /* time   - prints total macro run time,   */
                          /*          ignored with OPTIONS NOSTIMER; */
                          /* mprint - run with OPTIONS MPRINT.       */
                          /* Concatenate names for more than one     */
                          /* type of debugging.  Example:            */
                          /* DEBUG=vars dprint notes time mprint.    */
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
                          /* in the printer plot.  A null value can  */
                          /* be specified, VTOH=, when you want the  */
                          /* macro to just fill the window, like a   */
                          /* typical GPLOT.                          */
                          /*                                         */
                          /* Smaller values give you more lines and  */
                          /* smaller labels.  VTOH=1.75 is a good    */
                          /* alternative to VTOH=2 when you need     */
                          /* more lines to avoid collisions.         */
                          /* VTOH=1.75 means 7 columns for each 4    */
                          /* rows between ticks (7 / 4 = 1.75).      */
                          /* VTOH=2 means the plot will have 8       */
                          /* columns for each 4 rows between ticks.  */
                          /* Note that PROC PLOT sometimes takes     */
                          /* this value as a "hint," not a           */
                          /* specification so the actual value may   */
                          /* be slightly different, particularly     */
                          /* when a value other than 2.0 is          */
                          /* specified.  This is generally not a     */
                          /* problem; the macro adjusts accordingly. */
                          /*                                         */
makefit=-0.95,            /* Proportion of graphics window to use.   */
                          /* When the MAKEFIT= is negative, the      */
                          /* absolute value will be used, and the    */
                          /* final value may be changed if the macro */
                          /* thinks part of the plot may extend over */
                          /* the edge.  When a positive value is     */
                          /* specified, it will not be changed by    */
                          /* the macro.  When nonnull, the macro     */
                          /* uses GASK to determine the minimum and  */
                          /* maximum graphics window sizes and makes */
                          /* sure the plot can fit in them.  The     */
                          /* macro uses GOPPRINT= or GOPPLOT= to     */
                          /* determine the device.                   */
                          /*                                         */
unit=in,                  /* HSIZE=, VSIZE=, unit: in or cm.         */
                          /*                                         */
                          /* If you specify just the HSIZE= or the   */
                          /* VSIZE= but not both, you can control    */
                          /* the absolute size of one axis and the   */
                          /* size of the other axis will be scaled   */
                          /* accordingly.                            */
                          /*                                         */
hsize=,                   /* Horizontal graphics area size in UNIT=  */
                          /* units.  The default is the maximum size */
                          /* for the device.  By default when        */
                          /* OPTIONS=nocenter is not specified,      */
                          /* HSIZE= affects the size of the plot but */
                          /* not the HSIZE= GOPTION.  When           */
                          /* OPTIONS=nocenter is specified, HSIZE=   */
                          /* affects both the plot size and the      */
                          /* HSIZE= GOPTION.                         */
                          /*                                         */
vsize=,                   /* Vertical graphics area size in UNIT=    */
                          /* units.  The default is the maximum size */
                          /* for the device.  By default when        */
                          /* OPTIONS=nocenter is not specified,      */
                          /* VSIZE= affects the size of the plot but */
                          /* not the VSIZE= GOPTION.  When           */
                          /* OPTIONS=nocenter is specified, VSIZE=   */
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

*=========================initial macro stuff=========================;

%if not %index(%nrbquote(&debug),notes) %then %str(options nonotes;);
%let abort = 0;

options noserror;
%if %nrquote(&&plotitop) eq %nrstr(&)plotitop %then %let plotitop = ;
options serror;

*------store starting time, initialize a few variables-------;
data _null_;
   length name $ 8 debug value $ 500 glob $ 32767;
   __time = time();
   call symput('start',compress(put(__time,best15.)));

   *------override parameters?-------;
   glob = left(symget('plotitop'));
   if glob ne ' ' then put 'Overridden Parameters:';
   do while(glob ne ' ');
      i = index(glob, '=');
      name = substr(glob, 1, i - 1);
      glob = left(substr(glob, i + 1));
      i = index(glob, ',');
      if i = 0 then i = length(glob) + 1;
      value = substr(glob, 1, i - 1);
      glob = left(substr(glob, i + 1));
      put name +(-1) '=' value;
      call symput(name, trim(value));
      end;

   *------debugging flags-------;
   __debug = symget('debug');
   call symput('dbyes'   ,compress(put(index(__debug,'vars')  ,3.)));
   call symput('dbprint' ,compress(put(index(__debug,'dprint'),3.)));
   call symput('dbtime'  ,compress(put(index(__debug,'time')  ,3.)));
   call symput('dbmprint',compress(put(index(__debug,'mprint'),3.)));

   if _error_ then call symput('abort','1');
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;
%if &dbmprint %then %str(options mprint;);
%let red   = %nrbquote(&red);    %let green = %nrbquote(&green);
%let blue  = %nrbquote(&blue);   %let gout  = %nrbquote(&gout);
%let gname = %nrbquote(&gname);  %let gdesc = %nrbquote(&gdesc);

%if &dbyes %then %put _local_;

data _null_;

   *------mention var names for ordinary SAS syntax check-------;
   retain &plotvars &labelvar &typevar __junk 0

   array __1 &plotvars &labelvar &typevar __junk;

   *------guard against silly n-literals------;
   if length(symget('labelvar')) > 67 or length(symget('symvar')) > 67 or
      length(symget('typevar')) > 67 then do;
      put 'ERROR: Your names are too long!';
      call symput('abort','1');
      end;

   *------store current linesize and pagesize to restore later------;
   length __ls __ps __var $ 8 ;
   __ls   = getoption('linesize');
   __ps   = getoption('pagesize');
   __lab  = getoption('label');
   __var  = getoption('validvarname');
   __page = input(__ps, 8.);
   if __page < 200 then __logps = __page + 1;
   else __logps = 200;
   if __logps < 20 then __logps = 20;

   call symput('v7'      , put(index(__var, '6') eq 0, 1.));
   call symput('restorla', compress(__lab));
   call symput('restorls', compress(__ls));
   call symput('restorps', compress(__ps));
   call symput('logps'   , compress(put(__logps, best8.)));

   if _error_ then call symput('abort','1');
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

options label;

*------start parameter checking, initialization------;
%if %length(%nrbquote(&types))   > 500 or
   %length(%nrbquote(&symbols))  > 500 or
   %length(%nrbquote(&symcol))   > 500 or
   %length(%nrbquote(&symtype))  > 500 or
   %length(%nrbquote(&symsize))  > 500 or
   %length(%nrbquote(&symfont))  > 500 or
   %length(%nrbquote(&labsize))  > 500 or
   %length(%nrbquote(&labcol))   > 500 or
   %length(%nrbquote(&plotopts)) > 500 or
   %length(%nrbquote(&labfont))  > 500 %then %do;
   %put ERROR: A list is more than 500 characters.;
   %plotdump(TYPES SYMBOLS SYMCOL SYMTYPE SYMSIZE SYMFONT
             LABSIZE LABCOL PLOTOPTS LABFONT)
   %let abort = 1;
   %goto endit;
   %end;

*------initialization------;
data _null_;
   file log ps=&logps;
   length data f1-f11 $ 72 name $ 70 opts datatype plotvars $ 500;

   ok = 1;
   datatype = upcase(symget('datatype'));

   *------initialize some macro variables------;
   call symput('singular','1e-8');             /* essentially zero    */
   call symput('allblank','1');                /* all labels blank    */
   call symput('botblank','0');                /* symbol and lab blank*/
   call symput('nlines'  ,'0');                /* last line of plot   */
   call symput('symnumer','0');                /* character symvar    */
   call symput('typenum' ,'0');                /* character typevar   */
   call symput('search'  ,' ');                /* ls, place search    */
   call symput('actualls','-1');               /* actual line size    */
   call symput('symdummy','#');                /* vector, curve sym   */
   call symput('paintcol',' ');                /* paint color list    */
   call symput('paintmin','.');                /* paint variable min  */
   call symput('paintmax','.');                /* paint variable max  */
   call symput('paintnum','0');                /* num of paint colors */
   call symput('ncontour','0');                /* num of contour obs  */
   call symput('hcontour','0');                /* horizo contour size */
   call symput('vcontour','0');                /* vertic contour size */
   call symput('hcondir ','0');                /* horizo cont grid dir*/
   call symput('vcondir ','0');                /* vertic cont grid dir*/
   call symput('looklist','0');                /* look for listing    */
   call symput('sizsquar','1');                /* size square symbol  */
   call symput('ntitles' ,'0');                /* number of titles    */
   call symput('device'  ,symget('sysdevic')); /* current device      */
   call symput('datatype',trim(datatype));     /* upcased data type   */
   call symput('listtitl','*** Wrapped Listing of Point Locations ***');

   *------set vector scale factor------;
   name = ' ';
   do i = 5 to 2 by -1;
      if name = ' ' then name = scan(datatype,i,' ');
      end;
   if name = ' ' or nmiss(input(name,?? 32.)) then name = '1';
   call symput('biplot',trim(name));

   *------make sure there are input data------;
   data = left(upcase(symget('data')));
   if data in (' ' '_LAST_') then do;
      data = symget('syslast'); call symput('data',trim(data));
      end;
   if data = '_NULL_' then do;
      put 'ERROR: No input data set.';
      ok = 0;
      end;

   *------some options cannot have null values------;
   if symget('filepref') = ' ' then call symput('filepref','sasplot');
   if symget('out')      = ' ' then call symput('out'     ,'anno');
   if symget('tempdat1') = ' ' then call symput('tempdat1','tempdat1');
   if symget('tempdat2') = ' ' then call symput('tempdat2','tempdat2');
   if symget('tempdat3') = ' ' then call symput('tempdat3','tempdat3');
   if symget('tempdat4') = ' ' then call symput('tempdat4','tempdat4');
   if symget('tempdat5') = ' ' then call symput('tempdat5','tempdat5');
   if symget('tempdat6') = ' ' then call symput('tempdat6','tempdat6');
   if symget('regdat')   = ' ' then call symput('regdat'  ,'regdat');
   if symget('preproc')  = ' ' then call symput('preproc' ,'preproc');
   if symget('extraobs') = ' ' then call symput('extraobs','extraobs');
   if symget('radii')    = ' ' then call symput('radii'   ,'1, 2');
   if symget('cirsegs')  = ' ' then call symput('cirsegs' ,'.1');
   if symget('cursegs')  = ' ' then call symput('cursegs' ,'200');
   if symget('maxiter')  = ' ' then call symput('maxiter' ,'15');
   if symget('lsinc')    = ' ' then call symput('lsinc'   ,'15');
   if symget('place')    = ' ' then call symput('place'   ,'2 search');
   if symget('symlen')   = ' ' then call symput('symlen'  ,'1');
   if symget('ls')       = ' ' then call symput('ls','compute search');
   if symget('maxokpen') = ' ' then call symput('maxokpen','0');
   if symget('ticklen')  = ' ' then call symput('ticklen' ,'1.5');
   if symget('tickfor')  = ' ' then call symput('tickfor' ,'32.');
   if symget('hnobs')    = ' ' then call symput('hnobs'   ,'0');
   if symget('vnobs')    = ' ' then call symput('vnobs'   ,'0');

   *------is radii a variable name?------;
   name = upcase(substr(left(symget('radii')),1,1));
   call symput('radname', put('A' <= name <= 'Z' or name = '_' or
                              name = '"' or name = "'", 1.));

   *------always use a formchar------;
   call symput('procopts',
               trim(symget('procopts')) || " formchar='|----|+|---'");

   *------was TYPES= specified?------;
   call symput('typespec', put(symget('types') ne ' ',1.));

   *------set default plotvars------;
   plotvars = symget('plotvars');
   if plotvars = ' ' then do;
      if index(datatype,'MDS') or index(datatype,'MCA') or
         index(datatype,'ROW') or index(datatype,'COLUMN') or
         index(datatype,'CORRESP') then plotvars = 'Dim2 Dim1';
      else if index(datatype,'MDPREF') or index(datatype,'VECTOR') or
              index(datatype,'IDEAL') then plotvars = 'Prin2 Prin1';
      call symput('plotvars',trim(plotvars));
      end;

   *------check for conflicting data set names------;
   f1  = left(upcase(symget('data')));
   f2  = left(upcase(symget('out')));
   f3  = left(upcase(symget('tempdat1')));
   f4  = left(upcase(symget('tempdat2')));
   f5  = left(upcase(symget('tempdat3')));
   f6  = left(upcase(symget('tempdat4')));
   f7  = left(upcase(symget('tempdat5')));
   f8  = left(upcase(symget('tempdat6')));
   f9  = left(upcase(symget('preproc')));
   f10 = left(upcase(symget('extraobs')));
   f11 = left(upcase(symget('regdat')));
   opts = 'DATA OUT TEMPDAT1 TEMPDAT2 TEMPDAT3 TEMPDAT4 TEMPDAT5 ' ||
          'TEMPDAT6 PREPROC EXTRAOBS REGDAT';

   array f[11] f1-f11;

   do i = 1 to 11;
      if f[i] =: 'WORK.' then f[i] = substr(f[i],6);
      __name = f[i]; link norm; f[i] = __name;
      do j = 1 to (i - 1);
         if f[i] = f[j] then do;
            oi = scan(opts,i,' ');
            oj = scan(opts,j,' ');
            put 'ERROR: Data sets ' oi +(-1) '=' f[i]
                'and ' oj +(-1) '=' f[j] 'must be different.';
            if oj = 'DATA'
               then put 'WARNING: You may have not specified DATA=.';
            ok = 0;
            end;
         end;
      end;

   if not ok or _error_ then call symput('abort','1');
   return;

norm:
   * Normalize name, upper case, strip n-literals.
   * Input:  __name
   * Output: __name (updated)
   * Sample usage:
   *    __name = '"a b"n';
   *    link norm;
   * Creates: __name = 'A B';
   __name = upcase(__name);
   if substr(__name,1,1) in ("'", '"') then __name = dequote(__name);
   drop __name;
   return;

   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

*------store input data set variable names------;
proc contents data=&data noprint out=&tempdat1;
   run;

%if &syserr > 4 %then %do; %let abort = 1; %goto endit; %end;

proc sort data=&tempdat1(keep=varnum type name);
   by varnum;
   run;

%if &syserr > 4 %then %do; %let abort = 1; %goto endit; %end;

%if &dbprint %then %do;
   proc print data=&tempdat1;
      title3 "&tempdat1 - temporary data set, proc contents results";
      run;
   title3;
   %end;

*------look for the type variable, plot variables------;
data _null_;
   file log ps=&logps;

   length tvname vplotvar hplotvar upname vnormvar hnormvar $ 70
          __list plotvars __name $ 500 __c $ 1;
   retain tvname vplotvar hplotvar hnormvar vnormvar ' '
          ok 1 vfound hfound 0;

   if _n_ = 1 then do;
      f = (index(symget('datatype'), 'FUNCTION') or
           index(upcase(symget('symtype')), 'FUNCTION'));
      call symput('functype', put(f, 1.));
      __name = symget('typevar'); link norm; tvname = __name;

      __list = symget('plotvars'); __n = 1; plotvars = __list;
      link namescan; vplotvar = __name;
      link norm;     vnormvar = __name;
      link namescan; hplotvar = __name;
      link norm;     hnormvar = __name;

      __list = symget('paint');
      link namescan;
      call symput('paintvar',trim(__name));
      call symput('paint'   ,trim(__list));
      end;

   set &tempdat1 end=eof;
   upname = upcase(name);

   if upname = tvname or (tvname = ' ' and upname = '_TYPE_') then do;
      if tvname = ' ' then call symput('typevar','_type_');
      if (type = 1) then call symput('typenum','1');
      end;

   *------plotting variables must be numeric------;
   if (upname = hnormvar or upname = vnormvar) and
      type = 2 then do;
      put 'ERROR: PLOTVARS=' plotvars 'must be numeric.';
      ok = 0;
      end;

   *------set default plotting variables if not set yet------;
   if type = 1 then do;
      if hplotvar = ' ' then do;
         hplotvar = name; hnormvar = upname;
         end;
      else if vplotvar = ' ' then do;
         vplotvar = name; vnormvar = upname;
         end;
      end;

   *------make sure axis variables are found------;
   if upname = hnormvar then do;
      hfound = 1; __name = name; link nliteral; hplotvar = __name;
      end;

   if upname = vnormvar then do;
      vfound = 1; __name = name; link nliteral; vplotvar = __name;
      end;

   *------at end, output results------;
   if eof then do;
      call symput('vplotvar',trim(vplotvar));
      call symput('hplotvar',trim(hplotvar));
      __list = trim(hplotvar) || ' ' || trim(vplotvar);
      call symput('plotvars',trim(hplotvar) || ' ' || trim(vplotvar));
      name = 'A' || vplotvar;
      call symput('appvar1',trim(name));

      if vplotvar = ' ' or hplotvar = ' ' then do;
         put 'ERROR: Not enough variables to plot.';
         ok = 0;
         end;

      if not hfound then do;
         put 'ERROR: Horizontal axis variable ' hplotvar 'not found.';
         ok = 0;
         end;

      if not vfound then do;
         put 'ERROR: Vertical axis variable ' vplotvar 'not found.';
         ok = 0;
         end;

      if hnormvar = '_TYPE_' or vnormvar = '_TYPE_' then
         put 'WARNING: _TYPE_ is one of the axis variables.';

      end;

   if not ok or _error_ then call symput('abort','1');
   return;

norm:
   * Normalize name, upper case, strip n-literals.
   * Input:  __name
   * Output: __name (updated)
   * Sample usage:
   *    __name = '"a b"n';
   *    link norm;
   * Creates: __name = 'A B';
   __name = upcase(__name);
   if substr(__name,1,1) in ("'", '"') then __name = dequote(__name);
   drop __name;
   return;

nliteral:
   * Puts n-literal on name if necessary.
   * Input:  __name
   * Output: __name (updated)
   * Sample usage:
   *    __name = 'a b';
   *    link nliteral;
   * Creates: __name = "'a b'n";
   __v7 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789';
   __len = length(__name);
   if index(__name, ' ') < __len or
      '0' <= substr(__name, 1, 1) <= '9' or
      compress(upcase(__name), __v7) ne ' ' then do;
      __k = 1;
      do __i = 1 to length(__name);
         if substr(__name, __k, 1) eq "'" then do;
            substr(__name, __k + 1) = "'" || substr(__name, __k + 1);
            __k + 1;
            end;
         __k + 1;
         end;
      __name = "'" || trim(__name) || "'n";
      end;
   drop __name __i __k __v7 __len;
   return;

namescan:
   * Returns nth name from a list.
   * Input:  __list, __n
   * Output: __list, (trashed)
   *         __name
   * Sample usage:
   *    __list = 'a "b"n "C d"N d';
   *    __n = 2;
   *    link namescan;
   * Creates: __name = '"b"n'
   * Notes:   __name must have a length of at least 67.
   *          a much longer length (say > 256) helps guard
   *          against long, blank padded n-literals.;
   __list = left(__list);
   do __i = 1 to __n;
      __c = substr(__list, 1, 1);
      if trim(__c) in ("'", '"') then do;
         __e = 0;
         __len = length(__list);
         do __k = 2 to __len until(__e);
            if upcase(substr(__list, __k, 2)) in ("''", '""') then __k + 1;
            else if upcase(substr(__list, __k, 2)) = compress(__c || 'N')
               then __e = __k + 1;
            end;
         end;
      else __e = index(__list, ' ') - 1;
      if __e < 1 then __e = length(__list);
      __name = substr(__list, 1, __e);
      __list = left(substr(__list, __e + 2));
      end;

   *------guard against silly n-literals------;
   if length(__name) > 67 then do;
      put 'ERROR: Your names are too long!';
      call symput('abort','1');
      end;

   drop __list __n __i __k __len __name __c __e;
   return;
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

*------generate TYPES= list from data?------;
%if (&functype or %nrbquote(&datatype) = ) and
   (not &typespec) and &typevar ne %then %do;

   proc freq data=&data noprint order=data;
      tables &typevar / out=&tempdat2;
      run;

   %if &syserr > 4 %then %do; %let abort = 1; %goto endit; %end;

   %if &dbprint %then %do;
      proc print data=&tempdat2;
         title3 "&tempdat2 - temporary data set - proc freq results";
         run;
      title3;
      %end;

   data _null_;
      set &tempdat2 end=eof;
      length __list $ 500;
      retain __list;

      __list = trim(__list) || ' ' || compress(
               %if &typenum %then put(&typevar,best12.);
               %else &typevar;);

      if eof then call symput('types',trim(left(__list)));
      if _error_ then call symput('abort','1');
      run;

   %if &syserr > 4 %then %let abort = 1;
   %if &abort %then %goto endit;
   %end;

*------preliminary preprocessing that changes the data------;
data &preproc;
   set &data;

   if n(&vplotvar,&hplotvar) = 2;

   *------eliminate anti-ideal points------;
   %if &antiidea ne %then %do;
      if index(&typevar,'POINT') then do;
         if n(_issq_) and ((&antiidea >  0 and _issq_ > 0) or
            (&antiidea <= 0 and _issq_ < 0)) then do;
            &vplotvar = -(&vplotvar);
            %if &hplotvar ne &vplotvar %then %do;
               &hplotvar = -(&hplotvar);
               %end;
            end;
         end;
      %end;

   if _error_ then call symput('abort','1');
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

*------more initializations------;
data _null_;
   file log ps=&logps;

   length name symvar $ 70
          color framecol titlecol labelcol tickcol
          curvecol monochro font size name1-name2 tsize $ 12
          anele anele2 $ 16 word $ 24
          list list2 datatype types colors symbols symcol symtype
          symfont symsize labcol labfont labsize
          britypes rgbtypes exttypes $ 500;

   *------set default symbol variable------;
   symvar = symget('symvar');

   *------list of valid DATATYPE= values------;
   array dts[15] corresp mca row column mdpref mdpref2 vector ideal
         mds curve curve2 function contour square symbol;

   *------22 lists, 10 lists of ntypes elements,
                     9 single-element lists
                     3 longer lists------;
   listn = 22; listm = 10; listo = 19;
   array vars[22] $ types colors symbols symcol symtype symfont
                    symsize labcol labfont labsize        /* <= listm */
                    color framecol titlecol labelcol
                    tickcol curvecol monochro font cframe /* <= list0 */
                    britypes rgbtypes exttypes;

   ok = 1;

   *------check method------;
   name = symget('method');
   word = lowcase(compress(name,' 2'));
   if not (word in ('gplot' 'print' 'plot' 'none')) then do;
      put 'ERROR: METHOD=' name 'is not valid.';
      ok = 0;
      end;
   else do;
      call symput('method',trim(word));
      if word = 'plot' then call symput('looklist', '1');
      end;

   *------convert binary options ------;
   list = lowcase(symget('options'));
   call symput('diag'     ,put(index(list,'diag')     > 0,1.));
   call symput('border'   ,put(index(list,'border')   > 0,1.));
   call symput('expand'   ,put(index(list,'expand')   > 0,1.));
   call symput('closebord',put(index(list,'close')    > 0,1.));
   call symput('center'   ,put(index(list,'nocenter') = 0,1.));
   call symput('clip'     ,put(index(list,'noclip')   = 0,1.));
   call symput('delete'   ,put(index(list,'nodelete') = 0,1.));
   call symput('linetext' ,put(index(list,'textline') = 0,1.));
   call symput('squarplt' ,put(index(list,'square')   > 0,1.));
   print = (index(list,'noprint') = 0);
   call symput('legend' ,put(index(list,'nolegend')   = 0 and print,1.));
   call symput('history',put(index(list,'nohistory')  = 0 and print,1.));
   call symput('code'   ,put(index(list,'nocode')     = 0 and print,1.));

   *------make sure all options are recognized------;
   list2 = 'nocenter noclip textline square border expand close ' ||
           'nolegend nohistory nocode noprint nodelete diag';
   do i = 1 to 13;
      word = scan(list2,i); j = index(list,trim(word));
      if j then substr(list,j,length(word)) = ' ';
      end;
   if list ne ' ' then do;
      put 'ERROR: The following options are not recognized: '
          list +(-1) '.';
      ok = 0;
      end;

   *------check INTERPOL=------;
   list = lowcase(symget('interpol'));
   call symput('intrtick',put((index(list,'tick') or not
               (index(list,'ls') or index(list,'no'))),1.));
   call symput('intrls'  ,put((index(list,'ls') or
                               not index(list,'no')),1.));
   call symput('hlogscal',put((index(list,'hlog') > 0),1.));
   call symput('vlogscal',put((index(list,'vlog') > 0),1.));

   *------parse the DATATYPE= option------;
   datatype = symget('datatype'); list = datatype;
   do i = 1 to 15;
      call vname(dts[i],name);
      j = index(datatype,trim(upcase(name)));
      if j then substr(list,j,length(name)) = ' ';
      dts[i] = (j ne 0);
      call symput(name,compress(put(dts[i],1.)));
      end;

   list = compress(list,'0123456789.e+-');
   if list ne ' ' then do;
      put 'ERROR: Invalid DATATYPE=' datatype +(-1) '.';
      ok = 0;
      end;

   *------get the lists that must be quoted------;
   do i = 1 to listn;
      call vname(vars[i],name);
      vars[i] = symget(name);
      end;
   symtype  = lowcase(symtype);
   britypes = lowcase(britypes);
   rgbtypes = lowcase(rgbtypes);
   exttypes = lowcase(exttypes);

   *-----make sure these are set------;
   if font = ' ' then font = 'swiss';
   tsize = symget('tsize');
   if tsize = ' ' then do;
      tsize = '1'; call symput('tsize','1');
      end;

   *------process DATATYPE= option------;
   if datatype ne ' ' then do;

      if contour then do;
         symbols  = trim(symbols)  || " ''";
         symtype  = trim(symtype)  || ' contour';
         symsize  = trim(symsize)  || ' 1';
         symfont  = trim(symfont)  || ' solid';
         labsize  = trim(labsize)  || ' ' || tsize;
         labfont  = trim(labfont)  || ' ' || font;
         rgbtypes = trim(rgbtypes) || ' contour';
         exttypes = trim(exttypes) || ' contour';
         end;

      if square then do;
         symbols  = trim(symbols)  || ' U';
         symtype  = trim(symtype)  || ' square';
         symsize  = trim(symsize)  || ' 1';
         symfont  = trim(symfont)  || ' marker';
         labsize  = trim(labsize)  || ' ' || tsize;
         labfont  = trim(labfont)  || ' ' || font;
         rgbtypes = trim(rgbtypes) || ' square';
         exttypes = trim(exttypes) || ' square';
         end;

      if function then do;
         symbols  = trim(symbols)  || " ''";
         symtype  = trim(symtype)  || ' function';
         symsize  = trim(symsize)  || ' 1';
         symfont  = trim(symfont)  || ' ';
         labsize  = trim(labsize)  || ' ' || tsize;
         labfont  = trim(labfont)  || ' ' || font;
         exttypes = trim(exttypes) || ' function';
         name = symget('labelvar');
         if name = ' ' then call symput('labelvar','_blank_');
         if symvar = ' ' then put 'WARNING: Null symbol variable '
            'specified with ' 'DATATYPE=function.';
         end;

      if mdpref or vector or ideal then
         types = trim(types) || ' SCORE';

      if mdpref or vector or ideal or symbol then do;
         symbols = trim(symbols) || ' *';
         symtype = trim(symtype) || ' symbol';
         symsize = trim(symsize) || ' ' || tsize;
         symfont = trim(symfont) || ' ' || font;
         labsize = trim(labsize) || ' ' || tsize;
         labfont = trim(labfont) || ' ' || font;
         end;

      if corresp or row or mca or column then do;
         types = trim(types) || ' VAR OBS SUPVAR SUPOBS';
         if row then do;
            symtype = trim(symtype) || ' vector symbol symbol symbol';
            symbols = trim(symbols) || " '' * * *";
            end;
         else if column then do;
            symtype = trim(symtype) || ' symbol vector symbol symbol';
            symbols = trim(symbols) || " * '' * *";
            end;
         else do;
            symtype = trim(symtype) || ' symbol symbol symbol symbol';
            symbols = trim(symbols) || ' * * * *';
            end;

         symsize = trim(symsize) || repeat(' ' || trim(tsize),3);
         symfont = trim(symfont) || repeat(' ' || trim(font) ,3);
         labsize = trim(labsize) || repeat(' ' || trim(tsize),3);
         labfont = trim(labfont) || repeat(' ' || trim(font) ,3);
         end;

      if mdpref then do;
         types   = trim(types)   || ' CORR';
         symbols = trim(symbols) || " ''";
         symtype = trim(symtype) || ' vector';
         symsize = trim(symsize) || ' ' || tsize;
         symfont = trim(symfont) || ' ' || trim(font) || 'i';
         labfont = trim(labfont) || ' ' || trim(font) || 'i';
         if mdpref2
            then labsize = trim(labsize) || ' ' ||
                           compress(put(input(tsize, ?? 32.) * 0.75,
                                        best8.));
            else labsize = trim(labsize) || ' 0';
         end;

      size = compress(put(input(tsize,?? 32.) * 1.5, best8.));

      if vector then do;
         types   = trim(types)   || ' MCOEFFI';
         symbols = trim(symbols) || " ''";
         symtype = trim(symtype) || ' vector';
         symsize = trim(symsize) || ' ' || tsize;
         symfont = trim(symfont) || ' ' || trim(font) || 'i';
         labsize = trim(labsize) || ' ' || size;
         labfont = trim(labfont) || ' ' || trim(font) || 'i';
         end;

      if ideal then do;
         types   = trim(types)   || ' MPOINT';
         symbols = trim(symbols) || ' +';
         symtype = trim(symtype) || ' circle';
         symsize = trim(symsize) || ' ' || tsize;
         symfont = trim(symfont) || ' ' || trim(font) || 'i';
         labsize = trim(labsize) || ' ' || size;
         labfont = trim(labfont) || ' ' || trim(font) || 'i';
         end;

      if mds then do;
         types   = trim(types)   || ' CONFIG';
         symbols = trim(symbols) || ' *';
         symtype = trim(symtype) || ' symbol';
         symsize = trim(symsize) || ' ' || tsize;
         symfont = trim(symfont) || ' ' || font;
         labsize = trim(labsize) || ' ' || tsize;
         labfont = trim(labfont) || ' ' || font;
         end;

      if (mds or corresp or mca or row or column or mdpref or
          mdpref2 or vector or ideal) and symget('label') = ' '
         then call symput('label','typical');

      end;

   *------ in case these were only specified in the symtype------;
   if index(symtype, 'contour' ) then call symput('contour' , '1');
   if index(symtype, 'square'  ) then call symput('square'  , '1');

   *------set colors, other defaults------;
   if color    = ' ' then color    = 'cyan';
   if colors   = ' ' then colors   = color;
   if framecol = ' ' then framecol = color;
   if titlecol = ' ' then titlecol = color;
   if labelcol = ' ' then labelcol = color;
   if tickcol  = ' ' then tickcol  = color;
   if curvecol = ' ' then curvecol = color;
   if symtype  = ' ' then symtype  = 'symbol';
   if symsize  = ' ' then symsize  = tsize;
   if symfont  = ' ' then symfont  = font;
   if labsize  = ' ' then labsize  = tsize;
   if labfont  = ' ' then labfont  = font;
   if exttypes = ' ' then exttypes = "''";
   if rgbtypes = ' ' then rgbtypes = "''";
   labcol = trim(labcol) || ' ' || colors;
   symcol = trim(symcol) || ' ' || colors;

   *------default symbols when unspecified------;
   if index(symtype, 'symbol') and symvar eq '_symbol_' and
      symbols = ' ' then do;
      word = scan(symtype, 1, ' ');
      do i = 1 to 500 while(word ne ' ');
         if index(word, 'symbol')
            then symbols = trim(symbols) || ' *';
            else symbols = trim(symbols) || " ''";
         word = scan(symtype, i + 1, ' ');
         end;
      end;
   if symbols = ' ' then symbols = "''";

   *------count the number of types------;
   if types  = ' ' then types  = "''";
   do until(word eq ' ');
      ntypes + 1;
      word = scan(types, ntypes + 1, ' ');
      end;
   holdntyp = ntypes;

   *------output number of observation types------;
   call symput('ntypes',compress(put(ntypes,3.)));

   *------output lists, make sure they are quoted------;
   do i = 1 to listn;
      if i > listm then ntypes = 1;
      if i > listo then ntypes = 200;
      link qlist;
      end;

   ntypes = holdntyp;

   *------output sizes for squares------;
   do i = 1 to ntypes;
      word = scan(symtype, i, ' ');
      if index(word, 'square')
         then call symput('sizsquar',
                          compress(scan(symsize, i, ' '), " '"||'"'));
      end;

   *------check for constant symvar with vectors------;
   symcon = (symvar = ' ' or (compress(symvar,"'"||'"') ne symvar));
   if symcon then do;
      i = length(symvar);
      if i > 3 then do;
         word = upcase(substr(symvar, i - 1));
         if word in ('"N', "'N") then symcon = 0;
         end;
      end;
   call symput('symcon',put(symcon,1.)); /* constant symbol? */
   if symcon and index(symtype,'vector') then do;
      put 'ERROR: Constant SYMVAR= is not allowed with vectors.';
      ok = 0;
      end;

   *------parse RGBROUND= option------;
   list = symget('rgbround'); list2 = ' ';
   do i = 1 to 4;
      name = scan(list, i, ' ');
      num  = input(name, ?? 32.);
      if nmiss(num) then num = .;
      if i > 1 and num <= 0 then num = 1;
      list2 = trim(list2) || ' ' || compress(put(num,best8.));
      end;
   call symput('rgbround',trim(left(list2)));

   *------anything specified for the BRIGHT= option?------;
   if symget('bright') = ' ' then do;
      britypes = ' ';
      call symput('britypes', compress(britypes));
      end;

   *------anything specified for the PAINT= option?------;
   list = left(symget('paint'));
   if symget('paintvar') = ' ' then do;
      rgbtypes = ' ';
      call symput('rgbtypes', compress(rgbtypes));
      if contour then put 'WARNING: PAINT= was not specified '
                          'with a contour plot.';
      end;

   *------parse PAINT= option------;
   else do;

      *------count list elements------;
      do n = 1 to 500 until(name2 = ' ');
         name2 = scan(list,n,' ');
         if n(input(name2,?? 32.)) then name2 = ' ';
         end;
      n = n - 1;

      *------set default, when only a variable name is specified------;
      if n < 1 then do;
         n = 3; list = 'blue magenta red';
         end;

      else if n < 2 then do;
         n = 2; name2 = scan(list,1,' ');
         list = compress(name2) || ' ' || compress(name2);
         end;

      call symput('paintcol', trim(list));

      *------store, check number of list elements------;
      call symput('paintnum',compress(put(n,3.)));

      *------table of recognized colors and their hex rgb------;
      allcols = 'BLACK--BLUE---BROWN--GRAY---GREEN--OLIVE--ORANGE-' ||
                'PINK---PURPLE-RED----VIOLET-WHITE--YELLOW-MAGENTA' ||
                'CYAN---';
      hexcols = '000000 0000ff a05000 808080 00ff00 2a8307 ff8000 ' ||
                'ff0080 703070 ff0000 b090d0 ffffff ffff00 ff00ff ' ||
                '00ffff ';
      list2   = ' ';

      *------construct list of (decimal) RGB values------;
      do j = 1 to 3;
         do i = 1 to n;

            color = upcase(scan(list, i, ' '));
            k     = index(allcols,trim(color));

            *------grab hex code for name, or parse CXrrggbb------;
            if k then name1 = substr(hexcols,k + (j - 1) * 2,2);
            else      name1 = substr(color  ,(j - 1) * 2 + 3,2);

            *------check for validity------;
            num = input(name1,?? hex2.);
            if nmiss(num) then do;
               put 'ERROR: PAINT= color of ' color 'is not valid.';
               ok = 0; j = 4;
               end;

            name1 = compress(put(num,3.));
            if length(list2) + length(name1) + 1 > 500 then do;
               put 'ERROR: PAINT= list is too long.';
               ok = 0; j = 3; i = n;
               end;

            *------build list------;
            list2 = trim(list2) || ' ' || trim(name1);
            end;
         end;

      *------store line segment end points in list if specified------;
      list2 = left(list2);
      name1 = scan(list,n + 1,' ');
      if name1 ne ' ' then do;
         do i = 1 to n;
            name1 = scan(list,n + i,' ');
            if nmiss(input(name1,?? 32.)) then do;
               put 'ERROR: PAINT= data value of ' name1 'is not valid.';
               ok = 0;
               end;
            if length(list2) + length(name1) + 1 > 500 then do;
               put 'ERROR: PAINT= list is too long.';
               ok = 0; i = n;
               end;
            list2 = trim(list2) || ' ' || name1;
            end;
         name1 = scan(list,2 * n + 1,' ');
         if name1 ne ' ' then do;
            put 'ERROR: PAINT= data value list is too long.';
            ok = 0;
            end;
         end;

      *------store processed PAINT= list------;
      call symput('paint',trim(list2));
      end;

   if input(symget('legend'), ?? 32.) then do;
      awidth = 9;
      do i = 1 to ntypes;
         anele  = scan(types, i, ' ');
         awidth = max(awidth, length(anele) + 1);
         awidth = max(awidth, length(scan(symbols, i, ' ')) + 1);
         end;
      cols = min(15 + awidth * ntypes, &restorls);
      cols = 15 + floor((cols - 15) / awidth) * awidth;
      m = floor((&restorls - cols) / 2); cols = cols - 1;
      put / +m 'Types Legend  |' @@; list = types; link legend;
      list = repeat('-', cols); substr(list, 15, 1) = '+'; put +m list;
      put +m 'Symbol Types  |' @@; list = symtype; link legend;
      put +m 'Symbols       |' @@; list = symbols; link legend;
      put +m 'Symbol Colors |' @@; list = symcol;  link legendc;
      put +m 'Label  Colors |' @@; list = labcol;  link legendc;
      put +m 'Symbol Sizes  |' @@; list = symsize; link legend;
      put +m 'Label  Sizes  |' @@; list = labsize; link legend;
      put +m 'Symbol Fonts  |' @@; list = symfont; link legend;
      put +m 'Label  Fonts  |' @@; list = labfont; link legend;
      list = repeat('-', cols); put +m list;
      end;

   if _error_ or not ok then call symput('abort','1');
   stop;
   return;

   legend: *------print legend------;

   j = 0;
   do i = 1 to ntypes;
      anele = compress(scan(list, i, ' '), "'"||'"');
      link printele;
      end;
   put;
   return;

   legendc: *------print legend for colors------;

   j = 0;
   do i = 1 to ntypes;
      anele2 = scan(symtype, i, ' ');
      if index(rgbtypes, trim(anele2)) or
         index(britypes, trim(anele2)) then anele = ' ';
      else anele =  compress(scan(list, i, ' '), "'"||'"');
      link printele;
      end;
   put;
   return;

   printele: *------print one table element------;

   j = j + 1;
   if (15 + awidth * j) > &restorls then do;
      put / +(m+14) '|' @@;
      j = 1;
      end;
   put @(m + 17 + (j - 1) * awidth) anele $ @@;
   return;

   qlist: *------quote the elements of a list------;

   call vname(vars[i],name);
   list2 = ' ';
   list  = vars[i];
   charv = not index(upcase(name),'SIZE');
   word  = scan(list,1,' ');
   do n = 1 to ntypes while(word ne ' ');
      if charv then do;
         word = compress("'" || compress(word,"'"||'"') || "'");
         if not index(substr(word,2),"'") then do;
            put 'ERROR: The list element ' name +(-1) '='
                word 'is too long.';
            call symput('abort','1');
            stop;
            end;
         end;
      if (length(list2) + length(word)) >= 500 then do;
         put 'ERROR: The list ' name +(-1) '=' list2 'is too long.';
         call symput('abort','1');
         stop;
         end;
      else do;
         list2 = trim(list2) || ' ' || word;
         word  = scan(list,n + 1,' ');
         if word = ' ' and i <= listm then word = scan(list2,1,' ');
         end;
      end;

   call symput(name,trim(left(list2)));
   vars[i] = list2;
   return;

   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

*------inertias for correspondence analysis variable labels------;
%if &corresp %then %do;
   data &tempdat2;
      set &data;
      keep contr: inertia;
      if _type_ = 'INERTIA' then do;
         output;
         if _error_ then call symput('abort','1');
         stop;
         end;
      file log ps=&logps;
      put 'ERROR: _TYPE_ = "INERTIA" first observation not found.';
      call symput('abort','1');
      stop;
      run;

   %if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

   %if &dbprint %then %do;
      proc print data=&tempdat2;
         title3 "&tempdat2 - temporary data set, inertias";
         run;
      title3;
      %end;

   %end;

*------check, set remaining variables------;
data _null_;
   file log ps=&logps;

   set &tempdat1 end=eof;

   length __name setl labelvar tvname symvar paintvar upname $ 70;
   retain setl labelvar tvname symvar paintvar ' '
          paintfou typefoun 0 ok 1;

   upname = upcase(name);

   *------get current names------;
   if _n_ = 1 then do;
      tvname   = dequote(symget('typevar'));
      labelvar = dequote(symget('labelvar'));
      paintvar = dequote(symget('paintvar'));
      if &symcon then symvar = &symvar;
      else symvar = dequote(symget('symvar'));
      end;

   *------is the symbol variable numeric or character?------;
   if not &symcon and upname = upcase(symvar) and type = 1
      then call symput('symnumer','1');

   *------find _type_ variable------;
   if upname = upcase(tvname) then typefoun = 1;

   *------find PAINT= variable------;
   if upname = upcase(paintvar) and type = 1 then paintfou = 1;

   *------find default label variable name------;
   else if type = 2 and (setl = ' ' or upname ne '_TYPE_')
      then setl = name;

   *------at end, output results------;
   if eof then do;

      if labelvar = ' ' and setl ne ' ' then labelvar = setl;
      if labelvar = ' ' then labelvar = '_blank_';
      __name = labelvar; link nliteral;
      call symput('labelvar', trim(__name));

      *------assorted error checking------;
      if not typefoun and tvname ne ' ' then do;
         put 'ERROR: TYPEVAR=' tvname 'not found.';
         ok = 0;
         end;

      if not typefoun and input(symget('typespec'), ?? 32.) then do;
         put 'ERROR: A type variable must be available '
             'when TYPES= ' 'is specified.';
         ok = 0;
         end;

      if not (paintvar = ' ' or paintfou) then do;
         put 'ERROR: A numeric PAINT=' paintvar
             'variable was not found.';
         ok = 0;
         end;

      end;

   if not ok or _error_ then call symput('abort','1');
   return;

nliteral:
   * Puts n-literal on name if necessary.
   * Input:  __name
   * Output: __name (updated)
   * Sample usage:
   *    __name = 'a b';
   *    link nliteral;
   * Creates: __name = "'a b'n";
   __v7 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789';
   __len = length(__name);
   if index(__name, ' ') < __len or
      '0' <= substr(__name, 1, 1) <= '9' or
      compress(upcase(__name), __v7) ne ' ' then do;
      __k = 1;
      do __i = 1 to __len;
         if substr(__name, __k, 1) eq "'" then do;
            substr(__name, __k + 1) = "'" || substr(__name, __k + 1);
            __k + 1;
            end;
         __k + 1;
         end;
      __name = "'" || trim(__name) || "'n";
      end;
   drop __name __i __k __v7 __len;
   return;

   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

*------find minima, maxima------;
proc means data=&preproc noprint;
   output out=&tempdat1
      max(&vplotvar &hplotvar &paintvar)=vmax hmax
      %if &paintvar ne %then paintmax;
      min(&vplotvar &hplotvar &paintvar)=vmin hmin
      %if &paintvar ne %then paintmin;;
   run;

%if &syserr > 4 %then %do; %let abort = 1; %goto endit; %end;

%if &dbprint %then %do;
   proc print data=&tempdat1;
      title3 "&tempdat1 - temporary data set - proc means results";
      run;
   title3;
   %end;

*------set goptions------;
goptions &gopts2 device=&device
   %if &method = print %then &gopprint; %else &gopplot; &gopts;

*------check some parameters for valid values, set up others------;
data _null_;

   file log ps=&logps;
   length c $ 1 post $ 65
          vplotvar hplotvar labelvar $ 70 opt sysscp $ 8
          str place vecheadr vecheadw tinc device $ 12
          outward search vechead ls filepref $ 32
          label plotreq plotopts upopts pl
          list labely labelx label $ 500
          ind1 ind2 $ 35;

   ok = 1;

   call symput('somedata','0');
   set &tempdat1(drop=_type_);
   call symput('somedata','1');

   *------check missings------;
   if n(vmin,hmin,vmax,hmax) < 4 then do;
      put 'ERROR: At least one variable is all missing.';
      ok = 0;
      end;

   *------check for nonpositive data with log scales------;
   if (vmin <= 0 and &vlogscal) or (hmin <= 0 and &hlogscal) then do;
      put "ERROR: Nonpositive data with INTERPOL=&interpol..";
      ok = 0;
      end;

   *------store extend values------;
   list = symget('extend');
   call symput('close',compress(put(index(list,'close'),best3.)));
   if nmiss(input(scan(list,1,' '),?? 32.))
      then call symput('extendl',' ');
      else call symput('extendl',scan(list,1,' '));
   if nmiss(input(scan(list,2,' '),?? 32.))
      then call symput('extendr',' ');
      else call symput('extendr',scan(list,2,' '));
   if nmiss(input(scan(list,3,' '),?? 32.))
      then call symput('extendt',' ');
      else call symput('extendt',scan(list,3,' '));
   if nmiss(input(scan(list,4,' '),?? 32.))
      then call symput('extendb',' ');
      else call symput('extendb',scan(list,4,' '));

   *------make sure PAINT= minimum, maximum are stored------;
   %if &paintvar ne %then %do;

      length paintvar $ 32;
      paintvar = dequote(symget('paintvar'));

      if n(paintmin,paintmax) ne 2 then do;
         put 'ERROR: All missing PAINT= variable ' paintvar +(-1) '.';
         ok = 0;
         end;

      if paintmax - paintmin < &singular then do;
         put "ERROR: Insufficient range in PAINT= minimum and maximum.";
         ok = 0;
         end;

      else do;
         list = symget('rgbround'); n = input(scan(list,1,' '),?? 32.);
         if n(n) and n < 0 then n = -(paintmax - paintmin) / n;
         list = trim(list) || ' ' || compress(put(n,best8.));
         call symput('rgbround',trim(list));
         list = symget('paint');

         *------store the paint value list if unspecified------;
         if scan(list,3 * &paintnum + 1,' ') = ' ' then do;
            inc = (paintmax - paintmin) / (&paintnum - 1);
            do i = paintmin to paintmax by inc;
               if abs(i) < inc * 0.01 then opt = '0';
               else opt = compress(put(i,D8.));
               if length(list) + length(opt) + 1 > 500 then do;
                  put 'ERROR: PAINT= list is too long.';
                  ok = 0; i = paintmax;
                  end;
               list = trim(list) || ' ' || compress(opt);
               end;
            call symput('paint',trim(list));
            end;
         end;

      *------print the paint legend, the value for each color------;
      if &legend then do;
         length legend $ 64;
         legend = 'Paint Legend - ' || paintvar;
         m = floor((&restorls - length(legend)) / 2);
         put / +m legend;
         m = floor((&restorls - 24) / 2);
         put   +m '------------------------';
         do j = 1 to &paintnum;
            str = scan(symget('paintcol'), j, ' ');
            opt = scan(list, 3 * &paintnum + j, ' ');
            opt = right(opt);
            put +m +3 opt $char8. ' = ' str;
            end;
         put +m '------------------------';
         end;

      %end;

   %else %do;
      call symput('rgbround', trim(symget('rgbround')) || ' .');
      %end;

   *------set default file names------;
   sysscp   = symget('sysscp');
   filepref = symget('filepref');
   post     = symget('post');
   if post = ' ' then do;
      if sysscp = 'CMS'
         then post = trim(filepref) || ' ' || 'ps';
      else if sysscp =: 'VMS' or sysscp = 'WIN' or
              sysscp = 'OS2'
         then post = trim(filepref) || '.' || 'ps';
         else post = '.' || trim(filepref) || '.' || 'ps';
      end;

   *------compute (possibly adjusted) range------;
   isavec = index(symget('symtype'),'vector');
   if isavec then do;
      vmin = min(0,vmin); hmin = min(0,hmin);
      vmax = max(0,vmax); hmax = max(0,hmax);
      end;

   if &squarplt then do;
      vmax = max(vmax,hmax); hmax = vmax;
      vmin = min(vmin,hmin); hmin = vmin;
      end;

   if &hlogscal and not &vlogscal then range = vmax - vmin;
   else if &vlogscal and not &hlogscal then range = hmax - hmin;
   else range = max(vmax - vmin, hmax - hmin);
   range = max(1e4 * &singular,range);

   *------default increment, minima, maxima------;
   inc = input(symget('inc'),?? 32.);
   if n(inc) = 0 then do;
      inc = 10 ** ceil(log10(range) - 1.0);
      if range / inc >= 7.5 then inc = inc * 2;
      if range / inc <= 2.5 then inc = inc / 2;
      if range / inc <= 2.5 then inc = inc / 2;
      end;
   tinc = compress(put(inc,best12.));

   if n(inc) = 0 then do;
      put 'ERROR: Input data set is empty, constant, or corrupt.';
      ok = 0;
      end;

   call symput('inc' ,trim(tinc));
   call symput('hmax',compress(put(hmax,best15.)));
   call symput('hmin',compress(put(hmin,best15.)));
   call symput('vmax',compress(put(vmax,best15.)));
   call symput('vmin',compress(put(vmin,best15.)));

   *------construct plot request------;
   vplotvar = symget('vplotvar');
   hplotvar = symget('hplotvar');
   plotopts = symget('plotopts');
   labelvar = symget('labelvar');
   upopts   = upcase(plotopts);
   plotreq  = trim(vplotvar) || ' * ' || trim(hplotvar) || ' $ ' ||
              trim(labelvar) || ' = _symbol_';

   *------check for HREF= VREF= options------;
   if index(upopts,'HREF') or index(upopts,'VREF') then
      put 'WARNING: HREF= and VREF= PLOT options are ignored.  '
          'Specify ' 'them as ' 'macro options ' 'instead.';

   *------see if we need to generate a placement list------;
   pl = ' '; i = index(upopts,'PLACE');
   if i then do;
      pl = substr(plotopts,i);
      if i > 1 then plotopts = substr(plotopts,1,i - 1);
      else plotopts = ' ';
      end;

   *------handle horizontal ticks if not already specified------;
   c = 'h'; maxim = hmax; minim = hmin; logscal = &hlogscal;
   link maketick;

   *------handle vertical ticks if not already specified------;
   c = 'v'; maxim = vmax; minim = vmin; logscal = &vlogscal;
   link maketick;

   *------outward option------;
   outward = symget('outward');
   if outward ne 'none' then do;
      if outward = ' ' and isavec then outward = symget('symdummy');
      if outward ne ' '
         then plotopts = trim(plotopts) || ' outward="' ||
                         compress(outward,"'"||'" ') || '"';
      end;

   *------box option------;
   plotopts = left(trim(plotopts) || ' box');

   *------reference lines------;
   list = symget('href');
   if list ne ' ' then plotopts = trim(plotopts) || ' href=' || list;
   list = symget('vref');
   if list ne ' ' then plotopts = trim(plotopts) || ' vref=' || list;

   *------set up vector heads------;
   vechead  = symget('vechead');
   vecheadr = scan(vechead,1,' ');
   vecheadw = scan(vechead,2,' ');
   if vecheadw = ' ' then vecheadr = ' ';

   *------determine graphics area------;
   rc1 = ginit();
   call gask('maxdisp',units,xmax,ymax,xpix,ypix,rc2);
   call gask('device',device,rc3);
   rc4 = gterm();
   if rc1 or rc2 or rc3 or rc4 then do;
      put 'ERROR: GASK call for XMAX= and YMAX= failed.';
      call symput('abort','1');
      end;

   xmax = xmax * 100; ymax = ymax * 100;

   if symget('unit') = 'in' then do;
      xmax = xmax / 2.54; ymax = ymax / 2.54;
      end;

   xmax = floor(xmax * 100) / 100;
   ymax = floor(ymax * 100) / 100;

   *------max horizontal graph size------;
   if symget('xmax') = ' '
      then call symput('xmax',compress(put(xmax,9.2)));

   *-------max vertical graph size------;
   if symget('ymax') = ' '
      then call symput('ymax',compress(put(ymax,9.2)));

   *------construct typical label------;
   label = symget('label');
   if label = 'typical' then do;

      ind1 = substr(vplotvar,length(vplotvar),1);
      ind2 = substr(hplotvar,length(hplotvar),1);

      if not ('1' <= ind1 <= '9' and '1' <= ind2 <= '9')
         then label = ' ';

      else do;

         do i = length(vplotvar) to 1 by -1;
            x = substr(vplotvar,i,1);
            if not ('1' <= x <= '9') then do;
               ind1 = substr(vplotvar, i + 1);
               i = 0;
               end;
            end;

         do i = length(hplotvar) to 1 by -1;
            x = substr(hplotvar,i,1);
            if not ('1' <= x <= '9') then do;
               ind2 = substr(hplotvar, i + 1);
               i = 0;
               end;
            end;

         *------for correspondence analysis,     ------;
         *------put inertia percentages in labels------;
         %if &corresp %then %do;

            length pin1 pin2 $ 6;
            set &tempdat2;
            array contr[*] contr:;
            pin1 = put(100.0 * contr[input(ind1,8.)] / inertia,6.2);
            pin2 = put(100.0 * contr[input(ind2,8.)] / inertia,6.2);
            if pin1 ne ' ' and pin2 ne ' ' then do;
               labely = "Dimension " || compress(ind1) || ' (' ||
                         compress(pin1) || "%)";
               labelx = "Dimension " || compress(ind2) || ' (' ||
                         compress(pin2) || "%)";
               end;

            %end;

         if labelx = ' ' then do;
            labely = "Dimension " || compress(ind1);
            labelx = "Dimension " || compress(ind2);
            end;

         label = 'label ' ||
                 symget('vplotvar') || " = '" || trim(labely) || "' " ||
                 symget('hplotvar') || " = '" || trim(labelx) || "'";

         end;

      end;

   *------simple, imperfect, label statement check------;
   label = left(label);
   if label ne ' ' and compress(label,"'"||'="') = label then do;
      put 'ERROR: Invalid LABEL statement, LABEL=' label +(-1) '.';
      ok = 0;
      end;

   str = upcase(scan(label,1));
   if str ne 'LABEL' and str ne ' ' then label = 'label ' || label;
   call symput('label' ,trim(label));

   *------make sure we have britypes with BRIGHT= specified------;
   if symget('bright') ne ' ' and symget('britypes') = ' ' then do;
      put 'ERROR: Null BRITYPES= is not '
          'permitted ' 'with ' "BRIGHT=&bright..";
      ok = 0;
      end;

   *------set up search parameters------;
   ls = symget('ls');
   if index(ls,'search') then do;
      ls = scan(ls,1,' ');
      if ls = 'compute' then search = 'compute';
      else search = 'ls';
      end;
   if ls = ' ' or ls = 'compute' then ls = '.';

   if pl = ' ' then place = symget('place');
   else place = '.';
   if index(place,'search') then do;
      search = trim(search) || ' place'; place = scan(place,1,' ');
      end;
   if place = ' ' then place = '.';

   *------see if we should store device------;
   list = upcase(symget('gopprint'));
   if index(list,'DEVICE') then device = ' ';
   list = upcase(symget('gopplot'));
   if index(list,'DEVICE') then device = ' ';
   list = upcase(symget('gopts2'));
   if index(list,'DEVICE') then device = ' ';
   list = upcase(symget('gopts'));
   if index(list,'DEVICE') then device = ' ';

   *------output generated macro variables------;
   call symput('post'    ,trim(post));
   call symput('search'  ,trim(search));
   call symput('ls'      ,trim(ls));
   call symput('place'   ,trim(place));
   call symput('plotopts',trim(plotopts));
   call symput('plotreq' ,trim(plotreq));
   call symput('pl'      ,trim(pl));
   call symput('vecheadr',trim(vecheadr));
   call symput('vecheadw',trim(vecheadw));
   call symput('device'  ,trim(device));

   *------check some parameters for valid values------;
   opt='VECHEAD';  str=vecheadr;    min=   0; max= 100; link checkit;
   opt='VECHEAD';  str=vecheadw;    min=   0; max= 100; link checkit;
   opt='INC';      str=tinc;        min=1e-8; max=1e35; link checkit;
   opt='HNOBS';    str=symget(opt); min=   0; max=1e35; link checkit;
   opt='VNOBS';    str=symget(opt); min=   0; max=1e35; link checkit;
   opt='BIPLOT';   str=symget(opt); min=1e-8; max= 100; link checkit;
   opt='SYMLEN';   str=symget(opt); min=   1; max=  22; link checkit;
   opt='OFFSET';   str=symget(opt); min=   0; max=   1; link checkit;
   opt='TSIZE';    str=symget(opt); min=   0; max= 100; link checkit;
   opt='TICKLEN';  str=symget(opt); min= -10; max=  10; link checkit;
   opt='ANTIIDEA'; str=symget(opt); min=  -2; max=   2; link checkit;
   opt='CIRSEGS';  str=symget(opt); min=1e-4; max= 500; link checkit;
   opt='CURSEGS';  str=symget(opt); min=  20; max= 500; link checkit;
   opt='VTOH';     str=symget(opt); min=   1; max=   3; link checkit;
   opt='MAKEFIT';  str=symget(opt); min= -10; max=  10; link checkit;
   opt='HSIZE';    str=symget(opt); min= 0.1; max= 1e5; link checkit;
   opt='VSIZE';    str=symget(opt); min= 0.1; max= 1e5; link checkit;
   opt='XMAX';     str=symget(opt); min= 0.1; max= 1e5; link checkit;
   opt='YMAX';     str=symget(opt); min= 0.1; max= 1e5; link checkit;
   opt='HPOS';     str=symget(opt); min= 0.1; max= 1e5; link checkit;
   opt='VPOS';     str=symget(opt); min= 0.1; max= 1e5; link checkit;
   opt='PS';       str=symget(opt); min=  15; max= 200; link checkit;
   opt='MAXITER';  str=symget(opt); min=   1; max=  50; link checkit;
   opt='MAXOKPEN'; str=symget(opt); min=   0; max= 1e9; link checkit;
   opt='BRIGHT';   str=symget(opt); min=   5; max= 250; link checkit;
   opt='EXTENDL';  str=symget(opt); min= -50; max=  50; link checkit;
   opt='EXTENDR';  str=symget(opt); min= -50; max=  50; link checkit;
   opt='EXTENDT';  str=symget(opt); min= -50; max=  50; link checkit;
   opt='EXTENDB';  str=symget(opt); min= -50; max=  50; link checkit;
   opt='LSINC';    str=symget(opt); min=   1; max= 200; link checkit;
   opt='LS';       str=ls;          min=  64; max= 200;
   if ls ne '.' then link checkit;
   opt='PLACE';    str=place;       min=   0; max=  13;
   if place ne '.' then link checkit;

   list = compress(symget('tickaxes'));
   call symput('tickaxes',trim(list));
   if compress(list,'LRTBlrtbF') ne ' ' then do;
      put "ERROR: TICKAXES=&tickaxes is not valid.";
      ok = 0;
      end;

   call symput('frame',put(indexc(list,'RTrtF') > 0,1.));

   if not (symget('unit') in ('in' 'cm')) then do;
      put "ERROR: UNIT=&unit is not valid.";
      ok = 0;
      end;

   if scan(symget('lsizes'),5,' ') = ' ' then do;
      put "ERROR: LSIZES=&lsizes must specify five sizes.";
      ok = 0;
      end;

   if _error_ or not ok then call symput('abort','1');

   stop;
   return;

   checkit: *------check parameter range------;

   if str ne ' ' then do;
      param = input(str,?? 32.);
      if not (min <= param <= max) then do;
         ok = 0;
         put 'ERROR: ' opt +(-1) '=' str 'is not valid.  '
             'The valid range is ' min '<= ' opt '<= ' max +(-1) '.';
         end;
      end;
   return;

   maketick: *------handle ticks if not already specified------;

   minor = input(symget(c ||'minor'),?? 32.);
   if index(upopts,upcase(c) || 'AXIS') = 0 then do;
      if maxim > 0 then logmax = ceil(log10(maxim));
      else logmax = .;
      if minim > 0 then logmin = floor(log10(minim));
      else logmin = .;

      *------major ticks for log scale------;
      if logscal and n(logmin) then do;
         plotopts = trim(plotopts) || ' ' || c || 'axis=';
         do i = logmin to logmax;
            plotopts = trim(plotopts) || '1e' ||
                       compress(put(i,4.)) || ',';
            end;
         plotopts = substr(plotopts,1,length(plotopts) - 1);
         end;

      *------ordinary major ticks------;
      else plotopts = trim(plotopts) || ' ' || c || 'axis=by ' || tinc;

      *------minor ticks------;
      if minor >= 1 then do;
         if n(logmax) then do;
            if logscal then do;
               maxim = logmax; minim = logmin; i = 1;
               end;
            else i = inc;
            maxim = ceil(2 + maxim / i) * i;
            minim = floor(minim / i - 2) * i;
            call symput(c || 'minor',
                        compress(put(minim,best12.)) || ' to ' ||
                        compress(put(maxim,best12.)) || ' by ' ||
                        compress(put(i / (minor + 1),best12.)));
            end;
         else call symput(c||'minor',' ');
         end;
      end;

   *------number of minor ticks only allowed with inc is known------;
   else if n(minor) then do;
      put 'WARNING: Minor ticks are suppressed when HAXIS= '
          'or VAXIS= ' 'is specified ' 'on PLOTOPTS=.';
      call symput(c||'minor',' ');
      end;

   return;

   run;

%if &syserr > 4 %then %let abort = 1;
%if not &somedata %then %do;
   %put ERROR: At least one variable is all missing.;
   %let abort = 1;
   %end;
%if &abort %then %goto endit;

*------debugging output------;
%if &dbyes %then %put _local_;

*========================preprocessing of data========================;

*------preprocess the input data set------;
data &preproc
     %if &curve %then &regdat(keep=&plotvars);;

   file log ps=&logps;
   length _symbol_ $ 1 __symbol $ &symlen
          __lfont __lcolor __sfont __scolor __stype __color $ 12
          __otype $ 16 __temp $ 60 __excols $ 64;
   retain __nblank __bblank 0 __oldx __oldy . __color __excols ' ';
   drop __nblank __bblank __dummy __mrkwrn __nconto
        __oldx __oldy __excols;

   if _n_ = 1 then do;

      *------set the variable labels------;
      &label;
      call label(&vplotvar, __temp);
      call symput('labely',trim(__temp));
      call label(&hplotvar, __temp);
      call symput('labelx',trim(__temp));
      drop __temp;

      *------output x coordinates for the curve------;
      %if &curve %then %do;
         drop __inc;
         &vplotvar = .;
         %if &hlogscal %then %do;
            drop __logmin __logmax;
            __logmin = log10(&hmin); __logmax = log10(&hmax);
            __inc = max((__logmax - __logmin) / &cursegs,&singular);
            do __i = __logmin + 0.5 * __inc to __logmax by __inc;
               &hplotvar = exp(__i);
            %end;
         %else %do;
            __inc = max((&hmax - &hmin) / &cursegs,&singular);
            do &hplotvar = &hmin + 0.5 * __inc to &hmax by __inc;
            %end;
            output &regdat;
            end;
         %end;

      *------computed colors------;
      %if &paintvar ne %then %do;
         array __cols[3] __col1-__col3;
         array __paint[%eval(4 * &paintnum)] _temporary_ (&paint);
         drop __col1-__col3 __minx __maxx __minc __maxc __j __i __hold
              __minov __maxov;
         retain __minov __maxov;
         __minov = __paint[3 * &paintnum + 1];
         __maxov = __paint[4 * &paintnum];
         do __i = 3 * &paintnum + 2 to 4 * &paintnum;
            if __paint[__i] - __paint[__i - 1] < &singular then do;
               put 'ERROR: Invalid PAINT= data values.  '
                   'Numbers ' 'must increase.';
               call symput('abort','1');
               stop;
               end;
            end;
        %end;

      *------excluded colors------;
      __excols = lowcase(symget('excolors'));

      drop   __roupai __roured __rougre __roublu;
      retain __roupai __roured __rougre __roublu;
      array  __rou[5] __dummy __roured __rougre __roublu __roupai
                      (&rgbround);

      end;

   *------set up observation type variable------;
   %if &typevar eq %then %do;
      %let typevar = _type_;
      _type_ = repeat(' ',7);
      %end;

   *------preprocess raw data------;
   set &preproc;

   *------store label/symbol colors, sizes, fonts, types------;
   array a__ty[&ntypes] $ 15 _temporary_ (&types);
   __li = 1; drop __li __i;
   do __i = 1 to &ntypes;

      %if &typenum %then %do;
         __otype = left(put(&typevar,best12.));
         __hold = input(a__ty[__i], ?? 32.); drop __hold;
         if n(__hold) and abs(__hold - &typevar) <= &singular then do;
         %end;
      %else %do;
         &typevar = compress(&typevar);
         __otype  = &typevar;
         if &typevar = a__ty[__i] then do;
         %end;
         __li = __i;
         __i = &ntypes;
         end;
      end;

   array a__ls[&ntypes]           _temporary_ (&labsize);
   array a__ss[&ntypes]           _temporary_ (&symsize);
   array a__lf[&ntypes] $      12 _temporary_ (&labfont);
   array a__lc[&ntypes] $      12 _temporary_ (&labcol);
   array a__sf[&ntypes] $      12 _temporary_ (&symfont);
   array a__sc[&ntypes] $      12 _temporary_ (&symcol);
   array a__st[&ntypes] $      12 _temporary_ (&symtype);
   array a__sy[&ntypes] $ &symlen _temporary_ (&symbols);

   __lsize = a__ls[__li]; __ssize  = a__ss[__li];
   __lfont = a__lf[__li]; __lcolor = a__lc[__li];
   __sfont = a__sf[__li]; __scolor = a__sc[__li];
   __stype = a__st[__li]; __symbol = a__sy[__li];

   if upcase(__sfont) = 'MARKER' and
      upcase(__stype) = 'CONTOUR' then do;
      __mrkwrn + 1;
      __sfont = 'solid';
      if __mrkwrn = 1
         then put 'WARNING: The marker font is obsolete '
                  'for contour ' 'plots.  The '
                   'SOLID font ' 'will be ' 'used instead.';
      end;

   *------assign specialized symbols------;
   if symget('symvar') = '_symbol_' then do;
      _symbol_ = __symbol;
      if __stype in ('vector' 'circle') and _symbol_ = ' '
         then _symbol_ = "&symdummy";
      end;

   %if &symvar ne %then %do;
      else do;
         %if &symnumer %then %do;
            _symbol_ = left(put(&symvar,best12.));
            __symbol = left(put(&symvar,best12.));
            %end;
         %else %do;
            _symbol_ = &symvar; __symbol = &symvar;
            %end;
         end;
      %end;

   *------blank out labels?------;
   if __lsize = 0 or symget('labelvar') = '_blank_' then &labelvar = ' ';

   *------stretch vectors------;
   %if &biplot ne 1 %then %do;
      retain hmin &hmin hmax &hmax vmin &vmin vmax &vmax;
      if __stype = 'vector' then do;
         &vplotvar = &vplotvar * &biplot;
         %if &hplotvar ne &vplotvar %then %do;
            &hplotvar = &hplotvar * &biplot;
            %end;
         if vmin > &vplotvar then do;
            vmin = &vplotvar; call symput('vmin',put(vmin,best15.));
            end;
         if vmax < &vplotvar then do;
            vmax = &vplotvar; call symput('vmax',put(vmax,best15.));
            end;
         if hmin > &hplotvar then do;
            hmin = &hplotvar; call symput('hmin',put(hmin,best15.));
            end;
         if hmax < &hplotvar then do;
            hmax = &hplotvar; call symput('hmax',put(hmax,best15.));
            end;
         end;
      %end;

   *------computed colors------;
   %if &paintvar ne %then %do;
      __color = ' ';
      do __i = 2 to &paintnum;
         __minx  = __paint[3 * &paintnum + __i - 1];
         __maxx  = __paint[3 * &paintnum + __i];
         __hold  = &paintvar;
         if n(__hold) then do;
            if n(__roupai) then __hold = round(__hold,__roupai);
            __hold = max(min(__hold,__maxov),__minov);
            if __minx <= __hold <= __maxx then do;
               do __j = 1 to 3;
                  __minc = __paint[(__j - 1) * &paintnum + __i - 1];
                  __maxc = __paint[(__j - 1) * &paintnum + __i];
                  __cols[__j] = (__hold - __minx) * (__maxc - __minc) /
                                (__maxx - __minx) + __minc;
                  __cols[__j] = min(255,round(__cols[__j],
                                              __rou[__j + 1]));
                  end;
               __color = 'CX' || put(__col1,hex2.) ||
                         put(__col2,hex2.) || put(__col3,hex2.);
               end;
            end;
         end;
      %end;

   %else %if &red ne or &green ne or &blue ne %then %do;
      __color = 'CX' || put(%if &red ne %then round(&red,__roured);
                            %else 128;,hex2.)
                     || put(%if &green ne %then round(&green,__rougre);
                            %else 128;,hex2.)
                     || put(%if &blue ne %then round(&blue,__roublu);
                            %else 128;,hex2.);
      %end;

   *-------count contour observations, determine directions------;
   if __stype = 'contour' then do;
      __nconto + 1;
      if __nconto = 1 then do;
         __oldx = &hplotvar; __oldy = &vplotvar;
         end;
      else if __nconto = 2 then do;
         call symput('hcondir',compress(put(&hplotvar-__oldx,best8.)));
         call symput('vcondir',compress(put(&vplotvar-__oldy,best8.)));
         end;
      call symput('ncontour',compress(put(__nconto,best12.)));
      end;

   *------exclude observations in excluded color list------;
   if __color eq ' ' or index(__excols, trim(lowcase(__color))) = 0;

   *------adjustments to preprocessed data------;
   &adjust1;

   __xvar = &hplotvar; __yvar = &vplotvar;

   %if &hlogscal %then %do;
      if n(__xvar) then __xvar = log10(__xvar);
      else __xvar = .;
      %end;
   %if &vlogscal %then %do;
      if n(__yvar) then __yvar = log10(__yvar);
      else __yvar = .;
      %end;

   *------are labels all blank?------;
   if not __nblank then do;
      if &labelvar ne ' ' then do;
         __nblank = 1;
         call symput('allblank','0');
         end;
      end;

   *------are there obs with both label and symbol blank?------;
   if not __bblank then do;
      if &labelvar = ' ' and _symbol_ = ' ' then do;
         __bblank = 1;
         call symput('botblank','1');
         end;
      end;

   output &preproc;

   *------create regression data set for curve fitting------;
   %if &curve %then %str(output &regdat;);

   if _error_ then call symput('abort','1');
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

%if &dbprint %then %do;
   proc print data=&preproc;
      title3 "&preproc - preprocessed data";
      run;
   %if &curve %then %do;
      proc print data=&regdat;
         title3 "&regdat - regression data set for curve fitting";
         run;
      %end;
   title3;
   %end;

*------separate out obs that will not go through PROC PLOT------;
data &preproc &extraobs;

   retain __listit 0;  length __stype $ 12;

   *------make sure PROC PLOT scales axes based on min, max------;
   if _n_ = 1 then do;
      __stype = 'dummy';
      output &extraobs;
      do &hplotvar = &hmin, &hmax;
         do &vplotvar = &vmin, &vmax;
            output &preproc;
            end;
         end;
      __stype = ' ';
      end;

   set &preproc;

   if "&method" = "plot" then output &preproc;
   else if &labelvar = ' ' and __stype in (&exttypes)
      then output &extraobs;
   else if &labelvar ne ' ' or not (&allblank or (&botblank and
      (_symbol_ = ' ' and &labelvar = ' '))) then do;
      if __listit = 0 then do;
         call symput('looklist','1');
         __listit = 1;
         end;
      output &preproc;
      end;
   else output &extraobs;

   if _error_ then call symput('abort','1');
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

%if &dbprint %then %do;
   proc print data=&preproc;
      title3 "&preproc - preprocessed data";
      run;

   proc print data=&extraobs;
      title3 "&extraobs - extra observations data";
      run;
   title3;
   %end;

*------find regression function------;
%if &curve %then %do;

   %if &nknots ne %then %let nknots = nknots=&nknots;

   proc transreg data=&regdat &regprint;
      model ide(&vplotvar) = &regfun(&hplotvar / &regopts &nknots);
      output dap out=&regdat(keep=&appvar1 &plotvars);
      run;

   %if &syserr > 4 %then %do; %let abort = 1; %goto endit; %end;

   %if &curve2 %then %do;
      data &preproc;
         set &preproc &regdat(in=r where=(&vplotvar = .));
         if r then do;
            _symbol_  = "&symdummy";
            __stype   = 'curve';
            &vplotvar = &appvar1;
            __yvar    = &appvar1;
            __xvar    = &hplotvar;
            end;
         drop &appvar1;
         if _error_ then call symput('abort','1');
         run;

      %if &syserr > 4 %then %let abort = 1;
      %if &abort %then %goto endit;
      %end;

   %if &dbprint %then %do;
      proc print data=&preproc;
         title3 "&preproc - preprocessed data with curve";
         run;
      title3;
      %end;

   %end;

*------see if there is anything more than corners in preproc------;
*------initpen=0 when nothing to plot, otherwise missing    ------;
data _null_;
   if _n_ = 0 then set &preproc nobs=__nobs;
   call symput('initpen', scan('0 .', (__nobs > 4) + 1, ' '));
   if _error_ then call symput('abort','1');
   stop;
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

*------set up iteration parameters------;
%let done    = 0;
%let iternum = 0;

*------debugging output------;
%if &dbyes %then
   %plotdump(typevar data abort done iternum ls labely labelx
            allblank botblank looklist ncontour hcondir vcondir initpen);

*=========================create printer plot=========================;

%do %while(not &done);

   data _null_;
      file log ps=&logps;

      length pl $ 200 search $ 24;

      iternum = &iternum + 1; place = &place; ls = &ls;
      search = symget('search');
      ispl   = index(search,'place');
      isls   = index(search,'ls');
      isco   = index(search,'compute');

      *------adjust placement list, line size------;
      if iternum > 1 then do;
         if ispl then place = place + 1;
         if isls then ls = ls + &lsinc;
         end;
      if isco then ls = round(36 + 29 * iternum -
                              20 * log(iternum),5);
      isls = (isls or isco);

      *------last iteration if we cannot increment------;
      *------the parameters any more.             ------;
      if (isls and ispl and ls >= 200 and place >= 13) or
         (isls and not ispl and ls >= 200) or
         (ispl and not isls and place >= 13) or
         (not ispl and not isls and not isco)
         then maxiter = iternum;
         else maxiter = &maxiter;

      if ls > 200 then ls = 200;
      if ls <  64 then ls = 64;
      actualps = input(symget('ps'),?? 32.);
      if n(actualps) = 0 then do;
         vtoh = input(symget('vtoh'),?? 32.);
         if nmiss(vtoh) then do;
            hsize = input(symget('hsize'),?? 32.);
            vsize = input(symget('vsize'),?? 32.);
            if nmiss(hsize) then hsize = &xmax;
            if nmiss(vsize) then vsize = &ymax;
            actualps = round(ls * (vsize / (hsize * 2)));
            end;
         else actualps = round(10 + ls / vtoh,5);
         end;

      if actualps > 200 then actualps = 200;

      *------construct placement list------;
      if n(place) then do;

         if place > 13 then place = 13;
         if place = 0 then pl = '(s=center)';
         else pl = '(h=2 -2 : s=right left)';

         if place = 1 then
            pl = trim(pl) || ' (v=1 * h=0 -1 to -2 by alt)';

         else if place = 2 then
            pl = trim(pl) || ' (v=1 -1 * h=0 -1 to -5 by alt)';

         else if place > 2 then pl = trim(pl) ||
            ' (v=1 to 2 by alt * h=0 -1 to -10 by alt)';

         if place > 3 then do;
            pl = trim(pl) || ' (s=center right left * v=0 1 to '
                 || compress(put(place - 2,5.))
                 || ' by alt * h=0 -1 to '
                 || compress(put(-3 * (place - 2),5.))
                 || ' by alt * l= 1 to '
                 || compress(put(2 + (place - 3.5) / 3,5.))
                 || ')';
            end;

         pl = 'placement=(' || trim(pl) || ')';

         if place > 4 then do;
            pl = trim(pl) || ' penalty(7)=' ||
                 compress(put(1.5 * place,5.));
            end;

         end;

      call symput('maxiter' ,compress(put(maxiter,5.)));
      call symput('iternum' ,compress(put(iternum,5.)));
      call symput('place'   ,compress(put(place,5.)));
      call symput('ls'      ,compress(put(ls,5.)));
      call symput('actualps',compress(put(actualps,5.)));
      if pl ne ' ' then call symput('pl',trim(pl));

      if _error_ then call symput('abort','1');
      stop;
      run;

   %if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

   %if &dbyes %then %plotdump(actualps ls place iternum maxiter pl);

   *------initialize data sets because they might not be created------;
   data &tempdat3; value = &initpen; output; output; run;
   data &tempdat4;
      retain label symbol
             %if &v7 %then startposition; %else startpos; ' '
             vaxis haxis penalty lines vshift hshift
             length n %if &v7 %then vposition hposition;
                              %else vpositio hpositio; 0;
      run;

   options nonumber ls=&ls ps=&actualps;
   ods exclude all;
   proc plot &procopts data=&preproc %if &vtoh ne %then vtoh=&vtoh;;
      ods output LocateFacts(nowarn)=&tempdat3
                 Locate(nowarn)=&tempdat4 Plot=&tempdat5;
      plot &plotreq / &plotopts &pl list=-1;
      &adjust2; label &vplotvar = '#' &hplotvar = '#';
      run; quit;
   ods exclude none;

   options number ls=&restorls ps=&restorps;

   %if &syserr > 4 %then %let abort = 1;
   %if &abort %then %goto endit;

   %if &dbprint %then %do;
      proc print data=&tempdat3;
         title3 "&tempdat3 - temporary data set, locate facts";
         run;

      proc print data=&tempdat4;
         title3 "&tempdat4 - temporary data set, locate list";
         run;

      proc print data=&tempdat5 noobs;
         var batch;
         title3 "&tempdat5 - temporary data set, plot listing";
         run;
      title3;
      %end;

    *------look at the total penalty------;
   data _null_;
      file log ps=&logps;
      done = '0';
      set &tempdat3(obs=1 rename=(value=penalty));

      if n(penalty) && penalty <= &maxokpen then done = '1';
      iternum = &iternum;

      *------print iteration history line------;
      if &code then do;
         place = &place; ls = &ls; actualps = &actualps;
         m = floor((&restorls - 55) / 2);
         if iternum = 1 then do;
            put / +m +5
               'Iterative Scatter Plot of Labeled Points Macro' /;
            put +m 'Iteration' +4 'Place' +4 'Line Size'
                +4 'Page Size' +4 'Penalty';
            put +m
            '-------------------------------------------------------';
            end;

         m = m - 4;
         put +m iternum 9. place 11. ls 11. actualps 13. penalty 13.;
         end;

      if iternum >= &maxiter then done = '1';
      call symput('done',compress(done));

      if _error_ then call symput('abort','1');
      stop;
      run;

   %if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

   %if &done and &method = plot %then %do;

      *------write plot to screen------;
      proc plot &procopts data=&preproc %if &vtoh ne %then vtoh=&vtoh;;
         plot &plotreq / &plotopts &pl list=-1;
         &adjust2;
         run; quit;

      %goto endit;

      %end;

   *------debugging output------;
   %if &dbyes %then %plotdump(abort done);

   %end;

%let procopts = %str(proc plot &procopts data=&preproc);
%if &vtoh ne %then %let procopts = %str(&procopts vtoh=&vtoh);
%let procopts = %str(&procopts;);

*======================post-process plot, step 1======================;

*------put titles, footnotes in plot------;
proc sql;
   create view &tempdat6 as select * from dictionary.titles;
   quit;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

%if &dbprint %then %do;
   proc print data=&tempdat6;
      title3 "&tempdat6 - titles";
      run;
   title3;
   %end;

data _null_;
   set &tempdat6 end=eof;
   if type = 'T' then ntitles + 1;
   if eof then call symput('ntitles', put(ntitles, 2.));
   if _error_ then call symput('abort','1');
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

data &tempdat5(keep=batch);
   if 0 then set &tempdat5;
   set &tempdat6(where=(type='T') rename=(text=batch))
       &tempdat5
       &tempdat6(where=(type='F') rename=(text=batch));
   if type = 'F' or type = 'T' then do;
      i = round((&ls - length(batch)) / 2);
      if i > 0 then do;
         substr(batch, i) = batch;
         substr(batch, 1, i - 1) = ' ';
         end;
      end;
   if not (batch = ' ' and lag(batch) = ' ') then output;
   if type = 'T' and number = &ntitles then do; batch = ' '; output; end;
   if _error_ then call symput('abort','1');
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

*------start storing locations of lines------;
data &out(keep=tx ty comment);

   length c $ 1 line title $ &ls comment $ 25;
   retain coln top bottom afterbot nlines xlabline 0
          topn botn topextra botextra past         0
          vtick1 vtick2 htick1 htick2              .
          counttop                                 1
          right left col1 labcolum               &ls
          title                                  ' ';

   file log ps=&logps;

   link getline;

   *------skip leading blank lines------;
   if _n_ = 1 then do while(line = ' '); link getline; end;

   *------skip note lines and subsequent blanks lines------;
   if xlabline and _n_ > xlabline and not past then do;
      if line =: 'NOTE:' then line = ' ';
      if _n_ > xlabline + 1 then do;
         do while((line =: 'NOTE:' or line = ' ') and not eof);
            link getline;
            end;
         end;
      end;

   *------initialize------;
   length  = length(line);
   if line = ' ' then length = 0;
   comment = 'frame, tick';

   *------store first title line------;
   if title = ' ' then title = line;

   *------find frame location------;
   if left(line) =: '-' and index(line,'-+-') and
      substr(line,length,1) = '-' then do;
      bottom = _n_;           /* last line will be bottom line */
      if top = 0 then do;     /* top box line of plot          */
         top   = _n_;
         left  = index(line,'-');   /* left axis coordinate    */
         right = length;            /* right axis coordinate   */
         end;
      end;

   *------flag lines after the bottom line of the plot------;
   c = substr(line,left,1);
   if _n_ > top and afterbot = 0 and
      c = '-' and substr(line,right,1) = '-'
      then afterbot = 1;
   else if afterbot then afterbot = afterbot + 1;

   *------can we drop extra top of the plot lines?------;
   if top and _n_ > top and not afterbot then do;
      botn = botn + 1;
      if c = '+' then do;
         counttop = 0; botextra = 0; botn = 0;
         end;
      else do;
         if counttop then topn = topn + 1;
         if compress(line,'| ') = ' ' then do;
            if abs(_n_ - top - 1 - topextra) < &singular
               then topextra = topextra + 1;
            botextra = botextra + 1;
            end;
         end;
      end;

   *------output horizontal axis tick mark coordinates------;
   if afterbot = 1 then do;
      ty = .;
      do i = left to right;     /* output tick mark coords. */
         if substr(line,i,1) = '+' then do;
            tx = i;
            if      nmiss(htick1) then htick1 = tx;
            else if nmiss(htick2) then htick2 = tx;
            output &out;
            end;
         end;
      end;

   *------find last line in plot region------;
   if afterbot > 2 and not past then do;
      past = (index(line,"&listtitl") or index(line,title));
      if length and not past then do;
         nlines = _n_;

         *------find x-axis label line------;
         if compress(line) = '#' then do;
            xlabline = _n_;

            *------check for vertically printing ticks------;
            if not (afterbot = 3 or afterbot = 4) then do;
               put 'WARNING: Ticks may be printing vertically.  '
                   'The plot may be wrong.';
               if &intrtick then do;
                  call symput('intrls'  ,'1');
                  call symput('intrtick','0');
                  end;
               end;

            end;
         end;
      end;

   *------first, last nonblank column, label column------;
   if nlines = 0 and length then do;
      coln = max(coln,length); i = length + 1 - length(left(line));
      col1 = min(col1,i);
      if top then labcolum = min(labcolum,i);
      end;

   *------output vertical axis tick mark coordinates------;
   if top and substr(line,left,1) = '+' then do;
      tx = .; ty = _n_;
      if      nmiss(vtick1) then vtick1 = ty;
      else if nmiss(vtick2) then vtick2 = ty;
      output &out;
      end;

   *------on last observation, output results to macro variables------;
   if past or eof then do;

      *------check VTOH=------;
      vtoh = input(symget('vtoh'),?? 32.);
      if nmiss(vtoh) then vtoh = 2;
      else if n(htick1,htick2,vtick1,vtick2) = 4 then do;
         vtoh = vtick2 - vtick1;
         if vtoh > &singular then vtoh = (htick2 - htick1) / vtoh;
         end;
      * put vtoh= vtick2= vtick1= htick2= htick1=;
      call symput('vtoh',compress(put(vtoh,best15.)));

      *------size of plot parameters------;
      ls    = coln     - col1 + 1; left     = left     - col1 + 1;
      right = right    - col1 + 1; labcolum = labcolum - col1 + 1;
      hpos  = ls + 0.5 * (1.0 + abs(&ticklen)); vpos = nlines + 1;

      uhpos = input(symget('hpos'),?? 32.);
      if n(uhpos) then do;
         if uhpos < hpos then do;
            put "ERROR: HPOS=&hpos is too small.  "
                'The default ' 'minimum for ' 'this plot is ' hpos +(-1) '.';
            call symput('abort','1'); stop;
            end;
         else hpos = uhpos;
         end;

      uvpos = input(symget('vpos'),?? 32.);
      if n(uvpos) then do;
         if uvpos < vpos then do;
            put "ERROR: VPOS=&vpos is too small.  "
                'The default' 'minimum for ' 'this plot is ' vpos +(-1) '.';
            call symput('abort','1'); stop;
            end;
         else vpos = uvpos;
         end;

      hpos = ceil(hpos); vpos = ceil(vpos);

      *------scale plot to use MAKEFIT= proportion of area------;
      makefit1 = input(symget('makefit'),?? 32.);
      makefit  = makefit1;

      *------makefit value can be changed inside this loop------;
      do until(itfits);
         itfits = 1;

         *------determine hsize and vsize------;
         hsize = input(symget('hsize'),?? 32.);
         vsize = input(symget('vsize'),?? 32.);
         if n(vsize) and nmiss(hsize)
            then hsize = hpos * vsize / (vpos * vtoh);

         else do;
            if n(hsize) = 0 then hsize = &xmax;
            if n(vsize) = 0 then vsize = (vpos / hpos) * hsize * vtoh;
            end;

         *-------scale sizes so plot fits in specified area------;
         if n(makefit) then do;

            scale = max(hsize / (abs(makefit) * (&xmax)),
                        vsize / (abs(makefit) * (&ymax)));

            if scale > 1.0 then do;
               hsize = hsize / scale; vsize = vsize / scale;
               end;

            end;

         *------add extra positions to center plot, extend axes------;
         extendl = input(symget('extendl'),?? 32.);
         extendr = input(symget('extendr'),?? 32.);
         extendt = input(symget('extendt'),?? 32.);
         extendb = input(symget('extendb'),?? 32.);
         setlr   = (nmiss(extendl,extendr) = 2 and not &close);
         settb   = (nmiss(extendt,extendb) = 2 and not &close);

         *------centering, compute horizontal, vertical offsets------;
         if &center then do;
            hposoff = max(0.5 * ((&xmax) - hsize) / (hsize / hpos), 0);
            vposoff = max(0.5 * ((&ymax) - vsize) / (vsize / vpos), 0);
            hsize = &xmax; vsize = &ymax;
            end;

         else do; vposoff = 0; hposoff = 0; end;

         *------can we extend the left and right axes a little?------;
         if setlr then do;
            extendl = max(min(hposoff - 2.5, 2), 0); extendr = extendl;
            end;

         *------even the top and bottom extra positions------;
         if settb and topn > botn then do;
            i = topn - botn;
            if i <= topextra then extendt = -i; else extendb = i;
            i = -0.5 * sum(extendb,extendt);
            if i > 0 then do;
               extendb = sum(extendb,i); extendt = sum(extendt,i);
               end;
            end;

         *------set the extend variables if not set yet------;
         if nmiss(extendt) then extendt = 0;
         if nmiss(extendl) then extendl = 0;
         if nmiss(extendr) then extendr = 0;
         if nmiss(extendb) then extendb = 0;

         *------make the plot square------;
         if &squarplt and not &close then do;

            *------first try to adjust the left and right------;
            if setlr then do;
               setlr   = ((right - left + extendr + extendl) - (vtoh *
                         (bottom - top + extendt + extendb))) / 2;
               extendl = extendl - setlr;
               extendl = max(min(extendl,hposoff - 2.5),0);
               extendr = extendl;
               end;

            *------next try to adjust the top and bottom------;
            if settb then do;
               settb = ((right - left + extendr + extendl) -
                        (vtoh * (bottom - top + extendt + extendb))) /
                       (-2 * vtoh);
               extendt = extendt - settb; extendb = extendb - settb;
               if -extendt > topextra then extendt = -topextra;
               if -extendb > botextra then extendb = -botextra;
               end;

            *------check to see if we succeeded, should be zero------;
            setlr = (right - left + extendr + extendl) -
                     vtoh * (bottom - top + extendt + extendb);

            end;

         *------see if the plot fits, otherwise try again------;
         if n(makefit) and makefit < 0 and not &close and
            ((&squarplt and abs(setlr) > (&singular * hpos)) or
             (&center and (((extendl + extendr) > (2 * hposoff - 2.5))
             or ((extendt + extendb) > (2 * vposoff - 0.25))))) and
            makefit <= 0.75 * makefit1 then do;
            makefit = makefit * 0.99; itfits = 0;
            end;

         end;

      *------warn if we changed MAKEFIT= to make the plot square------;
      if n(makefit) and abs(makefit - makefit1) >
         &singular then put 'WARNING: MAKEFIT=' makefit 'was used '
         'to make ' 'the plot ' 'fit.';

      * put extendt= extendb= extendl= extendr= vtoh= hposoff= vposoff=;

      *------override default positions------;
      if n(uhpos)
         then hposoff = 0;
         else hpos    = ceil(hpos + 2.0 * hposoff);
      if n(uvpos)
         then vposoff = 0;
         else vpos    = ceil(vpos + 2.0 * vposoff);

      *------output plot size parameters------;
      call symput('col1',                  /* first column with data */
                  compress(put(col1,5.0)));
      call symput('actualls',              /* actual line size       */
                  compress(put(ls,5.0)));
      call symput('nlines',                /* number of lines        */
                  compress(put(nlines,5.0)));
      call symput('top',                   /* top boundary index     */
                  compress(put(top,5.0)));
      call symput('bottom',                /* bottom boundary index  */
                  compress(put(bottom,5.0)));
      call symput('left',                  /* left boundary index    */
                  compress(put(left,5.0)));
      call symput('right',                 /* right boundary index   */
                  compress(put(right,5.0)));
      call symput('labcolum',              /* label column           */
                  compress(put(labcolum,5.0)));
      call symput('xlabline',              /* hor. label row         */
                  compress(put(xlabline,5.0)));
      call symput('hposoff',               /* hor. offset to center  */
                  compress(put(hposoff,7.1)));
      call symput('vposoff',               /* ver. offset to center  */
                  compress(put(vposoff,7.1)));
      call symput('hpos',                  /* goptions hpos= value   */
                  compress(put(hpos,9.0)));
      call symput('vpos',                  /* goptions vpos= value   */
                  compress(put(vpos,9.0)));
      call symput('hsize',                 /* horizontal graph size  */
                  compress(put(hsize,9.2)));
      call symput('vsize',                 /* vertical graph size    */
                  compress(put(vsize,9.2)));
      call symput('extendl',               /* left extension         */
                  compress(put(extendl,9.2)));
      call symput('extendr',               /* right extension        */
                  compress(put(extendr,9.2)));
      call symput('extendt',               /* top extension          */
                  compress(put(extendt,9.2)));
      call symput('extendb',               /* bottom extension       */
                  compress(put(extendb,9.2)));

      if _error_ then call symput('abort','1');
      stop;

      end;

   if _error_ then call symput('abort','1');
   return;

   getline: set &tempdat5(keep=batch rename=(batch=line)) end=eof;
   return;

   run;

%if &syserr > 4 %then %let abort = 1;

%if &actualls <= 0 %then %do;
   %put ERROR: The plot is corrupt.;
   %let abort = 1;
   %end;

%if &abort %then %goto endit;

%if &dbprint %then %do;
   proc print data=&out;
      title3 "&out - intermediate results - line locations";
      run;
   title3;
   %end;

*------debugging output------;
%if &dbyes %then
   %plotdump(ntitles col1 actualls nlines top bottom left right
             labcolum xlabline vtoh hposoff vposoff hpos vpos
             hsize vsize abort extendl extendr extendt extendb);

*=======================post-process plot, step 2======================;

data &tempdat1(keep=function x y text angle position         /* text  */
               comment color size style n __xvar __yvar __otype)
     &tempdat2(keep=tx ty comment %if &radname %then &radii;
               color size style n __xvar __yvar);            /* lines */

   length text line label $ 200 function color style $ 8
          position symbol $ 1 comment $ 80;
   retain style ' ';

   function = 'LABEL'; color = ' '; size = .;

   *------read the plot------;
   if _n_ <= &nlines then do;

      comment = 'text';

      link getline;

      *------skip leading blank lines------;
      if _n_ = 1 then do while(line = ' ');
         link getline;
         end;

      *------skip note lines and subsequent blanks lines------;
      if _n_ > &xlabline then do;
         if line =: 'NOTE:' then line = ' ';
         if _n_ > &xlabline + 1 then do;
            do while((line =: 'NOTE:' or line = ' ') and not eof);
               link getline;
               end;
            end;
         end;

      line = substr(line, &col1, &actualls);

      *------is this an axis line?------;
      istop = (_n_ = &top); isbot = (_n_ = &bottom);

      *------set the y-axis label------;
      if &top <= _n_ <= &bottom and substr(line,&labcolum,1) = '#'
         then substr(line,&labcolum,1) = ' ';
      if _n_ = &nlines then do;
         y = ((2 * &nlines) - &top - &bottom) / 2;
         x = &labcolum; text = symget('labely');
         if &hposoff > 1 then x = x - 0.5;
         if length(text) > 1 then angle = 90;
         comment = 'text, y label'; color = &labelcol;
         output &tempdat1;
         end;

      angle = 0;

      *------remove stuff in the plot------;
      if &top <= _n_ <= &bottom then
         substr(line,&left,&right - &left + 1) = ' ';

      *------output ticks, titles, and so on------;
      y = &nlines - _n_;
      do iwhile = 1 to &ls while(line ne ' ');

         i = length(line) - length(left(line)) + 1;
         text = substr(line,i,1);

         *------isolate and process one label------;
         if text ne ' ' then do;

            position = '+'; x = i;

            *------special handling of title lines, xaxis label------;
            if _n_ < &top or _n_ >= &xlabline then do;
               text = left(line); i = &actualls + 1; line = ' ';
               x    = (&left + &right) / 2.0;

               *------special handling of title lines------;
               if _n_ < &top then do;
                  comment = 'text, title'; color = &titlecol;
                  end;

               *------footnotes------;
               else if _n_ > &xlabline then do;
                  comment = 'text, footnote'; color = &titlecol;
                  end;

               *------handle x-axis label------;
               else do;
                  text  = symget('labelx'); comment = 'text, x label';
                  color = &labelcol;
                  end;

               end;

            *------handle label outside the plot, not title------;
            else if i < &left or _n_ > &bottom then do;

               do j = i + 1 to &actualls until(done);
                  done = substr(line,j,1) = ' ';
                  end;

               text = substr(line,i,j - i + 1);
               substr(line,i,j - i + 1) = ' ';

               *------handle ticks------;
               position = '<'; x = i + length(text) - 1;
               if j < &left then comment = 'text, y tick';
               else if _n_ < &xlabline then comment = 'text, x tick';
               else comment = 'text, other';
               color = &tickcol;
               end;

            * put i= text= position=;

            *------output label------;
            if text ne ' ' then output &tempdat1;

            end;

         end;

      if iwhile >= &ls and symget('abort') ne '1' then do;
         put 'ERROR: The frame location algorithm is confused.';
         call symput('abort','1');
         stop;
         end;

      end;

   *------read the labels------;
   else do;

      *------bring in the extra observations------;
      eofextra = 0;
      p = '+'; tx = .; ty = .; vshift = .; hshift = .;
      text = ' '; n = 0; lines = 0;
      do while(not eofextra);
         set &extraobs(keep=__: %if &radname %then &radii;) end=eofextra;
         comment = 'extra,';
         if n(__xvar,__yvar) = 2 then link process;
         n = n + 1;
         end;

      *------determine point count------;
      set &tempdat3(firstobs=2 rename=(value=npoints));
      if nmiss(npoints) then do;
         npoints = 0;
         call symput('abort','1');
         end;

      sumh  = 0; sumv  = 0; sumx = 0; sumy = 0; sumx2 = 0; sumy2 = 0;
      sumxh = 0; sumyv = 0; nobs = 0;
      midh  = &actualls / 2; midv = &actualps / 2;
      midx  = (&hmax + &hmin) / 2; midy = (&vmax + &vmin) / 2;
      %if &hlogscal %then %do;
         midx = (log10(&hmax) + log10(&hmin)) / 2;
         %end;
      %if &vlogscal %then %do;
         midy = (log10(&vmax) + log10(&vmin)) / 2;
         %end;

      *------read the labels------;
      do pointn = 1 to npoints;

         link getlabel;

         ty = vpositio; tx = hpositio;
         comment = 'list,';
         if _error_ = 1 then put _all_;
         if      startpos = 'Center' then p = '+';
         else if startpos = 'Left'   then p = '<';
         else                             p = '>';
         text = symbol;
         i = n;
         set &preproc(keep=__: %if &radname %then &radii;) point=i;

         link process;

         end;

      *------least-squares slope and intercept------;
      b = .; a = .;
      if nobs > 0 then do;
         d = sumx - nobs * midx; b = nobs * sumx2 - d * d;
         if abs(b) < &singular then b = 0;
         else b = (nobs * sumxh - d * (sumh - nobs * midh)) / b;
         a = sumh / nobs - b * sumx / nobs;
         end;

      call symput('hslope',compress(put(b,best15.)));
      call symput('hinter',compress(put(a,best15.)));

      if nobs > 0 then do;
         d = (sumy - nobs * midy); b = (nobs * sumy2 - d * d);
         if abs(b) < &singular then b = 0;
         else b = (nobs * sumyv - d * (sumv - nobs * midv)) / b;
         a = sumv / nobs - b * sumy / nobs;
         end;

      call symput('vslope',compress(put(b,best15.)));
      call symput('vinter',compress(put(a,best15.)));

      if _error_ then call symput('abort','1');
      stop;

      end;

   if _error_ then call symput('abort','1');

   return;

   process: *------handle a point in the plot------;

   type = __stype;

   if n(tx) then do; tx = tx + &left + 1;             x = tx; end;
   if n(ty) then do; ty = ty + 1 + &nlines - &bottom; y = ty; end;
   colorset = 0;

   *------random colors------;
   %if &bright ne %then %do;
      array __rou[5] __dummy __roured __rougre __roublu __roupai
            (&rgbround);
      if type in (&britypes) then do;
         scalar = 2 * min(&bright,abs(255 - &bright));
         do until(red <= 255 and green <= 255 and blue <= 255);
            red   = &bright + (uniform(7) - 0.5) * scalar;
            green = &bright + (uniform(7) - 0.5) * scalar;
            blue  = &bright + (uniform(7) - 0.5) * scalar;
            norm  = (&bright) / mean(red,green,blue);
            green = green * norm; red = red * norm; blue = blue * norm;
            end;
         red   = round(red  ,__rou[2]); green = round(green,__rou[3]);
         blue  = round(blue ,__rou[4]); colorset = 1;
         color = 'CX' || put(red,hex2.) || put(green,hex2.) ||
                 put(blue,hex2.);
         end;

      %end;

   %else %if &red ne or &green ne or &blue ne or &paintvar ne
      %then %do;
      if type in (&rgbtypes) then do;
         color = __color;
         if color ne ' ' then colorset = 1;
         end;
      %end;

   *------store full label------;
   if lines then do;
      line = label;
      end;

   else do;
      line = compress('('||type||',') || ' ' ||
             compress(put(n,5.)||')');
      end;

   *------set colors, sizes, and so on for symbols------;
   if not colorset then color = __scolor;
   size  = __ssize; style = __sfont;
   if text = 'NONE' then text = ' ';
   %if not &symcon %then %str(if __symbol ne '00'x then text = __symbol;);
   if      type = 'contour' then function = 'BAR';
   else if type = 'square'  then function = 'SYMBOL';
   else                          function = 'LABEL';

   *------output list info to lines data set------;
   if type =: 'vector' or type =: 'circle' then do;
      comment = trim(comment) || ' ' || trim(type) || ', ' || line;
      output &tempdat2;
      end;

   *------output list info to text data set------;
   if (text ne ' ' and type ne 'dummy') or
      type = 'contour' or type = 'function' then do;
      position = '+';
      comment  = trim(scan(comment,1,' ')) || ' text, ' ||
                 trim(type) || ', ' || line;
      output &tempdat1;
      end;

   *------intermediate results for slope, intercept------;
   if n(tx,ty,__xvar,__yvar) = 4 then do;
      sumh  = sumh + tx;      sumv  = sumv + ty;
      sumx  = sumx + __xvar;  sumy  = sumy + __yvar;
      d     = __xvar - midx;  sumx2 = sumx2 + d * d;
      sumxh = sumxh + (tx - midh) * d;
      d     = __yvar - midy;  sumy2 = sumy2 + d * d;
      sumyv = sumyv + (ty - midv) * d; nobs = nobs + 1;
      * put sumh= tx= sumv= ty= sumx= __xvar= sumy= __yvar=
            midx= sumx2= sumxh= midy= sumy2= sumyv= nobs=;
      end;

   *------handle the label fragments------;
   if lines then do;

      if not colorset then color = __lcolor;
      size  = __lsize; style = __lfont; function = 'LABEL';

      position = p;
      comment  = trim(scan(comment,1,' ')) || ' text, label, ' ||
                 trim(type) || ', ' || line;
      shift    = floor(&singular + (lines - 1) / 2);

      do i = 1 to lines;
         if lines > 1 then link getlabel;
         text = label;
         if text ne ' ' and type ne 'dummy' then do;
            x = tx + hshift;
            y = 1 + ty + vshift + shift - i;
            output &tempdat1;
            end;
         end;
      end;

   return;

   getline: set &tempdat5(keep=batch rename=(batch=line)) end=eof;
   return;

   getlabel: set &tempdat4(keep=lines symbol n label vshift hshift
             %if &v7 %then vposition hposition startposition
                 rename=(startposition=startpos vposition=vpositio
                 hposition=hpositio);
             %else vpositio hpositio startpos;);
   return;

   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

%if &dbprint %then  %do;
   proc print data=&tempdat1;
      title3 "&tempdat1 - temporary data set - text";
      run;
   proc print data=&tempdat2;
      title3 "&tempdat2 - temporary data set - lines";
      run;
   %if &curve %then %do;
      proc print data=&regdat;
         title3 "&regdat - raw regression data set for curve fitting";
         run;
      %end;
   title3;
   %end;

*------delete intermediate data sets------;
%if &delete %then %do;
   proc datasets nolist; delete &extraobs; run;
   %end;

*------tick mark based slope and intercept------;
%if &intrtick or not &intrls %then %do;
   data _null_;
      merge &out(where=(index(substr(comment,1,12),'tick'))
                 keep=ty tx comment)
            &tempdat1(where=(index(comment,'tick')) keep=text comment);

      if n(ty) then ty = &nlines - ty;
      data = input(text,?? &tickfor);

      if (&hlogscal and n(tx)) or (&vlogscal and n(ty)) then do;
         if data > 0 then data = log10(data);
         else data = .;
         end;

      oldty = lag(ty); oldtx = lag(tx); olddata = lag(data);

      if n(data,olddata) = 2 then do;
         den = data - olddata;
         if abs(den) > &singular then do;
            if n(oldty,ty) = 2 then do;
               b = (ty - oldty) / den; a = ty - b * data;
               call symput('vslope',compress(put(b,best15.)));
               call symput('vinter',compress(put(a,best15.)));
               end;
            if n(oldtx,tx) = 2 then do;
               b = (tx - oldtx) / den; a = tx - b * data - &col1 + 1;
               call symput('hslope',compress(put(b,best15.)));
               call symput('hinter',compress(put(a,best15.)));
               end;
            end;
         end;

      if _error_ then call symput('abort','1');
      run;

   %if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;
   %end;

*------switch top and bottom to annotate coordinates------;
%let top    = %eval(&nlines - &top);
%let bottom = %eval(&nlines - &bottom);

*------make goptions statement------;
%let gopts2 = &gopts2 hpos=&hpos vpos=&vpos;
%let gopts2 = &gopts2 hsize=&hsize&unit vsize=&vsize&unit;
%if &method = print
   %then %let gopts2 = &gopts2 &gopprint;
   %else %let gopts2 = &gopts2 &gopplot;
%let gopts  = &gopts2 &gopts;
%if &device ne %then %let gopts = &gopts device=&device;
%let comlen = %length(&gopts);
%if &comlen < 100 %then %let comlen = 100;
%if &comlen > 200 %then %let comlen = 200;

*------debugging output------;
%if &dbyes %then
   %plotdump(abort top bottom gopts comlen hslope hinter vslope vinter);

*=================create annotate data set with lines=================;

data &out(keep=function x y comment color size style n);

   file log ps=&logps;
   length comment $ &comlen function color $ 8;
   retain curven -1 tickaxes "&tickaxes"
          vtoh &vtoh vinter &vinter hinter &hinter hslope &hslope
          vslope &vslope hmax &hmax vmax &vmax vmin &vmin hmin &hmin
          singular &singular left &left right &right top &top
          bottom &bottom;
   array lsizes[5] _temporary_ (&lsizes);

   if _n_ = 1 then do;

      *------check slope, intercept------;
      if n(hinter,vinter,hslope,vslope) ne 4 then do;
         put 'ERROR: Plot algorithm failed.  ' 'Coordinates could '
             'not be ' 'computed.  ' 'Vertically printing '
             'ticks is a ' 'possible cause.  ' 'Another '
             'explanation ' 'is no locate list ' 'was available.  '
             'Look ' 'at the ' 'printer plot ' 'to check.';
         call symput('abort','1');
         stop;
         end;

      *------how far should plot be extended?------;
      extendl = input(symget('extendl'), ?? 32.);
      extendr = input(symget('extendr'), ?? 32.);
      extendt = input(symget('extendt'), ?? 32.);
      extendb = input(symget('extendb'), ?? 32.);

      *------rectangle sizes for contour plots------;
      if &contour then do;
         nobs = &vnobs;
         if nobs <= 0 and &ncontour > singular
            then nobs = sqrt(&ncontour);
         nobs = nobs - 1;
         if nobs > singular
            then vcontour = abs(vslope) * (vmax - vmin) / nobs;
         else vcontour = .;
         call symput('vcontour',compress(put(vcontour,best15.)));
         nobs = &hnobs;
         if nobs <= 0 and &ncontour > singular
            then nobs = sqrt(&ncontour);
         nobs = nobs - 1;
         if nobs > singular
            then hcontour = abs(hslope) * (hmax - hmin) / nobs;
         else hcontour = .;
         call symput('hcontour',compress(put(hcontour,best15.)));

         *------optionally close up all white space------;
         if &close then do;
            if n(hcontour) then do;
               extendl = extendl + left -
                         (hinter + hslope * hmin - 0.5 * hcontour);
               extendr = extendr + (hinter + hslope * hmax +
                                    0.5 * hcontour) - right;
               end;
            if n(vcontour) then do;
               extendt = extendt + (vinter + vslope * vmax +
                                    0.5 * vcontour) - top;
               extendb = extendb + bottom -
                         (vinter + vslope * vmin - 0.5 * vcontour);
               end;
            end;
         end;

      *------optionally close up white space with square contours------;
      if &square and &close and not &contour then do;
         contour = &sizsquar * 0.5;
         extendl = extendl + left - (hinter + hslope * hmin) +
                   vtoh * contour;
         extendr = extendr + (hinter + hslope * hmax) - right +
                   vtoh * contour;
         extendt = extendt + (vinter + vslope * vmax) - top +
                   contour;
         extendb = extendb + bottom - (vinter + vslope * vmin) +
                   contour;
         end;

      if &close then do;
         call symput('extendl',compress(put(extendl,best15.)));
         call symput('extendr',compress(put(extendr,best15.)));
         call symput('extendt',compress(put(extendt,best15.)));
         call symput('extendb',compress(put(extendb,best15.)));
         end;

      *------final locations of the axes------;
      left = left - extendl; right  = right  + extendr;
      top  = top  + extendt; bottom = bottom - extendb;

      *------frame background color------;
      %if %nrbquote(&cframe) ne %then %do;
         color    = &cframe; size = 1; style = 'msolid';
         function = 'POLY'; y = top;
         comment  = 'frame, background, left top start'; x = left;   output;
         function = 'POLYCONT';
         comment  = 'frame, background, left bottom';    y = bottom; output;
         comment  = 'frame, background, right bottom';   x = right;  output;
         comment  = 'frame, background, right top';      y = top;    output;
         comment  = 'frame, background, left top';       x = left;   output;
         style = ' ';
         %end;

      *------draw frame------;
      color    = &framecol; size = lsizes[1];
      function = 'MOVE'; y = top;
      comment  = 'frame, box, left top start'; x = left;   output;
      function = 'DRAW';
      comment  = 'frame, box, left bottom';    y = bottom; output;
      comment  = 'frame, box, right bottom';   x = right;  output;
      if &frame then do;
         comment = 'frame, box, right top';    y = top;    output;
         comment = 'frame, box, left top';     x = left;   output;
         end;

      *------diagonal reference line------;
      if &diag then do;
         function = 'MOVE';                       x = left;
         comment  = 'frame, diag, left bottom';   y = bottom; output;
         comment  = 'frame, diag, right top';     x = right;
         function = 'DRAW';                       y = top;    output;
         end;

      *------vertical axis reference lines------;
      %if %nrbquote(&vref) ne %then %do;
         do __yvar = &vref;
            y = vinter + vslope * __yvar; temp = __yvar;
            if abs(temp) < singular * (vmax - vmin) then temp = 0;
            comment = 'frame, vref, ' || compress(put(temp,best12.));
            if bottom + 0.125 < y < top - 0.125 then do;
               x = left;  function = 'MOVE'; output;
               x = right; function = 'DRAW'; output;
               end;
            end;
         %end;

      *------horizontal axis reference lines------;
      %if %nrbquote(&href) ne %then %do;
         do __xvar = &href;
            x = hinter + hslope * __xvar; temp = __xvar;
            if abs(temp) < singular * (hmax - hmin) then temp = 0;
            comment = 'frame, href, ' || compress(put(temp,best12.));
            if left + 0.25 < x < right - 0.25 then do;
               y = top;    function = 'MOVE'; output;
               y = bottom; function = 'DRAW'; output;
               end;
            end;
         %end;

      *------vertical axis minor tick marks------;
      %if %nrbquote(&vminor) ne %then %do;
         halftick = 0.25 * abs(&ticklen); size = lsizes[2];
         do __yvar = &vminor;
            y = vinter + vslope * __yvar;
            if bottom + 0.125 < y < top - 0.125 then do;
               if index(tickaxes,'l') then do;
                  comment = 'frame, tick, minor, left';
                  x = left;            function = 'MOVE'; output;
                  x = left - halftick; function = 'DRAW'; output;
                  end;
               if index(tickaxes,'r') then do;
                  comment = 'frame, tick, minor, right';
                  x = right;            function = 'MOVE'; output;
                  x = right + halftick; function = 'DRAW'; output;
                  end;
               end;
            end;
         %end;

      *------horizontal axis minor tick marks------;
      %if %nrbquote(&hminor) ne %then %do;
         halftick = 0.25 * abs(&ticklen) / vtoh; size = lsizes[2];
         do __xvar = &hminor;
            x = hinter + hslope * __xvar;
            if left + 0.25 < x < right - 0.25 then do;
               if index(tickaxes,'b') then do;
                  comment = 'frame, tick, minor, bottom';
                  y = bottom;            function = 'MOVE'; output;
                  y = bottom - halftick; function = 'DRAW'; output;
                  end;
               if index(tickaxes,'t') then do;
                  comment = 'frame, tick, minor, top';
                  y = top;            function = 'MOVE'; output;
                  y = top + halftick; function = 'DRAW'; output;
                  end;
               end;
            end;
         %end;

      end;

   set &out &tempdat2(in=td2)
       %if &curve %then &regdat(obs=&cursegs in=inregdat);;

   *------uncategorize symbol locations------;
   i = (&intrtick or &intrls) and td2 and not (comment =: 'frame');
   if i or (nmiss(tx) and n(__xvar)) then tx = hinter + hslope * __xvar;
   if i or (nmiss(ty) and n(__yvar)) then ty = vinter + vslope * __yvar;

   *------figure out curve coordinates------;
   %if &curve %then %do;

      if inregdat then do;
         retain justdraw oldx . oldy patho 0;

         comment = 'curve'; color = &curvecol; curven = curven + 1;
         size    = lsizes[5]; text = ' '; n = curven;
         x2 = hinter + hslope * &hplotvar;
         y2 = vinter + vslope * &appvar1;
         %if &hlogscal %then %do;
            x2 = hinter + hslope * log10(&hplotvar);
            %end;
         %if &vlogscal %then %do;
            y2 = vinter + vslope * log10(&appvar1);
            %end;

         if nmiss(oldx) then do; x1 = x2;   y1 = y2;   oldx = 0; end;
         else                do; x1 = oldx; y1 = oldy; end;
         oldx = x2; oldy = y2;
         if abs(y2) > 1e5 and patho < 10 then do;
            patho = patho + 1;
            put 'WARNING: Extreme pathology suspected.';
            end;
         link clipline;
         end;

      else
      %end;

   if comment = 'frame, tick' then do;

      color = &framecol; size = lsizes[2];
      halftick = 0.5 * abs(&ticklen);
      ticklen  = (1 + (&ticklen > 0)) * halftick;

      *------adjust for blank cols------;
      if n(tx) then tx = tx - &col1 + 1;

      *------draw y-axis tick marks------;
      if nmiss(tx) then do;

         y = &nlines - ty;

         if bottom <= y <= top then do;

            if index(tickaxes,'L') then do;
               comment = 'frame, tick, left';
               x = left - halftick; function = 'MOVE'; output;
               x = x    + ticklen;  function = 'DRAW'; output;
               end;

            if index(tickaxes,'R') then do;
               comment = 'frame, tick, right';
               x = right + halftick; function = 'MOVE'; output;
               x = x     - ticklen;  function = 'DRAW'; output;
               end;

            end;

         end;

      *------draw x-axis tick marks------;
      else if nmiss(ty) then do;
         halftick = halftick / vtoh; ticklen = ticklen / vtoh; x = tx;

         if left <= x <= right then do;

            if index(tickaxes,'T') then do;
               comment = 'frame, tick, top';
               y = top + halftick; function = 'MOVE'; output;
               y = y   - ticklen;  function = 'DRAW'; output;
               end;

            if index(tickaxes,'B') then do;
               comment = 'frame, tick, bottom';
               y = bottom - halftick; function = 'MOVE'; output;
               y = y      + ticklen;  function = 'DRAW'; output;
               end;

            end;

         end;

      end;

   *------draw vectors------;
   else if index(substr(comment,1,13),'vector') then do;
      size = lsizes[3];

      x = hinter; y = vinter; function = 'MOVE'; output;
      x = tx;     y =  ty;    function = 'DRAW'; output;

      %if &vecheadw ne %then %do;
         r = index(comment,'vector');
         comment = substr(comment,1,r + 7) || 'head,' ||
                   substr(comment,r + 7);

         *------compute slope of vector------;
         vecslope = tx - hinter;
         if abs(vecslope) > singular
            then vecslope = (ty - vinter) / (vecslope / vtoh);
            else vecslope = .;

         *------find point on vector vecheadr distance from end------;
         r = -&vecheadr; fromx = tx; fromy = ty; link linept;
         fromx = tox; fromy = toy;

         *------slope of line perpendicular to vector------;
         if nmiss(vecslope) then vecslope = 0;
         else if abs(vecslope) > singular
            then vecslope = -1.0 / vecslope;
            else vecslope = .;

         *------draw vector head------;
         r = -&vecheadw; link linept;
         x = tox; y = toy; function = 'DRAW'; output;
         r = &vecheadw; link linept;
         x = tx;  y = ty;  function = 'MOVE'; output;
         x = tox; y = toy; function = 'DRAW'; output;
         %end;

      end;

   *------draw circles------;
   else if index(substr(comment,1,13),'circle') then do;
      size    = lsizes[4]; comsplit = 15 + (comment =: 'extra');
      holdcom = comment; pi = 3.1415926536;
      do rad = &radii;
         radius  = max(rad, 0) * hslope;
         comment = substr(holdcom,1,comsplit - 1) ||
                   compress(put(rad,best5.) || ',') || ' ' ||
                   compress('(' || put(__xvar,best5.) || ',' ||
                            put(__yvar,best5.) || '),') || ' ' ||
                   substr(holdcom,comsplit);
         i = (&hsize) / (&hpos);
         if "&unit" = 'cm' then i = i / 2.54;
         inc = 2.0 * pi / ceil(20 + (2.0 * pi * radius * i /
                                &cirsegs) ** 0.8);
         justdraw = 0; /* 1 means DRAW without MOVE */
         seg0     = (inc * singular) ** 2;

         do i = -pi + inc to pi by inc;
            x1 = tx + cos(i - inc) * radius;
            y1 = ty + sin(i - inc) * radius / vtoh;
            x2 = tx + cos(i)       * radius;
            y2 = ty + sin(i)       * radius / vtoh;
            link clipline;
            end;
         end;

      end;

   if _error_ then call symput('abort','1');
   return;

   %if &vecheadw ne %then %do;
      linept: *------find a point (tox,toy) r distance from     ------;
              *------(fromx,fromy) on a line with slope vecslope------;
              *------negative r means toward origin             ------;

      r = r * (&hpos) / (&hsize);
      if "&unit" = 'cm' then r = r * 2.54;
      vecdir = -sign(r);

      if n(vecslope) then do;
         vecm2 = sqrt(r * r / (1.0 + vecslope * vecslope));
         if fromx > hinter then vecm2 = -vecm2;
         tox = vecdir * vecm2 + fromx;
         toy = vecdir * vecslope * vecm2 / vtoh + fromy;
         end;

      else do;
         tox = fromx;
         if fromy > vinter then vecdir = -vecdir;
         toy = vecdir * abs(r) / vtoh + fromy;
         end;

      return;
      %end;

   clipline: *------clip a line that crosses an edge------;

   *------find where line crosses edges------;
   b = (x2 - x1); bi = (y2 - y1);
   xtop = .; xbottom = .; yleft = .; yright = .;

   if abs(b) > singular then do;
      b = (y2 - y1) / b; a = y1 - b * x1;
      yleft = b * left + a; yright = b * right + a;
      end;

   if abs(bi) > singular then do;
      bi = (x2 - x1) / bi; xtop = (top - y1) * bi + x1;
      xbottom = (bottom - y1) * bi + x1;
      end;

   *------adjust coordinates when line crosses edges------;
   if x1 < left   then do; x1 = left;   y1 = yleft;   justdraw = 0; end;
   if x1 > right  then do; x1 = right;  y1 = yright;  justdraw = 0; end;
   if y1 < bottom then do; y1 = bottom; x1 = xbottom; justdraw = 0; end;
   if y1 > top    then do; y1 = top;    x1 = xtop;    justdraw = 0; end;
   if x2 < left   then do; x2 = left;   y2 = yleft;                 end;
   if x2 > right  then do; x2 = right;  y2 = yright;                end;
   if y2 < bottom then do; y2 = bottom; x2 = xbottom;               end;
   if y2 > top    then do; y2 = top;    x2 = xtop;                  end;

   *------see if segment should be output------;
   if n(x1,x2,y1,y2) = 4 and
      ((x2 - x1) ** 2 + (y2 - y1) ** 2) > seg0 then do;

      *------draw line------;
      if not justdraw then do;
         x = x1; y = y1; function = 'MOVE'; output;
         %if &curve %then %do;
            if inregdat then do; curven = curven + 1; n = curven; end;
            %end;
         end;

      x = x2; y = y2; function = 'DRAW'; output;
      justdraw = 1;

      end;

   else justdraw = 0;
   return;

   run;
%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

%if &dbprint %then %do;
   proc print data=&out;
      title3 "&out - intermediate results - lines";
      run;
   title3;
   %end;

*------debugging output------;
%if &dbyes %then
   %plotdump(vcontour hcontour extendl extendr extendt extendb);

*------delete intermediate data sets------;
%if &delete %then %do;
   proc datasets nolist; delete &tempdat2 &regdat; run;
   %end;

*======================combine annotate data sets======================;

data &out;

   keep color style function comment x y size text
        position angle n obstype;

   length color style function $ 8 comment $ &comlen
          x y size 8 text $ &actualls position $ 1
          angle n 8 obstype $ 12 excolors $ 64;

   retain left right top bottom contok conoldx conoldy excolors;
   array lsizes[5] _temporary_ (&lsizes);

   if _n_ = 1 then do;
      left = &left - &extendl; right  = &right  + &extendr;
      top  = &top  + &extendt; bottom = &bottom - &extendb;
      contok = (n(&vcontour,&hcontour) = 2);
      excolors = lowcase(symget('excolors'));

      *------store goptions with annotate data set------;
      if not &functype then do;
         comment = symget('gopts'); n = 0; function = 'COMMENT';
         nobs + 1; output;
         end;

      *------draw border------;
      if &border then do;
         color    = &framecol; size = lsizes[1]; position = '+';
         function = 'MOVE'; y = &vpos - 0.001;
         if &closebord and &extendt < 0 then y = y + &extendt;
         comment  = 'border, box, left top start'; x = 0;
         if &closebord and &extendl < 0 then x = x - &extendl;
         output;
         function = 'DRAW';
         comment  = 'border, box, left bottom';    y = 0;
         if &closebord and &extendb < 0 then y = y - &extendb;
         output;
         comment  = 'border, box, right bottom';   x = &hpos - 0.001;
         if &closebord and &extendr < 0 then x = x + &extendr;
         output;
         comment  = 'border, box, right top';      y = &vpos - 0.001;
         if &closebord and &extendt < 0 then y = y + &extendt;
         output;
         comment  = 'border, box, left top';       x = 0;
         if &closebord and &extendl < 0 then x = x - &extendl;
         output;
         end;
      end;

   set %if &linetext %then &out &tempdat1(in=td1);
       %else &tempdat1(in=td1) &out;;

   if comment = 'text, y label' then x = x - &extendl;
   if comment = 'text, title'   then y = y + &extendt;
   if comment = 'text, x label' then y = y - &extendb;
   if comment = 'text, y tick'  then x = x - &extendl;
   if comment = 'text, x tick'  then y = y - &extendb;

   obstype = __otype; len = length(text);

   *------discard clipped ticks if necessary------;
   if comment = 'text, y tick' then do;
      if not (bottom <= y <= top) then return;
      end;

   else if comment = 'text, x tick' then do;
      d = right + floor(&singular + len / 2);
      if not (left <= x <= d) then return;
      end;

   *------uncategorize symbol locations------;
   i = (&intrtick or &intrls) and td1 and
       (comment =: 'list' or comment =: 'extra') and
       not (index(substr(comment,1,18),', text, label'));
   if i or (nmiss(x) and n(__xvar)) then x = &hinter + &hslope * __xvar;
   if i or (nmiss(y) and n(__yvar)) then y = &vinter + &vslope * __yvar;

   *------all angles are zero, except the y-axis label------;
   if nmiss(angle) then angle = 0;

   *------set position if not yet set------;
   if position = ' ' then position = '+';

   *------flag contour observations------;
   contour = (function = 'BAR' and style = 'solid');

   *------clip if necessary------;
   %if &clip %then %do;

      if comment =: 'extra, text' or comment =: 'list, text' and
         text ne ' ' and not contour then do;
         if y >= top or y <= bottom then return;
         if      position = '<' then d = x - len + 1 - left;
         else if position = '>' then d = x - left;
         else                        d = x - len / 2 - left;
         d = floor(d);
         if d <= 0 then do;
            if (len + d) > 0 then text = substr(text, 1, len + d);
                             else text = ' ';
            len = length(text);
            end;
         if      position = '<' then d = right - x;
         else if position = '>' then d = right - (x + len - 1);
         else                        d = right - (x + len / 2);
         d = floor(d);
         if d <= 0 then do;
            if (len + d) > 0 then text = substr(text, 1, len + d);
                             else text = ' ';
            len = length(text);
            end;
         end;
      %end;

   *------adjust justifications------;
   if      position = '<' then x = x + 0.5;
   else if position = '>' then x = x - 0.5;

   *------set line, text sizes------;
   if nmiss(size) then do;
      if function  = 'LABEL' then size = &tsize;
      else size = lsizes[1];
      end;

   *------set font------;
   if style = ' ' and not (comment =: 'list, text' or
      comment =: 'extra, text') then style = &font;

   *------if color is still not set, set it------;
   if color = ' ' then color = &color;

   *------offset (jitter) coincident symbols but not ptr symbols------;
   %if &offset ne and not &functype and
       %upcase(&symvar) ne %nrbquote('00'X) %then %do;
      length lastpos $ 1;
      retain oldx oldy 0 offsetn 1 lastpos '+';
      if comment =: 'list, text, symbol' or
         comment =: 'extra, text, symbol' then do;
         if abs(oldx - x) < &singular and
            abs(oldy - y) < &singular then do;

            *------usually go center, right, left, but skip right------;
            *------on second symbol if last label was right      ------;
            offsetn + 1 + (offsetn = 1 and lastpos = '>');
            x = x + (mod(offsetn,3) - 1) * (&offset);
            d = mod(floor((offsetn - 1) / 3) + 1, 3) - 1;
            y = y + d * (&offset) / (&vtoh);
            end;
         else do;
            offsetn = 1;
            oldx = x;
            oldy = y;
            end;
         end;
      lastpos = position;
      %end;

   *------override colors for monochrome device------;
   %if &monochro ne %then %str(color = &monochro;);

   *------exclude observations in excluded color list------;
   if index(excolors, trim(lowcase(color))) = 0;

   *------add offsets to center plot                ------;
   *------adjust y so bottom line has coordinate 0.5------;
   x = x + &hposoff; y = y + &vposoff + 0.5;

   *------set ID variable for sort for functions------;
   %if &functype %then %do;
      if index(comment, 'function') then n = .;
      else n = _n_;
      %end;

   *------create rectangles for contour plots------;
   if contour and contok then do;
      text = ' '; epsh = size * &hcontour; epsv = size * &vcontour;
      x1 = x - epsh / 2; x2 = x1 + epsh; epsh = epsh * 0.1;
      y1 = y - epsv / 2; y2 = y1 + epsv; epsv = epsv * 0.1;
      if      abs(x1 - conoldx) < epsh and abs(y1 - conoldy) < epsv
         then do; x = x2; y = y2; end;
      else if abs(x2 - conoldx) < epsh and abs(y1 - conoldy) < epsv
         then do; x = x1; y = y2; end;
      else if abs(x2 - conoldx) < epsh and abs(y2 - conoldy) < epsv
         then do; x = x1; y = y1; end;
      else if abs(x1 - conoldx) < epsh and abs(y2 - conoldy) < epsv
         then do; x = x2; y = y1; end;
      else do;
         function = 'MOVE'; nobs + 1;
         if (abs(&hcondir) < epsh and &vcondir > 0) or
            (abs(&vcondir) < epsv and &hcondir > 0) then do;
            x = x1; y = y1; output; x = x2; y = y2;
            end;
         else do; x = x2; y = y2; output; x = x1; y = y1; end;
         function = 'BAR';
         end;
      end;

   conoldx = x; conoldy = y;

   *------final adjustments------;
   %if not &functype %then %do; &adjust3; &adjust4; %end;

   nobs + 1; output;
   call symput('nobs',compress(put(nobs,5.)));
   if _error_ then call symput('abort','1');
   return;
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

%if &dbprint %then %do;
   proc print data=&out;
      title3 "&out - final annotate data set";
      run;
   title3;
   %end;

*------delete intermediate data sets------;
%if &delete %then %do;
   proc datasets nolist;
      delete &tempdat1 &tempdat3 &tempdat4 &tempdat5 &tempdat6;
      run;
   %end;

*------post-processing for functions------;
%if &functype %then %do;

   proc sort data=&out;
      by n obstype x;
      run;

   %if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

   data &out;

      keep color style function comment x y size text
           position angle n obstype;

      length color style function $ 8 comment $ &comlen
             x y size 8 text $ &actualls position $ 1
             angle n 8 obstype $ 12;

      array lsizes[5] _temporary_ (&lsizes);

      if _n_ = 1 then do;
         comment = symget('gopts'); n = 0; function = 'COMMENT';
         output;
         end;

      set &out;
      by n obstype;

      if index(comment, 'function,') then do;
         if first.obstype then function = 'MOVE';
         else function = 'DRAW';
         n = input(scan(comment,3,' (),'),?? 32.);
         size = lsizes[5];
         &adjust5;
         end;

      &adjust3; &adjust4;

      output;
      if _error_ then call symput('abort','1');
      run;

   %if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

   %if &dbprint %then %do;
      proc print data=&out;
         title3 "&out - annotate data set with functions";
         run;
      title3;
      %end;

   %end;

*------post-processing for expand------;
%if &expand %then %do;
   proc means noprint data=anno(where=(not (comment =: 'border')));
      var x y;
      output out=&tempdat1(drop=_type_ _freq_)
             min=minx miny max=maxx maxy;
      run;

   data anno;
      retain adjust extra left bottom;
      drop maxx maxy left bottom minx miny extra adjust;

      if _n_ = 1 then do;
         set &tempdat1;
         extra  = max(min(minx + &hpos - maxx - 4,
                         (miny + &vpos - maxy) * &vtoh) - 2, 0);
         adjust = &hpos / (&hpos - extra);
         left   = (&hpos - (maxx - minx) * adjust) / 2 + 1;
         bottom = (&vpos - (maxy - miny) * adjust) / 2;
         extra  = (extra > 1);
         end;

      set anno;

      if not (comment =: 'border') and extra then do;
         if function = 'BAR' then size = size * adjust;
         x = (x - minx) * adjust + left;
         y = (y - miny) * adjust + bottom;
         end;
      run;

   %if &delete %then %do;
      proc datasets nolist;
         delete &tempdat1;
         run;
      %end;

   %end;

*===========set up for and produce the graphical scatter plot==========;

goptions &gopts;
%if %index(%upcase(%nrbquote(&gopts)), GSASFILE)
   %then filename gsasfile "&post";;

*------print final plot request------;
data _null_;
   file log ps=&logps;
   length string $ 500;

   if &code then do;
      put / 'The following code will create the ' @@;
      if not &looklist then put '(empty) ' @@;
      put 'printer plot ' 'on which ' 'the '
          'graphical ' 'plot is ' 'based:' /;
      string  = "options nonumber ls=&ls ps=&actualps;";
      indent1 = 0; indent2 = 8; link wrap;
      string  = symget('procopts'); indent2 = 10; link wrap;
      string  = 'plot ' || trim(symget('plotreq')) || ' /';
      indent1 = 3; indent2 = 8; link wrap;
      string  = trim(symget('plotopts')) || ' list=1';
      indent1 = 8; link wrap;
      string  = trim(symget('pl')) || ';'; link wrap;
      string  = trim(symget('label')) || ';';
      indent1 = 3; indent2 = 9; link wrap;
      string  = trim(symget('adjust2')) || ';'; link wrap;
      string  = 'label ' || symget('vplotvar') || " = '#' " ||
                            symget('hplotvar') || " = '#';";
      link wrap;
      put +3 'run; quit;' /;
      put 'The plot was created with the following goptions:' /;
      string  = 'goptions ' || symget('gopts') || ';';
      indent1 = 0; link wrap;
      put / "The OUT=&out Annotate data set has &nobs observations.";
      end;

   if symget('method') = 'print'
      then put "The graphics stream file name is &post..";

   if &dbtime and upcase(getoption('STIMER')) ne 'NOSTIMER' then do;
      time = time();
      cum  = put(time - &start,6.1);
      put 'The PLOTIT macro used ' cum "seconds to create OUT=&out..";
      end;

   if _error_ then call symput('abort','1');
   return;

   wrap: *------print a line, wrapped and indented------;

   nlit = index(upcase(string), "'N");
   if nlit = 0 then nlit = index(upcase(string), '"N');
   string = left(string); len = length(string);
   if len < 2 or string = ' ' or string = ';' then return;
   i = index(string,' '||' ');
   do while(i < (len - 1) and not nlit);
      string = substr(string,1,i - 1) || substr(string,i + 1);
      len    = length(string); i = index(string,' '||' ');
      end;
   i = index(string,' ;');
   if i then string = substr(string,1,i - 1) || substr(string,i + 1);

   len = 1; end = 0; indent = indent1 + 1;

   do while(len > end);
      string = left(substr(string,end + 1)); len = length(string);
      nlit = index(upcase(string), "'N");
      if nlit = 0 then nlit = index(upcase(string), '"N');
      maxlen = &restorls - indent;
      if len > maxlen and not nlit then do;
         end = maxlen;
         do while(end > 0 and not (substr(string,end,1) in (' ' ',')));
            end = end - 1;
            end;
         end;
      else if len > maxlen then do;
         end = maxlen;
         if nlit then end = nlit + 2;
         end;
      else end = len;
      str = substr(string,1,end);
      put @indent str;
      indent = indent2 + 1;
      end;
   return;
   run;

%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

*------produce the plot------;
%if &method = gplot or &method = print %then %do;

   %if &method = print %then %str(options notes;);

   proc ganno annotate=&out
      %if &gout ne %then %do;
         gout=&gout
         %if &gname ne %then name="&gname";
         %if &gdesc ne %then description="&gdesc";
         %end;;
      run;

   %if &syserr > 4 %then %let abort = 1;

   %end;

options &restorla;

%endit: *------restore options, quit------;
options notes;
%if &dbmprint %then %str(options nomprint;);

%if &abort %then %put ERROR: The PLOTIT macro ended abnormally.;

%mend plotit;

*------debugging routine to dump out macro variables------;
%macro plotdump(stuff);

%let i = 1;
%let word = %scan(&stuff,1);
%do %while(&word ne);
   %put &word=&&&word...;
   %let i = %eval(&i + 1);
   %let word = %scan(&stuff,&i);
   %end;

%mend plotdump;
