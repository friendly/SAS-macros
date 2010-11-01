/*--------------------------------------------------------------*
  *    Name: map2gen.sas                                         *
  *   Title: Convert a SAS/Graph map to ArcView Generate format  *
        Doc: http://www.datavis.ca/sasmac/map2gen.html     
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 15 Nov 2005 08:57:48                                *
  * Revised: 12 Jan 2006 15:34:08                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The MAP2GEN macro reads a SAS/Graph map data set and writes the
 polygons in ArcView Generate format. This is a commonly used input
 format for various cartographic programs, but the details often
 matter.

==Method:

 Generate map files can be in either 'line' format (a simple list of
 line segments, usually forming polygons for geographic regions),
 or 'polygon' format (similar, except the ID line for each region
 also contains the (x,y) coordinates of the polygon center and the
 region label.

 Line format files can be written as shown below, from any SAS/Graph
 map.  Some programs that read such files require that the polygons
 be closed, so that the last X,Y pair is the same as the first X,Y
 pair; use CLOSE=1 if so.
 
  id1
  x-coord,y-coord
  x-coord,y-coord
  ...
  end
  id2
  x-coord,y-coord
  x-coord,y-coord
  ...
  end
  end

 Polygon format files are similar, except that the first line for each
 region also contains the X,Y coordinates of a center point in the polygon
 used for labeling.  In this case, a separate MAP2= data set is required,
 and is merged with the MAP= data set.

  id1,x-coord-label,y-coord-label,idlabel
  x-coord,y-coord
  x-coord,y-coord
  ...
  end
  id2,x-coord-label,y-coord-label,idlabel
  x-coord,y-coord
  ...
  end
  end


=Usage:

 The MAP2GEN macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%map2gen(map=maps.us);
 
==Parameters:

* MAP=        The (libname.)name of the map to be converted
               
* MAP2=       Data set containing polygon centers and ID labels when
              TYPE=POLY.  

* ID=         The name of an observation ID variable [Default: ID=ID]

* X=          Name of X (longitude) variable in MAP and MAP2 [Default: X=X]

* Y=          Name of Y (latitude) variable in MAP and MAP2 [Default: Y=Y]

* IDLABEL=    Label variable for region ID in the MAP2 data set, typically
              the name of the region.

* DLM=        Field delimiter used in writing output records. Use 
              DLM=%STR(,) for comma delimited. [Default: DLM=%STR( )]

* PRE=        Prefix string for each x,y line [Default: PRE=%STR()]

* CLOSE=      Close each polygon?  A non-zero value causes the first
              X,Y pair to be repeated at the end of each polygon.
              [Default: CLOSE=1]

* TYPE=       Line or polygon style? [Default: TYPE=LINE]

* XYFORMAT=   Format for writing X,Y values [Default: XYFORMAT=BEST9.]

* OUTFILE=     The name of the output file. You can specify a
               file path, or the output will be written to the
			   working directory. [Default: OUTFILE=&MAP.GEN]

=Example:

  %map2gen(
    map=mymaps.gfrance,
    map2=mymaps.gfrance2,
    id=dept,
    idlabel=department,
    type=poly
    );

 The output is written to GFRANCE.GEN.
 
 =*/

%macro map2gen(
	map=,             /* Name of the map to convert                        */
    map2=,            /* data set containing polygon centers and id labels */
	id=id,            /* region id variable                                */
    x=x,              /* name of X (longitude) variable in map and map2    */
    y=y,              /* name of Y (latitude) variable in map and map2     */
    idlabel=,         /* label variable for region id                      */
    dlm=%str( ),      /* field delimiter                                   */
    pre=%str(),       /* prefix string for each x,y line                   */
    close=1,          /* close each polygon?                               */
    type=line,        /* line or polygon style?                            */
    xyformat=best9.,  /* format for writing X,Y values                     */
	outfile=          /* Name of the output .gen file                      */
	);


%let type=%upcase(%substr(&type,1,4));

%if %length(&outfile)=0 %then %do;
    %let name=%scan(&map,-1, %str(.));
    %put name=&name;
    %let outfile=&name..gen;
    %end;
xxx
filename out "&outfile";
%put MAP2GEN: Writing &map to &outfile;

data _null_;
    %if &type=LINE %then %do;
    	set &map end=eof;
        %end;
    %else %do;
    	merge &map &map2 (rename=(&x=xc &y=yc)) end=eof;
        %end;
	by &id;

	retain x1 y1;
	format &x &y &xyformat;
	file out delimiter="&dlm";

    %* save first &x,&y to close;
	if first.&id then do;
		x1 = &x; y1 = &y;
        %if &type=LINE %then %do;
		    put &pre &id;
            %end;
        %else %do;
		    put &pre &id xc yc &idlabel;
            %end;
		end;

	put &pre &x  &y;
    
	if last.&id then do;
        %if &close %then %do;
    		put &pre  x1  y1;   /* allow for closing &x,&y */
            %end;
		put 'END';
		end;
	if eof then put 'END';
%mend;

