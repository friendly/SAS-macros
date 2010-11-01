/*-------------------------------------------------------------------*
 * LOGO SAS  - Logo-like annotate macros for SAS/Graph               *
 *                                                                   *
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <FRIENDLY@YORKVM1>          *
 * Created:  7 Feb 1989 16:22:37          Copyright (c) 1989         *
 * Revised:  7 Feb 1989 16:22:37          All rights reserved        *
 * Version:  1.0                                                     *
 *                                                                   *
 *-------------------------------------------------------------------*/
*goptions hsize=7in vsize=7in;              /* square plot env.*/
 
%macro LOGO;
%*------------------------------------------------------------------ *;
%* These SAS macros provide the ability to draw with the SAS/Graph   *;
%* ANNOTATE facility using the 'turtle-relative' drawing commands    *;
%* of the Logo language, rather than absolute X-Y coordinates.       *;
%* This makes drawing much easier for many types of figures.         *;
%* Since the SAS datastep has neither local variables, nor           *;
%* recursion, it is hard to some things that are very easy in Logo.  *;
%* Still, it is much easier than calculating X & Y coordinates.      *;
%*                                                                   *;
%* The macros make use of two new datastep variables, in addition    *;
%* to those used by the ANNOTATE facility:                           *;
%*    PEN = {'UP','DOWN'}  determines whether the turtle moves       *;
%*          or draws.                                                *;
%*    HEADING =turtles current heading, in degrees, where 0=NORTH    *;
%*          and angles increase clockwise.                           *;
%*                                                                   *;
%* The macros are:                                                   *;
%*    %logo           Compile the logo macros below                  *;
%*    %penup          Lift PEN up,  so turtle will move              *;
%*    %pendown        Put PEN down, so turtle will draw              *;
%*    %left(degrees)  Turn left (counterclockwise)                   *;
%*    %right(degrees) Turn right (clockwise)                         *;
%*    %forward(dist)  Move/draw forward in current HEADING           *;
%*    %back(dist)     Move/draw back in current HEADING              *;
%*    %inilogo        Initialize variables, move turtle home         *;
%*    %home           Move/draw to (0,0)                             *;
%*------------------------------------------------------------------ *;
%*  %logo causes the LOGO macros to be compiled. Use like ANNOMAC    *;
%*------------------------------------------------------------------ *;
   %put %str(    ) LOGO macros have been loaded ;
%mend LOGO;
 
%global pi;
%let pi=3.1415926;
 
%macro penup;
%*--------------------------------------------;
%* Put PEN up, so turtle will move            ;
%*--------------------------------------------;
    PEN='UP  ';
%mend  penup;
 
%macro pendown;
%*--------------------------------------------;
%* Put PEN down, so turtle will draw          ;
%*--------------------------------------------;
    PEN='DOWN';
%mend  pendown;
 
%macro left(degrees);
%*--------------------------------------------;
%* Turn LEFT by DEGREES (counterclockwise)    ;
%*--------------------------------------------;
    HEADING=mod(HEADING-(&degrees),360);
%mend  left;
 
%macro right(degrees);
%*--------------------------------------------;
%* Turn RIGHT by DEGREES (clockwise)          ;
%*--------------------------------------------;
    HEADING=mod(HEADING+(&degrees),360);
%mend  right;
 
%macro forward(distance);
%*--------------------------------------------;
%* Move/Draw FORWARD in the current HEADING   ;
%* - X & Y are in whatever XSYS, YSYS system  ;
%*   is being used, but MUST be absolute.     ;
%* - In turtle coordinates, HEADINGs go clock-;
%*   wise from North. In SAS geometry, angles ;
%*   go counterclockwise from East, so        ;
%*   HEADING = 90-angle                       ;
%* - PEN determines whether to MOVE or DRAW   ;
%*--------------------------------------------;
  select ( HEADING ) ;
    when (   0 )  Y = Y + &distance;
    when (  90 )  X = X + &distance;
    when ( 180 )  Y = Y - &distance;
    when ( 270 )  X = X - &distance;
    otherwise do;        /* general case */
      X=X+(&distance)*cos((90-HEADING)*atan(1)/45);
      Y=Y+(&distance)*sin((90-HEADING)*atan(1)/45);
    end;
  end;
  if PEN = 'UP' then FUNCTION = 'MOVE';
                else FUNCTION = 'DRAW';
  output;
%mend forward;
 
%macro back(distance);
%*--------------------------------------------;
%* Move/Draw BACK in the current HEADING      ;
%*--------------------------------------------;
    %forward (-1*(&distance));
%mend  back;
 
%macro home;
%*--------------------------------------------;
%* Move/Draw to (0,0)                         ;
%*--------------------------------------------;
    X=0; Y=0;
    if PEN = 'UP' then FUNCTION = 'MOVE';
                  else FUNCTION = 'DRAW';
    output;
%mend  home;
 
%macro inilogo;
%*---------------------------------------------;
%* Initialize logo variables & move turtle home;
%*  - use at start of data step, like dclanno  ;
%*---------------------------------------------;
    LENGTH PEN $ 4;
    %penup; %home; %pendown;
    HEADING=0;
%mend  inilogo;
