***********************************************************;
* MST.SAS -- Macro using SAS/IML to compute the minimum   *;
* spanning tree for a set of points.  Input to the macro  *;
* includes the input data set containing coordinates, the *;
* variable names containing the coordinates, and the      *;
* output data set containing the index and coordinates of *;
* the point to which each point will be joined.           *;
* Alternatively, a SAS/GRAPH annotate data set can be     *;
* constructed that contains the MOVE and DRAW commands to *;
* plot the MST using PROC GPLOT or PROC G3D.              *;
*                                                         *;
* Barry Moser, Dept. Experimental Statistics, LSU         *;
* Created: 01/24/91                                       *;
*  11/28/01 -- Added VAR2 option. The Var2 list is a list *;
*   of plot coordinates corresponding with the VAR        *;
*   coordinates for which the MST is computed. This is    *;
*   useful when combined with the PLOTIT macro.           *;
***********************************************************;
%Macro MST(Data=_LAST_,Out=MST,
           Var=Dim1 Dim2,
           BigValue=1E66,
           Summary=1,Annotate=1,
           Dim=2,
           Color=Yellow,
           LineType=1,LineSize=1,
           HSys=4,XSys=2,YSys=2,ZSys=2,
           Var2=);
%If &Dim=3 %Then %If "%Scan(&Var,3)"=" " %Then %Let Dim=2;

Proc IML;
 /* Read In The Coordinates */
Use &Data;
Read ALL Var{&Var} Into Coord;
Close &Data;

 /* Initialize Constants and Lists */
n=NRow(Coord);
BigValue=&BigValue;

 /* For List1: Group A = 0, Group B = 1 */
List1=J(n,1,1);
List2=J(n,1,1);
 /* Start With Obs 1 In Group A */
List1[1]=0;
 /* Compute Distances From Obs 1 To All Others */
List3=Sqrt((Coord-Repeat(Coord[1,],n,1))[,##]);
 /* Set Obs 1 To ``BigValue'' So We Do Not Select It Again */
List3[1]=BigValue;

 /* Now Build The MST */
Start;
 Do While (Any(List1)); /* Any obs still in set B? */
   /* Find Smallest Distance In Set B And Make It New Point In A */
   q=List3[>:<];      /* Index of Minimum */
   List1[q]=0;        /* Put It In Set A  */
   List3[q]=BigValue; /* Set It So We Want Find It Again */
   /* Compute Distances From All Points in B to Point Q and Update */
   DRange=Loc(List1); /* List of Indexes of Points in B */
   If NCol(DRange) > 0 Then /* Still Points in B to Check */
    Do j=1 To NCol(DRange);
      i=DRange[j];
      Dist=Sqrt(SSQ(Coord[i,]-Coord[q,]));
      If Dist < List3[i] Then  /* This Is A Join In The Tree */
       Do;
         List2[i]=q;
         List3[i]=Dist;
       End;
    End;
  End;
  Free List1 List3 DRange Dist q i j;
Finish;
Run;

%If &Summary %Then
 %Do;
   /* Print a Summary of the IML Results */
   Y=Coord[List2,];
   ObsNum=(1:n)`;
   Print "X Contains the From Coordinates,";
   Print "Y Contains the To Coordinates";
   Print ObsNum List2, Coord Y;
   Free Y ObsNum;
 %End;

%If &Annotate %Then
 %Do;
   %If "&Var2" NE "" %Then
   %Do;
   Use &Data;
   Read ALL Var{&Var2} Into Coord;
   Close &Data;
   %End;
    /* Create The Annotate Data Set */
   Color="&Color";
   HSys="&HSys"; XSys="&XSys"; YSys="&YSys"; ZSys="&ZSys";
   Angle=0; Line=&LineType; Position="5"; Rotate=0;
   Size=&LineSize;
   When="A";
   X=0; Y=0; Z=0;
   Function="Move    ";
   Create &Out
     Var{Color HSys XSys YSys ZSys Angle Line Position Function
         Rotate Size When X Y Z};
   Do j=2 To n;
     Function="Move    ";
     X=Coord[j,1]; Y=Coord[j,2];
     %If &Dim=3 %Then %Str(Z=Coord[j,3];);
     Append
       Var{Color HSys XSys YSys ZSys Angle Line Position Function
           Rotate Size When X Y Z};
     Function="Draw    ";
     X=Coord[List2[j],1]; Y=Coord[List2[j],2];
     %If &Dim=3 %Then %Str(Z=Coord[List2[j],3];);
     Append
       Var{Color HSys XSys YSys ZSys Angle Line Position Function
           Rotate Size When X Y Z};
   End;
   Close &Out;
 %End;
%Else
 %Do;
    /* This Data Set Contains The Observation Number
       And The Coordinates To Which The Observation
       Is To Be Joined.  It Should Be 1-to-1 Merged
       With The Original Data Set To Pair From and To
       Points. */
    Y=Coord[List2,];
    Y=List2||Y;
    CNames="Join"||{&Var};
    Create &Out From Y[ColName=CNames];
    Append From Y;
    Close &Out;
 %End;
%Str(Quit;);
%Mend MST;
