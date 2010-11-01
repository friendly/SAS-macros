*Options Mprint symbolgen;
Options dquote;
*--------------------------------------------------------------*
|        Pathfinder Network Generation Algorithm               |
|                                                              |
| Given a square matrix of distance estimates, find a set      |
| of links in a network and weights for the links so that      |
| the length of the shortest path thru the network accounts    |
| for each distance estimate.                                  |
|                                                              |
| The pathfinder algorithm is due to Roger Schvanaveldt at     |
| New Mexico State University.                                 |
|                                                              |
| Written by: Michael Friendly   16 Apr 87                     |
|             York University                                  |
| Comments to: friendly@yorku.ca                               |
|                                                              |
| Revised:                                                     |
|  Date  Vers Comments                                         |
|  4/24  1.1  Add calculation of STRESS                        |
|  4/27  1.2  Add calculation of in/out degree                 |
|             Find Hubert's Z for correspondence between D and |
|             fitted distances                                 |
*--------------------------------------------------------------*
| The PATHNET macro takes two datasets as input:               |
| A matrix of input distances for one or more samples (DATA)   |
| and a dataset containing labels for the items as a single    |
| character variable (ITEMLBL).                                |
| The variables in the distance matrix are specified by VARS=. |
| The DATA dataset may also have an optional numeric variable  |
| identifying each distance matrix (ID).                       |
*--------------------------------------------------------------*;
 
%MACRO PATHNET  (DATA=_LAST_,     /* Name of input dataset         */
                                  /* (default: most recent dataset)*/
                 VARS=,           /* List of item variables        */
  /* optional*/  ID=,             /* Subject ID variable           */
                 ITEMLBL=,        /* Item label dataset            */
                 RMETRIC=0,       /* Minkowski exponent for path   */
                                  /* length (0 => infinity)        */
                 QMETRIC=0,       /* Q metric: max path steps      */
                                  /*  (0 => nw-1)                  */
                 INFVALUE=0,      /* maximum distance value        */
                                  /*  (ignored if 0)               */
                 OUTLINK=_DATA_,  /* Name of output links dataset  */
                                  /*  (or NULL to suppress)        */
                 OUTDIST=_DATA_,  /* Name of output distances      */
                                  /*  (or NULL to suppress)        */
                 PRINT=NONE);     /* Print details (NONE/SOME/ALL) */
%LET VERS=1.2;
Title2 "Pathfinder Version &VERS";
%LET T=%STR(%'); %* For transpose;
 
proc matrix FW=5 ;
   fetch DM data=&DATA (keep= %QUOTE(&VARS)) /* Data matrix      */
            colname=TO;
   fetch  LAB data=&ITEMLBL type=CHAR;       /* Item labels      */
 
   NW = NCOL( DM );             * Number of words (items) ;
   NS = NROW( DM ) #/ NW;       * Number of subjects (matrices);
 
   If nrow(LAB) ^= NW then do;
      Note 'Number of item labels does not match number of variables';
   end;
   %IF &ID.NULL ^= NULL %THEN %DO;
      fetch SUBJ data=&DATA (keep=&ID)          /* Subject #s       */
            colname=IDS;
   %END;
   %ELSE %DO;
      Do SUBJECT=1 to NS;
         SUBJ=SUBJ // J(NW,1,SUBJECT);
         IDS = 'SUBJ';
      End;
      IDS =' ';
   %END;
   LABD = IDS || TO;
 
   R  = &RMETRIC;
   if R=0 or R>999 then do;
      R=1; infr=1;
      Note 'R metric = infinity';
      end;
   else do;
      infr=0;
      Note 'R metric'; Print R;
      end;
   Q  = &QMETRIC;
   If Q=0 or Q>NW then Q=NW-1;
   Note 'Q metric'; Print Q;
   precis=0.00001;
 
   DETAILS = 0; DEBUG = 0;
   %IF &PRINT=SOME %THEN %DO;
      DETAILS = 1;
   %END;
   %ELSE %IF &PRINT=ALL or &PRINT=DEBUG %THEN %DO;
      DETAILS = 1; DEBUG = NW;
   %END;
 
DO SUBJECT = 1 to NS;
GETDATA:
   row1 = 1 + NW * (SUBJECT-1);
   row2 = NW * SUBJECT;
   S  = SUBJ(row2,);
   D  = DM(row1:row2,);               * Extract rows for this subject;
   SW = J(NW,1,S);                    * Col vector of subj_no for label;
     SUBJID = SUBJECT || S;
     SID  = 'Subject' || IDS ;
     print SUBJID colname=SID format=8.;
     if DEBUG > 0 then do;
        note  Input Matrix;
        print D rowname=LAB  colname=LAB format=4.;
     end;
 
     loops=0;
     if any(vecdiag( D ) > 0) then do;
         PLACES = LOC( vecdiag(D) > 0);
         loops=1;
     end;
     symdis=0;                       * Is the matrix symmetric? ;
     if all (D = D&T) then do;
        NOTE 'The distances are symmetric and links are undirected';
        symdis=1;
     end;
     else do;
        NOTE 'The distances are asymmetric and links are directed';
     end;
     maxdat = D(<>,<>);
     infval = &INFVALUE;
     If infval>0 & any(D >=infval) then do;
        Note "Values >= than &INFVALUE treated as infinite";
        D = (D <  infval)#D
           +(D >= infval)#infval;
     end;
     If R = 1 then WR = D;
              else WR = D ## R;
     If R > 1 then maxdat = maxdat ## R;
     maxdat = (NW+1)#maxdat;
 
PFNGEN:
     OFFDIAG  = J(NW,NW,1) - I(NW);
     POSSIBLE = (D < infval) & (OFFDIAG | loops);
     MINDIS = WR;
     oldnlink=0;
 
     do COUNT = 2 to Q; /*until (DONE)*/
        DONE=1;
        OLDDIS = MINDIS;
        nlinks = 0;
        NET    = J(NW,NW,0);
        do row = 1 to NW;
           if symdis then start=row;
                     else start=1;
           do col = start to NW;
              if ((row ^=col) or loops)  then do;
                 V1 = OLDDIS(row,)&T;
                 V2 = WR(,col);
                 if infr then VEC = V1 <> V2;
                         else VEC = V1 +  V2;
                 min= (VEC // OLDDIS(row,col)) (><,);
                 MINDIS(row,col) = min;
                 if ((abs(min - WR(row,col)) < precis) and
                    ((infval=0) or (D(row,col)<infval))) then do;
                    NET(row,col) = D(row,col);
                    nlinks = nlinks+1;
                    end;
                    else NET(row,col) = maxdat;
                 done = done & (min = OLDDIS(row,col));
                 if symdis then do;
                    NET(col,row) = NET(row,col);
                    MINDIS(col,row) = min;
                 end;
              end;
           end;
        end;
        link  DOCOR;
        history = history // (count || nlinks || RM || HUB);
        if DEBUG>0 then do;
           Print COUNT;
           Print NET colname=LAB rowname=LAB;
*          Print MINDIS colname=LAB rowname=LAB;
        end;
        if done then goto OUT;
     end;
OUT:
     If DETAILS > 0 then do;
        Note Network Distances;
        Print MINOUT colname=LAB rowname=LAB;
     end;
     hl = 'COUNT' 'NLINKS' 'STRESS' 'R2-ADD' 'R2-LOG' 'SLOPE'
          'Z'     'PRZ'    'PRC'  ;
     rl = ' '; rl=J(nrow(history),1,rl);
     Print history colname=hl rowname=rl;
 
PFNOUT:
     %IF &OUTDIST ^=NULL %THEN %DO;
     DOUT = SW || MINOUT;
     Output DOUT data=&OUTDIST (rename=(ROW=ITEM))
            colname=LABD rowname=LAB;
     %END;
 
*    Print POSSIBLE;
     LINK = (NET = D) & POSSIBLE;
     WEIGHTS= LINK # D;
     If DETAILS > 0 then do;
        Print LINK   rowname=LAB colname=LAB;
        Print WEIGHTS rowname=LAB colname=LAB;
     end;
 
     Do row = 1 to NW;
        lnk = loc (LINK(row,));
        if any(lnk) then do c = 1 to ncol(lnk);
           col = lnk(,c);
           if symdis & row>col then goto next;
           LINKS = LINKS // (row || col || WEIGHTS(row,col) );
next:   end;
      end;
      hl = 'ROW' 'COL' 'WEIGHT';
      If DETAILS>0 then
         Print LINKS colname = hl;
      NL=nrow(LINKS);
     %IF &OUTLINK ^=NULL %THEN %DO;
      LINKO = J(NL,1,S) || LINKS;
      hl = 'ROE' 'COL' 'WEIGHT';      /* ROW variable generated by SAS */
      hl = IDS || hl;
 
      output LINKO out=&OUTLINK (drop=ROW rename=(ROE=ROW))
             colname=hl;
     %END;
      If symdis then do;
         DEGREE = LINK(+,);
         hl = ' ';
      end;
      else do;
         DEGREE = LINK(,+)&T // LINK(+,);
         hl ='OUT' 'IN';
      end;
      Note 'Network degree of each node';
      Print DEGREE colname=LAB rowname=hl ;
 
      free history LINKS hl nl DEGREE;
END; *-------- Loop over subjects----;
 
    stop;
 
DOCOR:
   if oldnlink ^= nlinks then do;
      link NETMIN;
      link CORREL;
      link HUBERT;
      end;
   oldnlink=nlinks;
return;
 
NETMIN:
   MINOUT = NET;
   donemin=0;
   do while (^donemin);
      OLDDIS = MINOUT;
      donemin= 1;
      do row = 1 to NW;
         if symdis then start=row;
                   else start=1;
         do col = start to NW;
            V1 = OLDDIS(row,)&T;
            V2 = NET(,col);
            VEC = V1 + V2;
            min= (VEC // OLDDIS(row,col)) (><,);
            MINOUT(row,col) = min;
            if symdis then MINOUT(col,row)=min;
            donemin = donemin & (min=OLDDIS(row,col));
         end;
      end;
   end;
free V1 V2 VEC min donemin;
return;
 
*-- Find correlation between matrix elements;
CORREL:
   do row = 1 to NW;
      if symdis then start=row;
                else start=1;
      do col = start to NW;
         if POSSIBLE(row,col) then do;
            V = D(row,col) || MINOUT(row,col);
            V = V || log(V+1);
            XY = XY // V;
         end;
      end;
   end;
   np = nrow(XY);
        /* Calculate STRESS */
   ST = (XY(,1)-XY(,2))(##,);
   ST = SQRT(ST) #/ SQRT(XY(,2)(##,));
        /* Correlate data, fit & log-log */
   SM = XY(+,);
   XPX= XY&T * XY - (SM&T*SM #/np);
   SM = 1 #/ SQRT(DIAG(XPX));
   RM = SM * XPX * SM;
   RM = (RM(1,2) || RM(3,4))##2 || (RM(3,4) #/(SM(3,3)#/SM(4,4)));
   RM = round( RM,.001 );
   RM = ST || RM;
   free XY np SM XPX V ST;
   return;  /* with RM */
 
HUBERT:
*------------------------------------------------------------------*
|  Given a proximity matrix, Q, and a structure matrix, C, this    |
|  procedure tests the hypothesis that the proximity matrix and    |
|  the structure matrix have the same underlying pattern of big    |
|  and small elements.                                             |
|                                                                  |
|  The test statistic, GAMMA, is the sum of products of corres-    |
|  ponding matrix elements. It is compared to the permutation      |
|  distribution, obtained by randomly permuting the rows & cols    |
|  of the C matrix, while keeping the Q matrix fixed. Closed       |
|  form expressions for the MEAN and STD of the distribution of    |
|  GAMMA were obtained by Mantel (1967) & applied to the problem   |
|  of structure testing in proximity scaling by Hubert & Schultz   |
|  1976, Br. J. Math & Stat. Psychol., 190-241.                    |
|                                                                  |
|  Two probability values are obtained for the Z value of GAMMA.   |
|  PRZ is based on a (liberal) assumption that the distribution    |
|  of GAMMA is normal. PRC is a conservative approach based on     |
|  Cantellis  inequality.                                          |
|                                                                  |
|  This proc MATRIX calculation is valid for any proximity matrix. |
|  In particular, it does not assume a symmetrix matrix.           |
*------------------------------------------------------------------*;
    NN1 = 1 #/ (NW # (NW-1));
    NN2 = NN1 #/ (NW-2);
    NN3 = NN2 #/ (NW-3);
 
    GAMMA = ( D # MINOUT) (+,+);   /* Observed statistic          */
 
MEANVAR:
   M = D ; link SUMS;              /* Find sums & sums of squares */
   QS = B;                         /* for first matrix            */
 
   M = MINOUT; link SUMS;          /* Same for second matrix      */
   CS = B;
 
   B = CS # QS;
   RH= (1 0 0 0 0 0 0 / 0 1 1 0 0 0 0 /
        0 0 0 1 2 1 0 / 0 0 0 0 0 0 1 ) * B;
 
   EGAMMA = NN1 # sqrt(B(1,));
 
   VC = -NN1##2 || NN1 || NN2 || NN3;
   VGAMMA =  sqrt( VC * RH );
   Z = (GAMMA - EGAMMA) #/ VGAMMA ;
   Z = round (Z, .0001);
   PRZ = round (1 - PROBNORM ( Z ), .001);
   PRC = round (1 #/ (1 + Z##2), .001);
 
 
   HUB = Z || PRZ || PRC ;
   free CS QS B VC RH GAMMA EGAMMA VGAMMA M PRZ PRC;
return; /* with HUB */
 
  *-- Find sums of squares of one matrix;
SUMS:
   B = J.(7,1,0);
   B(1,) = ( M(+,+) )##2;
   B(2,) = ( M##2 ) (+,+);
   B(3,) = ( M # M&T ) (+,+);
   B(4,) = ( (M(,+))##2 - ((M##2)(,+)) ) (+,);
   B(5,) = ( (M(,+) # M&T(,+)) - (M # M&T)(,+) ) (+,);
   B(6,) = ( (M(+,)) ##2 - ((M##2)(+,)) ) (,+);
   B(7,) = ( M(+,+) )##2 - ( (M(+,)) ##2)(,+)
           - 2 # ( M(,+) # (M(+,))&T )(+,)
           - ( (M(,+))##2)(+,)  +  (M # M&T)(+,+) + (M##2)(+,+);
 
   return;
%MEND PATHNET;
