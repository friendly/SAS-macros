
 /*------------------------------------------------------------------*
  * BIPLOT SAS  - Macro to construct a biplot of observations and    *
  *               variables. Uses IML.                               *
  *------------------------------------------------------------------*
  *  Author:  Michael Friendly            <FRIENDLY@YORKVM1>         *
  * Created:  1 Mar 1989 13:16:36                                    *
  * Revised:  20 Dec 1989 09:54:19                                   *
  * Version:  1.2                                                    *
  *                                                                  *
  *      From ``SAS System for Statistical Graphics, First Edition'' *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA      *
  *                                                                  *
  * ?CõÌ?FÇÝÆéÃƒ?v ( 1998.5 )                                        *
  *       ?EÃóÆ{?Þ?Œ???x???æ?í?ö?†?«?Ý?ñ?¨?‰??Ó                     *
  *       ?E?x?N?g???ŒÃã?‰?×?¶?Ü?L???æ?ƒ?Å?Ó                         *
  * ?CõÌ?FÇÝÆéÃƒ?v ( 1998.8 )                                        *
  *       ?E?í?ö?I?v?V???Ã?æÂ‡?‚??Ó                                 *
  *------------------------------------------------------------------*/

%macro BIPLOT
(
    data   = _LAST_,    /* Data set for biplot                      */
    var    = _NUMERIC_, /* Variables for biplot                     */
    id     = ID,        /* Observation ID variable                  */
    dim    = 2,         /* Number of biplot dimensions              */
    factype= SYM,       /* Biplot factor type: GH, SYM, JK or COV   */
    std    = STD,       /* How to standardize columns: NONE|MEAN|STD*/
    scale  = 1,         /* Scale factor for variable vectors        */
    out    = BIPLOT,    /* Output dataset: biplot coordinates       */
    anno   = BIANNO,    /* Output dataset: annotate labels          */
    pplot  = no,        /* Produce printer plot?                    */

    vardef   = df      ,/* Åã?U?v?Z?Î?ŒÅãÅÞ?F DF | N            */
    fontolab = hwdmx001,/* ?ƒÁŒ???x???Œ?t?H?Ã?g?D1?MS?S?V?b?N  */
    sizeolab = 0.6     ,/* ?ƒÁŒ???x???Œ?T?C?Y                   */
    fontvlab = hwdmx003,/* ÅõÄ???x???Œ?t?H?Ã?g?D1?MS?S?V?b?N  */
    sizevlab = 0.6     ,/* ÅõÄ???x???Œ?T?C?Y                   */
    sizevtop = 0.6     ,/* ?x?N?g??Ãã?iÜV?j?Œ?T?C?Y             */
    markvtop = C       ,/* ?x?N?g??Ãã?iÜV?jMARKER?t?H?Ã?g?ŒÂl   */
                        /* ?uC?v??u?£?v?D?uS?v??u?{?v         */
                        /* ?uK?v??ž?¢?u?ã?v?D?uG?v?ÁÔ?¢?u?ã?v */
    plot = n ,          /* ?O???t?í?ö?ŒÇLÆÌ: Y | N              */
    device = win ,      /* ?oÇ?f?o?C?X: WIN | WINPRTM | WINPRTC*/

    horigin = 0 cm,     /* ?O???t?í?ö?Î?Œ?¶Ç]ÄÂ                    */
    vorigin = 0 cm,     /* ?O???t?í?ö?Î?Œ?ëÇ]ÄÂ                    */
    hlength = 15 cm ,   /* ?O???t?í?ö?Î?Œõ?ÅÓ?É?ŒÂ´?Ì              */
    vlength = 15 cm ,   /* ?O???t?í?ö?Î?Œõ?ÂÒ?É?ŒÂ´?Ì              */
                        /* A4?¡?Œ?Þ??? hlength = 25 cm ÂðÃx       */
    horder  =,          /* %str( order = ( ------ ) ) , ?á?wÂÜ?´?Ý */
    vorder  =,          /* %str( order = ( ------ ) ) , ?á?wÂÜ?´?Ý */

                        /* AXIS ?æ?}?N???O?†?ÆÁO?wÂÜ?´?Ý?Þ??         */
    haxis   =,          /* AXIS statement for horizontal axis        */
    vaxis   =,          /* and for vertical axis- use to equate axes */

    hsym    = 0.4,      /* height of plot symbols                    */
    hval    = 0.6,      /* ?ÉÆ˜õ´?Œ???Ì                              */
    fval    = simplex,  /* ?ÉÆ˜õ´?Œ?t?H?Ã?g                          */
    lvref   = 2,        /* õ?ÂÒõö?Œõö?âÄ”??                          */
    lhref   = 2,        /* õ?ÅÓõö?Œõö?âÄ”??                          */
    dim1lab = f = hwdmx003 h = 0.8  "&factype Biplot of &std DATA",
    dim2lab = ' '
);

/*----- ?f?[?^?Z?b?gÂ??ŒÅõÄ???x???æ?ñÃÔ?´?Ý -----------------------*/

%if &data = _LAST_ %then %let data = %scan( &sysdata , 2 ) ;
%put NOTE: dataset name = &data . ;

data format ;
    if 0 then set &data ;
    keep start label fmtname type ;
    length start $8 label $40  ;
    fmtname = 'VLABELF' ;
    type    = 'C'       ;
    array vv( * ) &var  ;
    do i = 1 to dim( vv ) ;
        call vname( vv(i), start ) ;
        call label( vv(i), label ) ;
        output ;
    end ;
    stop ;
run ;
proc format cntlin = format ; 
run ;

%if %upcase( &vardef ) = DF %then %let vardef = %str( N - 1 ) ;

%let std = %upcase( &std ) ;

%let factype = %upcase( &factype ) ;
      %if &factype = GH  %then %let p =  0 ;
%else %if &factype = SYM %then %let p = .5 ;
%else %if &factype = JK  %then %let p =  1 ;
%else %if &factype = COV %then %let p =  0 ;
%else
    %do;
        %put BIPLOT: FACTYPE must be GH, SYM, or JK. "&factype" is not valid.;
        %goto done;
    %end;

Proc IML;
Start BIPLOT(Y, ID, VARS, OUT, power, scale ) ;
   N = nrow( Y ) ;
   P = ncol( Y ) ;
   %if &std = NONE
       %then Y = Y - Y[:] %str(;);             /* remove grand mean   */
       %else Y = Y - J(N,1,1)*Y[:,] %str(;);   /* remove column means */
   %if &std = STD %then %do;
      S = sqrt(Y[##,] / ( &vardef ) );
      Y = Y * diag (1 / S );
   %end;
   print "Standardized Type: &std , VARDEF = &vardef " ;

   *-- Singular value decomposition:
        Y is expressed as U diag(Q) V prime
        Q contains singular values, in descending order;
   call svd(u,q,v,y);

   reset fw=8 noname;
   percent = 100*q##2 / q[##];
      *-- cumulate by multiplying by lower triangular matrix of 1s;
   j = nrow( q ) ;
   tri= (1:j)` * repeat(1,1,j)  >= repeat(1, j, 1) * ( 1:j ) ;
   cum = tri * percent ;
   c1={'Singular Values'} ;
   c2={'Percent'        } ;
   c3={'Cum % '         } ;
   Print "Singular values and variance accounted for",,
         q       [ colname=c1 format=10.5    ]
         percent [ colname=c2 format=10.5    ]
         cum     [ colname=c3 format=10.5    ];

   d = &dim ;
   *-- Extract first  d  columns of U & V, and first  d  elements of Q;
   U = U[,1:d];
   V = V[,1:d];
   Q = Q[1:d];

   *-- Scale the vectors by QL, QR;
   * Scale factor 'scale' allows expanding or contracting the variable
     vectors to plot in the same space as the observations;
   QL= diag( Q ## power     ) ;
   QR= diag( Q ## (1-power) ) ;
   A = U * QL         ;
   B = V * QR # scale ;

  %if %upcase( &factype ) = COV %then
    %do ;
      A = SQRT( &vardef       ) # A ;
      B = ( 1 / SQRT(&vardef) ) # B ;
    %end ;

   OUT = A // B;

   *-- Create observation labels;
   id = id // vars`;
   type = repeat({"OBS "},n,1) // repeat({"VAR "},p,1);
   id  = concat(type, id);


   factype = {"GH" "Symmetric" "JK" }[ 1 + 2#power ] ;

%if %upcase( &factype ) = COV %then
    %do ;
        factype = { "COV" } ;
    %end ;

   print "Biplot Factor Type:" factype ;

   cvar = concat(shape({"DIM"},1,d), char(1:d,1.));
   print "Biplot coordinates",
         out[rowname=id colname=cvar];
   %if &pplot = YES %then
   call pgraf(out,substr(id,5),'Dimension 1', 'Dimension 2', 'Biplot');
      ;
   create &out  from out[rowname=id colname=cvar];
   append from out[rowname=id];
finish;

%if %upcase( &var ) = _NUMERIC_ %then %let vlist = var _NUM_   ;
%else                                 %let vlist = var{ &var } ;

   use &data;
   read all &vlist into y[ colname = vars rowname = &id ] ;
   power = &p;
   scale = &scale;
   run biplot(y, &id,vars,out, power, scale );
   quit;

 /*----------------------------------*
  |  Split ID into _TYPE_ and _NAME_ |
  *----------------------------------*/
data &out;
   set &out;
   drop id;
   length _type_ $3 _name_ $16;
   _type_ = kscan(id,1);
   _name_ = kscan(id,2);
run ;
 /*--------------------------------------------------*
  | Annotate observation labels and variable vectors |
  *--------------------------------------------------*/
data &anno;
   set &out;
   length function style $8 text $60 ;
   xsys='2'; ysys='2';
   text = _name_;

   if _type_ = 'OBS' then do;      /* Label the observation   */
      color='BLACK';
      x = dim1;
      y = dim2;
      position = '5' ;
      style    = "&fontolab" ;
      size     =  &sizeolab  ;
      function ='LABEL   '   ;
      output;
      end;

   if _type_ = 'VAR' then do;           /* Draw line from     */
      color='BLACK ';
      x = 0; y = 0;                     /* the origin to      */
      function='MOVE'    ; output;
      x = dim1; y = dim2;               /* the variable point */
      function='DRAW'    ; output;
      if dim1 >=0
         then position='6';             /* left justify       */
         else position='2';             /* right justify      */
      /*------- ?™?ÅŠ?‰?ŠÂu?æ?…ÂÜ?` -------------------------*/
      select ;
         when( x >= 0 & y >= 0 ) position = '3' ;
         when( x >= 0 & y <  0 ) position = '9' ;
         when( x <  0 & y >= 0 ) position = '1' ;
         when( x <  0 & y <  0 ) position = '7' ;
      end ;
      /*---- ÅõÄ???x???ã?€?Þ?ŽÅ\?µ?´?Ý ----------------------*/
      function = 'LABEL   '             ;
      text     = put( text, $vlabelf. ) ;
      style    = "&fontvlab"            ;
      size     =  &sizevlab             ;
      output;                           /* variable name      */

      /*------ ÜV?i?×?¶?Ü?j?æÅ`?± ----------------------------*/
      function = 'LABEL' ;
      x = dim1 ;
      y = dim2 ;
      style = 'MARKER  '  ;
      text  = "&markvtop" ;
      size  =  &sizevtop  ;
      position = '5'      ;

      ab = sqrt( x ** 2 + y ** 2 ) ;     /* ?ŽÅ“?ŒÂ´?Ì?æ???û?Ý     */
      sin = y / ab ;                     /* SIN?æ???û?Ý            */
      radian = arsin( sin ) ;            /* ?t?T?C?Ã?–õÄ?†???W?A?Ã */
      angle = abs( radian ) * 57.29578 ; /* ???W?A?Ã?æ?pÃx(õ×ÁŽÂl) */
      select ;                           /* ?™??Œ?æ??Åã?Å0-360Ãx  */
        when( sign(x) =  1 & sign(y) =  1 ) angle = angle + 270 ;
        when( sign(x) = -1 & sign(y) =  1 ) angle = 90 - angle  ;
        when( sign(x) = -1 & sign(y) = -1 ) angle = angle +  90 ;
        when( sign(x) =  1 & sign(y) = -1 ) angle = 270 - angle ;
        when( sign(x) =  0 & sign(y) =  1 ) angle =   0 ;
        when( sign(x) =  0 & sign(y) = -1 ) angle = 180 ;
        when( sign(x) =  1 & sign(y) =  0 ) angle = 270 ;
        when( sign(x) = -1 & sign(y) =  0 ) angle =  90 ;
      end ;
      output ;
      end;
run ;

%if %upcase( &plot ) = Y %then
    %do;
        %let hlength = %str( length = &hlength ) ;
        %let vlength = %str( length = &vlength ) ;
        goptions horigin = &horigin
                 vorigin = &vorigin
                 device  = &device
        ;
        symbol v = none ;
        %if %length( &haxis ) = 0 %then
            %do ;
                axis2 label = ( &dim1lab )
                &horder
                value = ( f = &fval h = &hval ) &hlength ;
                %let haxis = axis2 ;
            %end;
        %if %length( &vaxis ) = 0 %then
            %do ;
                axis1 label = ( &dim2lab )
                &vorder
                value = ( f = &fval h = &hval ) &vlength ;
                %let vaxis = axis1 ;
            %end;
        proc gplot data = &out ;
            plot dim2 * dim1 / anno = &anno frame
                   href  = 0 vref = 0 lvref = &lvref lhref = &lhref
                   vaxis =&vaxis haxis = &haxis
                   hm = 1 vm = 1 
                   des = "Biplot of &data"
            ;
        run ;
        quit;
    %end;
%done:
%mend BIPLOT;

