%macro stem(DATA=_LAST_,VAR=,ID=,idout=,OFACTOR=2,COLS=78,idlen=6);

%*******************************************************************;
%*Macro STEM:                                                       ;
%*-----------                                                       ;
%*Auteur: D.LADIRAY (INSEE-ENSAE, Aout 1991)                        ;
%*                                                                  ;
%*Cette macro permet de tracer le diagramme "stem & leaf" pour une  ;
%*variable num‚rique.                                               ;
%*                                                                  ;
%*Six paramŠtres peuvent etre pr‚cis‚s:                             ;
%*                                                                  ;
%*     DATA=       nom de la table SAS o— figure la variable …      ;
%*                 traiter. Par d‚faut, la derniŠre table SAS est   ;
%*                 utilis‚e.                                        ;
%*                                                                  ;
%*     VAR=        nom de la variable … analyser. Cette variable    ;
%*                 doit etre num‚rique.Ce paramŠtre est obligatoire.;
%*                                                                  ;
%*     ID=         nomme une variable identifiant. Si ce paramŠtre  ;
%*                 est renseign‚, le graphique repr‚sentera ces     ;
%*                 identifiants et non les valeurs de la variable.  ;
%*                 Le nombre de positions utilis‚es d‚pend de FORMAT;
%*                 Si ID est num‚rique, il peut y avoir             ;
%*                 "reformattage".                                  ;
%*                                                                  ;
%*     idout=     nomme une variable identifiant qui servira       ;
%*                 … repr‚senter les points "lointains" uniquement, ;
%*                 les autres individus ‚tant repr‚sent‚s par la    ;
%*                 valeur de la variable.                           ;
%*                 Le nombre de positions utilis‚es d‚pend de FORMAT;
%*                 Si IDOUT est num‚rique, il peut y avoir         ;
%*                 "reformattage".                                  ;
%*                                                                  ;
%*     OFACTOR=      nombre r‚el b qui permet de juger du             ;
%*                 caractŠre "lointain" : est lointain un individu  ;
%*                 dont la valeur de la variable est … plus de b    ;
%*                 intervalles inter-quartile du premier ou         ;
%*                 troisiŠme quartile. Par d‚faut, la valeur est 2. ;
%*                                                                  ;
%*     COLS=       ce nombre permet de g‚rer la largeur (nombre de  ;
%*                 colonnes) du graphe. Par d‚faut, il est pris     ;
%*                 ‚gal … 78 (options LINESIZE de SAS sous DOS).    ;
%*                                                                  ;
%* ATTENTION: CETTE MACRO TOURNE MAIS EST SANS DOUTE IMPARFAITE.    ;
%*            LES EVENTUELLES ERREURS DE SYNTAXE NE SONT PAS        ;
%*            CONTROLEES. EN PARTICULIER, DONNEZ UNE SEULE VALEUR   ;
%*            A CHAQUE PARAMETRE (ne pas utiliser de listes de      ;
%*            variables .... ou alors le r‚sultat pourrait etre     ;
%*            surprenant !).                                        ;
%*******************************************************************;
%*                                                                  ;
%* REFERENCES: Tout livre d"ANALYSE EXPLORATOIRE DES DONNEES", et en;
%*             particulier:                                         ;
%*                                                                  ;
%*       - "ABC of EDA" par P.VELLEMAN et D.HOAGLIN                 ;
%*                      Duxbury Press, 1981                         ;
%*       - "Exploratory Data Analysis" par J.W TUKEY                ;
%*                      Addison-Wesley, 1977                        ;
%*******************************************************************;

%IF &var eq %then %DO;
  %PUT  ERROR: pas de variable … analyser ;
  %GOTO %out;
  %END;

PROC SORT DATA=&data OUT=_code_(KEEP=&var &id &idout);
BY &var;

%*********************************************************************;
%*Dans cette premiŠre partie, on d‚termine les points "lointains"     ;
%*en cr‚ant une variable extr qui vaut 0 si le point est "lointain    ;
%*bas", 1 si il est "non lointain" et 2 si il est "lointain haut".    ;
%*Le caractŠre lointain se juge par la distance aux premier et        ;
%*troisiŠme quartiles, distance exprim‚e en intervalles interquartile.;
%*On repŠre en outre les valeurs manquantes (extr=-1) qui seront      ;
%*trait‚es … part dans le graphique.                                  ;
%*********************************************************************;

PROC UNIVARIATE NOPRINT DATA=_code_;
	VAR &var;
	OUTPUT OUT=_stats_ Q1=_q1x_ Q3=_q3x_ MEDIAN=_med_;

DATA _code_ ;
 DROP _q1x_ _q3x_;LENGTH _extr_ 3;
 RETAIN _q1x_ _q3x_ _med_;
 SET _code_;
 if _N_=1 then SET _stats_;
 if &var <=.Z then _extr_=-1;
           else _extr_=2-(&var < _q1x_ - &OFACTOR*(_q3x_ - _q1x_))
                        -(&var <=_q3x_ + &OFACTOR*(_q3x_ - _q1x_));

%*seuls les "non lointains" d‚finiront le graphique                 ;
%*les statistiques min, max et range (‚tendue) servent … g‚rer ce   ;
%*graphique.                                                        ;

PROC SUMMARY DATA=_code_;
	WHERE _extr_=1;
	VAR &var;
	OUTPUT OUT=_stats_ RANGE=_rx_ MIN=_minx_ MAX=_maxx_;


%*******************************************************************;
%*Cr‚ation du fichier qui servira au trac‚ du graphique.            ;
%*                                                                  ;
%*   - calcul du nombre approximatif de lignes du graphique (nlin)  ;
%*     le programme fait en sorte que le nombre final de lignes du  ;
%*     graphe ne d‚passe pas une quarantaine, pour un souci de      ;
%*     lisibilit‚.                                                  ;
%*                                                                  ;
%*   - cr‚ation des variables stem et leaf aprŠs calcul de la       ;
%*     variable unit‚ qui permet, par transformation de la variable ;
%*     de d‚part, de g‚n‚rer des valeurs significatives et toujours ;
%*     entiŠres de stem.                                            ;
%*                                                                  ;
%*   - en outre, ce programme g‚nŠre le cas ‚ch‚ant des valeurs de  ;
%*     stem ne correspondant pas … des valeurs initiales de la      ;
%*     variable ‚tudi‚e: des trous dans la distribution initiale    ;
%*     doivent se retrouver sur le graphe. Des observations         ;
%*     fictives sont cr‚‚es.                                        ;
%*                                                                  ;
%*******************************************************************;

DATA _code_;
	RETAIN _stem1_ 0 _rx_ _minx_ _maxx_ _med_ _nbre_  _unit_
       _nlin_  _stem2_ _leaf1_ _var1_
       %IF &id ne  %then %STR(_id1_ );
       %IF &idout ne %then %STR(_idext1_ );
       ;

	KEEP _extr_ _stem_ _leaf_ _med_ /*_minx_ _maxx_ */_unit_ &var
       %IF &id ne  %then %STR(_id_ );
       %IF &idout ne %then %STR(_idout_ ); ;
	SET _code_;

%*on cr‚e des variables identifiant caractŠre si n‚cessaire;
%*la syntaxe est un peu compliqu‚e mais on gŠre mieux les  ;
%*reformatages de cette fa‡on.                             ;

%IF &id ne  %then %STR(_id_=INPUT(LEFT(INPUT(&id,$12.)),$&idlen..););
%IF &idout ne %then
/*      %STR(_idout_=INPUT(LEFT(INPUT(&idout,$12.)),$&idlen..);); */
      %STR(_idout_=substr(LEFT(&idout),1,&idlen););

%*calcul de unit‚, stem et leaf                                     ;

 if _N_=1 then DO;
 SET _stats_;
 _nbre_=ROUND(10*LOG10(_freq_));
 _unit_=ROUND(LOG10(_rx_/_nbre_));
 _nlin_=ROUND((_maxx_-_minx_)/(10**_unit_));
 call symput('min', trim(left(put(_minx_,best10.))));
 call symput('max', trim(left(put(_maxx_,best10.))));
 END;
 if &var ^= . then DO;
 _stem_=INT(&var/(10**_unit_));
 _leaf_=ABS(ROUND((&var-(_stem_*(10**_unit_)))/(10**(_unit_-1))));
 END;

%*reconstitution des "trous" de la distribution initiale            ;

 if (&var > _minx_) AND (_extr_=1) AND (_stem_ - _stem1_>1)
 then DO;
      _stem2_=_stem_;_leaf1_=_leaf_;_var1_=&var;
      %IF &id ne  %then %STR(_id1_=_id_ ;);
      %IF &idout ne %then %STR(_idext1_=_idout_ ;);
      DO i=1 TO (_stem_-_stem1_-1);
      _stem_=_stem1_+i;
      if (_nlin_ > 36) then _stem_=_stem_-MOD(_stem_,ROUND(_nlin_/18));
      _leaf_=.;&var=.;
      %IF &id ne  %then %STR(_id_=' ' ;);
      %IF &idout ne %then %STR(_idout_=' ';);
      OUTPUT;
      END;
      _stem_=_stem2_;_leaf_=_leaf1_;&var=_var1_;
      %IF &id ne  %then %STR(_id_=_id1_ ;);
      %IF &idout ne %then %STR(_idout_=_idext1_;);
 END;
 _stem1_=_stem_;

%*on gŠre ici, approximativement, le nombre de lignes du graphe final;

 if (_nlin_ > 36) then _stem_=_stem_-MOD(_stem_,ROUND(_nlin_/18));
 OUTPUT;
RUN;
*proc print data=_code_;

%*******************************************************************;
%*programme permettant le trac‚ du graphique stem & leaf.           ;
%*on calcule aussi les effectifs totaux et par branche.             ;
%*******************************************************************;

DATA _NULL_;
RETAIN _eff_ _form_;
 FILE PRINT COL=_col ;
   SET _code_ END=_fin_ ;
	BY _extr_ _stem_;

%*gestion des variables … repr‚senter: variable initiale, feuilles, ;
%*ou variables identifiants selon le cas.                           ;

%IF &id ne %then %STR(_pleaf_=_id_;);
           %else %STR( _pleaf_=PUT(_leaf_,1.););
%IF &idout ne %then %STR(_pextr_=_idout_;);
               %else %STR(_pextr_=put(&var, best8.););
 if _leaf_ ^= . then _tot_+1;

 if _N_=1
 then DO;
	PUT / "Stem & Leaf of variable &var" /;
	PUT @8 "Smallest value:  &min";
	PUT @8 "Largest value:   &max";
	PUT @8 'Median:         ' _med_;
	_a_=INT(_med_/(10**_unit_));
	_b_=ABS(ROUND((_med_-(_a_*(10**_unit_)))/(10**(_unit_-1))));
	_c_=(10**_unit_);
	%IF &id eq %then %DO;
		%STR(PUT @8 'Unit:          ' _c_;);
		%STR(PUT @8 _a_  '!' _b_ 1. +1 'represents  ' _med_;);
	%END;
	%ELSE
		%STR(PUT "Observations are represented by the variable &id";);
	%IF &idout ne %then
		%STR(PUT "Outside obs. are represented by the variable &idout";);
	_form_=&cols-8;
	PUT / ' STEM' +1 '!' +1 'LEAVES' @_form_ '!'  '     n';
	PUT &cols*'-';
 END;

 if _extr_=-1
   then DO;
    if first._extr_ then _eff_=0;
    _eff_=_eff_+1;
    if last._extr_  then 
      put 'NMISS' +1 '!' @_form_ '!' _eff_ 6. ;
 END;
 else if (_extr_=0) or (_extr_=2)
   then DO;
    if first._extr_
       then DO;
         _eff_=0;
          if _col < &cols-11
             then DO;
               if _extr_=0 then put / '  LOW' +1 '!' _pextr_  @;
               if _extr_=2 then put / ' HIGH' +1 '!' _pextr_  @;
               _eff_=_eff_+1;
             END;
             else DO;
				 	PUT @_form_ '!';
               put @7 '!' @;
					PUT _pextr_ @;
					_eff_=_eff_+1;
					END;
       END;
       else DO;
          if _col < &cols-11
             then DO;PUT _pextr_ @;_eff_=_eff_+1;END;
             else DO;PUT @_form_ '!';
                     put @7 '!' @;PUT _pextr_ @;_eff_=_eff_+1;END;
       END;
    if last._extr_ then put @_form_ '!' _eff_ 6. /;
   END;
 else if _extr_=1 then DO;
    if first._stem_
     then DO;
        _eff_=0;
        put  _stem_ 5.  +1 '!' +1 @;
     END;
     ELSE
     if (_col>=&cols-11) AND (_leaf_ ^= .) then 
                            put @_form_ '!' / @7 '!' +1 @;
    if (_leaf_ ^= .) then DO;
           %IF &id eq %then %STR(PUT _pleaf_ 1. @;);
                      %else %STR(PUT _pleaf_ @;);
           _eff_=_eff_+1;END;
    if last._stem_ then put @_form_ '!' _eff_ 6.;
   END;
IF _fin_ then put &cols*'-' / 'Total' @7 '!' @_form_ '!' _tot_ 6.;
RUN;

%*******************************************************************;
%*un peu de m‚nage ....                                             ;
%*******************************************************************;

PROC DATASETS  nolist nowarn;
DELETE _code_ _stats_;
QUIT;

%out:
%MEND;
