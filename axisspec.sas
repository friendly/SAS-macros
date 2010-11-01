 /* SAS Macro AXISSPEC

    In a DATA step, calculates the  minima  and  maxima  for  a  list  of
    variables, and at the last observation in the dataset, defines global
    macro  variables  spec1,...,specp, where p is the number of variables
    being  processed.   These  macro   variables   define   "nice"   axis
    specifications that are generally more pleasing than those derived by
    PROC  PLOT  or PROC GPLOT.  The specifications are of the form low TO
    high BY inc.  The user may specify the number of  intervals  to  make
    and   may   optionally   specify   the  interval  width  (STEP).   If
    unspecified,  this  width  will  be  computed.   AXISSPEC  uses   the
    algorithm  of  J.  A.  Nelder, Applied Statistics 25:94-7, 1976.  The
    default number of intervals is N=10.

    Usage:

    DATA ...; SET ...  end=e;
    %AXISSPEC(VAR=x y z); *Specify VAR (in quotes if >1) first call only;
    %AXISSPEC;             *Used if x,y,z values change more than once
                            for current observation;
    %AXISSPEC(end=e,(N=),(STEP=)); *Issue once at end. e=1 if end of file;
    PROC PLOT;PLOT y*x/HAXIS=&spec1 VAXIS=&spec2; *For example;

    The %AXISSPEC  command  above  without  end=e  is  only  issued  when
    variable  values change within the current observation.  This happens
    for example when preparing for  overlaying  multiple  curves  on  one
    graph.   Suppose  for  example  that one wishes to plot a value v and
    lower and upper confidence limits cl,cu on the same graph.  One would
    specify commands such as the following:

    y=cl; %AXISSPEC(VAR=y); y=cu; %AXISSPEC(end=e);
    PROC PLOT;PLOT v*x='*' cl*x='.' cu*x='.'/OVERLAY VAXIS=&spec1;

    If variable values do not change  within  an  observation,  only  one
    AXISSPEC statement is needed:  %AXISSPEC(VAR=x y z,end=e);

    Author   : Frank Harrell
               Clinical Biostatistics, Duke University Medical Center
               Takima West Corporation
    Date     : 16 Sep 86
    Modified : 17 Sep 86
                                                                      */
%MACRO AXISSPEC(var=,end=,n=10,step=);
%let var=%SCAN(&var,1,'"''');

%if &var^=  %then %do;
   %LOCAL _nv_ i;
	%let _nv_=0;
     %do i=1 %TO 1000;
     %if %SCAN(&var,&i)=  %then %goto nomorev;
     %let _nv_=%EVAL(&_nv_+1);
     %end;
   %nomorev:

   DROP _rn_ _x_ _fmax_ _fmin_ _step_ _range_ _fact_ _omin_ _omax_
    _j_ _ctf_ _unit_1-_unit_13 _tol_ _bias_ _xmin_ _xmax_
    _k_ _min_1-_min_&_nv_ _max_1-_max_&_nv_;
   RETAIN _unit_1 1 _unit_2 1.2 _unit_3 1.4 _unit_4 1.5 _unit_5 1.6
    _unit_6 2 _unit_7 2.5 _unit_8 3 _unit_9 4 _unit_10 5 _unit_11 6
    _unit_12 8 _unit_13 10 
	 _tol_ 5E-6 _bias_ 1E-4
    _min_1-_min_&_nv_ 1E30 _max_1-_max_&_nv_ -1E30;
   ARRAY _unit_{13} _unit_1-_unit_13;
   ARRAY _var_{*} &var;
   ARRAY _min_{*} _min_1-_min_&_nv_;
	ARRAY _max_{*} _max_1-_max_&_nv_;
   %end;

	do _k_=1 TO DIM(_var_);
		_min_{_k_}=MIN(_var_{_k_},_min_{_k_});
		_max_{_k_}=MAX(_var_{_k_},_max_{_k_});
	end;

%if &end^=  %then %do;
if &end then do _k_=1 TO DIM(_var_);
_RN_=&n;
_FMAX_=_max_{_k_};
_FMIN_=_min_{_k_};
_X_=ABS(_FMAX_);
_OMIN_=_FMIN_;
_OMAX_=_FMAX_;
_FACT_=1;
_CTF_=0;
if _X_=0 then _X_=1;
if (_FMAX_-_FMIN_)/_X_<=_TOL_ then do; %*VALUES EFFECTIVELY EQUAL;
     if _FMAX_<0 then _FMAX_=0;
     else if _FMAX_=0 then _FMAX_=1;
     else _FMIN_=0;
     end;
%if &step^=  %then %do;
	_step_=&step;
	%goto SKIPSTEP;
	%end;
DROP _s_ _i_;

TRYAGAIN:
	_STEP_=(_FMAX_-_FMIN_)/_RN_*_FACT_;
	_S_=_STEP_;
%*The factor 1+1/_rn_ is inserted in the Nelder algorithm to insure
  that the resulting limits include all the data;
LOOP1:
	if _S_>=1 then GO TO LOOP10;
	_S_=_S_*10;
	GO TO LOOP1;
LOOP10:
	if _S_<10 then GO TO CALC;
	_S_=_S_/10;
	GO TO LOOP10;
CALC:_X_=_S_-_BIAS_;
     do _I_=1 TO 13;
      if _X_<=_UNIT_{_I_} then GO TO FOUND_U;
     end;

FOUND_U:
	_step_=_step_*_unit_{_i_}/_s_;
%SKIPSTEP:
	_range_=_step_*_rn_;

	%* MAKE FIRST ESTIMATE OF XMIN;
	_X_=.5*(1+(_FMIN_+_FMAX_-_RANGE_)/_STEP_);
	_J_=INT(_X_-_BIAS_);
	if _X_<0 then _J_=_J_-1;
	_XMIN_=_STEP_*_J_;

	%* TEST if XMIN COULD BE ZERO;
	if _FMIN_>=0 & _RANGE_>=_FMAX_ then _XMIN_=0;
	_XMAX_=_XMIN_+_RANGE_;
	%* TEST if XMAX COULD BE ZERO;
	if _FMAX_<=0 & _RANGE_>=-_FMIN_ then do;
		_XMAX_=0;
		_XMIN_=-_RANGE_;
		end;

	%if &step=  %then %do;
		if _CTF_<4 & ((_XMAX_<_OMAX_)|(_XMIN_>_OMIN_)) then do;
			_CTF_=_CTF_+1;
			_FACT_=_FACT_*(1+1/_RN_);
			GO TO TRYAGAIN;
			end;
		%end;
CALL SYMPUT("spec"||trim(left(_k_)),
 trim(left(_xmin_))||" TO "||trim(left(_xmax_))
 ||" BY "||trim(left(_step_)));
end;
%end;
%MEND;
