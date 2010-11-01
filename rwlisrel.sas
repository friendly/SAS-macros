/*
Macro to Read the Weight matrix from LISREL/PRELIS into SAS in a form
it can be used with PROC CALIS, as the INWGT= option

Parameters:
   dsread - name of the SAS data set containing the VAR variable with weights
	var -    name of the variable contianing the weights
	dswrit - name of the output dataset
	cov    - 0==??
	n      - number of original variables
	v      - names of original variables
*/

%MACRO RWLISREL(dsread,var,dswrit,cov,n,v);
 TITLE3 "DATA= &dsread, COVMAT= &cov, VNUMB= &n";
 DATA &dswrit (TYPE=WEIGHT);
 LENGTH _TYPE_ $8 _NAME_ $8 _NAM2_ $8 _NAM3_ $8;
 KEEP _TYPE_ _NAME_ _NAM2_ _NAM3_  &v;
 ARRAY _v_[&n] &v;
 RETAIN RWPEN .001; /* This weight is the reciprocal of WPEN= option */

 DO _i_= 1 TO &n;
   DO _j_= 1 TO _i_;
    DO _k_= 1 TO _i_;

      IF _k_ < _i_ THEN _ll_= _k_; ELSE _ll_= _j_;
      DO _l_= 1 TO _ll_;
        IF &cov = 0 THEN DO;
          _v_[_l_]= 0.;
          IF _i_=_j_ AND _i_=_k_ AND _k_=_l_ THEN DO;
            _v_[_l_]= RWPEN;
            GOTO done;
          END;
          IF _i_=_j_ OR _k_=_l_ THEN GOTO done;
        END;
        SET &dsread;
        _v_[_l_]= &var;   /* was var */
done:
      END;
      DO _l_= _ll_+1 to &n; _v_[_l_]= .; END;

      _TYPE_ = 'WEIGHT';
      CALL VNAME(_v_[_i_],_NAME_);
      CALL VNAME(_v_[_j_],_NAM2_);
      CALL VNAME(_v_[_k_],_NAM3_);
      OUTPUT;
    END;
   END;
 END;
 STOP;
 RUN;
%MEND RWLISREL;

