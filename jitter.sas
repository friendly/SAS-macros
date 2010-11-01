 /*--------------------------------------------------------------*
  *    Name: jitter.sas                                          *
  *   Title: Jitter variables to prevent overplotting            *
        Doc: http://www.datavis.ca/sasmac/jitter.html      
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 15 Sep 2003 08:58:49                                *
  * Revised: 12 May 2006 09:23:06                                *
  * Version: 1.1                                                 *
  *  - Added online documentation                                *
  * 1.1  Added WEIGHT= parameter                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The JITTER macro adds a small amount of noise to numeric variables,
 to avoid overplotting for discrete data.

=Usage:

 The JITTER macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%jitter(var=X1-X5);
 
==Parameters:

* DATA=       Input data set [Default: DATA=_LAST_]

* OUT=        Output data set (can be same as input) [Default: OUT=_DATA_]

* VAR=        Names of variable(s) to be jittered

* NEW=        Jittered result variables(s) (can be same as var) 
              [Default: NEW=&VAR]

* WEIGHT=     Observation frequency variable.  If specified, the value of
              that variable specifies the number of output observations
              produced for each input observation.

* UNIT=       Unit of var, smallest distance between successive 
              values. For multiple input variables, you can specify a 
			  list of (blank separated) numeric values. [Default: UNIT=1]

* MULT=       Multiplier for spread of jitter. [Default: MULT=1]

* SEED=       Seed for the random number generator.  Setting this to a
              non-zero variable gives reproducable results. [Default: SEED=0]

 =*/
 
%macro jitter(
   data=_last_,     /* input data set                          */
   out=_data_,      /* output data set (can be same as input)  */
   var=,            /* variable(s) to be jittered              */
   new=&var,        /* jittered result(s) (can be same as var) */
   weight=,         /* name of a count/freq variable          */
   unit= 1,         /* unit of var, smallest distance between  */
                    /* successive values (default=1)           */
   mult= 1,         /* multiplier for spread of jitter         */
   seed=0);
 
data &out;
	set &data;
	drop _i_ _J_ _u_ _k_ _wt_;
	array old{*} &var;
	array new{*} &new;
	array unit{256} _temporary_ (&unit &unit); 

	%if %length(&weight) %then %do;
		_wt_=&weight;
		%end;
	%else %do;
		_wt_=1;
		%end;
	
	do _k_ = 1 to _wt_;
		do _i_ = 1 to dim(old);
			if unit(_i_) ^= .
				then _u_ = unit(_i_);
				else _u_ = unit(1);
			_J_ = &mult * (.5*(uniform(&seed) - .5));
	  		new(_i_) = old(_i_) + _u_ * _J_;
			end;
		output;
		end;

/*
   _J_ = &mult * (.5*(uniform() - .5));
%* _J_ is distributed U(-.25,.25) * MULT;
   &new = &var + &unit * _J_;
 */
%mend;

