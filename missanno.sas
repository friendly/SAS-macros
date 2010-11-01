/*
Create an annotate data set to show missing observations 
in a scatterplot by symbols along the marginal axes
*/
%macro missanno(
    data=_last_,    /* name of input data set                   */
    x=,             /* name of X variable                       */
    y=,             /* name of X variable                       */
    at=1,           /* % location on missing axis               */
    jitter=1,       /* amount to jitter % location              */
    symbol=circle,  /* plot symbol for a missing observation    */
    color=red,      /* symbol color                             */
    size=1,         /* symbol size                              */
    when=b,         /* when to draw annotations                 */
	copy=,          /* variables to copy to output data set     */
	in=,            /* input annotate data set                  */
    out=missxy      /* name of output data set                  */
);
    data &out;
		keep &x &y x y xsys ysys function text size color &copy;
        set &data;
        where (&x=.) or (&y=.);
    	length function color text $8;
        function = 'symbol'; text = "&symbol"; size=&size; when="&when";
        color = "&color";
        if &x = . then do;
            xsys = '1';  ysys = '2';
            x = &at + &jitter * uniform(21342141);
            y = &y;
            output;
            end;
        if &y = . then do;
            xsys = '2';  ysys = '1';
            y = &at + &jitter * uniform(21342141);
            x = &x;
            output;
            end;
	run;

%if %length(&in)>0 %then %do;
    data &out;
        set &in &out;
    %end;
    
%mend;


