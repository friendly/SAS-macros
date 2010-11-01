/* Macro to give AIC and AICc summaries of logistic regression models. Written by Yufeng Ding. */

%Macro logisticsummary(data = ,
			class=,
			y_total=,
			y_success=,
			model1=,
			model2=,
			model3=,
                        model4=,
                        model5=,
                        model6=,
                        model7=,
                        model8=,
                        model9 = ,
                        model10=,
                        model11= ,
                        model12= ,
                        model13= ,
                        model14= ,
                        model15= ,
                        model16= ,
                        model17= ,
                        model18= ,
                        model19= ,
                        model20= ,
			link=,
			offset=,
			number_of_models=);

%do i = 1 %to &number_of_models;
   %let model_name&i = &&model&i;
%end;


%if (%length(&offset) EQ 0) %then
%do;
        %do i = 1 %to &number_of_models;
                proc genmod data=&data;
            	class &class;
                model &y_success/&y_total = &&model&i /dist=bin link=&link;
                ods output      Modelfit = logtmp
				ModelInfo = obstmp;
		ods exclude ParameterEstimates Modelfit ModelInfo ClassLevels;

		data loglikhd;
		set logtmp;
			modnum = &i;
                        where Criterion = 'Log Likelihood';
                                loglikhd = Value;
		keep modnum loglikhd;

                proc append base=result1 data = loglikhd force;

                data dev;
                set logtmp;
                        modnum = &i;
                        where Criterion = 'Deviance';
                                df=DF;
                                G2=Value;
		keep modnum df G2;

                proc append base=result2 data = dev force;

                data obstmp1;
                        set obstmp;
                        modnum=&i;
                        where Label1 = 'Observations Used';
                                ncells = nValue1;
                keep modnum ncells;
                proc append base=result3 data = obstmp1 force;

                data obstmp2;
                        set obstmp;
                        modnum=&i;
                        where Label1 = 'Number Of Trials';
                                nobs = nValue1;
                keep modnum nobs;
                proc append base=result4 data = obstmp2 force;

        %end;
%end;

%else
%do;
        %do i = 1 %to &number_of_models;
                proc genmod data=&data;
                class &class;
                model &y_success/&y_total = &&model&i  /dist=bin link=&link offset=&offset;
                ods output      Modelfit = logtmp
				ModelInfo = obstmp;
		ods exclude ParameterEstimates Modelfit ModelInfo ClassLevels;

                data loglikhd;
		set logtmp;
			modnum = &i;
                        where Criterion = 'Log Likelihood';
                                loglikhd = Value;
                keep modnum loglikhd;

                proc append base=result1 data = loglikhd force;

                data dev;
                set logtmp;
                        modnum = &i;
			where Criterion = 'Deviance';
                                df=DF;
                                G2=Value;
		keep modnum df G2;

                proc append base=result2 data = dev force;

                data obstmp1;
                        set obstmp;
                        modnum=&i;
                        where Label1 = 'Observations Used';
                                ncells = nValue1;
                keep modnum ncells;
                proc append base=result3 data = obstmp1 force;

                data obstmp2;
                        set obstmp;
                        modnum=&i;
                        where Label1 = 'Number Of Trials';
                                nobs = nValue1;
                keep modnum nobs;
                proc append base=result4 data = obstmp2 force;

        %end;
%end;

data logisticsum;
	merge result1 result2 result3 result4;
		by modnum;
	numpar=ncells-df;
	%do i=1 %to &number_of_models;
		if modnum =&i then model_name="&&model_name&i";
	%end;

	aic= G2+2*(numpar);
	aicc= G2+2*(numpar)*(nobs/(nobs-numpar-1));

        keep modnum model_name nobs G2 df loglikhd numpar aic aicc;

data logisticsum;
set logisticsum;
	proc sort;
		by aic;
data logisticsum;
set logisticsum;
	retain best;
	if _n_=1 then best=aic;
	daic=aic-best;

data logisticsum;
set logisticsum;
        proc sort;
                by aicc;
data logisticsum;
set logisticsum;
        retain best1;
        if _n_=1 then best1=aicc;
        daicc=aicc-best1;

data logisticsum;
set logisticsum;
        proc sort;
                by modnum;

proc print data=logisticsum noobs;
var modnum model_name nobs G2 df loglikhd numpar aic aicc daic daicc;
run;

proc datasets library = work;
     delete  dev loglikhd logtmp nobstmp obstmp result1 result2 result3 result4;
run;
quit;
%mend logisticsummary;

