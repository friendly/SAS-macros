/*
Title: Levine's test for homogeneity of variance

Levene's test for homogeneity of variances is equivalent to an ANOVA
of the absolute value of the deviations of scores from cell means.
This test is more robust to non-normality than Bartlett's. 
See Milliken and Johnson, "Analysis of Messy Data" (Lifetime Learning Publications).
*/

%macro levine(data=_last_, class=, response=);

proc glm data=&data noprint;
    classes &class;
    model &response = &class;
    output out = _resids_ r = res_y;
data _resids_; 
	set _resids_; 
	z = abs(res_y);

proc glm outstat=out noprint;
	classes &class;
	model z = &class / ss3;
	run;

data _null_;
	set out end=eof;
	retain dfe dfh sse ssh f prob a;
	select (_type_);
		when ('ERROR') do;  dfe=df;  sse=ss;  end;
		when ('SS3')   do;
			dfh=df;  ssh=ss;  a=df+1;
			end;
		end;
		
	if eof then do;
		file print;
		put  @5 'Levine''s test for equality of variances'/
		   @5 '(based on ANOVA of |deviations from cell means|)'//
		@10 "Dataset :" @25 "&data" /
		@10 "Response:" @25 "&response" /
		@10 "Factor:"   @25 "&class" /
		@10 'Levels:'   @25 a //
		
		@10 'Source'  @22 'df'    @30 'SS'    @39 'F'   @50 'Prob >F' //
		@10 "&class"  @20 dfh 3.  @26 ssh 9.4 @37 f 8.3 @48 prob 9.5  /
		@10 'Error'   @20 dfe 3.  @26 sse 9.4 /;
	end;
%mend levine;
