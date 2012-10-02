/*
mult.sas - by Hans-Peter Piepho,  JCGS 2004, 13(2), 456-466
Modified by M. Friendly
08 Nov 2004 15:11:38
*/

%macro mult(
	diffs=diffs,
	lsmeans=lsmeans,
	trt=, 
	alpha=0.05,
	p=probt,
	by=., by2=., by3=., 
	level=., level2=., level3=.
	);
                                       /*************** by and level added 7 Feb 2003*****/
/*********************************************************************************************/
/*                                                                                           */
/*  This macro finds a letters display for all pairwise comparisons                          */
/*  using the insert-and-absorb algorithm with sweeping                                      */
/*                                                                                           */ 
/* Requirements                                                                              */
/*                                                                                           */
/*  SAS/IML                                                                                  */
/*                                                                                           */
/*  Dataset diffs with the following variables:                                              */
/*     p = adjusted p-value                                                                  */
/*  Comparisons sorted by i=1 to t-1 and j=i+1 to t, where t is the number of treatments     */
/*  The ODS diffs automatically produces this variable in the required order                 */
/*  If adjusted p-values of the MIXED ODS diffs are to be used, relabeling is necessary,     */
/*  since PROBT is the p-value for an ordinary t-test                                        */
/*                                                                                           */
/* Dataset lsmeans with the following variables:                                             */
/*      trt (treatment label)                                                                */
/*      estimate (mean or other statistic to be compared)                                    */
/*                                                                                           */
/* Upon invocation of the macro, you need to specify the type I error rate using the         */
/* alpha= statement(Default = 5%)                                                            */
/*                                                                                           */
/* by = specifies first  by-variable.      Up to three by variables are allowed              */
/* by2= specifies second by-variable.                                                        */
/* by3= specifies third  by-variable.                                                        */
/*                                                                                           */
/* level = specifies the level of the first  by variable at which to compare lsmeans         */
/* level2= specifies the level of the second by variable at which to compare lsmeans         */
/* level3= specifies the level of the third  by variable at which to compare lsmeans         */
/*                                                                                           */
/* First version 3 March 2002                                                                */
/* 7 February 2003: added by and level options                                               */
/* 26 March 2003 modified by options                                                         */
/* Written by: Hans-Peter Piepho (piepho@uni-hohenheim.de)                                   */
/*********************************************************************************************/

/**************added Feb 7, 2003*****/

data diffs0;
set &diffs;
%if (&by ne .) %then %do;
  if &by=&level;
  if _&by=&level;
%end;
%if (&by2 ne .) %then %do;
  if &by2=&level2;
  if _&by2=&level2;
%end;
%if (&by3 ne .) %then %do;
  if &by3=&level3;
  if _&by3=&level3;
%end;


data lsmeans0;
set &lsmeans;
%if (&by ne .) %then %do;
  if &by=&level;
%end;
%if (&by2 ne .) %then %do;
  if &by2=&level2;
%end;
%if (&by3 ne .) %then %do;
  if &by3=&level3;
%end;

proc sort data=diffs0 out=diffs0;
by &trt _&trt;

proc sort data=lsmeans0 out=lsmeans0;
by &trt;
/**************end of added Feb 7, 2003*****/


proc iml;
use diffs0;
read all var {&p} into p;

use lsmeans0;
read all var {&trt} into label;
read all var {estimate} into est;

t=nrow(label);
count=0;
c=j(1,t,0);
do i=1 to t-1;
  do j= i+1 to t;
    count=count+1;
    if p[count]<&alpha then do;
	  done='no';
	  k=1;
	  do while(done='no');
	    n=nrow(c);
		found='no';
        if c[k,i]=0 then do;
   	      if c[k,j]=0 then do;
		    c1=c[k,];
		    c1[i]=1;
		    c2=c[k,];
		    c2[j]=1;
			c[k,i]=1;
			found='yes';
		  end;
        end;
		if found='yes' then do;
		  /*check if either c1 or c2 is redundant*/
		  m=nrow(c);
		  contain1=0;
		  contain2=0;
		  do w=1 to m;
		    check1=c1-c[w,];
		    min1=min(check1);
			if w=k then min1=-1;
			if min1>-1 then contain1=1; /*c1 is contained*/
		    check2=c2-c[w,];
		    min2=min(check2);
			if min2>-1 then contain2=1; /*c2 is contained*/  
		  end;
		  if contain1=1 then do;
		    free c_neu;
			do w=1 to m;
			  if abs(w-k)>0 then c_neu=c_neu//c[w,];
			end;
			c=c_neu;
		  end;
		  if contain2=0 then c=c//c2;
		  k=0;
		end;
		k=k+1;
		if k>n then done='yes';  
	  end;
	end;
/********************/

  end;
end;

/*clear superfluous letters*/

n=nrow(c);
c=c`; 

L={'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'};

g=j(t,n,'.');
do j=1 to n;
  do i=1 to t;
    if c[i,j]=0 then g[i,j]=L[j];
  end;
end;

do j=1 to n;
  started=1;
  do i=1 to t;
    if c[i,j]=0 then do;
	  covered=1;
	  if started=1 then do;
	    if sum(c[,j])=(t-1) then covered=0; 
	  end;
	  do ii=1 to t;
	    if abs(ii-i)>0 then do;
	      if c[ii,j]=0 then do;
		    cov_ii=0;
		    do jj=1 to n;
			  if abs(j-jj)>0 then do;
		        check=c[i,jj]+c[ii,jj]; if abs(check)<1e-10 then cov_ii=1;
        	  end;
		    end;
            if cov_ii=0 then covered=0;
		  end;
		end;
	  end;
      if covered=1 then do; c[i,j]=1; end;
      started=0;
    end;
  end;
end;

*L='a'//'b'//'c'//'d'//'e'//'f'//'g'//'h'//'i'//'j'//'k'//'l'//'m'//'n'//'o'//'p'//'q'//'r'//'s'//'t'//'u'//'v'//'w'//'x'//'y'//'z';

g=j(t,n,'.');
do j=1 to n;
  do i=1 to t;
    if c[i,j]=0 then g[i,j]=L[j];
  end;
end;
lsmean=est;

by =symget('by');
level=symget('level');
by2 =symget('by2');
level2=symget('level2');
by3 =symget('by3');
level3=symget('level3');

trt=symget('trt');
print trt
	%if (&by ne .) %then %do;
	by level
	%end;
	%if (&by2 ne .) %then %do;
	by2 level2 
	%end;
	%if (&by3 ne .) %then %do;
	by3 level3 
	%end;
	Label lsmean g;

run;

quit;

%mend mult;

