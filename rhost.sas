/*
Macro to allow choice of remote SAS/CONNECT host from 
	command line:
	setenv RHOST phoenix

	Then, place %rhost in your autoexec.sas file, to have this done
	automagically
*/

%macro rhost;

	%global rhost remote phoenix econ;
	
	%let phoenix=phoenix.yorku.ca;
	%let econ=stats.econ.yorku.ca;
	
	%let rhost =%SYSGET(RHOST);
	%if &rhost = %str() %then %let rhost=phoenix;
	%let remote = &rhost;
			
	options comamid=tcp remote=&rhost;
	filename rlink "~/sasuser/scripts/tcpunix.scr";
%mend;
