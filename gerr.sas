%macro gerr(DEVICE);
%*-------------------------------------------------------------------*
 * Called from GSTART when an invalid graphics device is specified.  *
 * SAS processing is terminated.                                     *
 *-------------------------------------------------------------------*;
 %IF &DEVICE ^= ? %THEN
   %PUT ERROR:  Invalid graphics device specified -- DEVICE=&DEVICE;
 %PUT GSTART-> Valid devices are:;
 %PUT GSTART->   &DEVS;
%MEND GERR;
