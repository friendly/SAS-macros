 /****************************************************************/
 /*          S A S   S A M P L E   L I B R A R Y                 */
 /*                                                              */
 /*    NAME: SYSLPUT                                             */
 /*   TITLE: Macro to perform opposite function of SYSRPUT       */
 /* PRODUCT: SAS/CONNECT                                         */
 /*  SYSTEM: ALL                                                 */
 /*    KEYS: CONNECT MACRO                                       */
 /*   PROCS:                                                     */
 /*    DATA:                                                     */
 /*                                                              */
 /* SUPPORT: ECL                         UPDATE: 01APR94         */
 /*     REF:                                                     */
 /*    MISC: 1. SYSLPUT allows the user to create a macro        */
 /*               variable on the remote host from local session */
 /*               input.                                         */
 /*          3. Copy this file from the sample diskette and      */
 /*             SUBMIT to your SAS session.                      */
 /****************************************************************/
%macro syslput(mv,val,remote=);

 /****************************************************************/
 /*  SYSLPUT is the opposite of SYSRPUT.  SYSLPUT creates a macro*/
 /*   variable in the remote environment.  The user must specify */
 /*   the macro variable and its value.  Optionally, the user    */
 /*   may specify the remote session id; the default session is  */
 /*   the current session.                                       */
 /****************************************************************/

   %global mvar value thost;

   %let mvar=&mv;
   %let value=&val;
   %let thost=&remote;

   options nosource;
   options nonotes;
   %let str=%str(rsubmit &thost;
      options nosource;
      options nonotes;
      data _null_;
         call symput("&mvar","&value");
      run;
      options notes source;
   endrsubmit;
   options notes source;);
   &str;
%mend syslput;

 /*----------------------------------------------------------------*
    EXAMPLES:

 (1) Macro variable to current (default) remote session:

     %syslput(rc,&sysinfo)

 (2) Macro variable to specified remote session:

     %syslput(flag,1,remote=mvs)

  *----------------------------------------------------------------*/
