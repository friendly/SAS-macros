 /*-------------------------------------------------------------------*
  * MISTRESS SAS    Missing data imputation for categorical data      *
  *                 by maximizing internal consistency                *
  *                                                                   *
  *-------------------------------------------------------------------*
  *  Author:  Stef van Buuren, TNO Leiden <BUUREN@NIPG.TNO.NL>        *
  * Created:  6  Aug 1992                                             *
  * Revised:  16 Nov 1992                                             *
  * Version:  1.17                                                    *
  *    Code:  SAS/IML 6.07                                            *
  *     See:  1 van Buuren, S. & van Rijckevorsel, J.L.A. (1992).     *
  *             Missing data imputation by maximizing internal        *
  *             consistency. Psychometrika, 57, xxx-xxx.              *
  *           2 van Buuren, S. (1992)                                 *
  *             MISTRESS SAS V1.17 User documentation.                *
  *                                                                   *
  * In this analyis, each entry h[i,j] of a data matrix h falls into  *
  * one of the following classes:                                     *
  * OBSERVED  The value is known and is analyzed as usual.            *
  * MISSING   The value is unknown and will be imputed.               *
  *           Missing values are equal to '.', '.a', or '.b', etc.    *
  *           All missing codes are treated alike.                    *
  * IDLE      The value is unknown and will not be imputed.           *
  *           Idle values are equal to the value of the macro         *
  *           input variable 'idle'. The default is 0.                *
  *                                                                   *
  *-------------------------------------------------------------------*/

%macro mistress(
       data=_LAST_,        /* name of input data set              */
       out=IMPUTED,        /* name of output data set             */
       var=_NUM_,          /* variable list, default: all numeric */
       wgtvar=_NONE_,      /* row weighting variable              */
       ndim=1,             /* dimensionality                      */
       idle = 0,           /* code for idle entries               */
       prt=1,              /* 0=none, 1=normal, 2=detailed        */
       maxit1=100,         /* maximal iterations, initial         */
       crit1=1E-6,         /* convergence criterion, initial      */
       maxit2=100,         /* maximal iterations, final           */
       crit2=1E-7,         /* convergence criterion, final        */
       out1=_NONE_,        /* name of object score data set       */
       out2=_NONE_,        /* name of cat. quant. data set        */
       file=print);        /* file for printed output             */

options validvarname=v6;
proc iml;
 /*-------------------------------------------------------------------*
  * IND(g, gv, cv, h, idle, vars)
  * Converts a data matrix h into an indicator matrix g.
  * Missing and idle values are coded as a row of zeroes.
  * INPUT   h    (nobs,nvar)   categorical data matrix
  *         idle               value of idle entries
  *         vars (1,nvar)      variable names
  * OUTPUT  g    (nobs,skj)    super indicator matrix
  *         gv   (1,skj)       list of columns of h
  *         cv   (1,skj)       list of original numerical values
  * The function returns 1 if error, 0 if OK.
  *-------------------------------------------------------------------*/
start IND (g, gv, cv, h, idle, vars);
free g gv cv;
low = h[><,><] - 1;
h = choose(h =., low, h);            /* temporary recode missing data */
do j = 1 to ncol(h);                 /* since design() does not work  */
    values = unique(h[,j]);          /* if data are missing           */
    bool = (values ^= idle & values ^= low);
    if any(bool) then do;
        gj = design(h[,j]);          /* make matrix                   */
        gj = gj[,loc(bool)];         /* delete missing and idle cols. */
        g  = g  || gj;
        gv = gv || j(1, ncol(gj), j);
        cv = cv || values[,loc(bool)];
        if ncol(gj) > 25 then
        put 'Warning: Variable ' (vars[,j]) ' has '
            (ncol(gj)) 'categories.';
    end;
    else do;                         /* Ai, no categories */
        put 'Error: Variable ' (vars[,j]) ' contains no valid data.';
        return(1);
    end;
end;
h = choose(h = low, ., h);           /* restore missing values        */
return(0);
finish IND;


 /*-------------------------------------------------------------------*
  * DEIND(hnew, g, gv, h, idle)
  * Converts an indicator matrix g into a data matrix hnew.
  * INPUT   g    (nobs,skj)    super indicator matrix
  *         gv   (1,skj)       identifies h-columns
  *         h                  original data
  *         idle               value of idle entries
  * OUTPUT  hnew (nobs,nvar)   imputed categorical data
  *-------------------------------------------------------------------*/
start DEIND(hnew, g, gv, h, idle);
free hnew;
do j = 1 to ncol(h);
    bool1 = (gv = j);
    if any(bool1) then do;
        gj = g[,loc(bool1)];
        values = unique(h[,j]);
        bool2 = (values ^= idle & values ^= .);
        if any(bool2) then values = values[,loc(bool2)];
    end;
    else do;                                /* categories not found   */
        gj = j(nobs, 1, 0);
        values = 0;
    end;
    hnew = hnew || gj * t(values);
end;
hnew = choose(h = idle, idle, hnew);        /* restore idle entries   */
finish DEIND;


 /*-------------------------------------------------------------------*
  * CHAR2NUM(hj, lab, cj, idle)
  * Converts the character variable cj into the numerical variable hj.
  * String "idle" converts to the numerical idle code.
  * INPUT   cj   (nobs,1)      character variable
  *         idle               idle code
  * OUTPUT  hj   (nobs,1)      numerical variable
  *         lab  (1,kj)        list of original values
  *-------------------------------------------------------------------*/
start CHAR2NUM(hj, lab, cj, idle);
flag = 0;
hj  = j(nrow(cj),1,.);
lab = unique(cj);
bool = (lab ^= ' ' & lab ^= "idle");
if any(bool) then lab = lab[,loc(bool)];
bool = (cj = "idle");
if any(bool) then hj[loc(bool),] = idle;
do k = 1 to ncol(lab);
    if k >= idle then flag=1;
    hj = choose(cj=lab[,k], (k+flag), hj);
end;
finish CHAR2NUM;


 /*-------------------------------------------------------------------*
  * NUM2CHAR(cj, lab, hj, idle)
  * Converts the numerical variable hj into the character variable cj.
  * Character labels are given is lab.
  * The numerical idle code converts to the string "idle".
  * INPUT   lab  (1,kj)        list of original values
  *         hj   (nobs,1)      numerical variable
  *         idle               idle code
  * OUTPUT  cj   (nobs,1)      character variable
  *-------------------------------------------------------------------*/
start NUM2CHAR(cj, lab, hj, idle);
flag = 0;
spaces = cshape(' ', 1, 1, nleng(lab));
cj = j(nrow(hj), 1, spaces);
bool = (hj = idle);
if any(bool) then cj[loc(bool),1] = "idle";
do k = 1 to ncol(lab);
    if k >= idle then flag=1;
    cj[loc(hj=k+flag),] = lab[,k];
end;
finish NUM2CHAR;


 /*-------------------------------------------------------------------*
  * WGRAM(x, g, nvar, nobs, ndim)
  * Weighted modified Gram-Schmidt orthogonalization.
  * On exit, X satisfies X'MX=I, where M is a diag(GG').
  * See Gifi (1990), Nonlinear Multivariate Analysis, pp. 169-171.
  * INPUT   x   (nobs,ndim)    array to be orthogonalized
  *         g   (nobs,skj)     indicator
  *         nvar               number of variables
  *         nobs               number of observations
  *         ndim               number of dimensions
  * OUTPUT  x   (nobs,ndim)    orthogonalized x
  *-------------------------------------------------------------------*/
start WGRAM (x, g, nvar, nobs, ndim);
mstar = repeat(g[,+], 1, ndim);
pstar = sqrt(j(nobs, ndim, nvar) / mstar);
xstar = x - mstar # (x[+,] / sum(g));
call gsorth(xstar, tt, lindep, xstar # pstar);
if lindep = 1 then do;
    put 'Error: Linear dependency in routine WGRAM';
    return(1);
end;
x = xstar # pstar;
return(0);
finish WGRAM;


 /*-------------------------------------------------------------------*
  * MISTLOSS(x, g, gv, y, nvar, nobs)
  * Computes the MISTRESS loss.
  * INPUT   x    (n,ndim)      object scores
  *         g    (n,skj)       super indicator matrix
  *         gv   (1,skj)       identifies h-columns
  *         y    (skj,ndim)    category quantifications
  *         nvar               number of variables
  *         nobs               number of observations
  * Function returns the current loss value.
  *-------------------------------------------------------------------*/
start MISTLOSS (x, g, gv, y, nvar, nobs);
ls = 0;
do j = 1 to nvar;
    id = loc(gv=j);
    gj = g[,id];
    ls = ls + sum(((x - gj * y[id,]) ## 2) # gj[,+]);
end;
ls =  ls / (nvar # nobs);
return (ls);
finish MISTLOSS;


 /*-------------------------------------------------------------------*
  * DEVMN(x)
  * Returns matrix X in deviations from its column means.
  * INPUT   x    (n,m)         input matrix
  * OUTPUT  x    (n,m)         output matrix
  *-------------------------------------------------------------------*/
start DEVMN(x);
x = x - repeat(x[:,], nrow(x), 1);
finish DEVMN;


 /*-------------------------------------------------------------------*
  * FINDY(y, g, d, x)
  * Computes Y=INV(D)G'X.
  * INPUT   x    (nobs,ndim)   object scores
  *         g    (nobs, skj)   indicator matrix
  *         d    (1,skj)       marginal frequencies
  * OUTPUT  y    (skj,ndim)    category quantifications
  *-------------------------------------------------------------------*/
start FINDY(y, g, d, x);
y = t((t(x) * g) / repeat(d, ncol(x), 1));
finish FINDY;


 /*-------------------------------------------------------------------*
  * CHECK1(h, idle)
  * Checks if each row contains at least one observation, and
  * deletes empty rows from h.
  * Works also for columns.
  * INPUT   idle               idle code
  * IN/OUT  h    (nobs,nvar)   data
  * The function returns 1 if error, 0 if OK.
  *-------------------------------------------------------------------*/
start CHECK1(h, idle);
nvar = ncol(h);
nobs = nrow(h);
bool = ((h <= .z | h = idle)[,+] < nvar);  /* find non-empty rows     */
if any(bool) then h = h[loc(bool),];
else do;                                   /* if all is unobserved    */
    put 'Error: Data matrix contains no observations.';
    return (1);
end;
d = nobs - nrow(h);
if d>0 then put 'Warning: ' d ' empty row(s) deleted.';
bool = ((h <= .z | h = idle)[+,] < nobs);  /* find non-empty columns  */
if any(bool) then h = h[,loc(bool)];
d = nvar - ncol(h);
if d>0 then put 'Warning: ' d ' empty column(s) deleted.';
return(0);
finish CHECK1;


 /*-------------------------------------------------------------------*
  * CHECK2(gv, d, nvar, ndim, h, idle, vars)
  * Checks the input data for empty and solitary categories.
  * Checks the range of ndim.
  * Checks if each row contains at least one observation.
  * INPUT   gv   (1,skj)       variable id
  *         d    (1,skj)       marginal frequencies
  *         nvar               number of variables
  *         ndim               number of dimensions
  *         h    (nobs,nvar)   data
  *         idle               idle code
  *         vars               variable names
  * The function returns 1 if error, 0 if OK.
  *-------------------------------------------------------------------*/
start CHECK2(gv, d, nvar, ndim,  h, idle, vars);
do j = 1 to nvar;
    bool = (gv = j);
    if any(bool) then dj = d[,loc(bool)];
    else do;
        put 'Error: Variable ' (vars[,j]) ' has no categories.'
        return(1);
    end;
    nid = sum(idle=h[,j]);
    if (ncol(dj)<1) || (ncol(dj)=1 & nid=0) then do;
        put 'Error: Too few categories in variable ' (vars[,j]);
        return(1);
    end;
    if ^all(dj >= 1) then do;
        put 'Error: Empty categories in variable ' (vars[,j]);
        return(1);
    end;
end;
if ndim>ncol(gv)-nvar then do;
    ndim = ncol(gv)-nvar;
    put 'Warning: Number of dimensions set to ' ndim;
end;
else if ndim<1 then do;
    ndim = 1;
    put 'Warning: Number of dimensions set to ' ndim;
end;
return(0);
finish CHECK2;


 /*-------------------------------------------------------------------*
  * RELMAT(nrel, y, g, gv, x, d, h)
  * Relocates imputations such that the sum of the ndim largest
  * eigenvalues of the correlation matrix of the optimally
  * scaled variables becomes maximal.
  * INPUT   y    (skj,ndim)    category quantifications
  *         g    (nobs, skj)   super-indicator matrix
  *         gv   (1,skj)       identifies h-columns
  *         x    (nobs,ndim)   object scores
  *         d    (1,skj)       marginal frequencies
  *         h    (nobs,nvar)   categorical data matrix
  * OUTPUT  nrel               number of relocations
  *         y    (skj,ndim)    category quantifications, updated
  *         g    (nobs, skj)   super-indicator matrix
  *         d    (1,skj)       marginal frequencies
  *-------------------------------------------------------------------*/
start RELMAT(nrel, y, g, gv, x, d, h);
nrel = 0;
do j = 1 to ncol(h);                    /* treat each var. separately */
    id = loc(gv = j);
    yj = y[id,];                                        /* initialize */
    gj = g[,id];
    dj = d[,id];
    idx = (h[,j] = .);
    if any(idx) then do;
        mj = loc(idx);                        /* index missing values */
        kj = nrow(yj);
        count = 0;
        do until (noc | count > 10);        /* until no changes occur */
            noc = 1;
            count = count + 1;                        /* safety count */
            do i = 1 to ncol(mj);       /* process i'th missing entry */
                ii = mj[i];
                cc = loc(gj[ii,]);            /* cc: current category */
                if (dj[cc] > 1) then do;
                    fact = dj / (dj + 1);
                    fact[cc] = dj[cc] / (dj[cc] - 1);
                    xi = repeat(x[ii,], kj, 1);
                    nc = ((xi - yj)[,##] # t(fact))[>:<,];
                    if (nc ^= cc) then do;      /* nc better than cc? */
                        nrel = nrel + 1;
                        noc = 0;
                        gj[ii,] = (1:kj = nc);            /* relocate */
                        dj[cc] = dj[cc]-1;                  /* update */
                        dj[nc] = dj[nc]+1;
                        run findy (yj, gj, dj, x);
                    end;
                end;
            end;
        end;
        if count > 5 then do;
            put 'Error: Iteration limit exceeded in RELMAT';
            return(1);
        end;
    end;
    run findy (yj, gj, dj, x);
    y[id,] = yj;                                             /* store */
    g[,id] = gj;
    d[,id] = dj;
end;
return(0);
finish RELMAT;


 /*-------------------------------------------------------------------*
  * INIMAT(y, g, gv, x, d, h)
  * Initializes missing values for discrete data by choosing the
  * 'closest' category.
  * INPUT   y    (skj,ndim)    category quantifications
  *         g    (nobs, skj)   super-indicator matrix
  *         gv   (1,skj)       identifies h-columns
  *         x    (nobs,ndim)   object scores
  *         d    (1,skj)       marginal frequencies
  *         h    (nobs,nvar)   categorical data matrix
  * OUTPUT  y    (skj,ndim)    category quantifications, updated
  *         g    (nobs, skj)   super-indicator matrix, imputed
  *         d    (1,skj)       marginal frequencies, updated
  *-------------------------------------------------------------------*/
start INIMAT(y, g, gv, x, d, h);
do j = 1 to ncol(h);                    /* treat each var. separately */
    idx = (h[,j] = .);
    if any(idx) then do;
        mj = loc(idx);                        /* index missing values */
        id = loc(gv = j);
        yj = y[id,];                                    /* initialize */
        gj = g[,id];
        kj = nrow(yj);
        do i = 1 to ncol(mj);           /* process i'th missing entry */
            ii = mj[i];
            xi = repeat(x[ii,], kj, 1);
            nc = ((xi - yj)[,##])[>:<,];              /* nc: new cat. */
            gj[ii,] = (1:kj = nc);       /* the actual initialization */
        end;
        g[,id] = gj;
    end;
end;
d = g[+,];
run findy (y, g, d, x);
finish INIMAT;


 /*-------------------------------------------------------------------*
  * CHISQ(warn, chi2, df, p, exp, dj1, obs, nmi)
  * Computes the one-sample chi-square value under the hypothesis
  * that imputations spread proportionally to the observed counts.
  * INPUT   dj1  (1,kj)        observed counts before imp.
  *         obs  (1,kj)        observed counts after  imp.
  *         nmi                number of missing values
  * OUTPUT  warn               =1 if expected counts are too low
  *         chi2               value of chi-square statistic
  *         df                 degrees of freedom
  *         p                  p-value
  *         exp  (1,kj)        expected counts after imp.
  *-------------------------------------------------------------------*/
start CHISQ(warn, chi2, df, p, exp, dj1, obs, nmi);
    nob = dj1[,+];
    exp = dj1 + (dj1 / nob) # nmi;
    chisq = sum((obs-exp)##2 / exp);
    df = ncol(obs) - 1;
    if df >= 1 then p = 1 - probchi(chisq, df);
    else p = 1;
    warn = 0;             /* warning rules of Siegel/Castellan, p. 49 */
    if df <= 1 & any(exp<5) then warn = 1;
    if df > 1 & any(exp<1) then warn = 1;
    perc = sum(exp<5)/ncol(exp);
    if df > 1 & perc > .2 then warn = 1;
finish CHISQ;


 /*-------------------------------------------------------------------*
  * PRINFO(gv, cv, d1, d2, h, idle, nobs, nvar, vars)
  * Prints a useful category table.
  * INPUT   gv   (1,skj)       links g to columns of h
  *         cv   (1,skj)       original data values for each cat.
  *         d1   (1,skj)       marginal frequencies before imputation
  *         d2   (1,skj)       marginal frequencies after  imputation
  *         h1   (nobs,nvar)   data before imputation
  *         h2   (nobs,nvar)   data after  imputation
  *         idle               code for idle  values
  *         nvar               number of variables
  *         nobs               number of observations
  *         vars (1, nvar)     variable names
  *-------------------------------------------------------------------*/
start PRINFO(gv, cv, d1, d2, h1, h2, idle, nobs, nvar, vars);
warn=0; chi2=0; df=0; p=0; exp=0;
put /'Imputation statistics...'/
/'Variable   Cat    Value        Counts               Percentages'
/'                        Before Expect After    Before Expect After   p';
do j = 1 to nvar;
    id   = loc(gv = j);
    cvj  = cv[,id];
    dj1  = d1[,id];
    dj2  = d2[,id];
    nmi1 = sum(nmiss(h1[,j]));                /* missing counts for j */
    nmi2 = sum(nmiss(h2[,j]));
    pmi1 = 100 # nmi1 / nobs;                 /* missing percentage   */
    pmi2 = 100 # nmi2 / nobs;
    nid1 = sum(idle=h1[,j]);                  /* idle counts for j    */
    nid2 = sum(idle=h2[,j]);
    pid1 = 100 # nid1 / nobs;                 /* idle percentage      */
    pex  = pid1;
    pid2 = 100 # nid2 / nobs;
    zero = 0;
    run chisq(warn, chi2, df, p, exp, dj1, dj2, nmi1);
    put /(vars[,j]) $8. '  miss      .  '  nmi1 6. zero 5. +3 nmi2 6.
        +4 pmi1 6.    +6  pmi2 6.;
    put             +8  '  idle' idle 9.2  nid1 6. nid1 5. +3 nid2 6.
        +4 pid1 6. pex 6. pid2 6.;
    do k = 1 to ncol(id);
        djk1 = dj1[,k];
        djk2 = dj2[,k];
        cvjk = cvj[,k];
        pmi1 = 100 # djk1 / nobs;
        pmi2 = 100 # djk2 / nobs;
        ex   = exp[,k];
        pex  = 100 # ex / nobs;
        if k=1 then
        put         +8 k 6. cvjk 9.2 djk1 6. ex 8.2 djk2 6.
        +4 pmi1 6. pex 6. pmi2 6. p 8.4;
        else
        put         +8 k 6. cvjk 9.2 djk1 6. ex 8.2 djk2 6.
        +4 pmi1 6. pex 6. pmi2 6.;
    end;
    if warn then
        put 'Warning: Expected counts too low for chi-square test';
end;
nmi1 = sum(nmiss(h1));
nmi2 = sum(nmiss(h2));
pmi1 = (100 # nmi1) / (nobs # nvar);
pmi2 = (100 # nmi2) / (nobs # nvar);
nid1 = sum(idle=h1);
nid2 = sum(idle=h2);
pid1 = (100 # nid1) / (nobs # nvar);
pex  = pid1;
pid2 = (100 # nid2) / (nobs # nvar);
put /' total    miss      .  ' nmi1 6. zero 5. +3 nmi2 6.
     +4 pmi1 6. zero 6. pmi2 6.;
put  ' total    idle' idle 9.2  nid1 6. +8 nid2 6.
     +4 pid1 6. pex 6.  pid2 6.//;
finish PRINFO;


 /*-------------------------------------------------------------------*
  * MISTRESS(hnew, x, y, d, gv, cv, eva, eta2, h, ndim, idle
  *         crit1, maxit1, crit2, maxit2, prt, vars)
  * The main MISTRESS iteration loop. Phase I ('missing data
  * passive') initializes x and y. Phase II is the relocation
  * MISTRESS algorithm.
  * INPUT   h    (nobs,nvar)   incomplete categorical data matrix
  *         ndim               number of dimensions
  *         crit1              convergence criterion, phase 1
  *         maxit1             maximum no. of iterations, phase 1
  *         crit2              idem, phase 2
  *         maxit2
  *         prt                printing flag (0, 1, 2)
  *         vars (1, nvar)     character: variable names
  * OUTPUT  hnew (nobs,nvar)   completed categorical data matrix
  *         x    (nobs,ndim)   object scores
  *         y    (skj, ndim)   quantifications
  *         d    (1,skj)       marginal frequencies after imputation
  *         gv   (1,skj)       variable indicator
  *         cv   (1,skj)       original data values per cat.
  *         eva  (ndim,1)      eigenvalues per dimension
  *         eta2 (ndim,1)      consistency per dimension (=eva/nvar)
  *-------------------------------------------------------------------*/
start MISTRESS(hnew, x, y, d, gv, cv, eva, eta2, h, ndim, idle,
              crit1, maxit1, crit2, maxit2, prt, vars);
if prt >= 1 then put /'MISTRESS V1.17 (c) NIPG-TNO, Leiden';
if check1(h, idle) then return(1);
ndim = floor(ndim);
nobs = nrow(h);
nosq = sqrt(nobs);
nvar = ncol(h);
nrel = 0; g = 0;
ptxt = {'Initial' 'Final  '};
if ind(g, gv, cv, h, idle, vars) then return(1);    /* make indicator */
d = g[+,];
d1 = d;
if check2(gv, d, nvar, ndim, h, idle, vars) then return(1);
/* start computations */
x = normal(j(nobs, ndim, 0));                         /* initialize x */
run devmn(x);
if wgram(x, g, nvar, nobs, ndim) then return(1);
x = x # nosq;
run findy(y, g, d, x);                                /* initialize y */
loss = mistloss(x, g, gv, y, nvar, nobs);

if prt>=2 then
    put /'The iteration history...'/
        ' Phase   Iter Reloc    Loss         Fit';
do phase = 1 to 2;                        /* initial and final phases */
    it = 0;
    oldloss = ndim;
    if phase = 1 then do;
        crit = crit1;
        maxit = maxit1;
    end; else do;
        crit = crit2;
        maxit = maxit2;
        run inimat(y, g, gv, x, d, h);
    end;
                                               /* main iteration loop */
    do while (abs(loss - oldloss) > crit & it < maxit);
        oldloss = loss;
        it = it + 1;
        x = g * y;
        run devmn(x);
        if wgram(x, g, nvar, nobs, ndim) then return(1);
        x = x # nosq;
        if phase = 1 then run findy(y, g, d, x);
        else if relmat(nrel, y, g, gv, x, d, h) then return(1);
        loss = mistloss(x, g, gv, y, nvar, nobs);
        fit = ndim - loss;
        if prt >= 2 then do;
            tx = ptxt[1,phase];
            put tx $8. it 4. nrel 4. loss 14.8 fit 14.8;
        end;
    end;
end;

ydy = t(y) * diag(d) * y;
call eigen(eva, evc, ydy);
x = x * evc;
run findy(y, g, d, x);
eva  = eva/nobs;
eta2 = eva/nvar;
run deind(hnew, g, gv, h, idle);
free g;
if prt >= 1 then do;
    run prinfo(gv, cv, d1, d, h, hnew, idle, nobs, nvar, vars);
    p = t(eta2||eva);
    cn = 'Dim1':concat('Dim',char(ndim));
    rn = {'Consistency','Eigenvalue'};
    mattrib p colname=cn rowname=rn format=6.2;
    print p;
end;
return(0);
finish MISTRESS;


 /*-------------------------------------------------------------------*
  * READMAT(h, lab, labx, vars, opt, idle)
  * Reads the data matrix. Recodes character into numerical.
  * INPUT   opt    (1,nvar)    variable names, or keyword
  *         idle               code for idle values
  * OUTPUT  h      (nobs,nvar) data matrix
  *         lab    (1,skj)     original data values for char vars
  *         labx   (1,skj)     variable indicator
  *         vars   (1,nvar)    colname names
  *-------------------------------------------------------------------*/
start READMAT(h, lab, labx, vars, opt, idle);
free h lab labx vars;
if opt="_NUM_" then do;
    read all var _NUM_ into h[colname=vars];
end;
else if opt="_CHAR_" then do;
    read all var _CHAR_ into c[colname=vars];
    do j = 1 to ncol(c);
        run char2num(hj, labj, c[,j], idle);
        h = h || hj;
        lab = lab || labj;
        labx = labx || j(1, ncol(labj), j);
    end;
end;
else do;
    allvars = t(contents());
    if opt="_ALL_" then invars = allvars;
    else do;
        command = concat("invars = ", opt, ";");
        call execute(command);
    end;
    do j = 1 to ncol(invars);
        thename = invars[,j];
        if any(thename=allvars) then do;            /* variable found */
            read all var thename into hj;
            r = type(hj);
            if r="C" then do;
                hc = hj;
                run char2num(hj, labj, hc, idle);
                lab = lab || labj;
                labx = labx || j(1, ncol(labj), j);
            end;
            h = h || hj;
            vars = vars || thename;
        end;
        else put 'Warning: Variable ' thename ' not found.';
    end;
end;
if ncol(lab)=0 then lab=' ';
if ncol(labx)=0 then labx=0;
if ncol(h)=0 then do;
    h = 0;
    vars = ' ';
end;
mattrib h colname=vars;
finish READMAT;


 /*-------------------------------------------------------------------*
  * WRITEMAT(mat, lab, labx, vars, idle, filnam)
  * Saves the matrix mat to a SAS data-set.
  * Recodes numerical into character.
  * INPUT   mat    (nobs,nvar) data matrix
  *         lab    (1,skj)     original data values for char vars
  *         labx   (1,skj)     variable indicator
  *         vars   (1,nvar)    variable names
  *         idle               code for idle values
  *         filnam             name of output dataset
  *-------------------------------------------------------------------*/
start WRITEMAT(mat, lab, labx, vars, idle, filnam);
names = rowcat(vars+' ');
if labx = 0 then do;                      /* only numerical variables */
    command = concat("create ",filnam," var {", names, "};");
    call execute(command);
    append from mat;
end;
else do;                             /* numerical-character variables */
    cj = 0;
    do j = 1 to ncol(mat);           /* split matrix into variables   */
        varname = vars[,j];
        idx = (labx = j);
        if idx = 0 then cj = mat[,j];
        else run NUM2CHAR(cj, lab[,loc(idx)], mat[,j], idle);
        command = concat(varname, " = cj;");
        call execute(command);
    end;                             /* and save them                 */
    command = concat("create ",filnam," var {", names, "};");
    call execute(command);
    append;
end;
command = concat("close ", filnam, ";");
call execute(command);
finish WRITEMAT;


 /*-------------------------------------------------------------------*
  * WGTROWS(h, vars, lab, labx, wgtvar)
  * Weights rows by variable wgtvar by matrix expansion.
  * Wgtvar is in h and contains only non-negative integers.
  * The weighting is deleted from h, vars, lab and labx.
  * INPUT   wgtvar             name of weighting variable
  * IN/OUT  h      (nobs,nvar) data matrix
  *         vars   (1,nvar)    variable names
  *         lab    (1,skj)     original data values per cat.
  *         labx   (1,skj)
  *-------------------------------------------------------------------*/
start WGTROWS(h, vars, lab, labx, wgtvar);
if wgtvar="_NONE_" then return(0);
bool = (wgtvar = vars);
if ^any(bool) then do;
    put 'Error: Weighting variable ' wgtvar ' not found.';
    return(1);
end;
j = (loc(bool))[1,1];
w = h[,j];
nv = ncol(h);
h = h[,loc(j^=(1:nv))];
vars = vars[,loc(j^=(1:nv))];
if any(labx=j) then do;
    put 'Error: Character variables cannot be weighting variables.';
    return(1);
end;
bool = (labx^=j);
if any(bool) then do;
    lab  = lab[,loc(bool)];
    labx = labx[,loc(bool)];
end;
bool =  (labx>j);
if any(bool) then labx[,loc(bool)] = labx[,loc(bool)] - 1;
if w=1 then return(0);
h1 = h;
free h;
n = nrow(h1);
do i = 1 to n;
    wgt = floor(w[i,]);
    if wgt>0 then h = h // repeat(h1[i,], wgt, 1);
end;
nr = compress(char(nrow(h)));
nc = compress(char(ncol(h)));
put 'Weighted data contains ' nr ' rows and ' nc ' columns.';
if nrow(h)=0 then h = 0;
return(0);
finish WGTROWS;


/* MAIN */
start DO_IT;
    reset noname nocenter noprint;
    lf = upcase("&file");
    if lf = 'LOG' then do;
        reset log;
        file LOG;
    end;
    else if lf = 'PRINT' then file PRINT;
    else file "&file";
    lab=0; labx=0; vars=0;
    hnew=0; h=0; x=0; y=0; d=0; gv=0; cv=0; eva=0; eta2=0;
    use &data;
    opt = upcase("&var");
    run readmat(h, lab, labx, vars, opt, &idle);
    opt = upcase("&wgtvar");
    err = wgtrows(h, vars, lab, labx, opt);
    if err then do;
        put 'Execution halted, no data analyzed.';
        return;
    end;

    err = mistress(hnew, x, y, d, gv, cv, eva, eta2, h, &ndim, &idle,
                &crit1, &maxit1, &crit2, &maxit2, &prt, vars);
    if err then do;
        put 'Execution halted, no data saved.';
        return;
    end;

    /* Save the solution */
    ofile = upcase("&out");
    if ofile^="_NONE_" then do;
        run writemat(hnew, lab, labx, vars, &idle, ofile);
    end;
    ofile = upcase("&out1");
    if ofile^="_NONE_" then do;
        varnames = 'x1':concat('x',char(&ndim));
        create &out1 from x[colname=varnames];
        append from x;
        close &out1;
    end;
    ofile = upcase("&out2");
    if ofile^="_NONE_" then do;
        varnames = 'y1':concat('y',char(&ndim));
        varnames = {var cat freq} || varnames;
        mat = t(gv//cv//d//t(y));
        create &out2 from mat[colname=varnames];
        append from mat;
        close &out2;
    end;
    put 'MISTRESS finished OK';
finish DO_IT;

run DO_IT;
quit;    /* quit IML */
%mend mistress;
