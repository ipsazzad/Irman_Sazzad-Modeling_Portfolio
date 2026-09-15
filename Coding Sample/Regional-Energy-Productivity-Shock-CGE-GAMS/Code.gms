$title Wyoming with trade 

set in institutions /AGR-A, ENG-A, REC-A, MISC-A, AGR-C, ENG-C, REC-C, MISC-C, LAB, CAP, HHDS, TRD/;
alias (in,ini)
set i(in) industry /AGR-A, ENG-A, REC-A, MISC-A/;
alias(i,ii)
set c(in) commodity /AGR-C, ENG-C, REC-C, MISC-C/;
set f(in) factor /LAB, CAP/;

set tt  /0*9/;

TABLE SAM(in,in) imported sam
$ONDELIM
$INCLUDE D:\Study\Fall 23\Computational 5530\GAMS\Codes\Code 4/SAM-2-FA23.csv
$OFFDELIM

Parameter shk(i)
/
AGR-A     0
ENG-A     0.01
REC-A     0
MISC-A    0
/;


Parameter  shkk(i,tt)  shock range;

shkk(i,tt)= shk(i)*(ord(tt)-1);

Display
shkk;

Parameters
LABb(i)
CAPb(i),
QDb(i,c),
FACpmtb(f)
HHXb(c)
HHXbb(i)
noncompbb
HHXbba
HHXbbe
HHXbbr
HHXbbm
HHXbb2(i)
IMba,
IMbe,
IMbr,
IMbm,
EXba,
EXbe,
EXbr,
EXbm
IMbb(i),
EXbb(i)
QDbb(i)
Qbb(i);

Labb(i) = SAM('LAB',i);
CAPb(i) = SAM('CAP',i);
QDb(i,c) = SAM(i,c);
FACpmtb(f) = SAM('HHDS',f);
HHXb(c) = sum(in, SAM(c,in));
HHXbb(i) = sum (c,QDb(i,C));
HHXbba =SAM('AGR-C','Hhds');
HHXbbe =SAM('ENG-C','Hhds');
HHXbbr =SAM('REC-C','Hhds');

HHXbbm =SAM('MISC-C','Hhds');

noncompbb=SAM('TRD','Hhds');

HHXbb2('AGR-A')=HHXbba;
HHXbb2('ENG-A')=HHXbbe;
HHXbb2('REC-A')=HHXbbr;
HHXbb2('MISC-A')=HHXbbm;

IMba=SAM('TRD','AGR-C' );
IMbe=SAM('TRD','ENG-C' );
IMbr=SAM('TRD','REC-C' );
IMbm=SAM('TRD','MISC-C');
EXba=SAM('AGR-C','TRD');
EXbe=SAM('ENG-C','TRD');
EXbr=SAM('REC-C','TRD');
EXbm=SAM('MISC-C','TRD');

IMbb('AGR-A') =IMba;
IMbb('ENG-A') =IMbe;
IMbb('REC-A') =IMbr;
IMbb('MISC-A') =IMbm;
EXbb('AGR-A') =EXba;
EXbb('ENG-A') =EXbe;
EXbb('REC-A') =EXbr;
EXbb('MISC-A') =EXbm;

QDbb(i)=sum (c,QDb(i,c));
Qbb(i)=QDbb(i)+ IMbb(i);



Display
LABb
CAPb
QDb
FACpmtb
HHXb
HHXbb,
SAM,
noncompbb
HHXbba
HHXbbe
HHXbbr
HHXbbm
HHXbb2,
IMba
IMbe
IMbr
IMbm
EXba
EXbe
EXbr
EXbm
IMbb
EXbb
QDbb
Qbb
;

Parameter sigmap(i)
/
AGR-A    .8
ENG-A    .8
REC-A    .8
MISC-A   .8
/;

Parameter sigmad(i)
/
AGR-A    3.15
ENG-A    3.15
REC-A    3.15
MISC-A   3.15
/;

Parameter sigmas(i)
/
AGR-A    2.9
ENG-A    2.9
REC-A    2.9
MISC-A   2.9
/;


Parameter sigmah /.8/;

Parameter wb /1/;
Parameter rb /1/;


Parameter PXb(i)
/
AGR-A    1
ENG-A    1
REC-A    1
MISC-A   1
/;



Parameters
rhop(i)
rhoh
beta(i)
theta(i)
incb
alpha(i)
QDb_chk(i)
LABb_chk(i)
CAPb_chk(i)
HHX_chk(i)
rhod(i)
betad(i)
thetad(i)
rhos(i)
betas(i)
thetas(i)
Qb_chk(i)
Xb_chk(i);

rhop(i) = (sigmap(i)-1)/sigmap(i);
rhoh = (sigmah-1)/sigmah;

incb =  sum(f, FACpmtb(f))-noncompbb;

beta(i)=(LABb(i))**(rhop(i)-1)/((CAPb(i))**(rhop(i)-1)+(LABb(i))**(rhop(i)-1));

theta(i) = sum (c,QDb(i,c))/((beta(i)*(CAPb(i)**rhop(i))+(1-beta(i))*(LABb(i)**rhop(i)))**(1/rhop(i)));
QDb_chk(i)=theta(i)*((beta(i)*(CAPb(i)**rhop(i))+(1-beta(i))*(LABb(i)**rhop(i)))**(1/rhop(i)));

LABb_chk(i) =(QDb_chk(i)/theta(i))*(((1-beta(i))/wb)**sigmap(i))*(((beta(i)**sigmap(i))*(rb**(1-sigmap(i)))+((1-beta(i))**sigmap(i))*(wb**(1-sigmap(i))))**(sigmap(i)/(1-sigmap(i))));
CAPb_chk(i) =(QDb_chk(i)/theta(i))*(((beta(i))/rb)**sigmap(i))*(((beta(i)**sigmap(i))*(rb**(1-sigmap(i)))+((1-beta(i))**sigmap(i))*(wb**(1-sigmap(i))))**(sigmap(i)/(1-sigmap(i))));

alpha(i)= ((HHXbb2(i)/incb)**(1/sigmah))/(sum(ii,(HHXbb2(ii)/incb)**(1/sigmah)));

HHX_chk(i) = ((alpha(i)**(sigmah))*(PXb(i)**(-sigmah))*incb)/(sum(ii,((alpha(ii)**(sigmah))*(PXb(ii)**(1-sigmah)))));

rhod(i)= (sigmad(i)-1)/sigmad(i);

betad(i)= (IMbb(i)**(1/sigmad(i)))/(QDbb(i)**(1/sigmad(i))+IMbb(i)**(1/sigmad(i)));
thetad(i) = Qbb(i)/((betad(i)*(IMbb(i)**rhod(i))+(1-betad(i))*(QDbb(i)**rhod(i)))**(1/rhod(i)));

rhos(i)= (1+sigmas(i))/sigmas(i);
betas(i)= (EXbb(i)**(1/(-sigmas(i))))/(HHXbb2(i)**(1/(-sigmas(i)))+EXbb(i)**(1/(-sigmas(i))));
thetas(i) = Qbb(i)/((betas(i)*(EXbb(i)**rhos(i))+(1-betas(i))*(HHXbb2(i)**rhos(i)))**(1/rhos(i)));

Qb_chk(i)=thetad(i)*((betad(i)*(IMbb(i)**rhod(i))+(1-betad(i))*(QDbb(i)**rhod(i)))**(1/rhod(i)));
Xb_chk(i)=thetas(i)*((betas(i)*(EXbb(i)**rhos(i))+(1-betas(i))*(HHXbb2(i)**rhos(i)))**(1/rhos(i)));

Display
rhop
beta
SAM
theta
QDb_chk
LABb_chk
CAPb_chk
incb
alpha
HHX_chk
rhod
betad
thetad
rhos
betas
thetas
Qb_chk
Xb_chk;

Parameters
QDb_tst(i)
LABb_tst(i)
CAPb_tst(i)
HHX_tst(i)
;

QDb_tst(i)  =QDb_chk(i)- sum(c, QDb(i,c));
LABb_tst(i)=LABb_chk(i)-LABb(i);
CAPb_tst(i)=CAPb_chk(i)-CAPb(i);
HHX_tst(i) =HHX_chk(i)- HHXbb2(i);

Display
QDb_tst
LABb_tst
CAPb_tst
HHX_tst
;


Variables
*markets
PD_b(i)     Domestic price
PEX_b(i)    Export price
PX_b(i)     Aggegrate allocation price 
PMQ_b(i)    Import price
PQ_b(i)     Aggregate price consumers page
w_b         wage
r_b         rental rate of capital
*HH demands
HHX_b(i)    household demand for goods
*Factor demands
LAB_b(i)    labor demand by firms
CAP_b(i)    capital demand by firms
*trade
Q_b(i)      aggregate production available (aggregate of domestic production and imports)
QM_b(i)     imports
X_b(i)      aggregate production available for absorption (aggregate of domestic demand and exports)
XE_b(i)     exports
*zero profits
QD_b(i)     domestic production
*income balance
INC_b       household incomes
;



Equations
*markets
GDS_b_eq(i)
LMKT_b_eq
KMKT_b_eq
PX_b_eq(i)
PQ_b_eq(i)
*HH demands
HHX_b_eq(i)
*Factor demands
LAB_b_eq(i)
CAP_b_eq(i)
*trade
ARM_b_eq(i)
ARMFOC_b_eq(i)
CET_b_eq(i)
CETFOC_b_eq(i)
Qtot_b_eq(i)
*zero profits
PRF_b_eq(i)
*income balance
INC_b_eq;


*markets
GDS_b_eq(i) .. X_b(i) =e= Q_b(i);
LMKT_b_eq   .. FACpmtb('LAB') =e= sum(i, LAB_b(i));
KMKT_b_eq   .. FACpmtb('CAP') =e= sum(i, CAP_b(i));
PX_b_eq(i)   .. PX_b(i)*X_b(i) =e= PD_b(i)*HHX_b(i) + PEX_b(i)*XE_b(i);
PQ_b_eq(i)   .. PQ_b(i)*Q_b(i) =e= PD_b(i)*QD_b(i) + PMQ_b(i)*QM_b(i);
*HH demands
HHX_b_eq(i) .. HHX_b(i) =e= ((alpha(i)**(sigmah))*(PQ_b(i)**(-sigmah))*INC_b)/(sum(ii,((alpha(ii)**(sigmah))*(PQ_b(ii)**(1-sigmah)))));
*Factor demands
LAB_b_eq(i) .. LAB_b(i) =e= (QD_b(i)/theta(i))*(((1-beta(i))/w_b)**sigmap(i))*(((beta(i)**sigmap(i))*(r_b**(1-sigmap(i)))+((1-beta(i))**sigmap(i))*(w_b**(1-sigmap(i))))**(sigmap(i)/(1-sigmap(i))));
CAP_b_eq(i) .. CAP_b(i) =e=(QD_b(i)/theta(i))*(((beta(i))/r_b)**sigmap(i))*(((beta(i)**sigmap(i))*(r_b**(1-sigmap(i)))+((1-beta(i))**sigmap(i))*(w_b**(1-sigmap(i))))**(sigmap(i)/(1-sigmap(i))));
*trade
ARM_b_eq(i)   .. Q_b(i) =e= thetad(i)*((betad(i)*(QM_b(i)**rhod(i))+(1-betad(i))*(QD_b(i)**rhod(i)))**(1/rhod(i)));
ARMFOC_b_eq(i)   ..  (QD_b(i)/QM_b(i)) =e= (((1-betad(i))/betad(i))*(PMQ_b(i)/PD_b(i)))**sigmad(i);
CET_b_eq(i)   .. X_b(i) =e= thetas(i)*((betas(i)*(XE_b(i)**rhos(i))+(1-betas(i))*(HHX_b(i)**rhos(i)))**(1/rhos(i)));
CETFOC_b_eq(i)   .. (HHX_b(i)/XE_b(i)) =e=  (((1-betas(i))/betas(i))*(PEX_b(i)/PD_b(i)))**(-sigmas(i));
Qtot_b_eq(i) .. Q_b(i) =e=  QD_b(i) + QM_b(i);
*zero profits
PRF_b_eq(i) .. PD_b(i)*QD_b(i) =e= w_b*LAB_b(i)+r_b*CAP_b(i);
*income balance
INC_b_eq    .. INC_b =e= w_b*(sum(i,LAB_b(i)))+r_b*(sum(i,CAP_b(i)))-noncompbb;

model mdl1 /
GDS_b_eq.Q_b
LMKT_b_eq.w_b
KMKT_b_eq.r_b
PX_b_eq.PX_b
PQ_b_eq.PQ_b
HHX_b_eq.HHX_b
LAB_b_eq.LAB_b
CAP_b_eq.CAP_b
ARM_b_eq.QD_b
ARMFOC_b_eq.QM_b
CET_b_eq.X_b
CETFOC_b_eq.XE_b
PRF_b_eq.PD_b
INC_b_eq.INC_b
/;

* Choose numeraire(s)
PEX_b.fx(i)=1;
PMQ_b.fx(i)=1;

* Set initial values of variables:
PD_b.l(i)=1;
w_b.l=1;
r_b.l=1;
PX_b.l(i)=1;
PQ_b.l(i)=1;
HHX_b.l(i)=HHX_chk(i);
LAB_b.l(i)=LABb_chk(i);
CAP_b.l(i)=CAPb_chk(i);
Q_b.l(i)=Qb_chk(i);
QM_b.l(i)=IMbb(i);
X_b.l(i)=Xb_chk(i);
XE_b.l(i)=EXbb(i);
QD_b.l(i)=QDb_chk(i);
INC_b.l=incb;


solve mdl1 using mcp;

*Policy Run

Variables
*markets
PD_p(i,tt)
PEX_p(i,tt)
PX_p(i,tt)
PMQ_p(i,tt)
PQ_p(i,tt)
w_p(tt)
r_p(tt)
*HH demands
HHX_p(i,tt)
*Factor demands
LAB_p(i,tt)
CAP_p(i,tt)
*trade
Q_p(i,tt)
QM_p(i,tt)
X_p(i,tt)
XE_p(i,tt)
*zero profits
QD_p(i,tt)
*income balance
INC_p(tt);
;



Equations
*markets
GDS_p_eq(i,tt)
LMKT_p_eq(tt)
KMKT_p_eq(tt)
PX_p_eq(i,tt)
PQ_p_eq(i,tt)
*HH demands
HHX_p_eq(i,tt)
*Factor demands
LAB_p_eq(i,tt)
CAP_p_eq(i,tt)
*trade
ARM_p_eq(i,tt)
ARMFOC_p_eq(i,tt)
CET_p_eq(i,tt)
CETFOC_p_eq(i,tt)
Qtot_p_eq(i,tt)
*zero profits
PRF_p_eq(i,tt)
*income balance
INC_p_eq(tt);


*markets
GDS_p_eq(i,tt) .. X_p(i,tt) =e= Q_p(i,tt);
LMKT_p_eq(tt)   .. FACpmtb('LAB') =e= sum(i, LAB_p(i,tt));
KMKT_p_eq(tt)   .. FACpmtb('CAP') =e= sum(i, CAP_p(i,tt));
PX_p_eq(i,tt)   .. PX_p(i,tt)*X_p(i,tt) =e= PD_p(i,tt)*HHX_p(i,tt) + PEX_p(i,tt)*XE_p(i,tt);
PQ_p_eq(i,tt)   .. PQ_p(i,tt)*Q_p(i,tt) =e= PD_p(i,tt)*QD_p(i,tt) + PMQ_p(i,tt)*QM_p(i,tt);
*HH demands
HHX_p_eq(i,tt) .. HHX_p(i,tt) =e= ((alpha(i)**(sigmah))*(PQ_p(i,tt)**(-sigmah))*INC_p(tt))/(sum(ii,((alpha(ii)**(sigmah))*(PQ_p(ii,tt)**(1-sigmah)))));
*Factor demands
LAB_p_eq(i,tt) .. LAB_p(i,tt) =e= (QD_p(i,tt)/(theta(i)*(1+shkk(i,tt))))*(((1-beta(i))/w_p(tt))**sigmap(i))*(((beta(i)**sigmap(i))*(r_p(tt)**(1-sigmap(i)))+((1-beta(i))**sigmap(i))*(w_p(tt)**(1-sigmap(i))))**(sigmap(i)/(1-sigmap(i))));
CAP_p_eq(i,tt) .. CAP_p(i,tt) =e=(QD_p(i,tt)/(theta(i)*(1+shkk(i,tt))))*(((beta(i))/r_p(tt))**sigmap(i))*(((beta(i)**sigmap(i))*(r_p(tt)**(1-sigmap(i)))+((1-beta(i))**sigmap(i))*(w_p(tt)**(1-sigmap(i))))**(sigmap(i)/(1-sigmap(i))));
*trade
ARM_p_eq(i,tt)   .. Q_p(i,tt) =e= thetad(i)*((betad(i)*(QM_p(i,tt)**rhod(i))+(1-betad(i))*(QD_p(i,tt)**rhod(i)))**(1/rhod(i)));
ARMFOC_p_eq(i,tt)   ..  (QD_p(i,tt)/QM_p(i,tt)) =e= (((1-betad(i))/betad(i))*(PMQ_p(i,tt)/PD_p(i,tt)))**sigmad(i);
CET_p_eq(i,tt)   .. X_p(i,tt) =e= thetas(i)*((betas(i)*(XE_p(i,tt)**rhos(i))+(1-betas(i))*(HHX_p(i,tt)**rhos(i)))**(1/rhos(i)));
CETFOC_p_eq(i,tt)   .. (HHX_p(i,tt)/XE_p(i,tt)) =e=  (((1-betas(i))/betas(i))*(PEX_p(i,tt)/PD_p(i,tt)))**(-sigmas(i));
Qtot_p_eq(i,tt) .. Q_p(i,tt) =e=  QD_p(i,tt) + QM_p(i,tt);
*zero profits
PRF_p_eq(i,tt) .. PD_p(i,tt)*QD_p(i,tt) =e= w_p(tt)*LAB_p(i,tt)+r_p(tt)*CAP_p(i,tt);
*income balance
INC_p_eq(tt)    .. INC_p(tt) =e= w_p(tt)*(sum(i,LAB_p(i,tt)))+r_p(tt)*(sum(i,CAP_p(i,tt)))-noncompbb;

model mdl2 /
GDS_p_eq.Q_p
LMKT_p_eq.w_p
KMKT_p_eq.r_p
PX_p_eq.PX_p
PQ_p_eq.PQ_p
HHX_p_eq.HHX_p
LAB_p_eq.LAB_p
CAP_p_eq.CAP_p
ARM_p_eq.QD_p
ARMFOC_p_eq.QM_p
CET_p_eq.X_p
CETFOC_p_eq.XE_p
PRF_p_eq.PD_p
INC_p_eq.INC_p
/;

* Choose numeraire(s)
PEX_p.fx(i,tt)=1;
PMQ_p.fx(i,tt)=1;

* Set initial values of variables:
PD_p.l(i,tt)=1;
w_p.l(tt)=1;
r_p.l(tt)=1;
PX_p.l(i,tt)=1;
PQ_p.l(i,tt)=1;
HHX_p.l(i,tt)=HHX_chk(i);
LAB_p.l(i,tt)=LABb_chk(i);
CAP_p.l(i,tt)=CAPb_chk(i);
Q_p.l(i,tt)=Qb_chk(i);
QM_p.l(i,tt)=IMbb(i);
X_p.l(i,tt)=Xb_chk(i);
XE_p.l(i,tt)=EXbb(i);
QD_p.l(i,tt)=QDb_chk(i);
INC_p.l(tt)=incb;


solve mdl2 using mcp;

Parameter
pnum,
pden
pratio
CV
;



pnum(tt) =  (sum(i,((alpha(i)**(sigmah))*(PQ_p.l(i,tt)**(1-sigmah)))))**(1/(1-sigmah));
pden(tt) =  (sum(i,((alpha(i)**(sigmah))*(PQ_b.l(i)**(1-sigmah)))))**(1/(1-sigmah));

pratio(tt) = pnum(tt)/pden(tt);

CV(tt)= INC_b.l*(pratio(tt))-INC_p.l(tt);


Display
pnum,
pden
pratio
CV;



Parameters
PD_pct
w_pct
r_pct
PX_pct
PQ_pct
HHX_pct
LAB_pct
CAP_pct
Q_pct
QM_pct
X_pct
XE_pct
QD_pct
INC_pct
plevel(tt);

PD_pct(i,tt)=(PD_p.l(i,tt)-PD_b.l(i))/PD_b.l(i);
w_pct(tt)=(w_p.l(tt)-w_b.l)/w_b.l;
r_pct(tt)=(r_p.l(tt)-r_b.l)/r_b.l;
PX_pct(i,tt)=(PX_p.l(i,tt)-PX_b.l(i))/PX_b.l(i);
PQ_pct(i,tt)=(PQ_p.l(i,tt)-PQ_b.l(i))/PQ_b.l(i);
HHX_pct(i,tt)=(HHX_p.l(i,tt)-HHX_b.l(i))/HHX_b.l(i);
LAB_pct(i,tt)=(LAB_p.l(i,tt)-LAB_b.l(i))/LAB_b.l(i);
CAP_pct(i,tt)=(CAP_p.l(i,tt)-CAP_b.l(i))/CAP_b.l(i);
Q_pct(i,tt)=(Q_p.l(i,tt)-Q_b.l(i))/Q_b.l(i);
QM_pct(i,tt)=(QM_p.l(i,tt)-QM_b.l(i))/QM_b.l(i);
X_pct(i,tt)=(X_p.l(i,tt)-X_b.l(i))/X_b.l(i);
XE_pct(i,tt)=(XE_p.l(i,tt)-XE_b.l(i))/XE_b.l(i);
QD_pct(i,tt)=(QD_p.l(i,tt)-QD_b.l(i))/QD_b.l(i);
INC_pct(tt)=(INC_p.l(tt)-INC_b.l)/INC_b.l;

plevel(tt)=(pnum(tt)-pden(tt))/pden(tt);


Display
PD_pct
w_pct
r_pct
PX_pct
PQ_pct
HHX_pct
LAB_pct
CAP_pct
Q_pct
QM_pct
X_pct
XE_pct
QD_pct
INC_pct
plevel;

File fr / fr.txt /;
put fr;
fr.nd=6;
put "Results, step, industry, shkk, CV, PD_pct, w_pct, r_pct, PX_pct, PQ_pct, HHX_pct, LAB_pct, CAP_pct, Q_pct, QM_pct, X_pct, XE_pct, QD_pct, INC_pct, PL_pct" /;
loop((tt,i),
    put ord(tt), card(i), shkk(i,tt), CV(tt), PD_pct(i,tt), w_pct(tt), r_pct(tt), PX_pct(i,tt), PQ_pct(i,tt), HHX_pct(i,tt), LAB_pct(i,tt), CAP_pct(i,tt), Q_pct(i,tt), QM_pct(i,tt), X_pct(i,tt), XE_pct(i,tt), QD_pct(i,tt), INC_pct(tt), plevel(tt) /
);
putclose;