$TITLE Five Island Spatial Equilibrium Problem

OPTION SOLPRINT=ON, SYSOUT=OFF, LIMROW=0, LIMCOL=0;

SETS
    I        ISLANDS /ONE, TWO, THREE, FOUR, FIVE/
    ALIAS (I, IP);

TABLE DIST(I, IP) 
         ONE    TWO    THREE    FOUR    FIVE
    ONE    0      8.83   12.50   17.68   22.00
    TWO   8.83    0      10.50   19.74   15.90
    THREE 12.50  10.50    0      11.03   11.89
    FOUR  17.68  19.74   11.03    0      18.20
    FIVE  22.00  15.90   11.89   18.20    0;

PARAMETERS
    SUPPLY(I) Production on each island /ONE 1080, TWO 1020, THREE 725, FOUR 1125, FIVE 1050/;

SCALARS
    TCOST    Transport cost per ton-mile /20/
    A        Coefficient for price /3000/
    B        Coefficient for price /2/;

VARIABLES
    M(I,IP)  Quantity of imports to I from IP
    X(I,IP)  Quantity of exports from I to IP
    SHIP     Total quantity shipped
    P(I)     Price of good on island I
    CONS(I)  Consumption on island I
    TTRANS   Total transport costs
    SURP(I)  Consumer and producer surplus on island I
    SURPLUS  Total surplus
    ARB(I,IP) Arbitrage;

POSITIVE VARIABLES X, M, CONS, P;

EQUATIONS
    ESURP       Calculating total surplus
    ISURP(I)    Surplus on island I
    ETRANS      Transport costs
    PRICE(I)    Determining price on island I
    CON(I)      Consumption on island I
    XEQM(I,IP)  Exports equal imports between islands
    ARBCHECK(I,IP) Arbitrage condition between islands
    XCHECK(I,IP)  Forbidding exports between islands
    MCHECK(I,IP)  Forbidding imports between islands;

ESURP..
    SURPLUS =E= SUM(I, SURP(I)) - TTRANS;

ISURP(I).. 
    SURP(I) =E= A*CONS(I) - B*CONS(I)*CONS(I)/2;

ETRANS.. 
    TTRANS =E= SUM(I, SUM(IP, TCOST * DIST(I, IP) * X(I, IP)));

PRICE(I).. 
    P(I) =E= A - B*CONS(I);

CON(I).. 
    CONS(I) =E= SUPPLY(I) - SUM(IP, X(I, IP)) + SUM(IP, M(I, IP));

* Balance exports and imports
XEQM(I,IP).. 
    X(I,IP) =E= M(IP,I);

* Verify no arbitrage opportunities exist
ARBCHECK(I,IP)..ARB(I,IP) =E= P(IP) - P(I) - DIST(I,IP)*TCOST;

* Forbid exports between islands
XCHECK(I,IP).. 
    X(I,IP) =E= 0;

* Forbid imports between islands
MCHECK(I,IP).. 
    M(I,IP) =E= 0;

MODEL ISLAND /ALL/;

SOLVE ISLAND USING NLP MAXIMIZING SURPLUS;

DISPLAY CONS.L, P.L, X.L, M.L, SURP.L, SURPLUS.L, TTRANS.L, ARBCHECK.L;