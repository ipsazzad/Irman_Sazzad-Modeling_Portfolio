* Kuhn-Tucker Minimization Problem

* Declare Variables
VARIABLES
    x1, x2,       
    obj;          

* Set Variable Bounds (Non-negativity)
x1.lo = 0;  
x2.lo = 0; 

* Declare Equations
EQUATIONS
    obj_def,       
    constraint1,   
    constraint2;   

* Define Objective Function
obj_def.. obj =E= POWER(x1 - 4, 2) + POWER(x2 - 4, 2);

* Define Constraints
constraint1.. 2*x1 + 3*x2 =G= 6;
constraint2.. -3*x1 - 2*x2 =G= -12;

* Specify the Model
MODEL kuhn_tucker /all/;

* Solve the Model
SOLVE kuhn_tucker USING NLP MINIMIZING obj;

* Display Results
DISPLAY x1.l, x2.l, obj.l;




