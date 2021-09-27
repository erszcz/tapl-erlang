true;
if false then true else false; 

x/;
x;

x = true;
x;
if x then false else x; 

lambda x. x;
(lambda x. x) (lambda x. x x); 
lambda _. true;

timesfloat (timesfloat 2.0 3.0) (timesfloat 4.0 5.0);

0; 
succ (pred 0);
iszero (pred (succ (succ 0))); 

let x=true in x;
let _=true in false;
