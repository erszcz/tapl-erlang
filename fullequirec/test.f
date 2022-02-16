Counter = Rec P. {get:Nat, inc:Unit->P}; 

p = 
let create = 
  fix 
    (lambda cr: {x:Nat}->Counter.
      lambda s: {x:Nat}.
        {get = s.x,
         inc = lambda _:Unit. cr {x=succ(s.x)}})
in
  create {x=0};
