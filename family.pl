%rules
parent(Parent, Child) :- mother(Parent, Child).
parent(Parent, Child) :- father(Parent, Child).

%facts
mother('Mary', 'Elizabeth').
mother('Mary', 'Margeret').
father('George', 'Elizabeth').
father('George', 'Margeret').