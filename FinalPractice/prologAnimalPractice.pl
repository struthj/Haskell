
% binary lives predicate
lives(zebra, savanna).     
lives(lion, savanna).      
lives(sloth, forest).      
lives(deer, forest).       
lives(shark, ocean).       

% binary eat predicate
eats(zebra, grass).
eats(lion, meat).
eats(sloth, leaves).
eats(deer, grass).
eats(shark, meat).


% query to return name, and location of any animal 
% that eats grass (zebra, deer)
grassAnimal :- eats(X,grass), lives(X, _).

% Define a predicate neighbor/2 that relates whether two animals live in the same place. For example, 
% the query neighbor(deer,Y) should find Y=sloth as a solution.
neighbor(X,Y) :- lives(X,R), lives(Y,R), R \= X.

% Define a predicate prey/1 that indicates whether an animal is prey. 
% An animal is prey if it is neighbors with an animal that eats meat.
prey(X) :- neighbor(X,Y), eats(Y,meat), not(eats(X,meat)).