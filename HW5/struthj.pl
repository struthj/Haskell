% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).

parents(X,Y) :- parent(X,Y).


%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parents(Y,X).


% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- female(X), parent(X,_).
isFather(X) :- male(X), parent(X,_).

% 3. Define a predicate `grandparent/2`.
grandparent(X,Y) :- parent(X,G),parent(G,Y).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(P,X), parent(P,Y), X \= Y, Y \= X.

% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- male(X), sibling(X,Y).
sister(X,Y) :- female(X), sibling(X,Y).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :- sibling(X,S), married(S,Y).
siblingInLaw(X,Y) :- married(S,X), sibling(Y,S).
% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X,Y) :- female(X), sibling(X,A), parent(A,Y).
aunt(X,Y) :- female(X), siblingInLaw(X,A), parent(A,Y).
uncle(X,Y) :- male(X), sibling(X,U), parent(U,Y).
uncle(X,Y) :- male(X), siblingInLaw(X,U), parent(U,Y).
% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- aunt(A,X), parent(A,Y).
cousin(X,Y) :- uncle(U,X), parent(U,Y).

% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,A), child(Y,A).
ancestor(X,Y) :- parent(X,Y).

% Extra credit: Define the predicate `related/2`.



%%
% Part 2. Language implementation
%%

num(N) :- number(N).
str(S) :- string(S).
bool(B) :- B = t | B = f.
% bool(true).
% add(X,Y) :- number(X) + number(Y).
% cmd(add(X,Y)) :- number(S) + number(Y).
% cmd :- num | str | bool
%     | 

remove([Head|Tail],Tail, Head).
addN(X,Y,R) :- R is +(X,Y).

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.

cmd(N, S1, S2) :- num(N), append([N], S1, S2).
cmd(S ,S1 , S2) :- str(S), append([S], S1, S2).
cmd(add, [X,Y|Tail], S2) :- num(X), num(Y), addN(X,Y,R), append([R],Tail, S2).
cmd(lte, [X,Y|Tail], S2) :- num(X), num(Y), (X =< Y -> append([t],Tail, S2) ; append([f],Tail, S2)).
cmd(if(P1,_), [t|Tail], S2) :- prog(P1, Tail, S2).
cmd(if(_,P2), [f|Tail], S2) :- prog(P2,Tail,S2).

% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.

prog([],S,S).
prog([P|P1], S1, S2) :- cmd(P, S1, R),  prog(P1, R, S2).


