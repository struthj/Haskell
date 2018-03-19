% Here is a Prolog database that describes a directed 
% and color-coded teleportation network in terms of a predicate portal/3. 
% For example, the fact portal(2,red,4) means that 
% we can move from position 2 to position 4 via a red portal, but not necessarily vice versa.

portal(1,blue,2).      
portal(3,red,4).      
portal(2,green,1).
portal(2,blue,3).      
portal(4,red,1).      
portal(3,green,2).
portal(3,blue,4).      
portal(4,red,2).     
portal(4,green,2).

% Define a recursive predicate path/3 that relates whether two positions are connected by portals 
% of a particular color. 
% For example, the query path(1,blue,4) should return true.

path(X,Y,Z) :- portal(X,Y,Z).
path(X,Y,Z) :- portal(X,Y,R), path(R,Y,Z).

% Using path/3, write a query that finds all of the colors for 
% which a path exists from position 3 to position 1. 
% Then list all solutions to this query.

% path(3,X, 1).

% Using path/3, write a query that finds all of the positions that can be reached from position 2 
% using only blue portals. 
% Then list all solutions to this query.

% path(2, blue, X).