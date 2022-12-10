/* ===========================================================================
    author : Isaiah Linares
     first : Isaiah Linares
    latest : Isaiah Linares

    Solves the 2x2x2 Rubik's mini-cube ...
=========================================================================== */

:- use_module(r2x2x2).

%----------------------------------------------------------------------------%
% sphere
% Used to represent the reachable state End from some start state Start using a path of moves Path.
% Note: End state is Step moves away from Start state
%
% Start | The Starting state for this sphere of reachable states
% State | The current state of rubix cube after the following the Path of moves from Start state
% Step  | How many moves away from Start state the current State is at
% Path  | The moves to get from Start to State
%
% Goal: We want to find a way to have all spheres 7 steps away from scrambled and solved state in the databases, 
% and also unify the spheres with the same end state but different start states. and combine poth of their paths to get our solution 

:- dynamic sphere/4.

% ----------------------------------------------------------------------------
%   twosy()/2 
%     +State: State of rubix cube
%     -Path:  The path of moves to Solved rubix cube state
%  
%   Uses find()/2 to exend the knoweldge base with more and more levels of 
%   move path spheres (starting from given state, and starting from 
%   a solved state) until we find two that match (this is the meet in the middle
%   strategy). Then the two paths are just appended into one.
%
% ----------------------------------------------------------------------------

twosy(State, Path) :-
    find(State, 0),
    state_zero(X),

    % Uses natural() to increas the level of breadth-search if we fail
    % to find two matching spherese
    natural(Level),
    write("Extending Level: "), write(Level), nl,
    find(State, Level),
    find(X, Level),

    %Check if two spheres meet
    sphere(State, Final, MatchLevel, First_Path),
    sphere(X, Final, MatchLevel, Second_Path_Rev),
    write("Found a match at Level: "), write(MatchLevel), nl,

    reverse(Second_Path_Rev, Second_Path),
    reversed_path(Second_Path, Last_Path),
    append(First_Path, Last_Path, Path),
    apply_path(State, Path).

% ----------------------------------------------------------------------------
%   reversed_path()/2
%   
%   Path | path to be reversed
%   Reversed | reversed path
%
%   Reverse the moves in a path of rubix cube moves e
%
% ----------------------------------------------------------------------------

reversed_path([Mv|Rest], [NewMv|Reverse]):-
    reversal(Mv, NewMv),
    reversed_path(Rest, Reverse).
reversed_path([],Reverse):-
    Reverse = [].

% ----------------------------------------------------------------------------
%   find()/2
%   
%   Start | Starting rubix cube state (represented in list form)
%   Level | Level of breadth to exend the knowledge base with (if it is 0 the
%   first level of spheres will be found, if > 0, the current shperes will 
%   be extended
%
%   Extends the breadth-level of spheres in the knowledge base starting from
%   'Start' by one
%
% ----------------------------------------------------------------------------

% Base Case
find(State, 0):-
    find_spheres(State, State, 0, []).
% So first breadth can pass
find(State, 0):-
    !.

find(Start, Level):-
    sphere(Start, State, Level, Path),
    find_spheres(Start, State, Level, Path).
%If all levels have been explored, give the pass on search
find(Start, Level):-
    !.
    
%----------------------------------------------------------------------------%
%   find_spheres()/4
%
%   Start | The Starting state for this sphere of reachable states
%   State | The current state of rubix cube after the following the Path of moves from Start state
%   Step  | How many moves away from Start state the current State is at
%   Path  | The moves to get from Start to State
%
%   Finds all the states reachable from 'State' with one moves, and records the respective sphere into our 
%   knowledge base using assertz, prolog will keep backtracking until all possible moves are exhausted

find_spheres(Start, State, Step, Path):-
    move(Mv), % chose some move you can make at this point
    move_transform(State, Mv, Next), % apply this move to the current state to get next state
    NewStep is Step + 1,
    append(Path, [Mv], NewPath),
    assertz(sphere(Start, Next, NewStep, NewPath)),
    fail.


% Simple algo for giving ascending natural numbers
natural(1).
natural(X):-
    natural(Y),
    X is Y + 1.