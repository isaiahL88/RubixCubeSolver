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

%----------------------------------------------------------------------------%
% twosy
%     +State: State of rubix cube
%     -Path:  The path of moves to Solved rubix cube state
% 
%     Uses auxilary predicate find() to first find the spheres of reachable states 7 moves out from some states.
%     It then just finds two spheres, one that starts at 'State' and is 7 moves out, and one the starts at a 'state_zero' and is 7 moves out
%     then, it just uses these two paths to create one path from State to Solved, using another auxilary predicate reversed_paths,
%     as well as some built in predicates.
%     Finally twosy just applies this path to 'State' using apply_path

twosy(State, Path) :-
    find_spheres(State, Start, 0, []),
    state_zero(X),
    % Uses natural() to increas the level of breadth-search if we fail
    % to find two matching spherese
    natural(Level),
    write("Starting Level: "), write(Level), nl,
    find(State, Level),
    find(X, Level),
    sphere(State, Final, Level, First_Path),
    sphere(X, Final, Level, Second_Path_Rev),
    write("Found a match at Level: "), write(Level).
    reverse(Second_Path_Rev, Second_Path),
    reversed_path(Second_Path, Last_Path),
    append(First_Path, Last_Path, Path),
    apply_path(State, Path).

%----------------------------------------------------------------------------%
%   Reverse the moves in a path of rubix cube moves
%   Note!!!!!: reverses with respect to moves, as is the moves have the reverse effect on rubix cube
%
%   reveersed_path(Path, Reversed)
%   Path | path to be reversed
%   Reversed | reversed path
reversed_path([Mv|Rest], [NewMv|Reverse]):-
    reversal(Mv, NewMv),
    reversed_path(Rest, Reverse).
reversed_path([],Reverse):-
    Reverse = [].

%----------------------------------------------------------------------------%
%   find(Start, Level)
%   Finds all spheres reachable from up to 7 moves from state 'Start' in a breadth-first fashion
%
%   Goes Level by level, using find_spheres, an auxilary predicate, to explore all spheres reachable from Level
%   Once find_spheres is exhausted for one spheres, prolog backtracks and finds another sphere at the same level
%   and exhausts all reachable sphers for this sphere and keeps doing this until it  find any until it moves onto the
%   next level

%No other spheres ate this level, go to next level
% idea for dynamic bfs: each time find(Start, Level) is called, assume all 1...Level levels of sphere are already 
% already recorded then just increment to Level + 1 and record all now reachable spheres into KB
% need some way to pass the query once all spheres at this level are already exlpored
% find(Start, Level):-
%     NewLevel is Level + 1,
%     write("")

% We then want prolog to backtrack to this rule after all reachable spheres at the current level have been explored
% find(Start, Level):-

find(Start, Level):-
    write("New Level is: " + NewLevel + "\n"),
    sphere(Start, State, NewLevel, Path),
    find_spheres(Start,State, NewLevel, Path).

%If all levels have been explored, give the pass on search
find(Start, Level):-
    !.
    
%----------------------------------------------------------------------------%
%   find_spheres(Start, State, Step, Path)
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


% Simple algo for giving increasingly bigger natural numbers
natural(1).
natrual(X):-
    natural(Y),
    X is Y + 1.