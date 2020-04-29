/**
 * Log. project: Rubik cube
 * FLP, 2019/2020
 * Vladislav Halva, xhalva04
 **/

% Reads a line from STDIN, ends on LF or EOF. 
readLine(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		readLine(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


% Tests EOF and LF character.
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


% Reads line from STDIN, ends on EOF. 
readLines(Ls) :-
	readLine(L,C),
	( C == end_of_file, Ls = [] ;
	  readLines(LLs), Ls = [L|LLs]
	).


% Program entry point 
% reads input
% checks basic input format and transforms it to inner representation
% solve the rubik cube and print the solution steps to STDOUT
%
main :- 
    prompt(_, ''),
    readLines(Input), !, % prevent backtracking if error occurs
    %checkInput(Input),
    transformInput(Input, Rubik),
    goalRubik(Goal),
    solve(Rubik, Goal, Steps),
    printSteps(Steps),
    halt.

% Input check - checks if lines have expected length and if 
% they consist of number between 1-6 (lines 4-6 can contain spaces too) 
% 

checkShortValues([]).
checkShortValues([H|T]) :- atom_number(H, Num), Num > 0, Num < 7, checkShortValues(T).

checkLongValues([]).
checkLongValues([H|T]) :- 
    atom_number(H, Num), Num > 0, Num < 7, checkLongValues(T).
checkLongValues([H|T]) :- 
    H = ' ', checkLongValues(T).

checkLongLine(Line) :- length(Line, Len), Len = 15, checkLongValues(Line).

checkShortLine(Line) :- length(Line, Len), Len = 3, checkShortValues(Line).  

checkInput([L1,L2,L3,L4,L5,L6,L7,L8,L9]) :- 
    checkShortLine(L1),
    checkShortLine(L2),
    checkShortLine(L3),
    checkShortLine(L7),
    checkShortLine(L8),
    checkShortLine(L9),
    checkLongLine(L4),
    checkLongLine(L5),
    checkLongLine(L6).
checkInput(_) :- write('Wrong input format\n').

% Transform input to a list of lists, where each lower level list
% containts one side of the cube. 
% 
transformInput([L1,L2,L3,L4,L5,L6,L7,L8,L9], Rubik) :-
    splitLineOnSpaces(L4,S4),
    splitLineOnSpaces(L5,S5),
    splitLineOnSpaces(L6,S6),
    linesToSide(L1,L2,L3, Top),
    linesToSides(S4,S5,S6,Sides),
    linesToSide(L7,L8,L9, Bot),
    mergeSurface(Top,Sides,Bot,Surface),
    stringsToNumbersLLs(Surface,Rubik).

% Splits list into sub-lists separated on space in input list. 
splitLineOnSpaces(L,Sections) :- splitLineOnSpacesAcc(L,[],Sections).
splitLineOnSpacesAcc([],AccR,[Acc]) :- reverse(AccR, Acc).
splitLineOnSpacesAcc(['\n'],AccR,[Acc]) :- reverse(AccR, Acc). 
splitLineOnSpacesAcc([' '|T],AccR,[Acc|Sections]) :- splitLineOnSpacesAcc(T,[],Sections), reverse(AccR, Acc). 
splitLineOnSpacesAcc([H|T],Acc, Sections) :- splitLineOnSpacesAcc(T,[H|Acc], Sections).

% Three lines (lists), each containt values for one line of a cubes side are merged 
linesToSide(L1, L2 ,L3 ,Side) :- append(L1, L2, L12), append(L12, L3, Side).

% Three lines, each with one line of 4 cube sides separated by space 
% (eg. first line contains first line of front, right, back and left side)
% are converted to 4 list, each for one side 
% 
linesToSides([],[],[],[]).
linesToSides([H1|L1], [H2|L2], [H3|L3], [Side|Sides]) :- 
    linesToSide(H1, H2, H3, Side), 
    linesToSides(L1,L2,L3,Sides).

% Merge top, list of sides (front, right, bottom and left) and bottom, to one list. 
mergeSurface(Top,Sides,Bot,[Top|Res]) :- mergeSidesAndBot(Sides,Bot,Res). 
mergeSidesAndBot([],Bot,[Bot]).
mergeSidesAndBot([H|Sides],Bot,[H|Res]) :- mergeSidesAndBot(Sides,Bot,Res). 

% Converts list of string representing numbers to list of numbers. 
stringsToNumbersL([],[]).
stringsToNumbersL([H|T], [Converted|Res]) :- atom_number(H, Converted), stringsToNumbersL(T, Res).  

% Converts list of list of strings representing numbers to list of numbers. 
stringsToNumbersLLs([],[]).
stringsToNumbersLLs([H|In],[ConvertedList|Out]) :- 
    stringsToNumbersL(H, ConvertedList), stringsToNumbersLLs(In, Out).

% Prints Rubik cube to STDOUT in desired format. 
printRubik(
    [[Top1,Top2,Top3,Top4,Top5,Top6,Top7,Top8,Top9],
    [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    [Bot1,Bot2,Bot3,Bot4,Bot5,Bot6,Bot7,Bot8,Bot9]]
) :- 
    writef("%w%w%w\n", [Top1,Top2,Top3]),
    writef("%w%w%w\n", [Top4,Top5,Top6]),
    writef("%w%w%w\n", [Top7,Top8,Top9]),
    writef("%w%w%w %w%w%w %w%w%w %w%w%w\n", [F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3]),
    writef("%w%w%w %w%w%w %w%w%w %w%w%w\n", [F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6]),
    writef("%w%w%w %w%w%w %w%w%w %w%w%w\n", [F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9]),
    writef("%w%w%w\n", [Bot1,Bot2,Bot3]),
    writef("%w%w%w\n", [Bot4,Bot5,Bot6]),
    writef("%w%w%w\n", [Bot7,Bot8,Bot9]).
    
% Prints list of Rubik cubes to STDOUT. 
printSteps([]).
printSteps([H]) :- printRubik(H), !.
printSteps([H|Steps]) :- printRubik(H), write('\n'), printSteps(Steps).

% The solved (goal) Rubik cube. 
goalRubik(
    [[5,5,5,5,5,5,5,5,5],
    [1,1,1,1,1,1,1,1,1],
    [2,2,2,2,2,2,2,2,2],
    [3,3,3,3,3,3,3,3,3],
    [4,4,4,4,4,4,4,4,4],
    [6,6,6,6,6,6,6,6,6]]
    ).


% 
% Breadth first search - solution of Rubik cube with closed.   
% 

:-dynamic closed/2, open/2.
closed(placeholder,p). % prevents error if no other exists while 'not' invoked 

% Solve Rubik cube using BFS
% Rubik - the start configuration of cube 
% Goal - solved cube
% Steps - list of steps from start to solved cube (Var)
% 
solve(Rubik, Goal, Steps) :- assertz(open(Rubik,Rubik)) ,bfs(Goal, StepsRev), reverse(StepsRev, Steps), !.

% Breadth first search itself
% Goal - solved cube
% Steps - solution steps (Var) 
% 
bfs(_,_) :- not(open(_,_)), !, fail. % empty queue - solution does not exist
bfs(Head, Steps) :- % solution found
    open(Pred,Head), 
    assertz(closed(Pred,Head)), 
    findPathInClosed(Head, [], Steps).
bfs(Goal, Steps) :-
    open(Pred,Head),
    findall(
        [Head|X],
        (step(Head,X), not(open(_,X)), not(closed(_,X))),
        Expanded
    ),
    assertz(closed(Pred,Head)),
    assertzOpenAll(Expanded),
    retract(open(Pred,Head)),
    bfs(Goal, Steps).

% Returns reversed path from start configuration to solved cube from closed.
findPathInClosed(Goal, Acc, Steps) :- 
    closed(Pred, Goal), 
    retract(closed(Pred,Goal)), 
    findPathInClosed(Pred, [Goal|Acc], Steps).
findPathInClosed(_, Acc, Acc).

% Add all elements of lists to open (append queue) 
assertzOpenAll([]).
assertzOpenAll([[P|H]|T]) :- assertz(open(P,H)), assertzOpenAll(T).




% Definitions of all valid moves with Rubik cube. 
% Rotate: Top, Front, Right, Back, Left, Bottom
% Each rotation can be performed clockwise and counterclockwise. 
% 

step(Before,After) :- rotateTopClockwise(Before,After).
step(Before,After) :- rotateTopCounterClockwise(Before,After).
step(Before,After) :- rotateFrontClockwise(Before, After).
step(Before,After) :- rotateFrontCounterClockwise(Before, After).
step(Before,After) :- rotateRightClockwise(Before,After).
step(Before,After) :- rotateRightCounterClockwise(Before, After).
step(Before,After) :- rotateBackClockwise(Before,After).
step(Before,After) :- rotateBackCounterClockwise(Before, After).
step(Before,After) :- rotateLeftClockwise(Before,After).
step(Before,After) :- rotateLeftCounterClockwise(Before, After).
step(Before,After) :- rotateBotClockwise(Before, After).
step(Before,After) :- rotateBotCounterClockwise(Before,After).

rotateTopClockwise(
    [[Top1,Top2,Top3,Top4,Top5,Top6,Top7,Top8,Top9],
    [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    Bot]
    ,
    [[Top7,Top4,Top1,Top8,Top5,Top2,Top9,Top6,Top3],
    [R1,R2,R3,F4,F5,F6,F7,F8,F9],
    [B1,B2,B3,R4,R5,R6,R7,R8,R9],
    [L1,L2,L3,B4,B5,B6,B7,B8,B9],
    [F1,F2,F3,L4,L5,L6,L7,L8,L9],
    Bot]
).

rotateTopCounterClockwise(
    [[Top1,Top2,Top3,Top4,Top5,Top6,Top7,Top8,Top9],
    [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    Bot]
    ,
    [[Top3,Top6,Top9,Top2,Top5,Top8,Top1,Top4,Top7],
    [L1,L2,L3,F4,F5,F6,F7,F8,F9],
    [F1,F2,F3,R4,R5,R6,R7,R8,R9],
    [R1,R2,R3,B4,B5,B6,B7,B8,B9],
    [B1,B2,B3,L4,L5,L6,L7,L8,L9],
    Bot]
).

rotateFrontClockwise(
    [[Top1,Top2,Top3,Top4,Top5,Top6,Top7,Top8,Top9],
    [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    Back,
    [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    [Bot1,Bot2,Bot3,Bot4,Bot5,Bot6,Bot7,Bot8,Bot9]]
    ,
    [[Top1,Top2,Top3,Top4,Top5,Top6,L9,L6,L3],
    [F7,F4,F1,F8,F5,F2,F9,F6,F3],
    [Top7,R2,R3,Top8,R5,R6,Top9,R8,R9],
    Back,
    [L1,L2,Bot1,L4,L5,Bot2,L7,L8,Bot3],
    [R7,R4,R1,Bot4,Bot5,Bot6,Bot7,Bot8,Bot9]]
).

rotateFrontCounterClockwise(
    [[Top1,Top2,Top3,Top4,Top5,Top6,Top7,Top8,Top9],
    [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    Back,
    [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    [Bot1,Bot2,Bot3,Bot4,Bot5,Bot6,Bot7,Bot8,Bot9]]
    ,
    [[Top1,Top2,Top3,Top4,Top5,Top6,R1,R4,R7],
    [F3,F6,F9,F2,F5,F8,F1,F4,F7],
    [Bot3,R2,R3,Bot2,R5,R6,Bot1,R8,R9],
    Back,
    [L1,L2,Top9,L4,L5,Top8,L7,L8,Top7],
    [L3,L6,L9,Bot4,Bot5,Bot6,Bot7,Bot8,Bot9]]
).

rotateRightClockwise(
    [[Top1,Top2,Top3,Top4,Top5,Top6,Top7,Top8,Top9],
    [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    Left,
    [Bot1,Bot2,Bot3,Bot4,Bot5,Bot6,Bot7,Bot8,Bot9]]
    ,
    [[Top1,Top2,F3,Top4,Top5,F6,Top7,Top8,F9],
    [F1,F2,Bot3,F4,F5,Bot6,F7,F8,Bot9],
    [R7,R4,R1,R8,R5,R2,R9,R6,R3],
    [Top9,B2,B3,Top6,B5,B6,Top3,B8,B9],
    Left,
    [Bot1,Bot2,B7,Bot4,Bot5,B4,Bot7,Bot8,B1]]
).

rotateRightCounterClockwise(
    [[Top1,Top2,Top3,Top4,Top5,Top6,Top7,Top8,Top9],
    [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    Left,
    [Bot1,Bot2,Bot3,Bot4,Bot5,Bot6,Bot7,Bot8,Bot9]]
    ,
    [[Top1,Top2,B7,Top4,Top5,B4,Top7,Top8,B1],
    [F1,F2,Top3,F4,F5,Top6,F7,F8,Top9],
    [R3,R6,R9,R2,R5,R8,R1,R4,R7],
    [Bot9,B2,B3,Bot6,B5,B6,Bot3,B8,B9],
    Left,
    [Bot1,Bot2,F3,Bot4,Bot5,F6,Bot7,Bot8,F9]]
).

rotateBackClockwise(
    [[Top1,Top2,Top3,Top4,Top5,Top6,Top7,Top8,Top9],
    Front,
    [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    [Bot1,Bot2,Bot3,Bot4,Bot5,Bot6,Bot7,Bot8,Bot9]]
    ,
    [[R3,R6,R9,Top4,Top5,Top6,Top7,Top8,Top9],
    Front,
    [R1,R2,Bot9,R4,R5,Bot8,R7,R8,Bot7],
    [B7,B4,B1,B8,B5,B2,B9,B6,B3],
    [Top3,L2,L3,Top2,L5,L6,Top1,L8,L9],
    [Bot1,Bot2,Bot3,Bot4,Bot5,Bot6,L1,L4,L7]]
).

rotateBackCounterClockwise(
    [[Top1,Top2,Top3,Top4,Top5,Top6,Top7,Top8,Top9],
    Front,
    [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    [Bot1,Bot2,Bot3,Bot4,Bot5,Bot6,Bot7,Bot8,Bot9]]
    ,
    [[L7,L4,L1,Top4,Top5,Top6,Top7,Top8,Top9],
    Front,
    [R1,R2,Top1,R4,R5,Top2,R7,R8,Top3],
    [B3,B6,B9,B2,B5,B8,B1,B4,B7],
    [Bot7,L2,L3,Bot8,L5,L6,Bot9,L8,L9],
    [Bot1,Bot2,Bot3,Bot4,Bot5,Bot6,R9,R6,R3]]
).

rotateLeftClockwise(
    [[Top1,Top2,Top3,Top4,Top5,Top6,Top7,Top8,Top9],
    [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    Right,
    [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    [Bot1,Bot2,Bot3,Bot4,Bot5,Bot6,Bot7,Bot8,Bot9]]
    ,
    [[B9,Top2,Top3,B6,Top5,Top6,B3,Top8,Top9],
    [Top1,F2,F3,Top4,F5,F6,Top7,F8,F9],
    Right,
    [B1,B2,Bot7,B4,B5,Bot4,B7,B8,Bot1],
    [L7,L4,L1,L8,L5,L2,L9,L6,L3],
    [F1,Bot2,Bot3,F4,Bot5,Bot6,F7,Bot8,Bot9]]
).

rotateLeftCounterClockwise(
    [[Top1,Top2,Top3,Top4,Top5,Top6,Top7,Top8,Top9],
    [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    Right,
    [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    [Bot1,Bot2,Bot3,Bot4,Bot5,Bot6,Bot7,Bot8,Bot9]]
    ,
    [[F1,Top2,Top3,F4,Top5,Top6,F7,Top8,Top9],
    [Bot1,F2,F3,Bot4,F5,F6,Bot7,F8,F9],
    Right,
    [B1,B2,Top7,B4,B5,Top4,B7,B8,Top1],
    [L3,L6,L9,L2,L5,L8,L1,L4,L7],
    [B9,Bot2,Bot3,B6,Bot5,Bot6,B3,Bot8,Bot9]]
).

rotateBotClockwise(
    [Top,
    [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    [Bot1,Bot2,Bot3,Bot4,Bot5,Bot6,Bot7,Bot8,Bot9]]
    ,
    [Top,
    [F1,F2,F3,F4,F5,F6,L7,L8,L9],
    [R1,R2,R3,R4,R5,R6,F7,F8,F9],
    [B1,B2,B3,B4,B5,B6,R7,R8,R9],
    [L1,L2,L3,L4,L5,L6,B7,B8,B9],
    [Bot7,Bot4,Bot1,Bot8,Bot5,Bot2,Bot9,Bot6,Bot3]]
).

rotateBotCounterClockwise(
    [Top,
    [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    [Bot1,Bot2,Bot3,Bot4,Bot5,Bot6,Bot7,Bot8,Bot9]]
    ,
    [Top,
    [F1,F2,F3,F4,F5,F6,R7,R8,R9],
    [R1,R2,R3,R4,R5,R6,B7,B8,B9],
    [B1,B2,B3,B4,B5,B6,L7,L8,L9],
    [L1,L2,L3,L4,L5,L6,F7,F8,F9],
    [Bot3,Bot6,Bot9,Bot2,Bot5,Bot8,Bot1,Bot4,Bot7]]
).