% 1 
prvek(H, [H|_]) :- !.
prvek(H, [_|T]) :- prvek(H, T).

rozdil([], _, []). 
rozdil([H|T], S, R) :- prvek(H, S) -> rozdil(T, S, R). 
rozdil([H|T], S, [H|P]) :- rozdil(T, S, P).

% 2. N dam
sequence(0, []) :- !.
sequence(N, [N|T]) :- Nprev is N-1, sequence(Nprev,T).

queens(Solution) :- queens(8, Solution).
queens(N, Solution) :- sequence(N, Seq), permutation(Seq, Solution), test(Solution).

test([]) :- !.
test([H|T]) :- test(H, 1, T), test(T).

test(_, _, []) :- !.
test(Pos, Dist, [H|T]) :- Diff is abs(Pos-H), NextDist is Dist + 1, (Diff \= Dist) -> test(Pos, NextDist, T). 

% 3. cesty koně
:- dynamic velikost/2, poz/2.

testPoz(X,Y) :- velikost(XR,YR), X > 0, Y > 0, X =< XR, Y =< YR.

skok(X, Y, XN, YN) :- XN is X + 2, YN is Y + 1, testPoz(XN, YN).
skok(X, Y, XN, YN) :- XN is X + 2, YN is Y - 1, testPoz(XN, YN).
skok(X, Y, XN, YN) :- XN is X - 2, YN is Y + 1, testPoz(XN, YN).
skok(X, Y, XN, YN) :- XN is X - 2, YN is Y - 1, testPoz(XN, YN).
skok(X, Y, XN, YN) :- XN is X + 1, YN is Y + 2, testPoz(XN, YN).
skok(X, Y, XN, YN) :- XN is X - 1, YN is Y + 2, testPoz(XN, YN).
skok(X, Y, XN, YN) :- XN is X + 1, YN is Y - 2, testPoz(XN, YN).
skok(X, Y, XN, YN) :- XN is X - 1, YN is Y - 2, testPoz(XN, YN).

cesta(X, Y, X, Y, [X:Y]) :- !.
cesta(X, Y, XE, YE, [X:Y|T]) :- 
    assertz(poz(X,Y)),
    skok(X,Y, XN, YN),
    \+(poz(XN,YN)),
    cesta(XN, YN, XE, YE, T).
cesta(X, Y, _, _, _) :- retract(poz(X,Y)), !, fail.

cesty(XR, YR, XS, YS, XE, YE, N) :- 
    XR > 0, YR > 1,
    assertz(velikost(XR, YR)),
    testPoz(XS, YS), testPoz(XE, YE),
    findall(Cesta, cesta(XS, YS, XE, YE, Cesta), Cesty),
    length(Cesty, N),
    retract(velikost(_, _)),
    retractall(poz(_, _)).

% 4. asociativní paměť

slovnik(D,_, _) :- var(D), !, fail.
slovnik(_, K ,V) :- var(K), var(V), !, fail.
% % vyhledani hodnoty
slovnik(D, K, V) :- var(V), T=..[D,K,V], call(T).
% % vyhledani klicu
slovnik(D, K, V) :- var(K), T=..[D,K,V], call(T).
% % modifikace
slovnik(D, K, V) :- T=..[D,K,_], retract(T), N=..[D,K,V], assertz(N).
% % vlozeni
slovnik(D, K, V) :- dynamic(D/2), T=..[D,K,V], assertz(T).
