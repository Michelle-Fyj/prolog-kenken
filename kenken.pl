    length_flipped(N, L):- length(L, N).

    domain_flipped(N, L):- fd_domain(L, 1, N).

% a b c
% d e f
% g h i

% a d g
% b e h
% c f i

% transpose the N*N matrix
    transpose_2([], []).
    transpose_2([X|Y], R):- transpose_3(X, [X|Y], R).

    transpose_3([], _, []).
    transpose_3([_|B], M, [ResH|ResT]):- appendTranspose(M, ResH, M1), transpose_3(B, M1, ResT).

    appendTranspose([], [], []).
    appendTranspose([[A|B]|Remain], [A|D], [B|C]):-appendTranspose(Remain, D, C).


    % if there is constrain
    constrains([], _).
    constrains([Head|Tail], T) :- constrains_match(Head, T), constrains(Tail, T).
    constrains_match(+(S, L), T):- sumList(S, L, T).
    constrains_match(*(P, L), T):- productList(P, L, T).
    constrains_match(-(D, J, K), T):- difference(D, J, K, T).
    constrains_match(/(Q, J, K), T):- division(Q, J, K, T).
    
    getValue([I|J], T, V):- nth1(I, T, L), nth1(J, L, V).
    
    sumList(0, [], _).
    sumList(S, [Pos|Rest], T):- getValue(Pos, T, V), sumList(RestSum, Rest, T), S #= RestSum + V.

    productList(1, [], _).
    productList(P, [Pos|Rest], T):- getValue(Pos, T, V), productList(RestPro, Rest, T), P #= RestPro * V.

    difference(D, PosX, PosY, T):- getValue(PosX, T, X), getValue(PosY, T, Y), D #= X-Y.
    difference(D, PosX, PosY, T):- getValue(PosX, T, X), getValue(PosY, T, Y), D #= Y-X.
    
    division(Q, PosX, PosY, T):- getValue(PosX, T, X), getValue(PosY, T, Y), X #= Q*Y.
    division(Q, PosX, PosY, T):- getValue(PosX, T, X), getValue(PosY, T, Y), Y #= Q*X.   
    

    kenken(N, C, T):- 
        length(T, N),
        maplist(length_flipped(N), T),
		maplist(domain_flipped(N), T),
		maplist(fd_all_different, T),
        transpose_2(T, TransT),
	    maplist(fd_all_different, TransT),
		constrains(C, T),		     
		maplist(fd_labeling, T).





% get a matrix of size N*N, with all rows a permutation of N numbers, col rules not restricted
    getDecList(0, []):-!.
    getDecList(N, [N|T]):- M is N-1, getDecList(M, T).



    % if there is constrain
    plain_constrains([], _). 
    plain_constrains([Head|Tail], T) :- plain_constrains_match(Head, T), plain_constrains(Tail, T).
    plain_constrains_match(+(S, L), T):- plain_sumList(S, L, T).
    plain_constrains_match(*(P, L), T):- plain_productList(P, L, T).
    plain_constrains_match(-(_, _, _), _).
    plain_constrains_match(/(_, _, _), _).

    plain_sumList(0, [], _).
    plain_sumList(S, [Pos|Rest], T):- getValue(Pos, T, V), plain_sumList(RestSum, Rest, T), S is RestSum + V.  

    plain_productList(1, [], _).
    plain_productList(P, [Pos|Rest], T):- getValue(Pos, T, V), plain_productList(RestPro, Rest, T), P is RestPro * V.

                                                         


    multiply(Q, A, [A, B]) :- B is Q * A.
    addD(D, A, [A, B]):- B is A + D.

    division_possibilities(Q, R, N) :-
        Limit is floor(N / Q),
        getDecList(Limit, R1),
        maplist(multiply(Q), R1, R).

    difference_possibilities(D, R, N):-
        Limit is N - D,
        getDecList(Limit, R1),
        maplist(addD(D), R1, R).

    checkArithmetic([], _, _).
    checkArithmetic([/(Q, PosJ, PosK)|Tail], T, N):- 
        getValue(PosJ, T, Vj), 
        getValue(PosK, T, Vk), 
        division_possibilities(Q, R, N), 
        member(PermutationJK, R), 
        permutation([Vj, Vk], PermutationJK), 
        checkArithmetic(Tail, T, N).
    checkArithmetic([-(D, PosJ, PosK)|Tail], T, N):-
        getValue(PosJ, T, Vj),
        getValue(PosK, T, Vk),
        difference_possibilities(D, R, N),
        member(PermutationJK, R), 
        permutation([Vj, Vk], PermutationJK), 
        checkArithmetic(Tail, T, N).
    checkArithmetic([+(_, _)|Tail], T, N):- checkArithmetic(Tail, T, N).
    checkArithmetic([*(_, _)|Tail], T, N):- checkArithmetic(Tail, T, N).

% check if all rows in the transposed matrix are permutations of N numbers                                                                   
    plain_kenken(N, C, T):- 
        getDecList(N, DL), 
        length(T, N),
        maplist(length_flipped(N), T),
        checkArithmetic(C, T, N),
        maplist(permutation(DL), T),
        transpose_2(T, TT),
        maplist(permutation(DL), TT),
        plain_constrains(C, T).
