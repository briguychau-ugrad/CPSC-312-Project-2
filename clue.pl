/*
 * CPSC 312
 * Project 2 - Clue Assistant
 * 
 * Brian Chau 30006118 b8z7
 * Daniel Lu 75592063 a7e7
 *
 */
 
:- dynamic hascard/2, currplayer/1, nocard/2, maybecard/2, oneofcard/2, totalplayers/1, me/1, accuse/1.

/* Define all the weapons */
weapon(knife).
weapon(candlestick).
weapon(revolver).
weapon(rope).
weapon(leadpipe).
weapon(wrench).

/* Define all the rooms */
room(kitchen).
room(ballroom).
room(conservatory).
room(billiardroom).
room(library).
room(study).
room(hall).
room(lounge).
room(diningroom).

/* Define all the suspects */
suspect(colonelmustard).
suspect(missscarlet).
suspect(professorplum).
suspect(mrgreen).
suspect(mrswhite).
suspect(mrspeacock).

/* A player is numbered 1-6. */
player(player1).
player(player2).
player(player3).
player(player4).
player(player5).
player(player6).

/* Playerorder keeps track of the order of players */
playerorder(player1,1).
playerorder(player2,2).
playerorder(player3,3).
playerorder(player4,4).
playerorder(player5,5).
playerorder(player6,6).

/* Keeps track of "me" */
me(player1).

/* Keeping track of turns; 0 implies not started */
currplayer(0).

/* Dummy predicates 
playerorder(dummy,0).*/
totalplayers(0).

/* to start game for debug */
/* I have a few cards
	kitchen
	ballroom
	hall
	mrgreen
	professorplum
	rope
*/

/* TESTING CODE COMMENTED OUT

teststart :- numplayers(3),addcard(player1,kitchen),addcard(player1,ballroom),addcard(player1,hall),addcard(player1,mrgreen),addcard(player1,professorplum),addcard(player1,rope),start.

testend :- numplayers(3),addcard(player1,kitchen),addcard(player1,ballroom),addcard(player1,hall),addcard(player1,mrgreen),addcard(player1,professorplum),addcard(player1,rope),addcard(player3,knife),addcard(player3,candlestick),addcard(player3,conservatory),addcard(player3,diningroom),addcard(player3,library),addcard(player3,mrspeacock),addcard(player2,wrench),addcard(player2,leadpipe),addcard(player2,study),addcard(player2,lounge),addcard(player2,missscarlet),addcard(player2,mrswhite),start.

testalmostend :- numplayers(3),addcard(player1,kitchen),addcard(player1,ballroom),addcard(player1,hall),addcard(player1,mrgreen),addcard(player1,professorplum),addcard(player1,rope),addcard(player3,knife),addcard(player3,candlestick),addcard(player3,conservatory),addcard(player3,diningroom),addcard(player3,library),addcard(player3,mrspeacock),addcard(player2,wrench),addcard(player2,leadpipe),addcard(player2,study),addcard(player2,lounge),addcard(player2,missscarlet),start.
*/
/* WANT Solution = billiardroom, colonelmustard, revolver. */
/* need colonelmustard false. use mysuggestfalse(hall, colonelmustard, rope). */
/* need mrswhite true. use mysuggesttrue(hall, mrswhite, rope, player2, mrswhite). */
/* need mrswhite true. use othersuggestfalse(study, colonelmustard, wrench, player2) then othersuggestfalse(library, colonelmustard, knife, player3). */
/* need mrswhite true. use othersuggesttrue(hall, mrswhite, rope, player2, player3). */

/* General predicates */
card(X) :- weapon(X).
card(X) :- room(X).
card(X) :- suspect(X).

/* Specifying number of players */
numplayers(N) :- N>2,N<7,!,retract(totalplayers(_)),assert(totalplayers(N)).

/* Adding a card */
/* hascard/2 is the statement that a player has a given card */
addcard(P,C) :- maybecard(Q,C),not(Q=P),false,!.
addcard(P,C) :- maybecard(P,C),retract(maybecard(P,C)),!,addcard(P,C).
addcard(P,C) :- nocard(Q,C),not(Q=P),retract(nocard(Q,C)),!,addcard(P,C).
addcard(P,C) :- oneofcard(P,L),member(C,L),retract(oneofcard(P,L)),!,addcard(P,C).
addcard(P,C) :- oneofcard(Q,L),member(C,L),not(P=Q),select(C,L,N),retract(oneofcard(Q,L)),hasoneof(Q,N),!,addcard(P,C).
addcard(P,C) :- not(nocard(_,C)),not(hascard(_,C)),not(oneofcard(_,L)),member(C,L),player(P),assert(hascard(P,C)),!.

/* Doesn't have a card */
/* nocard/2 is the statement that a player doesn't have a given card */
doesnthave(_,C) :- hascard(_,C),!.
doesnthave(P,C) :- maybecard(P,C),retract(maybecard(P,C)),assert(accuse(C)),!.
doesnthave(P,C) :- oneofcard(P,L),member(C,L),select(C,L,N),retract(oneofcard(P,L)),hasoneof(P,N),!,doesnthave(P,C).
doesnthave(P,C) :- not(hascard(_,C)),not(nocard(P,C)),assert(nocard(P,C)),!.

/* May have card: either he has it or it's the solution */
/* maybecard/2 is the statement that a player has a card, or its the soln */
mighthave(P,C) :- maybecard(P,C),!.
mighthave(P,C) :- hascard(P,C),!.
mighthave(P,C) :- oneofcard(Q,L),member(C,L),select(C,L,N),retract(oneofcard(Q,L)),hasoneof(Q,N),!,mighthave(P,C).
mighthave(P,C) :- maybecard(Q,C),not(P=Q),retract(maybecard(Q,C)),assert(accuse(C)),!.
mighthave(P,C) :- nocard(P,C),retract(nocard(P,C)),assert(accuse(C)),!.
mighthave(P,C) :- assert(maybecard(P,C)),!.

/* Has One Of: The given player definitely has one of the following cards. */
/* oneofcard/2 is the statement that a player definitely has one of the cards in the list. */
hasoneof(P,L) :- findall(X,hoohelper1(X,L),A),ofchelper(P,A).

hoohelper1(X,L) :- member(X,L),not(hascard(_,X)),not(maybecard(_,X)).

ofchelper(_,[]) :- !.
ofchelper(P,[X]) :- addcard(P,X),!.
ofchelper(P,L) :- length(L,N),N=\=1,!,assert(oneofcard(P,L)),!.

start :- currplayer(0),retract(currplayer(0)),assert(currplayer(1)).

/* I made a suggestion, and nobody proved me wrong */
mysuggestfalse(R,S,W) :- room(R),suspect(S),weapon(W),!,me(M),!,haveorassert(M,R),haveorassert(M,S),haveorassert(M,W),!.

/* I made a suggestion, and somebody proved me wrong */
/*
A is where I am.
C is where the other player is.
B is what I want to check.
D is total players
*/
mysuggesttrue(R,S,W,P,K) :- room(R),suspect(S),weapon(W),!,me(M),playerorder(M,A),playerorder(P,C),!,totalplayers(D),B is 1 + A mod D,recursesuggest(R,S,W,B,C,K),!.

/*
We check current position A
B is where we check next
*/
recursesuggest(K,S,W,A,C,K) :- A=\=C,playerorder(P,A),doesnthave(P,S),doesnthave(P,W),totalplayers(D),B is 1 + A mod D,recursesuggest(K,S,W,B,C,K).
recursesuggest(R,K,W,A,C,K) :- A=\=C,playerorder(P,A),doesnthave(P,R),doesnthave(P,W),totalplayers(D),B is 1 + A mod D,recursesuggest(R,K,W,B,C,K).
recursesuggest(R,S,K,A,C,K) :- A=\=C,playerorder(P,A),doesnthave(P,R),doesnthave(P,S),totalplayers(D),B is 1 + A mod D,recursesuggest(R,S,K,B,C,K).
recursesuggest(_,_,_,A,C,K) :- A=:=C,playerorder(P,C),assert(hascard(P,K)).

/* Someone else made a suggested, and nobody proved them wrong */
othersuggestfalse(R,S,W,P) :- room(R),suspect(S),weapon(W),!,playerorder(P,_),!,mighthave(P,R),mighthave(P,S),mighthave(P,W),!.

/* Someone else made a suggestion, and someone (NOT ME) proved them right */
/*
A is where PS - playersuggested is
C is where PP - playerproved is
B is what I want to check
D is total players
*/
othersuggesttrue(R,S,W,PS,PP) :- room(R),suspect(S),weapon(W),!,playerorder(PS,A),playerorder(PP,C),!,totalplayers(D),B is 1 + A mod D,recursesuggestother(R,S,W,B,C),!.

/*
We check current position A
B is where we check next
*/
recursesuggestother(R,S,W,A,C) :- A=\=C,playerorder(M,A),me(M),!,totalplayers(D),B is 1 + A mod D,recursesuggestother(R,S,W,B,C).
recursesuggestother(R,S,W,A,C) :- A=\=C,playerorder(P,A),doesnthave(P,R),doesnthave(P,S),doesnthave(P,W),totalplayers(D),B is 1 + A mod D,recursesuggestother(P,S,W,B,C).
recursesuggestother(_,_,_,A,C) :- A=:=C,playerorder(M,A),me(M),!.
recursesuggestother(R,S,W,A,C) :- A=:=C,playerorder(P,A),sort([R,S,W],X),hasoneof(P,X).
/* TODO Finish this function thing */

/* Given a card, checks either the given player has the card, or asserts that it's the correct accusation */
haveorassert(P,C) :- hascard(P,C).
haveorassert(P,C) :- not(hascard(P,C)),assert(accuse(C)).

/* Give an accusation */
accusation(R,S,W) :- update,accuse(R),room(R),accuse(S),suspect(S),accuse(W),weapon(W),!.

/* Give a suggestion */
suggestion(R,S,W) :- update,findall(X,accuse(X),L),length(L,3),write_ln('make accusation instead'),!,accusation(R,S,W).
suggestion(R,S,W) :- update,findall(X,accuse(X),L),length(L,2),suggestion2(R,S,W),!.
suggestion(R,S,W) :- update,findall(X,accuse(X),L),length(L,1),suggestion1(R,S,W),!.
suggestion(R,S,W) :- update,findall(X,accuse(X),L),length(L,0),room(R),not(hascard(_,R)),suspect(S),not(hascard(_,S)),weapon(W),not(hascard(_,W)),!.

suggestion2(R,S,W) :- room(RX),accuse(RX),suspect(SX),accuse(SX),!,suggestionhelperR(R,1),suggestionhelperS(S,1),weapon(W),not(hascard(_,W)),!.
suggestion2(R,S,W) :- room(RX),accuse(RX),weapon(WX),accuse(WX),!,suggestionhelperR(R,1),suggestionhelperW(W,1),suspect(S),not(hascard(_,S)),!.
suggestion2(R,S,W) :- suspect(SX),accuse(SX),weapon(WX),accuse(WX),!,suggestionhelperS(S,1),suggestionhelperW(W,1),room(R),not(hascard(_,R)),!.

suggestion1(R,S,W) :- room(RX),accuse(RX),!,suggestionhelperR(R,1),suspect(S),not(hascard(_,S)),weapon(W),not(hascard(_,W)),!.
suggestion1(R,S,W) :- suspect(SX),accuse(SX),!,suggestionhelperS(S,1),room(R),not(hascard(_,R)),weapon(W),not(hascard(_,W)),!.
suggestion1(R,S,W) :- weapon(WX),accuse(WX),!,suggestionhelperW(W,1),room(R),not(hascard(_,R)),suspect(S),not(hascard(_,S)),!.

suggestionhelperR(R,O) :- playerorder(P,O),hascard(P,R),room(R),!.
suggestionhelperR(R,O) :- playerorder(P,O),room(R),not(hascard(P,R)),!,totalplayers(N),M is N - 2,A is (O + M) mod N + 1,!,suggestionhelperR(R,A).

suggestionhelperS(S,O) :- playerorder(P,O),hascard(P,S),suspect(S),!.
suggestionhelperS(S,O) :- playerorder(P,O),suspect(S),not(hascard(P,S)),!,totalplayers(N),M is N - 2,A is (O + M) mod N + 1,!,suggestionhelperS(S,A).

suggestionhelperW(W,O) :- playerorder(P,O),hascard(P,W),weapon(W),!.
suggestionhelperW(W,O) :- playerorder(P,O),weapon(W),not(hascard(P,W)),!,totalplayers(N),M is N - 2,A is (O + M) mod N + 1,!,suggestionhelperW(W,A).

/* Print all known facts */
printallknown :- hascard(P,C),write(P),write(' has '),write_ln(C).
printallknown :- nocard(P,C),write(P),write(' doesn\'t have '),write_ln(C).
printallknown :- oneofcard(P,L),write(P),write(' must have one of '),write_ln(L).
printallknown :- maybecard(P,C),write(P),write(' either has '),write(C),write_ln(' or it is accused.').

/* Updates all accusations */
update :- ignore(updateroom),ignore(updatesuspect),ignore(updateweapon),true,!.

updateroom :- room(R),accuse(R),!.
updateroom :- room(R),findall(X,nocard(X,R),Z),totalplayers(N),length(Z,N),!,assert(accuse(R)),!.
updateroom :- room(R),not(accuse(R)),!,room(X),not(hascard(_,X)),findall(A,room(A),B),filterout(B,X,Y),length(Y,8),!,assert(accuse(X)).

updatesuspect :- suspect(S),accuse(S),!.
updatesuspect :- suspect(S),findall(X,nocard(X,S),Z),totalplayers(N),length(Z,N),!,assert(accuse(S)),!.
updatesuspect :- suspect(S),not(accuse(S)),!,suspect(X),not(hascard(_,X)),findall(A,suspect(A),B),filterout(B,X,Y),length(Y,5),!,assert(accuse(X)).

updateweapon :- weapon(W),accuse(W),!.
updateweapon :- weapon(W),findall(X,nocard(X,W),Z),totalplayers(N),length(Z,N),!,assert(accuse(W)),!.
updateweapon :- weapon(W),not(accuse(W)),!,weapon(X),not(hascard(_,X)),findall(A,weapon(A),B),filterout(B,X,Y),length(Y,5),!,assert(accuse(X)).

filterout([X],X,[]).
filterout([X|Y],X,Y) :- not(hascard(_,X)),allhas(Y).
filterout([X|XS],Y,[X|ZS]) :- not(X=Y),hascard(_,X),filterout(XS,Y,ZS).

allhas([]).
allhas([X|XS]) :- hascard(_,X),allhas(XS).
