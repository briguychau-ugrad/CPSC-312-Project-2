/*
 * CPSC 312
 * Project 2 - Clue Assistant
 * 
 * Brian Chau 30006118 b8z7
 * Daniel Lu 75592063 a7e7
 *
 */
 
:- dynamic currplayer/1, hascard/2, nocard/2, playerorder/2, totalplayers/1, me/1, accuse/1.

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

/* Keeping track of turns */
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
teststart :- numplayers(3),addplayer(missscarlet),addplayer(mrswhite),addplayer(mrgreen),myplayer(mrgreen),addcard(mrgreen,kitchen),addcard(mrgreen,ballroom),addcard(mrgreen,hall),addcard(mrgreen,mrgreen),addcard(mrgreen,professorplum),addcard(mrgreen,rope),start.

testend :- numplayers(3),addplayer(missscarlet),addplayer(mrswhite),addplayer(mrgreen),myplayer(mrgreen),addcard(mrgreen,kitchen),addcard(mrgreen,ballroom),addcard(mrgreen,hall),addcard(mrgreen,mrgreen),addcard(mrgreen,professorplum),addcard(mrgreen,rope),addcard(mrswhite,knife),addcard(mrswhite,candlestick),addcard(mrswhite,conservatory),addcard(mrswhite,diningroom),addcard(mrswhite,library),addcard(mrswhite,mrspeacock),addcard(missscarlet,wrench),addcard(missscarlet,leadpipe),addcard(missscarlet,study),addcard(missscarlet,lounge),addcard(missscarlet,missscarlet),addcard(missscarlet,mrswhite),start.

testalmostend :- numplayers(3),addplayer(missscarlet),addplayer(mrswhite),addplayer(mrgreen),myplayer(mrgreen),addcard(mrgreen,kitchen),addcard(mrgreen,ballroom),addcard(mrgreen,hall),addcard(mrgreen,mrgreen),addcard(mrgreen,professorplum),addcard(mrgreen,rope),addcard(mrswhite,knife),addcard(mrswhite,candlestick),addcard(mrswhite,conservatory),addcard(mrswhite,diningroom),addcard(mrswhite,library),addcard(mrswhite,mrspeacock),addcard(missscarlet,wrench),addcard(missscarlet,leadpipe),addcard(missscarlet,study),addcard(missscarlet,lounge),addcard(missscarlet,missscarlet),start.
/* need colonelmustard false. use mysuggestfalse(hall, colonelmustard, rope). */
/* need mrswhite true. use mysuggesttrue(hall, mrswhite, rope, missscarlet, mrswhite). */

/* General predicates */
card(X) :- weapon(X).
card(X) :- room(X).
card(X) :- suspect(X).

/* A player is also a suspect */
player(X) :- suspect(X).

/* Specifying number of players */
numplayers(N) :- N>2,N<7,!,retract(totalplayers(_)),assert(totalplayers(N)).

addplayer(X) :- player(X),!,not(playerorder(X,_)),!,currplayer(N),totalplayers(Z),N<Z,!,Q is N + 1,assert(playerorder(X,Q)),assert(currplayer(Q)),retract(currplayer(N)).

/* Specify which player you are */
myplayer(X) :- playerorder(X,_),not(me(_)),assert(me(X)).

/* Adding a card */
addcard(P,C) :- nocard(Q,C),not(Q=P),retract(nocard(Q,C)),!,addcard(P,C).
addcard(P,C) :- not(nocard(_,C)),not(hascard(_,C)),player(P),!,assert(hascard(P,C)).

/* Doesn't have a card */
doesnthave(_,C) :- hascard(_,C),!.
doesnthave(P,C) :- not(hascard(_,C)),not(nocard(P,C)),assert(nocard(P,C)),!.

/* May have card: either he has it or it's the solution */
mighthave(_,C) :- 

start :- me(_),currplayer(N),totalplayers(M),N=:=M,retract(currplayer(_)),assert(currplayer(1)).

/* Display order of turns */
displayorder :- printorder(1).
printorder(N) :- totalplayers(X),X<N,!,me(A),write('My character: '),write_ln(A),!.
printorder(N) :- totalplayers(X),B is X + 1,B>N,Q is N + 1,!,playerorder(A,N),!,write(N),write(' - '),write_ln(A),!,printorder(Q).

/* I made a suggestion, and nobody proved me wrong */
mysuggestfalse(R,S,W) :- room(R),suspect(S),weapon(W),!,me(M),!,haveorassert(M,R),haveorassert(M,S),haveorassert(M,W),!.

/* I made a suggestion, and somebody proved me wrong */
/*A is where I am.
C is where the other player is.
B is what I want to check.
D is total players*/
mysuggesttrue(R,S,W,P,K) :- room(R),suspect(S),weapon(W),!,me(M),playerorder(M,A),playerorder(P,C),!,totalplayers(D),B is 1 + A mod D,recursesuggest(R,S,W,B,C,K),!.

recursesuggest(K,S,W,B,C,K) :- B=\=C,playerorder(P,B),doesnthave(P,S),doesnthave(P,W),totalplayers(D),A is 1 + B mod D,recursesuggest(K,S,W,A,C,K).
recursesuggest(R,K,W,B,C,K) :- B=\=C,playerorder(P,B),doesnthave(P,R),doesnthave(P,W),totalplayers(D),A is 1 + B mod D,recursesuggest(R,K,W,A,C,K).
recursesuggest(R,S,K,B,C,K) :- B=\=C,playerorder(P,B),doesnthave(P,R),doesnthave(P,S),totalplayers(D),A is 1 + B mod D,recursesuggest(R,S,K,A,C,K).
recursesuggest(_,_,_,B,C,K) :- B=:=C,playerorder(P,C),assert(hascard(P,K)).

/* Someone else made a suggested, and nobody proved them wrong */
othersuggestfalse(R,S,W,P) :- 

/* Given a card, checks either the given player has the card, or asserts that it's the correct accusation */
haveorassert(P,C) :- hascard(P,C).
haveorassert(P,C) :- not(hascard(P,C)),assert(accuse(C)).

/* Give an accusation */
accusation(R,S,W) :- update,accuse(R),room(R),accuse(S),suspect(S),accuse(W),weapon(W),!.

/* Print all known facts */
printallknown :- hascard(P,C),write(P),write(' has '),write_ln(C).
printallknown :- nocard(P,C),write(P),write(' doesn\'t have '),write_ln(C).

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









