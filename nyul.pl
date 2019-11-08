% Nemreg uj gyerek rajzfilm-sorozat indult a Gyerek-TV musoran,
% Nyuszi-csalad cimmel. A megadott informaciok alapjan allapitsa meg,
% hogy az egyes csaladtagok hany evesek, es hogy kinek a hangjan
% szolalnak meg.
% A rajzfilmfigura es a hangja nem feltetlenul azonos
% nemu. Bubi Nyuszi es Dundi nyuszi lanyok, Cumi nyuszi és Muki nyuszi
% fiuk.

solution(Nyuszicsalad) :-

mezei(_, Nyuszicsalad),
horvath(_, Nyuszicsalad),
lelkes(_, Nyuszicsalad),
kovacs(_,Nyuszicsalad),

szinkron(A, Nyuszicsalad), keresztnev(A, adel),
szinkron(B, Nyuszicsalad), keresztnev(B, gabor),
szinkron(C, Nyuszicsalad), keresztnev(C, patrik),
szinkron(D, Nyuszicsalad), keresztnev(D, sarolta),

szinkron(AA, Nyuszicsalad), nyuszi(AA, bubi_nyuszi),
szinkron(BB, Nyuszicsalad), nyuszi(BB, cumi_nyuszi),
szinkron(CC, Nyuszicsalad), nyuszi(CC, dundi_nyuszi),
szinkron(DD, Nyuszicsalad), nyuszi(DD, muki_nyuszi),

szinkron(AAA, Nyuszicsalad), eletkor(AAA, 2),
szinkron(BBB, Nyuszicsalad), eletkor(BBB, 3),
szinkron(CCC, Nyuszicsalad), eletkor(CCC, 4),
szinkron(DDD, Nyuszicsalad), eletkor(DDD, 5),


% 1.Ha osszeadjuk az Adel és Gabor hangjan megszolalo nyuszik eletkorat,
% ugyanazt a szamot kapjuk, mint amikor a Patrik és Sarolta altal eletre
% keltett nyuszik korat adjuk ossze.
 keresztnev(E,adel), szinkron(E,Nyuszicsalad), keresztnev(F,gabor),  szinkron(F,Nyuszicsalad), keresztnev(G,patrik),  szinkron(G,Nyuszicsalad), keresztnev(H,sarolta), szinkron(H,Nyuszicsalad),eletkor_osszeg(E,F,G,H),

% 2.Mezei keresztneve a listaban kozvetlenul megelozi annak a fiunak a
% keresztnevet, aki Bubi Nyuszinak kolcsonzi a hangjat.
 mezei(I,Nyuszicsalad),szinkron(I,Nyuszicsalad),nyuszi(J,bubi_nyuszi), szinkron(J,Nyuszicsalad), next(I,J,Nyuszicsalad), megelozfiu(I,J),

% 3.A Lelkes vezeteknevu lany Muki Nyuszi "szinkronja", "aki"
% fiatalabb,mint Dundi Nyuszi.
 lelkes(K,Nyuszicsalad), szinkron(K, Nyuszicsalad), nyuszi(K,muki_nyuszi), lany([adel,sarolta],K), szinkron(L, Nyuszicsalad), nyuszi(L,dundi_nyuszi), fiatalabb(K,L),

% 4. Gabor egy fiu-nyuszinak adta a hangjat, aki vagy 3, vagy 5
% eves.
 szinkron(M,Nyuszicsalad), keresztnev(M,gabor), fiunyuszi([cumi_nyuszi,muki_nyuszi],M), vagy([3,5],M),

% 5. Horvath keresztneve kozvetlenul megelozi annak a gyerekszinesznek a
%keresztnevet, aki a 2 eves nyuszinak kolcsonzi a hangjat.
 horvath(N,Nyuszicsalad), szinkron(N,Nyuszicsalad),szinkron(P,Nyuszicsalad), eletkor(P,2), next(N,P,Nyuszicsalad), megeloz(N,P).


szinkron(X, nyuszicsalad(X,_,_,_)).
szinkron(X, nyuszicsalad(_,X,_,_)).
szinkron(X, nyuszicsalad(_,_,X,_)).
szinkron(X, nyuszicsalad(_,_,_,X)).

vezeteknev(szinkron(X,_,_,_), X).
keresztnev(szinkron(_,X,_,_), X).
nyuszi(szinkron(_,_,X,_), X).
eletkor(szinkron(_,_,_,X), X).

mezei(X, nyuszicsalad(X,_,_,_)) :- vezeteknev(X, mezei). % Lerögzitem a vezetékneveket.
horvath(X, nyuszicsalad(_,X,_,_)) :- vezeteknev(X, horvath).
lelkes(X, nyuszicsalad(_,_,X,_)) :- vezeteknev(X, lelkes).
kovacs(X, nyuszicsalad(_,_,_,X)) :- vezeteknev(X, kovacs).

next(X,Y, H) :- right(X,Y,H). % Közvetlen követik-e egymást.
right(X,Y, nyuszicsalad(X,Y,_,_)).
right(X,Y, nyuszicsalad(_,X,Y,_)).
right(X,Y, nyuszicsalad(_,_,X,Y)).


fiatalabb(szinkron(_,_,_,2), szinkron(_,_,_,3)). % az elso a másodiknál fiatalabb-e.
fiatalabb(szinkron(_,_,_,2), szinkron(_,_,_,4)).
fiatalabb(szinkron(_,_,_,2), szinkron(_,_,_,5)).
fiatalabb(szinkron(_,_,_,3), szinkron(_,_,_,4)).
fiatalabb(szinkron(_,_,_,3), szinkron(_,_,_,5)).
fiatalabb(szinkron(_,_,_,4), szinkron(_,_,_,5)).

megeloz(szinkron(_,adel,_,_), szinkron(_,gabor,_,_)). % Keresztnevek egymást követik-e.
megeloz(szinkron(_,gabor,_,_), szinkron(_,patrik,_,_)).
megeloz(szinkron(_,patrik,_,_), szinkron(_,sarolta,_,_)).

megelozfiu(szinkron(_,adel,_,_), szinkron(_,gabor,_,_)). % Keresztneveknél fiuk követik-e egymást.
megelozfiu(szinkron(_,gabor,_,_), szinkron(_,patrik,_,_)).

eletkor_osszeg(X,Y,Z,W) :- eletkor(X, 2), eletkor(Y, 5), eletkor(Z, 3),eletkor(W, 4). % Életkorok összeadásának összes lehetséges elofordulása.
eletkor_osszeg(X,Y,Z,W) :- eletkor(X, 2), eletkor(Y, 5), eletkor(Z, 4),eletkor(W, 3).
eletkor_osszeg(X,Y,Z,W) :- eletkor(X, 5), eletkor(Y, 2), eletkor(Z, 3),eletkor(W, 4).
eletkor_osszeg(X,Y,Z,W) :- eletkor(X, 5), eletkor(Y, 2), eletkor(Z, 4),eletkor(W, 3).
eletkor_osszeg(X,Y,Z,W) :- eletkor(X, 3), eletkor(Y, 4), eletkor(Z, 2),eletkor(W, 5).
eletkor_osszeg(X,Y,Z,W) :- eletkor(X, 3), eletkor(Y, 4), eletkor(Z, 5),eletkor(W, 2).
eletkor_osszeg(X,Y,Z,W) :- eletkor(X, 4), eletkor(Y, 3), eletkor(Z, 2),eletkor(W, 5).
eletkor_osszeg(X,Y,Z,W) :- eletkor(X, 4), eletkor(Y, 3), eletkor(Z, 5),eletkor(W, 2).

vagy([H|_],Nyuszicsalad) :- eletkor(Nyuszicsalad, H).
vagy([_|T],Nyuszicsalad) :- vagy(T,Nyuszicsalad). % vagy 3, vagy pedig 5 éves.

lany([H|_],Nyuszicsalad) :- keresztnev(Nyuszicsalad, H).
lany([_|T],Nyuszicsalad) :- lany(T,Nyuszicsalad). % Lány, tehát vagy Adél vagy Sarolta.

fiunyuszi([H|_],Nyuszicsalad) :- nyuszi(Nyuszicsalad, H).
fiunyuszi([_|T],Nyuszicsalad) :- fiunyuszi(T,Nyuszicsalad). % Fiu nyuszi, tehát vagy Muki vagy Cumi.

