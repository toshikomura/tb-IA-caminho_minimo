% f_folhas mantem uma pilha com o valor da funcao f(Ex), onde Ex sao folhas do percurso.
pertence_a(X, [X|_]) :-!.
pertence_a(X, [_|C]) :-
	pertence_a(X, C).

trajetoria_impressa(t(_,_,T)) :-
	trajetoria_impressa_1(T).

trajetoria_impressa_1([ r(raiz,Raiz) ]) :-
	write('Estado Inicial: '),
	write(Raiz),
	write('. \n'),!.

trajetoria_impressa_1([ r(Operacao,Nodo) | Resto ]) :-
	trajetoria_impressa_1(Resto),
	write(Operacao),
	write(' a cidade: '),
	write(Nodo),
	write('. \n').

% Restauracao da monitonicidade, maior(FN,FEx,FEpai_x).
trajetoria_expandida(Limite,t(F, G, [r(Ramo,Nodo)| Resto]),t(FN, G1, [r('vai para', Filho), r(Ramo,Nodo) | Resto])) :-
	rota(Nodo,Filho,Custo),
	not(produz_ciclo(Filho, [r(Ramo,Nodo) | Resto])),
	G1 is G + Custo,
	funcao_h(Filho, H1),
	F1 is G1 + H1,
	maior(FN,[F1,F]),
	insere_f_folhas(FN),
	FN =< Limite,
	remove_f_folhas(FN).

insere_f_folhas(F):-
	f_folhas(Lista),
	retract(f_folhas(Lista)),
	assertz(f_folhas([F|Lista])),!.

remove_f_folhas(F1):-
	f_folhas([F1|Lista]),
	retract(f_folhas([F1|Lista])),
	assertz(f_folhas(Lista)),!.

produz_ciclo(Estado, Trajetoria) :-
	!,pertence_a(r(_,Estado), Trajetoria).

funcao_h(Ea, H) :-
	e_estado_final(Ef),
	coord(Ef, XEf, YEf),
	coord(Ea, XEa, YEa),
	X is (XEa-XEf)*(XEa-XEf) + (YEa-YEf)*(YEa-YEf),
	H = sqrt(X),!.

possui_estado_final(t(_,_,[r(_,Nodo)|_])) :-
	e_estado_final(Nodo).

e_limite(t(F,_,_),Limite):-
	F =< Limite,!.

menor(X,[X]).
menor(Min,[X,Y|R]):-
	X =< Y,!,
	menor(Min,[X|R]).
menor(Min,[_,Y|R]):-
	menor(Min,[Y|R]).

maior(X,[X]).
maior(Mai,[X,Y|R]):-
	X >= Y,!,
	maior(Mai,[X|R]).
maior(Mai,[_,Y|R]):-
	maior(Mai,[Y|R]).

novo_limite(NL):-
	!,
	f_folhas(Lista),
	menor(NL,Lista),
	retract(f_folhas(Lista)),
	assertz(f_folhas([])).

caminho_por_IDA(Limite,Solucao, Solucao) :-
	e_limite(Solucao,Limite),
	possui_estado_final(Solucao),!.

caminho_por_IDA(Limite,Trajetoria, Solucao) :-
	trajetoria_expandida(Limite,Trajetoria,T2),
	caminho_por_IDA(Limite,T2,Solucao).

solucao_computada_por_IDA(Limite,t(HEi,0,[r(raiz,Ei)]),Solucao):-
	caminho_por_IDA(Limite,t(HEi,0,[r(raiz,Ei)]),Solucao).

solucao_computada_por_IDA(_,_,Solucao) :-
	novo_limite(NL),
	e_estado_inicial(Ei,HEi),
	solucao_computada_por_IDA(NL,t(HEi,0,[r(raiz,Ei)]),Solucao),!.

% Limite inicial FEi = HEi
ida(Ei,Ef) :-
	rota(Ei,_,_),
	rota(_,Ef,_),
	assertz(f_folhas([])),
	assertz(e_estado_final(Ef)),
	funcao_h(Ei,HEi),
	assertz(e_estado_inicial(Ei,HEi)),
	solucao_computada_por_IDA(HEi,t(HEi,0,[r(raiz,Ei)]),Solucao),
	trajetoria_impressa(Solucao),
	retract(e_estado_final(Ef)),
	retract(e_estado_inicial(Ei,HEi)),
	f_folhas(L),
	retract(f_folhas(L)),!.

rota(a,z,75).
rota(a,t,118).
rota(a,s,140).
rota(z,a,75).
rota(z,o,71).
rota(t,l,111).
rota(t,a,118).
rota(s,r,80).
rota(s,o,151).
rota(s,a,140).
rota(s,f,99).
rota(o,z,71).
rota(o,s,151).
rota(l,m,70).
rota(l,t,111).
rota(r,s,80).
rota(r,p,97).
rota(r,c,146).
rota(f,s,99).
rota(f,b,211).
rota(m,l,70).
rota(m,d,75).
rota(p,r,97).
rota(p,c,138).
rota(p,b,101).
rota(c,d,120).
rota(c,r,146).
rota(c,p,138).
rota(d,m,75).
rota(d,c,120).
rota(b,u,85).
rota(b,f,211).
rota(b,g,90).
rota(b,p,101).
rota(g,b,90).
rota(u,b,85).
rota(u,h,98).
rota(u,v,142).
rota(h,e,86).
rota(h,u,98).
rota(e,h,86).
rota(v,i,92).
rota(v,u,142).
rota(i,n,87).
rota(i,v,92).
rota(n,i,87).

coord(a,20,160) :-!.
coord(z,30,190) :-!.
coord(t,20,90) :-!.
coord(s,100,140) :-!.
coord(o,40,210) :-!.
coord(l,70,60) :-!.
coord(r,120,100) :-!.
coord(f,160,140) :-!.
coord(m,80,40) :-!.
coord(p,170,70) :-!.
coord(c,150,20) :-!.
coord(d,90,20):-!.
coord(b,230,40):-!.
coord(g,220,20):-!.
coord(u,250,60):-!.
coord(h,290,60):-!.
coord(e,290,20):-!.
coord(v,270,120):-!.
coord(i,240,190):-!.
coord(n,190,200):-!.
:- dynamic e_estado_final/1.
:- dynamic e_estado_inicial/2.
:- dynamic f_folhas/1.
