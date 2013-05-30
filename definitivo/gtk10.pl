:- prolog_language('pop11').
false -> popmemlim;
false -> pop_prolog_lim;
10e7 -> pop_callstack_lim;
true -> popdprecision;
12 -> pop_pr_places;
:- prolog_language('prolog').

% -------------------------------------------------------------
% PREDICADOS UTILITARIOS (PROBLEMA DO CAMINHO ENTRE 2 CIDADES).
% -------------------------------------------------------------

% OBs: Muitas das funções foram reaproveitadas do código do material da disciplina
% foi acrescentado nas arestas do grafo +1 atributo, que é custo da aresta.

% Construcao do grafo.
% Ligacao entre os nodos e os seus respectivos custos.

ligada_a(a,z,75).
ligada_a(a,t,118).
ligada_a(a,s,140).
ligada_a(z,a,75).
ligada_a(z,o,71).
ligada_a(t,l,111).
ligada_a(t,a,118).
ligada_a(s,r,80).
ligada_a(s,o,151).
ligada_a(s,a,140).
ligada_a(s,f,99).
ligada_a(o,z,71).
ligada_a(o,s,151).
ligada_a(l,m,70).
ligada_a(l,t,111).
ligada_a(r,s,80).
ligada_a(r,p,97).
ligada_a(r,c,146).
ligada_a(f,s,99).
ligada_a(f,b,211).
ligada_a(m,l,70).
ligada_a(m,d,75).
ligada_a(p,r,97).
ligada_a(p,c,138).
ligada_a(p,b,101).
ligada_a(c,d,120).
ligada_a(c,r,146).
ligada_a(c,p,138).
ligada_a(d,m,75).
ligada_a(d,c,120).
ligada_a(b,u,85).
ligada_a(b,f,211).
ligada_a(b,g,90).
ligada_a(b,p,101).
ligada_a(g,b,90).
ligada_a(u,b,85).
ligada_a(u,h,98).
ligada_a(u,v,142).
ligada_a(h,e,86).
ligada_a(h,u,98).
ligada_a(e,h,86).
ligada_a(v,i,92).
ligada_a(v,u,142).
ligada_a(i,n,87).
ligada_a(i,v,92).
ligada_a(n,i,87).

% coordenadas de cada nodo
coord(a,20,160) :-
    !.
coord(z,30,190) :-
    !.
coord(t,20,90) :-
    !.
coord(s,100,140) :-
    !.
coord(o,40,210) :-
    !.
coord(l,70,60) :-
    !.
coord(r,120,100) :-
    !.
coord(f,160,140) :-
    !.
coord(m,80,40) :-
    !.
coord(p,170,70) :-
    !.
coord(c,150,20) :-
    !.
coord(d,90,20):-
    !.
coord(b,230,40):-
    !.
coord(g,220,20):-
    !.
coord(u,250,60):-
    !.
coord(h,290,60):-
    !.
coord(e,290,20):-
    !.
coord(v,270,120):-
    !.
coord(i,240,190):-
    !.
coord(n,190,200):-
    !.
% Final da construcao do grafo.

% Memoria dinamica para estado inicial, estado final e folhas.
:- dynamic e_estado_final/1.
:- dynamic e_estado_inicial/2.
:- dynamic folhas_da_arvore/1.

% Predicado que verifica se um elemento pertence a lista.
pertence_a(X, [ X| _]) :-
    !.
pertence_a(X, [ _| C]) :-
	pertence_a( X, C).

% Predicado que checa se um nodo ja esta no caminho.
produz_ciclo( Estado, Trajetoria) :-
	!,
    pertence_a( r( _, Estado), Trajetoria).

% Predicado que encontra o menor custo dos nodos que esta nas folhas.
encontra_menor_custo( X, [ X]).

encontra_menor_custo( Menor, [ X, Y| R]) :-
	X =< Y,
    !,
	encontra_menor_custo( Menor, [ X| R]).

encontra_menor_custo( Menor,[ _, Y| R]) :-
	encontra_menor_custo( Menor, [ Y| R]).

% Predicado que encontra o maior custo dos nodos que esta nas folhas.
encontra_maior_custo( X, [ X]).

encontra_maior_custo( Maior, [ X, Y| R]) :-
	X >= Y,
    !,
	encontra_maior_custo( Maior, [ X| R]).

encontra_maior_custo( Maior, [ _, Y| R]) :-
	maior( Maior, [ Y| R]).

% Predicado que verifica se o ultimo nodo eh o estado final.
econtrou_o_estado_final( t( _, _, [ r( _, Nodo)| _])) :-
	e_estado_final( Nodo).

% Predicado que verifica se o custo do filho nao ultrapassau o custo fixo.
nodo_esta_no_limite( t( Custo_Caminho, _, _), Limite_Fixo) :-
	Custo_Caminho =< Limite_Fixo,
    !.

% Predicado para incluir um nodo a+ nas folhas.
inclui_nodo_nas_folhas( Novo_Nodo) :-
	folhas_da_arvore( Folhas),
	retract( folhas_da_arvore( Folhas)),
	assertz( folhas_da_arvore([ Novo_Nodo| Folhas])),
    !.

% Predicado para deletar um nodo das folhas.
remove_nodo_das_folhas( Nodo_Removido) :-
	folhas_da_arvore( [ Nodo_Removido| Folhas]),
	retract( folhas_da_arvore([ Nodo_Removido| Folhas])),
	assertz( folhas_da_arvore( Folhas)),
    !.

% Predicado que atualiza o limite de custo fixo.
atualiza_limite_fixo( Novo_Limite_Fixo) :-
	!,
	folhas_da_arvore( Folhas),
	encontra_menor_custo( Novo_Limite_Fixo, Folhas),
	retract( folhas_da_arvore( Folhas)),
	assertz( folhas_da_arvore( [])).

% Predicado que calcula o valor de custo a partir de uma heuristica, no caso a distancia euclidiana.
funcao_h( Nodo_Atual, Custo_Caminho) :-
	e_estado_final( Nodo_Final),
	coord( Nodo_Final, XEf, YEf),
	coord( Nodo_Atual, XEa, YEa),
	X is ( XEa - XEf)*( XEa - XEf) + ( YEa - YEf)*( YEa - YEf),
	Custo_Caminho = sqrt( X),
    !.

% Predicado que calcula (F = G + H) e compara se F =< Limite Fixo, e faz a restauracao do monitonicidade.
inseridas( Limite_Fixo, t( F, G, [ r( Ramo, Nodo)| Resto]), t( F_Maior, G_aux, [ r( 'vai para', Filho), r( Ramo, Nodo) | Resto])) :-
	ligada_a( Nodo, Filho, Custo_Caminho),
	not( produz_ciclo( Filho, [ r( Ramo, Nodo) | Resto])),
	funcao_h( Filho, Custo_Caminho_Filho_Nodo_Final),
	G_aux is G + Custo_Caminho,
	F_aux is G_aux + Custo_Caminho_Filho_Nodo_Final,
	encontra_maior_custo( F_Maior,[ F_aux, F]),
	inclui_nodo_nas_folhas( F_Maior),
	F_Maior =< Limite_Fixo,
	remove_nodo_das_folhas( F_Maior).

% Predicado que caminha pela arvore baseando-se no Limite Fixo.
verifica_menor_caminho( Limite_Fixo, Trajetoria_do_Menor_Caminho_S, Trajetoria_do_Menor_Caminho_S) :-
	nodo_esta_no_limite( Trajetoria_do_Menor_Caminho_S, Limite_Fixo),
 	econtrou_o_estado_final( Trajetoria_do_Menor_Caminho_S),
    !.

verifica_menor_caminho( Limite_Fixo, Trajetoria, Trajetoria_do_Menor_Caminho_S) :-
	inseridas( Limite_Fixo, Trajetoria, Pre_Trajetoria_do_Menor_Caminho_S),
	verifica_menor_caminho(  Limite_Fixo, Pre_Trajetoria_do_Menor_Caminho_S, Trajetoria_do_Menor_Caminho_S).

% Predicado que chama a caminhada pela arvore e atualiza o Limite Fixo.
calcula_menor_caminho( Limite_Fixo, t( Limite_Nodo_Inicial, 0, [ r( raiz, Nodo_Inicial)]), Trajetoria_do_Menor_Caminho_S):-
    verifica_menor_caminho( Limite_Fixo, t( Limite_Nodo_Inicial, 0, [ r( raiz, Nodo_Inicial)]), Trajetoria_do_Menor_Caminho_S).

calcula_menor_caminho( _, _, Trajetoria_do_Menor_Caminho_S) :-
    atualiza_limite_fixo( Novo_Limite_Fixo),
    e_estado_inicial( Nodo_Inicial, Limite_Nodo_Inicial),
    calcula_menor_caminho( Novo_Limite_Fixo, t( Limite_Nodo_Inicial, 0, [ r( raiz, Nodo_Inicial)]), Trajetoria_do_Menor_Caminho_S),
    !.

% Predicado para impressao do caminho minimo.
trajetoria_impressa( t( _, _, T)) :-
	trajetoria_impressa_1( T).

trajetoria_impressa_1( [ r( raiz, Raiz) ]) :-
	write( 'Estado Inicial: '),
	write( Raiz),
	write( '. \n'),!.

trajetoria_impressa_1( [ r( Operacao, Nodo) | Resto ]) :-
	trajetoria_impressa_1( Resto),
	write( Operacao),
	write( ' a cidade: '),
	write( Nodo),
	write( '. \n').
% Fim do predicado de impressao do caminho minimo.

% -------------------------------------------------------------
% BUSCA HEURISTICA ATRAVES DO ALGORITMO IDA *.
% -------------------------------------------------------------
ida( Nodo_Inicial, Nodo_Final) :-
	ligada_a( Nodo_Inicial, _, _),
	ligada_a( _, Nodo_Final, _),
	assertz( folhas_da_arvore( [])),
	assertz( e_estado_final( Nodo_Final)),
	funcao_h( Nodo_Inicial, Limite_Nodo_Inicial),
	assertz(e_estado_inicial( Nodo_Inicial, Limite_Nodo_Inicial)),
	calcula_menor_caminho( Limite_Nodo_Inicial, t( Limite_Nodo_Inicial, 0, [ r( raiz, Nodo_Inicial)]), Trajetoria_do_Menor_Caminho_S),
	trajetoria_impressa( Trajetoria_do_Menor_Caminho_S),
	retract( e_estado_final( Nodo_Final)),
	retract( e_estado_inicial( Nodo_Inicial, Limite_Nodo_Inicial)),
	folhas_da_arvore( Folhas),
	retract( folhas_da_arvore( Folhas)),
    !.
