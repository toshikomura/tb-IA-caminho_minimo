% -------------------------------------------------------------
% PREDICADOS UTILITARIOS (PROBLEMA DO CAMINHO ENTRE 2 CIDADES)
% -------------------------------------------------------------

% construção do grafo
% ligação entre os nodos e os seus respectivos custos
ligada_a(a, s, 140).
ligada_a(a, t, 118).
ligada_a(a, z, 75).
ligada_a(b, f, 211).
ligada_a(b, g, 90).
ligada_a(b, p, 101).
ligada_a(b, u, 85).
ligada_a(c, d, 120).
ligada_a(c, p, 138).
ligada_a(c, r, 146).
ligada_a(d, c, 120).
ligada_a(d, m, 75).
ligada_a(e, h, 86).
ligada_a(f, b, 211).
ligada_a(f, s, 99).
ligada_a(g, b, 90).
ligada_a(h, e, 86).
ligada_a(h, u, 98).
ligada_a(i, n, 87).
ligada_a(i, v, 92).
ligada_a(l, m, 70).
ligada_a(l, t, 111).
ligada_a(m, d, 75).
ligada_a(m, l, 70).
ligada_a(n, i, 87).
ligada_a(o, s, 151).
ligada_a(o, z, 71).
ligada_a(p, b, 101).
ligada_a(p, c, 138).
ligada_a(p, r, 97).
ligada_a(r, c, 146).
ligada_a(r, p, 97).
ligada_a(r, s, 80).
ligada_a(s, a, 140).
ligada_a(s, f, 99).
ligada_a(s, o, 151).
ligada_a(s, r, 80).
ligada_a(t, a, 118).
ligada_a(t, l, 111).
ligada_a(u, b, 85).
ligada_a(u, h, 98).
ligada_a(u, v, 142).
ligada_a(v, i, 92).
ligada_a(v, u, 142).
ligada_a(z, a, 75).
ligada_a(z, o, 71).

% coordenadas de cada nodo
coord(a, 20, 162) :-
!.
coord(b, 230, 45) :-
!.
coord(c, 147, 18) :-
!.
coord(d, 90, 17) :-
!.
coord(e, 292, 22) :-
!.
coord(f, 165, 137) :-
!.
coord(g, 216, 18) :-
!.
coord(h, 290, 60) :-
!.
coord(i, 242, 185) :-
!.
coord(l, 74, 65) :-
!.
coord(m, 37, 77) :-
!.
coord(n, 195, 205) :-
!.
coord(o, 44, 210) :-
!.
coord(p, 170, 73) :-
!.
coord(r, 105, 115) :-
!.
coord(s, 98, 137) :-
!.
coord(t, 20, 93) :-
!.
coord(u, 254, 60) :-
!.
coord(v, 275, 120) :-
!.
coord(z, 26, 88).
% final da construção do grafo

% memoria dinamica para estado inicial, estado final e folhas
:- dynamic e_estado_inicial/2.
:- dynamic e_estado_final/2.
:- dynamic folhas_da_arvore/2.

% função que checa se um nodo já esta ao caminho
pertence_a( X, [ X| _]) :-
    !.

pertence_a( X, [ _| C]) :-
    pertence_a( X, C).

% função que verifica se o nodo já existe na trajetoria
produz_ciclo( Nodo, Trajetoria) :-
    !,
    pertence_a( r( _, Nodo), Trajetoria).

% função que encontra o menor custo dos nodos que esta nas folhas
encontra_menor_custo( X, [ X]).

encontra_menor_custo( Menor, [ X, Y | Resto]) :-
    X =< Y,
    !,
    encontra_menor_custo( Menor, [ X| Resto]).

encontra_menor_custo( Menor,[ _, Y| Resto]) :-
    econtra_menor_custo( Menor, [ Y| Resto]).

% função que encontra o maior custo dos nodos que esta nas folhas
encontra_maior_custo( X, [ X]).

encontra_maior_custo( Maior, [ X, Y| Resto]) :-
    X >= Y,
    !,
    encontra_maior_custo( Maior, [ X| Resto]).

encontra_maior_custo( Maior, [ _, Y| Resto]) :-
    encontra_maior_custo( Maior, [ Y| Resto]).

% função que verifica se o último nodo é o estado final
encontrou_o_estado_final( t( _, _, [ r( _, Nodo)| _])) :-
    e_estado_final( Nodo).

% função que verifica se o custo do filho não ultrapassau o custo fixo
nodo_esta_no_limite ( t( Custo_Caminho, _, _), Custo_Fixo) :-
    Custo_Caminho =< Custo_Fixo,
    !.

% função para incluir um nodo a+ nas folhas
inclui_nodo_nas_folhas( Novo_Nodo) :-
    folhas( Folhas), % pega todas as folhas
    retract( folhas_da_arvore( Folhas)), % deleta as folhas da memória
    assertz( folhas_da_arvore( [ Novo_Nodo| Folhas])), % concatena o novo nodo na frente das folhas e coloca na memória
    !.

% função para deletar um nodo a+ nas folhas
deleta_nodo_nas_folhas( Nodo_Removido) :-
    folhas( [ Nodo_Removido | Folhas]), % pega todas as folhas com o nodo a ser deletado
    retract( folhas_da_arvore( [ Nodo_Removido | Folhas])), % deleta as folhas e o nodo a ser deletado da memória
    assertz( folhas_da_arvore( Folhas)), % coloca só as folhas na memória
    !.

% função que atualiza o limite de custo fixo
atualiza_limite_fixo( Novo_Limite_Fixo) :-
    !,
    folhas_da_arvore( Folhas),
    encontra_menor( Novo_Limite_Fixo, Folhas),
    retract( folhas_da_arvore( Folhas)),
    assertz( folhas_da_arovre( [ ])).

% função que calcula o valor de H
funcao_h( Nodo_Atual, Limite_Nodo) :-
    e_estado_final( Nodo_Final), % pega o estado final
    coord( Nodo_Final, XEf, YEf), % procura as coordenadas do estado final
    coord( Nodo_Atual, XEa, YEa), % procura as coordenadas do estado incial
    X is ( XEa - XEf)*( XEa - XEf) + ( YEa - YEf)*( YEa - YEf), % calcula a distancia euclidiana
    prolog_eval( sqrt( X), Limite_Nodo), % termina o calcula
    !.

% função que calcula (F = G + H) e compara F =< Limite Fixo
inseridas( Limite_Fixo, t( F, G, [ r( Ramo, Nodo)| Resto]), t( FN, G_aux, [ r( 'vai para', Filho), r( Ramo, Nodo) | Resto])) :-
    ligada_a( Nodo, Filho, Custo_Caminho),
    not( produz_ciclo( Filho, [ r( Ramo, Nodo) | Resto])),
    G_aux is G + Custo_Caminho,
    funcao_h( Filho,  Custo_Caminho_Filho_Nodo_Final),
    F_aux is G_aux + Custo_Caminho_Filho_Nodo_Final,
    maior( FN, [ F_aux, F]),
    insere_f_folhas( FN),
    FN =< Limite_Fixo,
    remove_f_folhas( FN).

% função que caminha pela arvore baseando-se no Limite Fixo
verifica_Menor_Caminho( Limite_Fixo, Trajetoria_do_Menor_Caminho_S, Trajetoria_do_Menor_Caminho_S) :-
    nodo_esta_no_limite( Limite_Fixo, Trajetoria_do_Menor_Caminho_S, Trajetoria_do_Menor_Caminho_S),
    econtrou_o_estado_final( Trajetoria_do_Menor_Caminho_S),
    !.

verifica_Menor_Caminho( Limite_Fixo, Trajetoria, Trajetoria_do_Menor_Caminho_S) :-
    inseridas( Limite_Fixo, Trajetoria, Pre_Trajetoria_do_Menor_Caminho_S),
    verifica_Menor_Caminho( Limite_Fixo, Pre_Trajetoria_do_Menor_Caminho_S, Trajetoria_do_Menor_Caminho_S).

% função que chama a caminhada pela arvore e atualiza o Limite Fixo
calcula_Menor_Caminho( Limite_Fixo, t( Limite_Fixo_Inicial, 0, [ r( raiz, Nodo_Inicial)]), Trajetoria_do_Menor_Caminho_S) :-
    verifica_Menor_Caminho( Limite_Fixo, t( Limite_Fixo_Inicial, 0, [ r ( raiz, Nodo_Inicial)]), Trajetoria_do_Menor_Caminho_S).

calcula_Menor_Caminho( _, _,Trajetoria_do_Menor_Caminho_S) :-
    atualiza_limite( Novo_Limite_Fixo),
    e_estado_inicial( Nodo_Inicial, Limite_Nodo_Inicial),
    calcula_Menor_Caminho( Novo_Limite_Fixo, t( Limite_Fixo_Inicial, 0, [ r ( raiz, Nodo_Inicial)]), Trajetoria_do_Menor_Caminho_S),
    !.

% função para impressão do caminho minimo
trajetoria_impressa( t( F, G, T)) :-
    trajetoria_impressa_1( T).

trajetoria_impressa_1( [ r( raiz, Raiz) ]) :-
    write( 'Estado Inicial: '),
    write( Raiz),
    write( '. \n'),
    !.

trajetoria_impressa_1( [ r( Operacao, Nodo) | Resto ]) :-
    trajetoria_impressa_1( Resto),
    write( Operacao),
    write( ' a cidade: '),
    write( Nodo),
    write( '. \n').
% fim da função de impressão do caminho minimo

% -------------------------------------------------------------
% BUSCA HEURISTICA PELO IDA*
% -------------------------------------------------------------
ida( Estado_Inicial, Estado_Final) :-

    % procura quem esta ligado ao esta incial e depois quem estado ligado ao estado final
    ligada_a( Nodo_Inicial, _, _),
    ligada_a( _, Nodo_Final, _),

    assertz( e_estado_final( Nodo_Final)), % iniciando a memoria que armazena o estado final
    funcao_h( Nodo_Inicial, Limite_Nodo_Inicial),
    assertz( e_estado_inicial( Nodo_Inicial, Limite_Nodo_Inicial)),
    assertz( folhas_da_arvore( [ ])), % iniciando memoria vazia que armazena todas as folhas

    calcula_Menor_Caminho( Limite_Nodo_Inicial, t( Limite_Nodo_Inicial, 0, [ r ( raiz, Nodo_Inicial)]), Trajetoria_do_Menor_Caminho_S),
    trajetoria_impressa( Trajetoria_do_Menor_Caminho_S),

    % deleta o esta final, estado inicial e folhas da memoria
    folhas( Folhas), % pega as folhas
    retract(folhas_da_arvore( Folhas)), % deleta as folhas
    retract(e_estado_final( Nodo_Final)), % deleta o estado final
    retract(e_estado_inicial( Nodo_Inicial, Limite_Nodo_Incial)), % deleta o estado incial
    !.
