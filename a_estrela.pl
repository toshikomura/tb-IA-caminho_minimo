% -------------------------------------------------------------
% PREDICADOS UTILITARIOS (PROBLEMA DO CAMINHO ENTRE 2 CIDADES)
% -------------------------------------------------------------
pertence_a(X, [X|_]) :-
!.
pertence_a(X, [_|C]) :-
pertence_a(X, C).
trajetoria_impressa(t(F,G,T)) :-
trajetoria_impressa_1(T).
trajetoria_impressa_1([ r(raiz,Raiz) ]) :-
write('Estado Inicial: '),
write(Raiz),
write('. \n'),
!.
trajetoria_impressa_1([ r(Operacao,Nodo) | Resto ]) :-
trajetoria_impressa_1(Resto),
write(Operacao),
write(' a cidade: '),
write(Nodo),
write('. \n').
ligada_a(a,b).
ligada_a(m,f).
ligada_a(q,p).
ligada_a(p,s).
ligada_a(c,d).
ligada_a(d,n).
ligada_a(n,h).
ligada_a(h,g).
ligada_a(b,m).
ligada_a(f,q).
ligada_a(p,n).
ligada_a(b,c).
ligada_a(d,q).
ligada_a(d,g).
ligada_a(n,s).
coord(a, 2, 4) :-
!.
coord(c, 4, 2) :-
!.
coord(f, 7, 8) :-
!.
coord(h, 10, 1) :-
!.
coord(n, 11, 3) :-
!.
coord(q, 11, 7) :-
!.
coord(b, 5, 6) :-
!.
coord(d, 7, 4) :-
!.
coord(g, 8, 2) :-
!.
coord(m, 9, 6) :-
!.
coord(p, 12, 6) :-
!.
coord(s, 13, 2).

inserida(T, [], [T]).
inserida(t(F1,G1,Ramos1), [ t(F2,G2,Ramos2) | Resto ],
[ t(F1,G1,Ramos1), t(F2,G2,Ramos2) | Resto ]) :-
F1 < F2,
!.
inserida(t(F1,G1,Ramos1), [ t(F2,G2,Ramos2) | Resto ],
[ t(F2,G2,Ramos2) | Resto2 ]) :-
inserida(t(F1,G1,Ramos1), Resto, Resto2).
inseridas_as_trajetorias([], Fila, Fila).
inseridas_as_trajetorias([T|C], Fila, Fila3) :-
inserida(T,Fila, Fila2),
inseridas_as_trajetorias(C, Fila2, Fila3).
trajetoria_expandida(t(F, G, [r(Ramo,
Nodo)
| Resto]),
t(F1, G1, [r(vai_para, Filho), r(Ramo,Nodo) | Resto])) :-
ligada_a(Nodo, Filho),
not(produz_ciclo(Filho, [r(Ramo,Nodo) | Resto])),
funcao_h(Filho, H1),
custo(Nodo, Filho, Custo),
G1 is G + Custo,
F1 is G1 + H1.
produz_ciclo(Estado, Trajetoria) :-
pertence_a(r(_,Estado), Trajetoria).
custo(Nodo, Filho, Custo) :-
coord(Nodo, XN, YN),
coord(Filho, XF, YF),
X is (XN-XF)*(XN-XF) + (YN-YF)*(YN-YF),
prolog_eval(sqrt(X),Custo),
!.
funcao_h(Ea, H) :-
e_estado_final(Ef),
coord(Ef, XEf, YEf),
coord(Ea, XEa, YEa),
X is (XEa-XEf)*(XEa-XEf) + (YEa-YEf)*(YEa-YEf),
prolog_eval(sqrt(X),H),
!.
adjacentes(X, Y, Z) :-
findall(X, Y, Z),
!.
adjacentes(_ , _ , [] ).
e_estado_final(s).
possui_estado_final(t(_,_,[r(_,Nodo)|_])) :-
e_estado_final(Nodo).
% -------------------------------------------------------------
% BUSCA HEURISTICA PELO A*
% -------------------------------------------------------------
solucao_computada_por_A_estrela([ Solucao | _ ], Solucao) :-
possui_estado_final(Solucao),
!.
solucao_computada_por_A_estrela([ Trajetoria | Fila ], Solucao) :-
adjacentes(T2, trajetoria_expandida(Trajetoria, T2), Lista_de_Trajetorias_Expandidas),
inseridas_as_trajetorias(Lista_de_Trajetorias_Expandidas, Fila, Fila_Expandida),
solucao_computada_por_A_estrela(Fila_Expandida, Solucao).
% *** Exemplo de Estado Inicial: b
solucao_da_busca_por_A_estrela_a_partir_do(Estado_Inicial) :-
funcao_h(Estado_Inicial, H),
solucao_computada_por_A_estrela([ t(H,0,[r(raiz,Estado_Inicial)]) ], Trajetoria),
trajetoria_impressa(Trajetoria).
