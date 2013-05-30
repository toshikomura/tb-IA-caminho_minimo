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
ligada_a(a,s).
ligada_a(a,t).
ligada_a(a,z).
ligada_a(b,f).
ligada_a(b,g).
ligada_a(b,p).
ligada_a(b,u).
ligada_a(c,d).
ligada_a(c,p).
ligada_a(c,r).
ligada_a(d,c).
ligada_a(d,m).
ligada_a(e,h).
ligada_a(f,b).
ligada_a(f,s).
ligada_a(g,b).
ligada_a(h,e).
ligada_a(h,u).
ligada_a(i,n).
ligada_a(i,v).
ligada_a(l,m).
ligada_a(l,t).
ligada_a(m,d).
ligada_a(m,l).
ligada_a(n,i).
ligada_a(o,s).
ligada_a(o,z).
ligada_a(p,b).
ligada_a(p,c).
ligada_a(p,r).
ligada_a(r,c).
ligada_a(r,p).
ligada_a(r,s).
ligada_a(s,a).
ligada_a(s,f).
ligada_a(s,o).
ligada_a(s,r).
ligada_a(t,a).
ligada_a(t,l).
ligada_a(u,b).
ligada_a(u,h).
ligada_a(u,v).
ligada_a(v,i).
ligada_a(v,u).
ligada_a(z,a).
ligada_a(z,o).
coord(a, 2, 4) :-
!.
coord(b, 4, 2) :-
!.
coord(c, 7, 8) :-
!.
coord(d, 10, 1) :-
!.
coord(e, 11, 3) :-
!.
coord(f, 11, 7) :-
!.
coord(g, 5, 6) :-
!.
coord(h, 9, 6) :-
!.
coord(i, 12, 6) :-
!.
coord(l, 12, 6) :-
!.
coord(m, 12, 6) :-
!.
coord(n, 12, 6) :-
!.
coord(o, 12, 6) :-
!.
coord(p, 12, 6) :-
!.
coord(r, 12, 6) :-
!.
coord(s, 12, 6) :-
!.
coord(t, 12, 6) :-
!.
coord(u, 12, 6) :-
!.
coord(v, 12, 6) :-
!.
coord(z, 13, 2).

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
