:- prolog_language('pop11').
false -> popmemlim;
false -> pop_prolog_lim;
10e7 -> pop_callstack_lim;
true -> popdprecision;
12 -> pop_pr_places;
:- prolog_language('prolog').

pertence_a(X, [X|_]):-
    !.
pertence_a(X, [_|C]):-
    pertence_a(X,C).

ligada_a(a,z,75).
ligada_a(a,s,140).
ligada_a(a,t,118).
ligada_a(b,g,90).
ligada_a(b,p,101).
ligada_a(b,f,211).
ligada_a(b,u,85).
ligada_a(c,d,120).
ligada_a(c,p,138).
ligada_a(c,r,146).
ligada_a(d,c,120).
ligada_a(d,m,75).
ligada_a(e,h,86).
ligada_a(f,b,211).
ligada_a(f,s,99).
ligada_a(g,b,90).
ligada_a(h,e,86).
ligada_a(h,u,98).
ligada_a(i,n,87).
ligada_a(i,v,92).
ligada_a(l,m,70).
ligada_a(l,t,111).
ligada_a(m,d,75).
ligada_a(m,l,70).
ligada_a(n,i,87).
ligada_a(o,z,71).
ligada_a(o,s,151).
ligada_a(p,c,138).
ligada_a(p,b,101).
ligada_a(p,r,97).
ligada_a(r,c,146).
ligada_a(r,p,97).
ligada_a(r,s,80).
ligada_a(s,a,140).
ligada_a(s,f,99).
ligada_a(s,o,151).
ligada_a(s,r,80).
ligada_a(t,l,111).
ligada_a(t,a,118).
ligada_a(u,b,85).
ligada_a(u,h,98).
ligada_a(u,v,142).
ligada_a(v,i,92).
ligada_a(v,u,142).
ligada_a(z,a,75).
ligada_a(z,o,71).

coord(a,17,162):-!.
coord(b,230,45):-!.
coord(c,148,18):-!.
coord(d,91,16):-!.
coord(e,294,20):-!.
coord(f,164,136):-!.
coord(g,218,17):-!.
coord(h,293,60):-!.
coord(i,242,186):-!.
coord(l,75,64):-!.
coord(m,79,37):-!.
coord(n,194,205):-!.
coord(o,44,210):-!.
coord(p,170,73):-!.
coord(r,116,106):-!.
coord(s,99,138):-!.
coord(t,21,93):-!.
coord(u,254,60):-!.
coord(v,276,119):-!.
coord(z,28,189):-!.

produz_ciclo(Estado, Trajetoria):-
    pertence_a(r(_,Estado),Trajetoria).

funcao_h(Ea,Ef,H):-
    coord(Ef,XEf,YEf),
    coord(Ea,XEa,YEa),
    X is (XEa-XEf) * (XEa-XEf) + (YEa-YEf) * (YEa-YEf),
    prolog_eval(sqrt(X),H),
    !.

maior_entre_dois(F1,F2,F1):-
    F1>F2,
    !.
maior_entre_dois(F1,F2,F2):-
    not(F1>F2),
    !.

correcao_monotonicidade(Fpai,Ffilho,F):-
    maior_entre_dois(Fpai,Ffilho,F),
    !.
guarda_menor_f(Lim,F1):-
    F1 > Lim,
    menor_F_no_momento(F2),
    F2 is mais_infinito,
    retractall(menor_F_no_momento(_)),
    assertz(menor_F_no_momento(F1)),
    !.
guarda_menor_f(Lim,F1):-
        F1 > Lim,
        menor_F_no_momento(F2),
        F1 < F2,
        retractall(menor_F_no_momento(_)),
        assertz(menor_F_no_momento(F1)),
        !.
guarda_menor_f(Lim,F1).

expandir_trajetoria(Ef,Limcusto,t(F, G, [r(Ramo,Nodo)|Resto]),t(F1,G1,[r(vai_para,Filho),r(Ramo,Nodo)|Resto])):-
    ligada_a(Nodo,Filho,Custo),
    not(produz_ciclo(Filho,[r(Ramo,Nodo)|Resto])),
    funcao_h(Filho,Ef,H1),
    G1 is G + Custo,
    Fpossivel is G1 + H1,
    correcao_monotonicidade(F,Fpossivel,F1),
    guarda_menor_f(Limcusto,F1),
    not(F1 > Limcusto).

possui_estado_final(Ef,t(_,_,[r(_,Nodo)|_])):-
        Nodo is Ef.

trajetoria_expandida(Ef,Limcusto,Solucao,Solucao):-
    possui_estado_final(Ef,Solucao),
    !.
trajetoria_expandida(Ef,Limcusto,t(F, G, [r(Ramo,Nodo)|Resto]),Solucao):-
    expandir_trajetoria(Ef,Limcusto,t(F, G, [r(Ramo,Nodo)|Resto]),t(F1,G1,[r(vai_para,Filho),r(Ramo,Nodo)|Resto])),
    trajetoria_expandida(Ef,Limcusto,t(F1,G1,[r(vai_para,Filho),r(Ramo,Nodo)|Resto]),Solucao).

busca_ida_estrela(Ef,Limcusto,t(F, G, [r(Ramo,Raiz)|Resto]),Solucao):-
    trajetoria_expandida(Ef,Limcusto,t(F, G, [r(raiz,Raiz)|Resto]),Solucao),
    !.
busca_ida_estrela(Ef,Limcusto,t(F, G, [r(Ramo,Raiz)|Resto]),Solucao):-
    menor_F_no_momento(F1),
    retractall(menor_F_no_momento(_)),
    assertz(menor_F_no_momento(mais_infinito)),
        busca_ida_estrela(Ef,F1,t(F,G,[r(Ramo,Raiz)|Resto]),Solucao).

imprimir_trajetoria_minima([r(raiz,Raiz)]):-
        write('Estado Inicial: '),
        write(Raiz),
        write('. \n'),
        !.
imprimir_trajetoria_minima([r(Operacao,Nodo)|Cauda]):-
        imprimir_trajetoria_minima(Cauda),
        write(' vai para a cidade: '),
        write(Nodo),
        write('. \n').

trajetoria_impressa(t(F,G,T)):-
        imprimir_trajetoria_minima(T).

ida(Raiz,Ef):-
    assertz(menor_F_no_momento(mais_infinito)),
    funcao_h(Raiz,Ef,H1),
    busca_ida_estrela(Ef,H1,t(H1,0,[r(raiz,Raiz)]),Solucao),
    trajetoria_impressa(Solucao).
