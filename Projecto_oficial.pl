%Nome: Claudia Ramires Numero: 86286

%--------------------------------
%Predicado propaga
	%Dado um puzzle, a variavel Posicoes sera 
	%prenchida  apartir da base nomeadamente do Pos
	%por ordem crescente
%--------------------------------

propaga_1([], _, []).
propaga_1([T|_], Pos, T) :- member(Pos,T).
propaga_1([_|ListaT], Pos, T) :- propaga_1(ListaT, Pos, T).    


%serve_para_contar_pela_base_da_Pos
esquerda([], _, []).                                              
esquerda([X|_], X, [X]).
esquerda([X|L], Y, [X|R]):- 
				(X \= Y), 
				esquerda(L, Y, R).

propaga([Termometros, _, _], Pos, Posicoes) :- 
				propaga_1(Termometros, Pos, R), 
				esquerda(R, Pos, R2), 
				sort(R2,Posicoes).

				
%--------------------------------
%Predicado nao_altera_linhas_anteriores
	% Tendo uma lista de Posicoes, ira-se prencher a lista Ja_Preenchidas, com as posicoes que tenham
	%do tuplo(L,C) a Linha (L) inferior a dada pelo L.
%--------------------------------

%funcao_auxiliar_para_determinar_se_a_linha_escolhida_e_inferior_a_pertencente_ao_tuplo_da_lista_de_Posicoes

menor_esq((L,_), LM):- L < LM.             

todas_linhas_anteriores([], _, []).
todas_linhas_anteriores([X|L1], L, [X|R]):- 
				menor_esq(X, L), 
				todas_linhas_anteriores(L1, L, R).
todas_linhas_anteriores([X|L1], L, R):- 
				\+menor_esq(X, L), 
				todas_linhas_anteriores(L1, L, R).

%funcao_auxiliar_que_serve_para_validar_caso_pertenca_a_lista_Ja_Preenchidas

valida_todos([], _).
valida_todos([X|LA], Ja_Preenchidas):- 
				member(X, Ja_Preenchidas), 
				valida_todos(LA, Ja_Preenchidas).

nao_altera_linhas_anteriores(Posicoes, L, Ja_Preenchidas):- 
				todas_linhas_anteriores(Posicoes, L, LA), 
				valida_todos(LA, Ja_Preenchidas).


%--------------------------------
%Perdicado verifica_parcial
	%O predicado verifica_parcial permite verifica se uma possibilidade
	%para preencher uma linha nao viola os totais das colunas tendo
	%em atencao as escolhas feitas anteriormente
%--------------------------------


verifica_parcial_aux([MaxC|_], Dim, Dim, LP):- verifica_parcial_vertical(MaxC, Dim, LP).                          

verifica_parcial_aux([MaxC|L], Coluna,Dim, LP):- 	
				Coluna < Dim,
				verifica_parcial_vertical(MaxC, Coluna, LP),        
				Coluna2 is Coluna+1, 
				verifica_parcial_aux(L, Coluna2,Dim, LP).

verifica_parcial([_,_,TC], Ja_Preenchidas, Dim, Pos):- 
				append(Ja_Preenchidas,Pos, Total), 
				sort(Total, LP),
				verifica_parcial_aux(TC, 1, Dim, LP).


%funcao_auxiliar_para_avaliar_se_o_dim_e_igual_a_coluna_caso_seja_decrementa_caso_contrario_ate_chegar_a_lista_vazia
verifica_parcial_vertical(MaxC, Y, [(_,Y)|LP]):-
				M2 is MaxC - 1,
				verifica_parcial_vertical(M2, Y, LP).
				
verifica_parcial_vertical(MaxC, Dim, [(_,Y)|LP]):- 
				(Dim \= Y), 
				verifica_parcial_vertical(MaxC, Dim, LP).
				

verifica_parcial_vertical(MaxC, _, []):- MaxC >=0.


%--------------------------------
%Predicado possibilidades_linha
	%Determinar as possibilidades existentes para preencher uma determinada linha
	%tendo em atencao as escolhas ja feitas para preencher as linhas anteriores
	%e os totais das colunas
%--------------------------------

%funcao_auxiliar_que_avalia_em_linha_esta_o_perdicado
qual_linha((L,_), L).


%funcao_auxiliar_fornecida_nos_slides_para_fazer_as_combinacoes_de_todas_as_possibilidades
combinacao(0, _, []).
combinacao(N, L, [X | Combinacoes]) :-
	N > 0, 
	append(_, [X | Combinacoes_apos], L), 
	N_1 is N - 1, 
	combinacao(N_1, Combinacoes_apos, Combinacoes).


%funcao_auxiliar_que_fornece_lista_propagada				
lista_toda_propagada(_,[],[]).
lista_toda_propagada(Puz, [Lista|Lista_resto], [L_P|L_Propagada]):-
				propaga(Puz, Lista, L_P),
				lista_toda_propagada(Puz, Lista_resto, L_Propagada).


%funcao_auxiliar_propaga_todas_as_combinacoes_possiveis				
adjunta(_,_,[],[]).				
adjunta(Puz, Obrigatorias, [Lista|Lista2], [Lista3|Lista4]):-
				append(Obrigatorias, Lista, L3),
				lista_toda_propagada(Puz, L3, X),
				flatten(X, Y),
				sort(Y, Lista3),
				adjunta(Puz,Obrigatorias, Lista2, Lista4).
				
				
%funcao_auxiliar_que_ve_se_comprimento_da_linha_e_posterior_as_japreenchidas				
verificacao_compr(Puz, Ja_Preenchidas, Len, Poss, Linha):-
				verifica_parcial(Puz, Ja_Preenchidas, Len, Poss),
				nao_altera_linhas_anteriores(Poss, Linha, Ja_Preenchidas).
				
%funcao_auxiliar_para_ver_limite_da_linha		
caso_passe_limite_linha(Posicoes, Total, Linha):-
				findall(C, member((Linha,C), Posicoes), Tot),
				length(Tot, Total).

			
possibilidades_linha_rec(Puz, [Pos|Posicoes_linha], Linha, Len, Total, Ja_Preenchidas, Possibilidades_L):- 
				findall((Linha, C), member((Linha, C), Ja_Preenchidas), Obrigatorias),
				findall(X, combinacao(Total, [Pos|Posicoes_linha], X), Possi),
				adjunta(Puz, Obrigatorias, Possi, Pre_pre_Possib),
				findall(X, (member(X, Pre_pre_Possib), verificacao_compr(Puz, Ja_Preenchidas, Len, X, Linha)), Pre_possib),
				findall(X, (member(X, Pre_possib), caso_passe_limite_linha( X, Total, Linha)), Possibilidades_L).
				
				
possibilidades_linha(Puz, [X|Posicoes_linha], Total, Ja_Preenchidas, Possibilidades_L):- 
				qual_linha(X, Linha), 
				length([X|Posicoes_linha], Len), 
				possibilidades_linha_rec(Puz, [X|Posicoes_linha], Linha, Len, Total, Ja_Preenchidas, Uma_lista_qlq),
				sort(Uma_lista_qlq, Possibilidades_L), !.
				
