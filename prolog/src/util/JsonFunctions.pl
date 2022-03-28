:- use_module(library(http/json)).

% Fato dinâmico para gerar o id dos agentes
id(1).
incrementa_id :- retract(id(X)), Y is X + 1, assert(id(Y)).
:- dynamic id/1.

% Lendo arquivo JSON puro
lerJSON(FilePath, File) :-
	open(FilePath, read, F),
	json_read_dict(F, File).

% Regras para listar todos agentes
exibirDecksAux([]).
exibirDecksAux([H|T]) :- 
	write("ID:"), writeln(H.id),
	write("Name:"), writeln(H.name), 
	write("Cards:"), writeln(H.cards), nl, exibirDecksAux(T).

exibirDecks(FilePath) :-
	lerJSON(FilePath, Decks),
	exibirDecksAux(Decks).

% Criando representação em formato String de um agente em JSON
deckToJSON(Nome, Cards, ID, Out) :-
	swritef(Out, '{"name":"%w","cards":%q,"id":%w}', [Nome, Cards, ID]).

% Convertendo uma lista de objetos em JSON para 
decksToJSON([], []).
decksToJSON([H|T], [X|Out]) :- 
	deckToJSON(H.name, H.cards, H.id, X), 
	decksToJSON(T, Out).

% Salvar em arquivo JSON
salvarDeck(FilePath, Nome, Cards) :- 
	id(ID), incrementa_id,
	lerJSON(FilePath, File),
	decksToJSON(File, ListaDecksJSON),
	deckToJSON(Nome, Cards, ID, DeckJSON),
	append(ListaDecksJSON, [DeckJSON], Saida),
	open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

salvarCard(FilePath, IdDeck, NewCard) :-
	

% Mudando o name de um agente
editarNomeDeckJSON([], _, _, []).
editarNomeDeckJSON([H|T], H.id, Nome, [_{id:H.id, name:Nome, cards:H.cards}|T]).
editarNomeDeckJSON([H|T], Id, Nome, [H|Out]) :- 
	editarNomeDeckJSON(T, Id, Nome, Out).

editarNomeDeck(FilePath, IdDeck, NovoNome) :-
	lerJSON(FilePath, File),
	editarNomeDeckJSON(File, IdDeck, NovoNome, SaidaParcial),
	decksToJSON(SaidaParcial, Saida),
	open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Mudando o name de um agente
addCardsDeckJSON([], _, _, []).
addCardsDeckJSON([H|T], H.id, NovoCard, [_{id:H.id, name:H.name, cards: NovoCard}]).
addCardsDeckJSON([H|T], Id, NovoCard, [H|Out]) :- 
	addCardsDeckJSON(T, Id, NovoCard, Out).

addCardsDeck(FilePath, IdDeck, NovoCard) :-
	lerJSON(FilePath, File),
	addCardsDeckJSON(File, IdDeck, NovoCard, SaidaParcial),
	decksToJSON(SaidaParcial, Saida),
	open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Removendo agente
removerDeckJSON([], _, []).
removerDeckJSON([H|T], H.id, T).
removerDeckJSON([H|T], Id, [H|Out]) :- removerDeckJSON(T, Id, Out).

removerDeck(FilePath, Id) :-
	lerJSON(FilePath, File),
	removerDeckJSON(File, Id, SaidaParcial),
	decksToJSON(SaidaParcial, Saida),
	open(FilePath, write, Stream), write(Stream, Saida), close(Stream).