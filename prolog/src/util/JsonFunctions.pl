:- use_module(library(http/json)).

% Fato dinâmico para gerar o id dos agentes
id(1).
idAdd :- retract(id(X)), Y is X + 1, assert(id(Y)).
:- dynamic id/1.

% Lendo arquivo JSON puro
readJSON(FilePath, File) :-
	open(FilePath, read, F),
	json_read_dict(F, File),
	close(F). 

% Regras para listar todos agentes
showDecksAux([]).
showDecksAux([H|T]) :- 
	write("ID:"), writeln(H.id),
	write("Name:"), writeln(H.name), 
	write("Cards:"), writeln(H.cards), nl, showDecksAux(T).

showDecks(FilePath) :-
	readJSON(FilePath, Decks),
	showDecksAux(Decks).

% Criando representação em formato String de um agente em JSON
deckToJSON(Name, Cards, ID, Out) :-
	swritef(Out, '{"name":"%w","cards":%q,"id":%w}', [Name, Cards, ID]).

% Convertendo uma lista de objetos em JSON para 
decksToJSON([], []).
decksToJSON([H|T], [X|Out]) :- 
	deckToJSON(H.name, H.cards, H.id, X), 
	decksToJSON(T, Out).

% Salvar em arquivo JSON
writeDeck(FilePath, Name, Cards) :- 
	id(ID), idAdd,
	readJSON(FilePath, File),
	decksToJSON(File, DecksListJSON),
	deckToJSON(Name, Cards, ID, DeckJSON),
	append(DecksListJSON, [DeckJSON], OutJSON),
	open(FilePath, write, Stream), write(Stream, OutJSON), close(Stream).

% Mudando o name de um agente
updateDeckNameJSON([], _, _, []).
updateDeckNameJSON([H|T], H.id, Name, [_{id:H.id, name:Name, cards:H.cards}|T]).
updateDeckNameJSON([H|T], Id, Name, [H|Out]) :- 
	updateDeckNameJSON(T, Id, Name, Out).

updateDeckName(FilePath, DeckId, NewName) :-
	readJSON(FilePath, File),
	updateDeckNameJSON(File, DeckId, NewName, Out),
	decksToJSON(Out, OutJSON),
	open(FilePath, write, Stream), write(Stream, OutJSON), close(Stream).

% Mudando o name de um agente
addCardJSON([], _, _, []).
addCardJSON([H|T], H.id, NewCard, [_{id:H.id, name:H.name, cards: Cards}]) :-
	append([NewCard], H.cards, Cards).
addCardJSON([H|T], Id, NewCard, [H|Out]) :- 
	addCardJSON(T, Id, NewCard, Out).

addCard(FilePath, DeckId, NewCard) :-
	readJSON(FilePath, File),
	addCardJSON(File, DeckId, NewCard, Out),
	decksToJSON(Out, OutJSON),
	open(FilePath, write, Stream), write(Stream, OutJSON), close(Stream).

% Removendo agente
deleteDeckJSON([], _, []).
deleteDeckJSON([H|T], H.id, T).
deleteDeckJSON([H|T], Id, [H|Out]) :- deleteDeckJSON(T, Id, Out).

deleteDeck(FilePath, Id) :-
	readJSON(FilePath, File),
	deleteDeckJSON(File, Id, Out),
	decksToJSON(Out, OutJSON),
	open(FilePath, write, Stream), write(Stream, OutJSON), close(Stream).