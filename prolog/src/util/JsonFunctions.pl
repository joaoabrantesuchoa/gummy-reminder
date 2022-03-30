:-module('jsonfunctions', [
	showDecks/0,
	updateDeckName/2,
	createDeck/2,
	deleteDeck/1,
	addCard/2,
	removeCard/2,
	shuffleCards/1
]).
:- use_module(library(http/json)).

% Lendo arquivo JSON puro
readJSON(File) :-	
	nb_getval(dbPath, Path),
	open(Path, read, F),
	json_read_dict(F, File),
	close(F). 

writeJSON(JSON) :-
	nb_getval(dbPath, Path),
	open(Path, write, Stream), write(Stream, JSON), close(Stream).

% Regras para listar todos agentes
showDecksAux([]).
showDecksAux([H|T]) :- 
	write("Name:"), writeln(H.name), 
	write("Cards:"), writeln(H.cards), nl, showDecksAux(T).

showDecks() :-
	readJSON(Decks),
	showDecksAux(Decks).

% Criando representação em formato String de um agente em JSON
deckToJSON(Name, Cards, Out) :-
	swritef(Out, '{"name":"%w","cards":%q}', [Name, Cards]).

% Convertendo uma lista de objetos em JSON para 
decksToJSON([], []).
decksToJSON([H|T], [X|Out]) :- 
	deckToJSON(H.name, H.cards, X), 
	decksToJSON(T, Out).

% Salvar em arquivo JSON
createDeck(Name, Cards) :- 
	readJSON(File),
	decksToJSON(File, DecksListJSON),
	deckToJSON(Name, Cards, DeckJSON),
	append(DecksListJSON, [DeckJSON], OutJSON),
	writeJSON(OutJSON).

% Mudando o name de um agente
updateDeckNameJSON([], _, _, []).
updateDeckNameJSON([H|T], H.name, NewName, [_{name:NewName, cards:H.cards}|T]).
updateDeckNameJSON([H|T], DeckName, NewName, [H|Out]) :- 
	updateDeckNameJSON(T, DeckName, NewName, Out).

updateDeckName(DeckName, NewName) :-
	readJSON(File),
	updateDeckNameJSON(File, DeckName, NewName, Out),
	decksToJSON(Out, OutJSON),
	writeJSON(OutJSON).

% Mudando o name de um agente
addCardJSON([], _, _, []).
addCardJSON([H|_], H.name, NewCard, [_{name:H.name, cards: Cards}]) :-
	append([NewCard], H.cards, Cards).
addCardJSON([H|T], DeckName, NewCard, [H|Out]) :- 
	addCardJSON(T, DeckName, NewCard, Out).

addCard(DeckName, NewCard) :-
	readJSON(File),
	addCardJSON(File, DeckName, NewCard, Out),
	decksToJSON(Out, OutJSON),
	writeJSON(OutJSON).

% Mudando o name de um agente
removeCardJSON([], _, _, []).
removeCardJSON([H|_], H.name, CardToRemove, [_{name:H.name, cards: Cards}]) :-
	delete(H.cards, CardToRemove, Cards).
removeCardJSON([H|T], DeckName, CardToRemove, [H|Out]) :- 
	removeCardJSON(T, DeckName, CardToRemove, Out).

removeCard(DeckName, CardToRemove) :-
	readJSON(File),
	removeCardJSON(File, DeckName, CardToRemove, Out),
	decksToJSON(Out, OutJSON),
	writeJSON(OutJSON).


% Mudando o name de um agente
shuffleCardsJSON([], _, _, []).
shuffleCardsJSON([H|_], H.name, [_{name:H.name, cards: Cards}]) :-
	random_permutation(H.cards, Cards).
shuffleCardsJSON([H|T], DeckName, [H|Out]) :- 
	shuffleCardsJSON(T, DeckName, Out).

shuffleCards(DeckName) :-
	readJSON(File),
	shuffleCardsJSON(File, DeckName, Out),
	decksToJSON(Out, OutJSON),
	writeJSON(OutJSON).

% Removendo agente
deleteDeckJSON([], _, []).
deleteDeckJSON([H|T], H.name, T).
deleteDeckJSON([H|T], DeckName, [H|Out]) :- deleteDeckJSON(T, DeckName, Out).

deleteDeck(DeckName) :-
	readJSON(File),
	deleteDeckJSON(File, DeckName, Out),
	decksToJSON(Out, OutJSON),
	writeJSON(OutJSON).