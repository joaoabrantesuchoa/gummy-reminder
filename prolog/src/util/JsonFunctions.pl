:-module('jsonfunctions', [
	readJSON/1,
	writeJSON/1,
	deckToJSON/3,
	decksToJSON/2,
	deleteDeckJSON/3,
	updateDeckNameJSON/4,
	shuffleCardsJSON/3,
	addCardJSON/4,
	removeCardJSON/4,
	checkNameDeckAvailableJSON/2
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

% Criando representação em formato String de um agente em JSON
deckToJSON(Name, Cards, Out) :-
	swritef(Out, '{"name":"%w","cards":%q}', [Name, Cards]).

% Convertendo uma lista de objetos em JSON para 
decksToJSON([], []).
decksToJSON([H|T], [X|Out]) :- 
	deckToJSON(H.name, H.cards, X), 
	decksToJSON(T, Out).

% Mudando o name de um agente
updateDeckNameJSON([], _, _, []).
updateDeckNameJSON([H|T], H.name, NewName, [_{name:NewName, cards:H.cards}|T]).
updateDeckNameJSON([H|T], DeckName, NewName, [H|Out]) :- 
	updateDeckNameJSON(T, DeckName, NewName, Out).

% Mudando o name de um agente
addCardJSON([], _, _, []).
addCardJSON([H|_], H.name, NewCard, [_{name:H.name, cards: Cards}]) :-
	append([NewCard], H.cards, Cards).
addCardJSON([H|T], DeckName, NewCard, [H|Out]) :- 
	addCardJSON(T, DeckName, NewCard, Out).

% Mudando o name de um agente
removeCardJSON([], _, _, []).
removeCardJSON([H|_], H.name, CardToRemove, [_{name:H.name, cards: Cards}]) :-
	delete(H.cards, CardToRemove, Cards).
removeCardJSON([H|T], DeckName, CardToRemove, [H|Out]) :- 
	removeCardJSON(T, DeckName, CardToRemove, Out).

% Mudando o name de um agente
shuffleCardsJSON([], _, _, []).
shuffleCardsJSON([H|_], H.name, [_{name:H.name, cards: Cards}]) :-
	random_permutation(H.cards, Cards).
shuffleCardsJSON([H|T], DeckName, [H|Out]) :- 
	shuffleCardsJSON(T, DeckName, Out).

% Removendo agente
deleteDeckJSON([], _, []).
deleteDeckJSON([H|T], H.name, T).
deleteDeckJSON([H|T], DeckName, [H|Out]) :- deleteDeckJSON(T, DeckName, Out).

% Verifica se o nome do deck já existe.
checkNameDeckAvailableJSON([H|_], H.name).
checkNameDeckAvailableJSON([_|T], DeckName) :- checkNameDeckAvailableJSON(T, DeckName).