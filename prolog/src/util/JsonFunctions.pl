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
	checkNameDeckAvailableJSON/2,
	deckExistsJSON/3
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
addCardJSON([H|Rest], H.name, NewCard, [_{name:H.name, cards: Cards}|RestOut]) :-
	append([NewCard], H.cards, Cards),
	addCardJSON(Rest, H.name, NewCard, RestOut).
addCardJSON([H|T], DeckName, NewCard, [H|Out]) :- 
	addCardJSON(T, DeckName, NewCard, Out).

% Mudando o name de um agente
removeCardJSON([], _, _, []).
removeCardJSON([H|Rest], H.name, CardToRemove, [_{name:H.name, cards: Cards}|RestOut]) :-
	delete(H.cards, CardToRemove, Cards),
	removeCardJSON(Rest, H.name, CardToRemove, RestOut).
removeCardJSON([H|T], DeckName, CardToRemove, [H|Out]) :- 
	removeCardJSON(T, DeckName, CardToRemove, Out).

% Mudando o name de um agente
shuffleCardsJSON([], _, _).
shuffleCardsJSON([H|Rest], DeckName, [_{name:H.name, cards: Cards}|RestOut]) :-
	length(Cards, L),
	(H.name == DeckName -> 
		(L > 0 -> 
			random_permutation(H.cards, Cards),
			shuffleCardsJSON(Rest, DeckName, RestOut)
			)
			;
			Cards = H.cards,
			shuffleCardsJSON(Rest, DeckName, RestOut)
		).
shuffleCardsJSON([H|T], DeckName, [H|Out]) :- 
	shuffleCardsJSON(T, DeckName, Out).

% Removendo agente
deleteDeckJSON([], _, []).
deleteDeckJSON([H|T], H.name, T).
deleteDeckJSON([H|T], DeckName, [H|Out]) :- deleteDeckJSON(T, DeckName, Out).

deckExistsJSON([], _, "no").
deckExistsJSON([H|T], DeckName, Exists):-
  (
    H.name == DeckName -> Exists = "yes";
    deckExistsJSON(T, DeckName, Exists)).