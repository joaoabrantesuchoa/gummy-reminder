:-module('cardController', [
  addCard/2,
  removeCard/2
]).
:-use_module('../util/jsonfunctions.pl').

addCard(DeckName, NewCard) :-
	readJSON(File),
	addCardJSON(File, DeckName, NewCard, Out),
	decksToJSON(Out, OutJSON),
	writeJSON(OutJSON).

removeCard(DeckName, CardToRemove) :-
	readJSON(File),
	removeCardJSON(File, DeckName, CardToRemove, Out),
	decksToJSON(Out, OutJSON),
	writeJSON(OutJSON).
