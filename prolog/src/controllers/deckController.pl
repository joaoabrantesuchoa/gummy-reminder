:-module('deckController', [
  createDeck/2,
  deleteDeck/1,
  editDeckName/2,
  shuffleCards/1,
  showDecks/0
]).
:-use_module('../util/JsonFunctions.pl').

createDeck(Name, Cards) :-
	jsonfunctions:readJSON(File),
	\+ checkNameDeckAvailableJSON(File, Name),
	decksToJSON(File, DecksListJSON),
	deckToJSON(Name, Cards, DeckJSON),
	append(DecksListJSON, [DeckJSON], OutJSON),
	writeJSON(OutJSON).
deleteDeck(DeckName) :-
	readJSON(File),
	deleteDeckJSON(File, DeckName, Out),
	decksToJSON(Out, OutJSON),
	writeJSON(OutJSON).
editDeckName(DeckName, NewName) :-
	readJSON(File),
	updateDeckNameJSON(File, DeckName, NewName, Out),
	decksToJSON(Out, OutJSON),
	writeJSON(OutJSON).
shuffleCards(DeckName) :-
	readJSON(File),
	shuffleCardsJSON(File, DeckName, Out),
	decksToJSON(Out, OutJSON),
	writeJSON(OutJSON).
showDecksAux([]).
showDecksAux([H|T]) :- 
	write("Name:"), writeln(H.name), 
	write("Cards:"), writeln(H.cards), nl, showDecksAux(T).
showDecks() :-
	readJSON(Decks),
	showDecksAux(Decks).
