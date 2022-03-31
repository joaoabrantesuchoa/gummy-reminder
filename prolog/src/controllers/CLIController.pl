:-module('CLIController', [
  gummyReminderLogo/0,
  initialMenu/0,
  mainMenu/0
]).

:-use_module('../util/jsonfunctions.pl').

gummyReminderLogo():-
  nl,
  writeln("----------------------------------------------------------------------"),
  writeln("            _______  __   __  __   __  __   __  __   __               "),
  writeln("           |       ||  | |  ||  |_|  ||  |_|  ||  | |  |              "),
  writeln("           |    ___||  | |  ||       ||       ||  |_|  |              "),
  writeln("           |   | __ |  |_|  ||       ||       ||       |              "),
  writeln("           |   ||  ||       ||       ||       ||_     _|              "),
  writeln("           |   |_| ||       || ||_|| || ||_|| |  |   |                "),
  writeln("           |_______||_______||_|   |_||_|   |_|  |___|                "),
  writeln("  ______   _______  __   __  ___  __    _  ______   _______  ______   "),
  writeln(" |   _  | |       ||  |_|  ||   ||  |  | ||      | |       ||   _  |  "),
  writeln(" |  | | | |    ___||       ||   ||   |_| ||   _   ||    ___||  | | |  "),
  writeln(" |  |_| |_|   |___ |       ||   ||       ||  | |  ||   |___ |  |_| |_ "),
  writeln(" |   __  ||    ___||       ||   ||  _    ||  |_|  ||    ___||   __  | "),
  writeln(" |  |  | ||   |___ | ||_|| ||   || | |   ||       ||   |___ |  |  | | "),
  writeln(" |__|  |_||_______||_|   |_||___||_|  |__||______| |_______||__|  |_| "), nl,
  writeln("----------------------------------------------------------------------"),
  nl.
                
initialMenu():-     
  nl,
  writeln("──────────────────── Bem-vindo ────────────────────\n\n"),
  writeln("             Aprenda com o auxílio de             \n"),
  writeln("              cartões de memorização             \n\n"),
  writeln("         > Pressione ENTER para iniciar <          "),
  nl.

mainMenu:- 
  nl,
  readJSON(Decks),
  maplist(getDeckName, Decks, DeckNames),
  length(DeckNames, L),
  (
    L == 0 -> MenuDecks = 'Você não possui decks';
    listDecksNamesAndIndex(1, DeckNames, IndexedNames),
    atomic_list_concat(IndexedNames, "\n", MenuDecks)
  ),
  writeln(MenuDecks).

getDeckName(E, Out):-
  Out = E.name.

listDecksNamesAndIndex(L, [], []).
listDecksNamesAndIndex(L, [H|T], [HOut|Rest]):-
  atomic_list_concat([L, " - ", H], HOut),
  L2 is L+1,
  listDecksNamesAndIndex(L2, T, Rest).
