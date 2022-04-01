:-module('CLIController', [
  gummyReminderLogo/0,
  initialMenu/0,
  mainMenu/0
]).

:-use_module('../util/JsonFunctions.pl').
:-use_module(library(readutil)).
:- set_prolog_flag('encoding', 'utf8').
:- style_check(-singleton).

gummyReminderLogo():-
  nl,
  line,
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
  writeln(" |__|  |_||_______||_|   |_||___||_|  |__||______| |_______||__|  |_| ").


cardLine():- writeln("        - - - - - - - - - - - - - - - - - - - - - - - - - -\n"). 

line:- 
  repl("\u2500", 70, L),
  atomic_list_concat(L, LineStr),
  writeln(LineStr).
lineAtom(Len, R):-
  repl("\u2500", Len, L),
  atomic_list_concat(L, LineStr),
  R = LineStr.
repl(X, N, L) :-
    length(L, N),
    maplist(=(X), L).

initialMenu():-     
  nl,
  lineAtom(30, Line30Length),
  lineAtom(29, Line29Length),
  atomic_list_concat([Line30Length, " Bem-vindo ", Line29Length], BemVindo),
  writeln(BemVindo),
  write("\n"),
  writeln("                        Aprenda com o aux\u00EDlio de"),
  writeln("                         cart\u00F5es de memoriza\u00E7\u00E3o\n"),
  writeln("               > Pressione qualquer tecla para iniciar <\n"),
  line,
  nl.

mainMenu:- 
  readJSON(Decks),
  maplist(getDeckName, Decks, DeckNames),
  length(DeckNames, L),
  (
    L == 0 -> MenuDecks = 'Voc\u00EA n\u00E3o possui decks';
    listDecksNamesAndIndex(1, DeckNames, IndexedNames),
    atomic_list_concat(IndexedNames, "\n", DecksList),
    atomic_concat("Seus decks:\n\n", DecksList, MenuDecks)
    
  ),
  writeln(MenuDecks),
  write("\n        [c] Criar deck  [e] Escolher deck  [s] Sair\n"),
  write("\n> O que voc\u00EA deseja? "),
  readLine(Option),
  string_upper(Option, OptionUpper),
  menuOptionsDeck(OptionUpper).

getDeckName(E, Out):-
  Out = E.name.

listDecksNamesAndIndex(_, [], []).
listDecksNamesAndIndex(L, [H|T], [HOut|Rest]):-
  atomic_list_concat([L, " - ", H], HOut),
  L2 is L+1,
  listDecksNamesAndIndex(L2, T, Rest).

createDeckMenu():-
  write("\nDigite o nome do deck: "),
  readLine(NameDeck),
  createDeck(NameDeck, []), nl, line,
  mainMenu().

chooseDeckMenu():-
  write("\n> Escolha o n\u00FAmero do deck: "),
  readLine(NumDeck),
  chooseDeck(NumDeck), !.

chooseDeck(NumDeckStr):-
  nl, line, 
  number_string(NumDeck, NumDeckStr),
  readJSON(Decks), length(Decks, LenDecks),
  NumDeck > 0, NumDeck =< LenDecks,
  Indice is NumDeck - 1, nth0(Indice, Decks, Deck),
  string_concat("\n<<  ", Deck.name, ParcialString),
  string_concat(ParcialString, "  >>", StringName),
  writeln(StringName),
  writeln("\n[i] Iniciar revis\u00E3o  [e] Editar nome  [a] Add carta\n"),
  writeln("          [R] Remover deck   [X] Voltar"),
  write("\n> O que voc\u00EA deseja? "),
  readLine(Option), nl, line,
  string_upper(Option, OptionUpper),
  menuOptionsChoosedDeck(OptionUpper, Deck), !.

chooseDeck(NumDeck):-
  writeln("\n# N\u00FAmero inv\u00E1lido #\n"),
  line, nl, mainMenu().

editDeckNameMenu(Deck):-
  write("\n> Qual o novo nome do deck? "),
  readLine(NewDeckName),
  editDeckName(Deck.name, NewDeckName),
  writeln("\nNome alterado com sucesso!\n"),
  line, nl, mainMenu().

cardsMenu(Deck, []):-
  length(Deck.cards, LenCards), LenCards =:= 0,
  writeln("\n                       Esse deck est\u00E1 vazio :(\n"), 
  line, nl, mainMenu(), !.

cardsMenu(Deck, []):-
  writeln("\n                  Voc\u00EA concluiu o estudo desse deck :D\n"), 
  line, nl, mainMenu(), !.

cardsMenu(Deck, [H|T]):-
  cardQA(Deck, H), cardsMenu(Deck, T).

cardQA(Deck, Card):-
  string_concat("\n<<  ", Deck.name, ParcialString),
  string_concat(ParcialString, "  >>", StringName),
  nth0(0, Card, Front), nth0(1, Card, Back), nl,
  writeln(Front),
  writeln("\n        > Pressione ALGUMA LETRA para revelar a resposta <    \n"),
  get_single_char(_), cardLine(),
  writeln(Back),
  writeln("\n        [e] Editar carta  [r] Remover carta  [x] Voltar  "),
  writeln("                 > Pressione c para continuar <         "),
  write("\n> O que voc\u00EA deseja? "),
  readLine(Option), nl, line,
  string_upper(Option, OptionUpper),
  menuOptionsCard(OptionUpper, Deck, Card).
  
addCardMenu(Deck):-
  writeln("\n> Qual ser\u00E1 a frente da carta?"),
  readLine(Front),
  writeln("\n> Qual ser\u00E1 o verso da carta?"),
  readLine(Back),
  Card = [Front, Back],
  addCard(Deck.name, Card),
  writeln("\nCarta adicionada com sucesso!\n"),
  line, nl,
  mainMenu().

removeDeckMenu(Deck):-
  writeln("\n> Tem certeza que deseja remover o deck? [y] "),
  readLine(Option), string_upper(Option, OptionUpper),
  confirmRemove(OptionUpper, Deck).

confirmRemove("Y", Deck):- 
  deleteDeck(Deck.name), 
  writeln("\nO deck foi removido com sucesso!\n"),
  line, nl, mainMenu(), !.
confirmRemove(_, _):- nl, line, nl, mainMenu().

editCardMenu(Deck, Card):-
  removeCard(Deck.name, Card),
  writeln("\n> Qual ser\u00E1 a frente da carta? "),
  readLine(Front),
  writeln("\n> Qual ser\u00E1 o verso da carta? "),
  readLine(Back),
  append([Front], [Back], NewCard),
  addCard(Deck.name, NewCard),
  writeln("\nCarta editada com sucesso!\n"),
  mainMenu().

removeCardMenu(Deck, Card):-
  write("\n> Tem certeza que deseja remover a carta? [y] "),
  readLine(Option), string_upper(Option, OptionUpper),
  confirmRemoveCard(OptionUpper, Deck, Card).

confirmRemoveCard("Y", Deck, Card):- 
  removeCard(Deck.name, Card), 
  writeln("\nA carta foi removida com sucesso!\n"),
  line, nl, mainMenu(), !.
confirmRemoveCard(_, _, _):- nl, line, nl, mainMenu().

errorMenu():-
  nl, writeln("########################## Op\u00E7\u00E3o inv\u00E1lida! #########################\n"), mainMenu().

menuOptionsDeck("C") :- createDeckMenu(), !.
menuOptionsDeck("E") :- chooseDeckMenu(), !.
menuOptionsDeck("S") :- halt, !.
menuOptionsDeck(_) :- errorMenu().

menuOptionsChoosedDeck("I", Deck) :-
  length(Deck.cards, L),
  (
    L == 0 -> 
    writeln("\n                       Esse deck est\u00E1 vazio :(\n"),
    line,
    mainMenu
    ;
    shuffleCards(Deck.name),
    cardsMenu(Deck, Deck.cards), !
    ).
menuOptionsChoosedDeck("E", Deck) :- editDeckNameMenu(Deck), !.
menuOptionsChoosedDeck("A", Deck) :- addCardMenu(Deck), !.
menuOptionsChoosedDeck("R", Deck) :- removeDeckMenu(Deck), !.
menuOptionsChoosedDeck("X", _) :- nl, mainMenu(), !.
menuOptionsChoosedDeck(_, _) :- errorMenu().

menuOptionsCard("E", Deck, Card) :- editCardMenu(Deck, Card), !.
menuOptionsCard("R", Deck, Card) :- removeCardMenu(Deck, Card), !.
menuOptionsCard("X", _, _) :- nl, mainMenu(), !.
menuOptionsCard("C", _, _) :- !.
menuOptionsCard(_, _, _) :- errorMenu().

readLine(R):- read_line_to_codes(user_input,Cs), atom_codes(A, Cs), atomic_list_concat(L, ' ', A), atom_string(A, R).
