:-module('CLIController', [
  gummyReminderLogo/0,
  initialMenu/0,
  mainMenu/0
]).

:-use_module('../util/JsonFunctions.pl').
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
  writeln("                        Aprenda com o auxílio de"),
  writeln("                        cartões de memorização\n"),
  writeln("               > Pressione qualquer tecla para iniciar <\n"),
  line,
  nl.

mainMenu:- 
  readJSON(Decks),
  maplist(getDeckName, Decks, DeckNames),
  length(DeckNames, L),
  (
    L == 0 -> MenuDecks = 'Você não possui decks';
    listDecksNamesAndIndex(1, DeckNames, IndexedNames),
    atomic_list_concat(IndexedNames, "\n", DecksList),
    atomic_concat("Seus decks:\n\n", DecksList, MenuDecks)
    
  ),
  writeln(MenuDecks),
  write("\n        [c] Criar deck  [e] Escolher deck  [s] Sair\n"),
  write("\n> O que você deseja? "),
  read(Option),
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
  writeln("\nDigite o nome do deck:"),
  read(NameDeck),
  createDeck(NameDeck, []), nl, line,
  mainMenu().

chooseDeckMenu():-
  writeln("\n> Escolha o número do deck: "),
  read(NumDeck),
  chooseDeck(NumDeck), !.

chooseDeck(NumDeck):-
  nl, line, 
  readJSON(Decks), length(Decks, LenDecks),
  NumDeck > 0, NumDeck =< LenDecks,
  Indice is NumDeck - 1, nth0(Indice, Decks, Deck),
  string_concat("\n<<  ", Deck.name, ParcialString),
  string_concat(ParcialString, "  >>", StringName),
  writeln(StringName),
  writeln("\n[i] Iniciar revisão  [e] Editar nome  [a] Add carta\n"),
  writeln("          [R] Remover deck   [X] Voltar"),
  write("\n> O que você deseja? "),
  read(Option), nl, line,
  string_upper(Option, OptionUpper),
  menuOptionsChoosedDeck(OptionUpper, Deck), !.

chooseDeck(NumDeck):-
  writeln("\n# Número inválido #\n"),
  line, nl, mainMenu().

editDeckNameMenu(Deck):-
  writeln("\n> Qual o novo nome do deck?"),
  read(NewDeckName), atom_string(NewDeckName, StrDeckName),
  editDeckName(Deck.name, StrDeckName),
  writeln("\nNome alterado com sucesso!\n"),
  line, nl, mainMenu().

cardsMenu(Deck, []):-
  length(Deck.cards, LenCards), LenCards =:= 0,
  writeln("\n                       Esse deck está vazio :(\n"), 
  line, nl, mainMenu(), !.

cardsMenu(Deck, []):-
  writeln("\n                  Você concluiu o estudo desse deck :D\n"), 
  line, nl, mainMenu(), !.

cardsMenu(Deck, [H|T]):-
  cardQA(Deck, H), cardsMenu(Deck, T).

cardQA(Deck, Card):-
  string_concat("\n<<  ", Deck.name, ParcialString),
  string_concat(ParcialString, "  >>", StringName),
  nth0(0, Card, Front), nth0(1, Card, Back), nl,
  writeln(Front),
  writeln("\n        > Pressione ALGUMA LETRA para revelar a resposta <    \n"),
  read(Letra), cardLine(),
  writeln(Back),
  writeln("\n        [e] Editar carta  [r] Remover carta  [x] Voltar  "),
  writeln("                 > Pressione c para continuar <         "),
  write("\n> O que você deseja? "),
  read(Option), nl, line,
  string_upper(Option, OptionUpper),
  menuOptionsCard(OptionUpper, Deck, Card).
  
addCardMenu(Deck):-
  writeln("\n> Qual será a frente da carta?"),
  read(Front), atom_string(Front, StrFront),
  writeln("\n> Qual será o verso da carta?"),
  read(Back), atom_string(Back, StrBack),
  Card = [StrFront, StrBack],
  addCard(Deck.name, Card),
  writeln("\nCarta adicionada com sucesso!\n"),
  line, nl,
  mainMenu().

removeDeckMenu(Deck):-
  writeln("\n> Tem certeza que deseja remover o deck? [y]"),
  read(Option), string_upper(Option, OptionUpper),
  confirmRemove(OptionUpper, Deck).

confirmRemove("Y", Deck):- 
  deleteDeck(Deck.name), 
  writeln("\nO deck foi removido com sucesso!\n"),
  line, nl, mainMenu(), !.
confirmRemove(_, _):- nl, line, nl, mainMenu().

editCardMenu(Deck, Card):-
  removeCard(Deck.name, Card),
  writeln("\n> Qual será a frente da carta?"),
  read(Front), atom_string(Front, StrFront),
  writeln("\n> Qual será o verso da carta?"),
  read(Back), atom_string(Back, StrBack),
  Card = [StrFront, StrBack],
  addCard(Deck.name, Card),
  writeln("\nCarta editada com sucesso!\n"),
  mainMenu().

removeCardMenu(Deck, Card):-
  writeln("\n> Tem certeza que deseja remover a carta? [y]"),
  read(Option), string_upper(Option, OptionUpper),
  confirmRemoveCard(OptionUpper, Deck, Card).

confirmRemoveCard("Y", Deck, Card):- 
  removeCard(Deck.name, Card), 
  writeln("\nA carta foi removida com sucesso!\n"),
  line, nl, mainMenu(), !.
confirmRemoveCard(_, _, _):- nl, line, nl, mainMenu().

errorMenu():-
  nl, writeln("########################## Opção inválida! #########################\n"), mainMenu().

menuOptionsDeck("C") :- createDeckMenu(), !.
menuOptionsDeck("E") :- chooseDeckMenu(), !.
menuOptionsDeck("S") :- halt, !.
menuOptionsDeck(_) :- errorMenu().

menuOptionsChoosedDeck("I", Deck) :- cardsMenu(Deck, Deck.cards), !.
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
