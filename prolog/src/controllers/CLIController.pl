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
  writeln(" |__|  |_||_______||_|   |_||___||_|  |__||______| |_______||__|  |_| "), nl,
  line,
  nl.

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
  writeln("\n"),
  writeln("                        Aprenda com o auxílio de\n"),
  writeln("                        cartões de memorização\n"),
  writeln("               > Pressione qualquer tecla para iniciar <\n"),
  line,
  nl.

mainMenu:- 
  nl,
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
  write("\n              [C] Criar deck  [E] Escolher deck  [S] Sair\n"),
  write("\n> O que você deseja? "),
  read(Option),
  % (
  %   Option == "C"; Option == "c" -> createDeckMenu();
  %   Option == "E"; Option == "e" -> chooseDeckMenu();
  %   Option == "S"; Option == "s" -> halt
  % ).
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
  writeln("Digite o nome do deck:"),
  read(NameDeck),
  createDeck(NameDeck, []),
  mainMenu().

chooseDeckMenu():-
  writeln("\n> Escolha o número do deck: "),
  read(NumDeck),
  chooseDeck(NumDeck).

chooseDeck(NumDeck):-
  readJSON(Decks), length(Decks, LenDecks),
  NumDeck > 0, NumDeck =< LenDecks,
  Indice is NumDeck - 1, nth0(Indice, Decks, Deck),
  string_concat("\n<<  ", Deck.name, ParcialString),
  string_concat(ParcialString, "  >>", StringName),
  writeln(StringName),
  writeln("\n[I] Iniciar revisão  [E] Editar nome  [A] Add carta\n"),
  writeln("          [R] Remover deck   [X] Voltar"),
  write("\n> O que você deseja? "),
  read(Option),
  string_upper(Option, OptionUpper),
  menuOptionsChoosedDeck(OptionUpper, Deck), !.

chooseDeck(NumDeck):-
  writeln("\n# Número inválido #\n"),
  mainMenu().

editDeckNameMenu(Deck):-
  writeln("> Qual o novo nome do deck?"),
  read(NewDeckName), atom_string(NewDeckName, StrDeckName),
  editDeckName(Deck.name, StrDeckName),
  writeln("\nNome alterado com sucesso!\n"),
  mainMenu().

cardsMenu(Deck, []):-
  length(Deck.cards, LenCards), LenCards =:= 0,
  writeln("\n        Esse deck está vazio :(\n"), 
  mainMenu(), !.

cardsMenu(Deck, []):-
  writeln("        Você concluiu o estudo desse deck :D\n"), 
  mainMenu(), !.

cardsMenu(Deck, [H|T]):-
  cardQA(Deck, H), cardsMenu(Deck, T).

cardQA(Deck, Card):-
  string_concat("\n<<  ", Deck.name, ParcialString),
  string_concat(ParcialString, "  >>", StringName),
  nth0(0, Card, Front), nth0(1, Card, Back), nl,
  writeln(Front),
  writeln("\n        > Pressione ENTER para revelar a resposta <    \n"),
  get_single_char(_),
  writeln(Back),
  writeln("\n  [E] Editar carta  [R] Remover carta  [X] Voltar  "),
  writeln("          > Pressione C para continuar <         "),
  write("\n> O que você deseja? "),
  read(Option),
  string_upper(Option, OptionUpper),
  menuOptionsCard(OptionUpper, Deck, Card).
  
addCardMenu(Deck):-
  writeln("> Qual será a frente da carta?"),
  read(Front), atom_string(Front, StrFront),
  writeln("\n> Qual será o verso da carta?"),
  read(Back), atom_string(Back, StrBack),
  Card = [StrFront, StrBack],
  addCard(Deck.name, Card),
  writeln("\nCarta adicionada com sucesso!\n"),
  mainMenu().

removeDeckMenu(Deck):-
  writeln("\n> Tem certeza que deseja remover o deck? [Y]"),
  read(Option), string_upper(Option, OptionUpper),
  confirmRemove(OptionUpper, Deck).

confirmRemove("Y", Deck):- 
  deleteDeck(Deck.name), 
  writeln("\nO deck foi removido com sucesso!\n"),
  mainMenu(), !.
confirmRemove(_, _):- mainMenu().

editCardMenu(Deck, Card):-
  removeCard(Deck.name, Card),
  writeln("> Qual será a frente da carta?"),
  read(Front), atom_string(Front, StrFront),
  writeln("\n> Qual será o verso da carta?"),
  read(Back), atom_string(Back, StrBack),
  Card = [StrFront, StrBack],
  addCard(Deck.name, Card),
  writeln("\nCarta editada com sucesso!\n"),
  mainMenu().

errorMenu():-
  writeln("################# Opção inválida! #################\n").

menuOptionsDeck("C") :- createDeckMenu(), !.
menuOptionsDeck("E") :- chooseDeckMenu(), !.
menuOptionsDeck("S") :- halt, !.
menuOptionsDeck(_) :- errorMenu().

menuOptionsChoosedDeck("I", Deck) :- cardsMenu(Deck, Deck.cards), !.
menuOptionsChoosedDeck("E", Deck) :- editDeckNameMenu(Deck), !.
menuOptionsChoosedDeck("A", Deck) :- addCardMenu(Deck), !.
menuOptionsChoosedDeck("R", Deck) :- removeDeckMenu(Deck), !.
menuOptionsChoosedDeck("X", _) :- mainMenu(), !.
menuOptionsChoosedDeck(_, _) :- errorMenu().

menuOptionsCard("E", Deck, Card) :- editCardMenu(Deck, Card), !.
menuOptionsCard("R", Deck, Card) :- removeCardMenu(Deck, Card), !.
menuOptionsCard("X", _, _) :- mainMenu(), !.
menuOptionsCard("C", _, _) :- !.
menuOptionsCard(_, _, _) :- errorMenu().
