:-module('CLIController', [
  gummyReminderLogo/0,
  initialMenu/0,
  mainMenu/0
]).

:-use_module('../util/JsonFunctions.pl').
:-use_module(library(readutil)).
:- set_prolog_flag('encoding', 'utf8').
:- style_check(-singleton).

% Descrição:
%		Imprime a logo do projeto
% Parâmetros: Sem parâmetros
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


% Descrição:
%		Imprime a linha para ser mostrada no meio do card.
% Parâmetros: Sem parâmetros
cardLine():- writeln("        - - - - - - - - - - - - - - - - - - - - - - - - - -\n"). 

% Descrição:
%		Imprime uma linha no tamanho padrão.
% Parâmetros: Sem parâmetros
line:- 
  repl("\u2500", 70, L),
  atomic_list_concat(L, LineStr),
  writeln(LineStr).

% Descrição:
%		Gera uma linha no tamanho definido
% Parâmetros: 
%   1: +Len: O tamanho desejado
%   2: -R: O resultado
lineAtom(Len, R):-
  repl("\u2500", Len, L),
  atomic_list_concat(L, LineStr),
  R = LineStr.

% Descrição:
%		Replica uma string N vezes
% Parâmetros: 
%   1: +X: A string a ser replicada
%   2: +N: A quantidade de vezes
%   3: -L: O resultado
repl(X, N, L) :-
    length(L, N),
    maplist(=(X), L).

% Descrição:
%		Imprime o menu inicial.
% Parâmetros: Sem parâmetros
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

% Descrição:
%		Imprime o menu principal (de loop).
%   Aqui é lida a opção da ação que o usuário executará depois: "Criar Deck"
%   "Escolher Deck", ou "Fechar o programa". Depois o usuário é redirecionado
%   para a ação escolhida.
% Parâmetros: Sem parâmetros
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

% Descrição:
%		Obtêm o nome de um deck
% Parâmetros: 
%   1: +E: Elemento de Deck
%   2: -Out: O resultado
getDeckName(E, Out):-
  Out = E.name.

% Descrição:
%		Converte uma lista de nomes de decks para uma lista de nomes de decks com
%   índices como sufixo. Segue o padrão: (índice - nome do deck)
% Parâmetros: 
%   1: +L: (Layer) índice a ser incrementado
%   2: DecksNameList: A lista de nomes de decks
%   2: -Out: O resultado
listDecksNamesAndIndex(_, [], []).
listDecksNamesAndIndex(L, [H|T], [HOut|Rest]):-
  atomic_list_concat([L, " - ", H], HOut),
  L2 is L+1,
  listDecksNamesAndIndex(L2, T, Rest).

% Descrição:
%		Imprime o menu de criação de deck.
%   Aqui são lidas as informações do novo deck a ser criado.
% Parâmetros: Sem parâmetros
createDeckMenu():-
  write("\nDigite o nome do deck: "),
  readLine(NameDeck),
  createDeck(NameDeck, []), nl, line,
  mainMenu().

% Descrição:
%		Imprime o menu de seleção de deck.
%   Aqui é lida a opção do deck a ser operado.
% Parâmetros: Sem parâmetros
chooseDeckMenu():-
  write("\n> Escolha o n\u00FAmero do deck: "),
  readLine(NumDeck),
  chooseDeck(NumDeck), !.

% Descrição:
%		Imprime o menu de deck selecionado.
%   Aqui é lida a opção do deck a ser operado. Também imprime as opções
%   de operação sobre o deck selecionado. Depois coleta para redirecionar
%   o usuário para "Iniciar revisão", "Editar nome do deck", "Adicionar
%   carta", "Apagar deck" ou "Voltar ao menu principal".
% Parâmetros:
%   1: +NumDeckStr: Número do deck, como string.
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

% Descrição:
%		Condição de entrada inválida
% Parâmetros: 
%   1: +NumDeck: Número do deck.
chooseDeck(NumDeck):-
  writeln("\n# N\u00FAmero inv\u00E1lido #\n"),
  line, nl, mainMenu().

% Descrição:
%		Edita o nome de um deck.
% Parâmetros: 
%   1: +Deck: Um deck.
editDeckNameMenu(Deck):-
  write("\n> Qual o novo nome do deck? "),
  readLine(NewDeckName),
  editDeckName(Deck.name, NewDeckName),
  writeln("\nNome alterado com sucesso!\n"),
  line, nl, mainMenu().

% Descrição:
%		Condição de deck vazio.
% Parâmetros: 
%   1: +Deck: Um deck.
cardsMenu(Deck, []):-
  length(Deck.cards, LenCards), LenCards =:= 0,
  writeln("\n                       Esse deck est\u00E1 vazio :(\n"), 
  line, nl, mainMenu(), !.

% Descrição:
%		Condição de deck concluído.
% Parâmetros: 
%   1: +Deck: Um deck.
%   2: +Cards: A lista de cards do deck.
cardsMenu(Deck, []):-
  writeln("\n                  Voc\u00EA concluiu o estudo desse deck :D\n"), 
  line, nl, mainMenu(), !.

% Descrição:
%		Itera sobre as cartas de um deck apresentando-as. Depois retorna ao
%   menu do card.
% Parâmetros: 
%   1: +Deck: Um deck.
%   2: +Cards: A lista de cards do deck.
cardsMenu(Deck, [H|T]):-
  cardQA(Deck, H), cardsMenu(Deck, T).

% Descrição:
%		Apresenta um card ao usuário
% Parâmetros: 
%   1: +Deck: Um deck.
%   2: +Card: O card a ser apresentado.
cardQA(Deck, Card):-
  string_concat("\n<<  ", Deck.name, ParcialString),
  string_concat(ParcialString, "  >>", StringName),
  nth0(0, Card, Front), nth0(1, Card, Back), nl,
  writeln(Front),
  writeln("\n        > Pressione qualquer tecla para revelar a resposta <        \n"),
  get_single_char(_), cardLine(),
  writeln(Back),
  writeln("\n           [e] Editar carta  [r] Remover carta  [x] Voltar           "),
  writeln("                  > Pressione ENTER para continuar <                  "),
  write("\n> O que voc\u00EA deseja? "),
  readLine(Option), nl, line,
  string_upper(Option, OptionUpper),
  menuOptionsCard(OptionUpper, Deck, Card).
  
% Descrição:
%		Imprime o menu de criar um card
% Parâmetros: 
%   1: +Deck: O deck a ser alterado.
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

% Descrição:
%		Imprime o menu de apagar um deck
% Parâmetros: 
%   1: +Deck: O deck a ser removido.
removeDeckMenu(Deck):-
  writeln("\n> Tem certeza que deseja remover o deck? [y] "),
  readLine(Option), string_upper(Option, OptionUpper),
  confirmRemove(OptionUpper, Deck).

% Descrição:
%		Confirma a remoção de um deck
% Parâmetros: 
%   1: +Option: Se "Y", será removido o deck.
%   2: +Deck: O deck a ser removido.
confirmRemove("Y", Deck):- 
  deleteDeck(Deck.name), 
  writeln("\nO deck foi removido com sucesso!\n"),
  line, nl, mainMenu(), !.
% Descrição:
%		Confirma a remoção de um deck
% Parâmetros: 
%   1: +Option: Se não for "Y", retorna ao menu principal.
%   2: +Deck: O deck a ser removido.
confirmRemove(_, _):- nl, line, nl, mainMenu().

% Descrição:
%		Edita um card de um deck
% Parâmetros: ,
%   1: +Deck: O deck a ser alterado.
%   2: +Card: O card a ser substituído.
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

% Descrição:
%		Remove um card de um deck
% Parâmetros: ,
%   1: +Deck: O deck a ser alterado.
%   2: +Card: O card a ser removido.
removeCardMenu(Deck, Card):-
  write("\n> Tem certeza que deseja remover a carta? [y] "),
  readLine(Option), string_upper(Option, OptionUpper),
  confirmRemoveCard(OptionUpper, Deck, Card).

% Descrição:
%		Confirma a remoção de uma carta
% Parâmetros: 
%   1: +Option: Se "Y", será removido a carta.
%   2: +Deck: O deck a ser alterado.
%   3: +Card: A carta a ser removida.
confirmRemoveCard("Y", Deck, Card):- 
  removeCard(Deck.name, Card), 
  writeln("\nA carta foi removida com sucesso!\n"),
  line, nl, mainMenu(), !.
% Descrição:
%		Confirma a remoção de uma carta
% Parâmetros: 
%   1: +Option: Se não for "Y", retorna ao menu principal.
%   2: +Deck: O deck a ser alterado.
%   3: +Card: A carta a ser removida.
confirmRemoveCard(_, _, _):- nl, line, nl, mainMenu().

% Descrição:
%		Imprime a mensagem de opção inválida e retorna ao menu.
% Parâmetros: Sem parâmetros
errorMenu():-
  nl, writeln("########################## Op\u00E7\u00E3o inv\u00E1lida! #########################\n"), mainMenu().

% Descrição:
%		Premissa de redirecionamento de ação do menu principal.
% Parâmetros: 
%   1: +Option: Se for "C", redireciona ao menu de criação de deck;
%               Se for "E", redireciona ao menu de escolher de deck;
%               Se for "S", fecha o programa;
%               Se for nenhuma acima, mostra "opção inválida";
menuOptionsDeck("C") :- createDeckMenu(), !.
menuOptionsDeck("E") :- chooseDeckMenu(), !.
menuOptionsDeck("S") :- halt, !.
menuOptionsDeck(_) :- errorMenu().

% Descrição:
%		Premissa de redirecionamento de ação de operação sobre deck.
% Parâmetros: 
%   1: +Option: Se for "I", redireciona para a revisão (cardsQA);
%               Se for "E", redireciona ao menu de editar nome de deck;
%               Se for "A", redireciona ao menu de adicionar card;
%               Se for "R", redireciona ao menu de apagar deck;
%               Se for "X", volta ao menu inicial;
%               Se for nenhuma acima, mostra "opção inválida";
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

% Descrição:
%		Premissa de redirecionamento de ação de operação sobre card.
% Parâmetros: 
%   1: +Option: Se for "E", redireciona ao menu de editar card;
%               Se for "R", redireciona ao menu de remover;
%               Se for "X", redireciona ao menu principal;
%               Se não for nenhuma acima, continua a iterar sobre os cards;
menuOptionsCard("E", Deck, Card) :- editCardMenu(Deck, Card), !.
menuOptionsCard("R", Deck, Card) :- removeCardMenu(Deck, Card), !.
menuOptionsCard("X", _, _) :- nl, mainMenu(), !.
menuOptionsCard("C", _, _) :- !.
menuOptionsCard(_, _, _) :- !.

% Descrição:
%		Premissa de leitura de string da entrada padrão.
% Parâmetros: 
%   1: +R: Resultado
readLine(R):- read_line_to_codes(user_input,Cs), atom_codes(A, Cs), atomic_list_concat(L, ' ', A), atom_string(A, R).
