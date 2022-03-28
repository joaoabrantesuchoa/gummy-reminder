:- use_module('models/deck.pl', [cadastra_deck/4]).

deck(Id, Nome, Cards).

main:-
  cadastra_deck(id(1), nome("Inglês"), cards("Teste"), Deck1),
  writeln(Deck1).