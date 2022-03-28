:- use_module('models/deck.pl', [cadastra_deck/4]).

main:-
  cadastra_deck(1, "Inglês", [ ["Teste", "Test1"] ], Deck1),
  writeln(Deck1).