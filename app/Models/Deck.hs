module Models.Deck where
  import qualified Models.Card as C

  data Deck = Deck {
    name:: String,
    cards:: [C.Card]
  } deriving (Show, Read, Eq)

  -- addCard:: Deck -> C.Card -> Deck
  -- addCard deck newCard = 
  -- 	Deck (deck) (name deck) (cards deck ++ [newCard])

  --setName:: Deck -> String -> Deck
  --setName deck newName = 
  --	Deck {idDeck deck, name = newName, cards deck}

  --getNome:: Deck -> String
  --getNome name = 