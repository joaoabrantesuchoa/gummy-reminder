module Models.Deck where
	import qualified Models.Card as C
	import Data.List

  data Deck = Deck {
    name:: String,
    cards:: [C.Card]
  } deriving (Show, Read, Eq)

	addCard:: Deck -> C.Card -> Deck
	addCard deck newCard = 
	 	Deck (name deck) (cards deck ++ [newCard])

	removeCard:: Deck -> C.Card -> Deck
	removeCard deck card =
		Deck (name deck) (delete card (cards deck))

	editCard:: Deck -> C.Card -> String -> String -> Deck
	editCard deck card newFront newBack = do
		let editedCard = C.Card newFront newBack
		let editedDeck = removeCard deck card
		Deck (name deck) (cards editedDeck ++ [editedCard])