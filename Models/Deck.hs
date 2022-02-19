module Models.Deck where
	import qualified Models.Card as C

	data Deck = Deck {
		idDeck:: Int,
		name:: String,
		cards:: [C.Card]
	} deriving (Show, Read)

	addCard:: Deck -> C.Card -> Deck
	addCard deck newCard = 
		Deck (idDeck deck) (name deck) (cards deck ++ [newCard])

	--setName:: Deck -> String -> Deck
	--setName deck newName = 
	--	Deck {idDeck deck, name = newName, cards deck}

	--getNome:: Deck -> String
	--getNome name = 