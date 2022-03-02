module Models.Card where
	data Card = Card {
		front:: String,
		back:: String
	} deriving (Show, Read, Eq)