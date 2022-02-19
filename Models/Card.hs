module Models.Card where
	data Card = Card {
		id:: Int,
		front:: String,
		back:: String
	} deriving (Show, Read)