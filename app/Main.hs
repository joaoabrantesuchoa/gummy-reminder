module Main where

import Models.Card
import Models.Deck
	
main:: IO()
main = do
	let card1 = Card 1 "Teste1" "Resposta1"
	let deck1 = Deck 1 "Deck1" []
	let newDeck = addCard deck1 card1 
	let card2 = Card 2 "asdasdasdas" "czxczxczx" 
	let newDeck2 = addCard newDeck card2 
	putStrLn $ show card1 
	putStrLn $ show deck1 
	putStrLn $ show newDeck
	putStrLn $ show newDeck2
