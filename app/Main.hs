module Main where
import Util.TxtController
import Util.DeckController
import Models.Deck
import Models.Card
import Data.List (delete)

main :: IO()
main = do    
  decks <- loadDB
  print decks
--   printDB
--   editDeckAndSave "deck1" "Deck1"
--   printDB
  -- let deck1 = Deck "Deck1" []
  -- let card1 = Card  "Jose" "Augusto"
  -- let newDeck = addCard deck1 card1 
  -- putStrLn $ show newDeck 
  -- let card2 = Card  "Joao" "Gabriel" 
  -- let newDeck2 = addCard newDeck card2 
  -- putStrLn $ show newDeck2
  -- let newDeck3 = removeCard newDeck2 card1
  -- putStrLn $ show newDeck3
  -- let newDeck4 = editCard newDeck3 card2 "Jose" "Augusto"
  -- putStrLn $ show newDeck4