module Util.TxtController where
  import System.IO
  import System.Directory
  import Models.Deck
  import Data.List (delete)

  -- |Loads Decks database into memory, and returns it as a list ([Deck])
  loadDB :: IO [Deck]
  loadDB = do
    file <- readFile "./database/Decks.txt"
    let lista = (read file :: [Deck])
    seq (length lista) (return ())
    return lista
    
  -- |Prints Decks database into stdin
  printDB :: IO ()
  printDB = do
    file <- readFile "./database/Decks.txt"
    let lista = (read file :: [Deck])
    seq (length lista) (return ())
    print lista

  -- |Writes given deck list into Decks database.
  writeDB :: [Deck] -> IO ()
  writeDB deck = do
    writeFile "./database/Decks.txt" (show deck)
