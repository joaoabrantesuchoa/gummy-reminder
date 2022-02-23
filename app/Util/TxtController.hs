module Util.TxtController where
  import System.IO
  import System.Directory
  import Models.Deck
  import Data.List (delete)


  loadDB :: IO([Deck])
  loadDB = do
    file <- readFile "./database/Decks.txt"
    let lista = (read file :: [Deck])
    seq (length lista) (return ())
    return lista
    
  addDeck :: Deck -> IO ()
  addDeck deck = do
    db <- loadDB 
    let addedList = db ++ [deck]
    writeFile "./database/Decks.txt" (show addedList)

  searchByName :: String -> IO Deck
  searchByName nameToSearch = do
    db <- loadDB
    let m = filter (eqName nameToSearch) db
    return (head m)

  -- TODO: Não está feito
  -- removeByName :: String -> IO ()
  -- removeByName nameToSearch = do
  --   db <- loadDB
  --   let deck = searchByName nameToSearch
  --   let decks = filter ()
  --   writeFile "./database/Decks.txt" (show decks)


  eqName :: String -> Deck -> Bool
  eqName nameToSearch deck = nameToSearch == (name deck)