
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : DeckController
Description : Controller para ações em database/Decks.txt
Copyright   : Mesma do projeto
License     : Mesma do projeto
Maintainer  : Pedro Casado (pgscasado.pessoal@gmail.com)
Stability   : development
Portability : POSIX

Esse módulo foi feito para realizar CRUD e operações extras sobre Decks (definido por 'Models.Decks')
-}
module Util.DeckController where
  import Models.Deck
  import Util.TxtController (loadDB, writeDB)
  import Data.List (elemIndex, permutations)
  import Data.Maybe (fromMaybe)
  import Models.Card
  import GHC.IO (unsafePerformIO)
  import System.Random (getStdGen, randomRIO)

  -- |Returns the deck names from the database.
  getDecksNames :: IO [String]
  getDecksNames = do
    db <- loadDB
    let names = [name deck | deck <- db]
    return names

  -- |Adds a deck to the database, but doesn\'t saves it.
  add :: Deck -> IO [Deck]
  add deck = do
    db <- loadDB
    let addedList = db ++ [deck]
    return addedList

  -- |Adds a deck to the database, and saves it.
  --
  -- This action will carry changes to 'database/Decks.txt'.
  class CanAdd v where
    addAndSave :: v -> IO [Deck]
  instance CanAdd Deck where
    addAndSave deck = do
      addedList <- add deck
      writeDB addedList
      return addedList
  instance CanAdd String where
    addAndSave nameDeck = do
      addedList <- add Deck { name=nameDeck, cards=[] }
      writeDB addedList
      return addedList


  class CanSearch v where
    -- |Searches a Deck in the database, by name.
    search :: v -> IO Deck
  instance CanSearch String where
    search nameToSearch = do
      db <- loadDB
      let m = filter (nameToSearch >-=) db
      return (if null m then Deck { name="NIL", cards=[] } else head m)

  class CanRemove v where
    -- |Removes a Deck from database, but doesn\'t removes it from database, instead it does just returns it, while this deck can be searched by name (receiving a 'String') or by a proper 'Models.Deck'.
    remove :: v -> IO [Deck]
  instance CanRemove String where
    remove nameToSearch = do
      db <- loadDB
      deck <- search nameToSearch
      let decks = filter (\deckToCompare -> not (deck >== deckToCompare)) db
      return decks
  instance CanRemove Deck where
    remove deck = do
      db <- loadDB
      let decks = filter (\deckToCompare -> not (deck >== deckToCompare)) db
      return decks

  class CanRemoveAndSave v where
    -- |Removes a Deck from database permanently, while this deck can be searched by name (receiving a 'String') or by a proper 'Models.Deck'.
    --
    -- This action will carry changes to 'database/Decks.txt'.
    removeAndSave :: v -> IO [Deck]
  instance CanRemoveAndSave String where
    removeAndSave nameToSearch = do
      decks <- remove nameToSearch
      writeDB decks
      return decks
  instance CanRemoveAndSave Deck where
    removeAndSave deck = do
      decks <- remove deck
      writeDB decks
      return decks

  class CanEditDeckName v1 v2 where
    editDeck :: v1 -> v2 -> IO [Deck]
  instance CanEditDeckName String String where
    editDeck deckName newDeckName = do
      db <- loadDB
      let dbAsNames = map name db
      let idx = fromMaybe (-1) (elemIndex deckName dbAsNames)
      if idx == -1 then (do
        print "Index doesn't exists"
        return db) else (do
        let oldElm = db!!idx
        let newElm = Deck { name=newDeckName, cards=cards oldElm }
        let (s, _:end) = splitAt idx db
        let newDb = s ++ newElm : end
        return newDb)


  instance CanEditDeckName String [Card] where
    editDeck deckName newCards = do
      db <- loadDB
      let dbAsNames = map name db
      let idx = fromMaybe (-1) (elemIndex deckName dbAsNames)
      if idx == -1 then (do
        putStrLn "Couldn't find the Deck"
        return db) else (do
        let oldElm = db!!idx
        let newElm = Deck { name=name oldElm, cards=newCards }
        let (s, _:end) = splitAt idx db
        let newDb = s ++ newElm : end
        return newDb)

  class CanEditDeckNameAndSave v1 v2 where
    editDeckAndSave :: v1 -> v2 -> IO [Deck]
  instance CanEditDeckNameAndSave String String where
    editDeckAndSave deckName newDeckName = do
      deck <- editDeck deckName newDeckName
      writeDB deck
      return deck
  instance CanEditDeckNameAndSave String [Card] where
    editDeckAndSave deckName newCards = do
      deck <- editDeck deckName newCards
      writeDB deck
      return deck

  class CanShuffleDeck a where
    shuffleDeck :: a -> IO [Deck]
  instance CanShuffleDeck Deck where
    shuffleDeck deck = do
      db <- loadDB
      foundDeck <- search (name deck)
      shuffledDeck <- rndPermutation (cards foundDeck)
      editDeck (name deck) shuffledDeck
  instance CanShuffleDeck String where
    shuffleDeck deckName = do
      db <- loadDB
      foundDeck <- search deckName
      shuffledDeck <- rndPermutation (cards foundDeck)
      editDeck deckName shuffledDeck

  class CanShuffleDeckAndSave a where
    shuffleDeckAndSave :: a -> IO [Deck]
  instance CanShuffleDeckAndSave Deck where
    shuffleDeckAndSave deck = do
      newDb <- shuffleDeck deck
      print newDb
      writeDB newDb
      return newDb
  instance CanShuffleDeckAndSave String where
    shuffleDeckAndSave deckName = do
      newDb <- shuffleDeck deckName
      print newDb
      writeDB newDb
      return newDb

  rndElem :: [a] -> IO a
  rndElem xs = do
    index <- randomRIO (0, length xs - 2)
    return $ xs !! index

  rndPermutation :: [a] -> IO [a]
  rndPermutation = rndElem . permutations

  -- |Returns true if left-hand name equals to right-hand deck\'s name.
  (>-=) :: String -> Deck -> Bool
  (>-=) cName deck = cName == name deck

  -- |Returns true if both decks are equals
  (>==) :: Deck -> Deck -> Bool
  -- TODO: Há alguma especificidade para implementar aqui?
  (>==) deck1 deck2 = deck1 == deck2
