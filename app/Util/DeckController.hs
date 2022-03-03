{-# LANGUAGE TypeSynonymInstances #-}
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
  import Data.List (elemIndex)
  import Data.Maybe (fromMaybe)

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
  addAndSave :: Deck -> IO [Deck]
  addAndSave deck = do
    db <- loadDB
    let addedList = db ++ [deck]
    writeDB addedList
    return addedList

  class CanSearch v where
    -- |Searches a Deck in the database, by name.
    search :: v -> IO Deck
  instance CanSearch String where
    search nameToSearch = do
      db <- loadDB
      let m = filter (\deck -> nameToSearch >-= deck) db
      return (if length m == 0 then Deck { name="NIL", cards=[] } else head m)

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
      db <- loadDB
      deck <- search nameToSearch
      let decks = filter (\deckToCompare -> not (deck >== deckToCompare)) db
      writeDB decks
      return decks
  instance CanRemoveAndSave Deck where
    removeAndSave deck = do
      db <- loadDB
      let decks = filter (\deckToCompare -> not (deck >== deckToCompare)) db
      writeDB decks
      return decks

  class CanEditDeckName v1 v2 where
    editDeck :: v1 -> v2 -> IO Deck
  instance CanEditDeckName String String where
    editDeck deckName newDeckName = do
      db <- loadDB
      let dbAsNames = map (\elm -> (name elm)) db
      let idx = fromMaybe (-1) (elemIndex deckName dbAsNames)
      case idx == -1 of
        True -> do 
          print "Index doesn't exists"
          return Deck { name="NIL", cards=[] }
        False -> do
          let oldElm = db!!idx
          let newElm = Deck { name=newDeckName, cards=(cards oldElm) }
          let (s, _:end) = splitAt idx db
          let newDb = s ++ newElm : end
          return newElm
          
  class CanEditDeckNameAndSave v1 v2 where
    editDeckAndSave :: v1 -> v2 -> IO [Deck]
  instance CanEditDeckNameAndSave String String where
    editDeckAndSave deckName newDeckName = do
      db <- loadDB
      let dbAsNames = map (\elm -> (name elm)) db
      let idx = fromMaybe (-1) (elemIndex deckName dbAsNames)
      case idx == -1 of
        True -> do 
          putStrLn "Couldn't find the Deck"
          return db
        False -> do
          let oldElm = db!!idx
          let newElm = Deck { name=newDeckName, cards=(cards oldElm) }
          let (s, _:end) = splitAt idx db
          let newDb = s ++ newElm : end
          writeDB newDb
          return newDb

  -- |Returns true if left-hand name equals to right-hand deck\'s name.
  (>-=) :: String -> Deck -> Bool
  (>-=) cName deck = (cName) == (name deck)
  
  -- |Returns true if both decks are equals
  (>==) :: Deck -> Deck -> Bool
  -- TODO: Há alguma especificidade para implementar aqui?
  (>==) deck1 deck2 = deck1 == deck2
