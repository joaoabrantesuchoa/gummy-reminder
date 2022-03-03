module Main where
import Util.TxtController
import Util.DeckController
import Util.CLIController
import Models.Deck
import Models.Card
import Data.Char

main :: IO ()
main = do
  putStrLn gummyReminderLogo
  putStrLn initialMenu
  getLine
  putStrLn putLine
  mainMenu

mainMenu:: IO ()
mainMenu = do
  menu <- menuDecks
  putStrLn menu

  putStrLn "[C] Criar deck  [E] Escolher deck  [R] Remover deck"
  putStrLn "\n> O que você deseja? "
  option <- getLine
  putStrLn ""
  menuOptions (map toUpper option)

menuOptions:: String -> IO ()
menuOptions option | option == "C" = createDeckMenu
                   | option == "E" = chooseDeckMenu
                   | option == "R" = removeDeckMenu
                   | otherwise = errorMenu

createDeckMenu:: IO ()
createDeckMenu = do
  putStrLn putLine
  putStrLn "create"

chooseDeckMenu:: IO ()
chooseDeckMenu = do
  putStrLn "> Escolha o número do deck: "
  numDeck <- readLn
  db <- loadDB
  putStrLn ""
  
  case (numDeck > 0 && numDeck <= length db) of
    True -> do
      let deck = db!!(numDeck-1)
      cardsMenu (cards deck)
    False -> do
      putStrLn "\n# Número inválido #\n"
      chooseDeckMenu 

removeDeckMenu:: IO ()
removeDeckMenu = do
  putStrLn "remove"

errorMenu:: IO()
errorMenu = do
  putStrLn "# Opção inválida #\n"
  mainMenu

cardsMenu:: [Card] -> IO()
cardsMenu cards = do
  let headCard = (head cards)

  case length cards == 0 of
    True -> do
      putStrLn putLine
      putStrLn "        Você concluiu o estudo desse deck :D      \n"
      putStrLn putLine
      mainMenu
    False -> do
      cardQA headCard
      cardsMenu $ tail cards
  

cardQA:: Card -> IO()
cardQA card = do
  putStrLn putLine
  putStrLn (front card)

  putStrLn "\n    > Pressione ENTER para revelar a resposta <    "
  getLine

  putStrLn putLine

  putStrLn (back card)
  putStrLn "\n        > Pressione ENTER para continuar <       "
  getLine 
  return ()
