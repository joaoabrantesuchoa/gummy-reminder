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
  mainMenu


mainMenu:: IO ()
mainMenu = do
  putStrLn putLine

  menu <- menuDecks
  putStrLn menu

  putStrLn "   [C] Criar deck  [E] Escolher deck  [S] Sair"
  putStrLn "\n> O que você deseja? "
  option <- getLine
  putStrLn ""
  menuOptions (map toUpper option)


menuOptions:: String -> IO ()
menuOptions option | option == "C" = createDeckMenu
                   | option == "E" = chooseDeckMenu
                   | option == "S" = quitMenu
                   | otherwise = errorMenu


createDeckMenu:: IO ()
createDeckMenu = do
  putStrLn "Digite o nome do deck:"
  nameDeck <- getLine
  addAndSave nameDeck
  putStrLn "\nDeck criado com sucesso!\n"
  mainMenu


chooseDeckMenu:: IO ()
chooseDeckMenu = do
  putStrLn "> Escolha o número do deck: "
  numDeck <- readLn
  db <- loadDB
  putStrLn ""
  
  case (numDeck > 0 && numDeck <= length db) of
    True -> do
      putStrLn putLine
      let deck = db!!(numDeck-1)
      putStrLn $ "<<  " ++ (name deck) ++ "  >>\n"
      putStrLn "[I] Iniciar revisão  [E] Editar nome  [A] Add carta\n          [R] Remover deck   [X] Voltar"
      putStrLn "\n> O que você deseja?"
      option <- getLine
      putStrLn ""
      deckMenuOptions (map toUpper option) deck
    False -> do
      putStrLn "\n# Número inválido #\n"
      chooseDeckMenu 


deckMenuOptions:: String -> Deck -> IO ()
deckMenuOptions option deck | option == "I" = cardsMenu deck (cards deck)
                            | option == "E" = editDeckNameMenu deck
                            | option == "A" = addCardMenu deck
                            | option == "R" = removeDeckMenu deck
                            | option == "X" = mainMenu
                            | otherwise = errorMenu


cardsMenu:: Deck -> [Card] -> IO()
cardsMenu deck deckCards = do
  deckSearch <- search (name deck)
  case (length (cards deckSearch)) == 0 of
    True -> do
      putStrLn putLine
      putStrLn "              Esse deck está vazio :(           \n"
      mainMenu
    False -> do 
      shuffleDeckAndSave deckSearch
      let headCard = (head deckCards)

      case length deckCards == 0 of
        True -> do
          putStrLn putLine
          putStrLn "        Você concluiu o estudo desse deck :D      \n"
          mainMenu
        False -> do
          cardQA deck headCard
          cardsMenu deck $ tail deckCards
  

cardQA:: Deck -> Card -> IO()
cardQA deck card = do
  putStrLn putLine
  putStrLn $ "<<  " ++ (name deck) ++ "  >>\n"
  putStrLn (front card)

  putStrLn "\n    > Pressione ENTER para revelar a resposta <    "
  getLine

  putStrLn putLineCard

  putStrLn (back card)
  putStrLn "\n  [E] Editar carta  [R] Remover carta  [X] Voltar  "
  putStrLn "        > Pressione ENTER para continuar <         "  
  putStrLn "\n> O que você deseja? "  
  option <- getLine 
  putStrLn ""
  cardMenuOptions (map toUpper option) deck card


cardMenuOptions:: String -> Deck -> Card -> IO ()
cardMenuOptions option deck card | option == "E" = editCardMenu deck card 
                                 | option == "R" = removeCardMenu deck card
                                 | option == "X" = mainMenu
                                 | option == "" = return() 
                                 | otherwise = errorMenu       


editDeckNameMenu:: Deck -> IO ()
editDeckNameMenu deck = do
  putStrLn putLine
  putStrLn "> Qual o novo nome do deck?"
  newDeckName <- getLine
  editDeckAndSave (name deck) newDeckName
  putStrLn "\nNome alterado com sucesso!\n"
  mainMenu


addCardMenu:: Deck -> IO ()
addCardMenu deck = do
  putStrLn putLine
  putStrLn "> Qual será a frente da carta?"
  front <- getLine

  putStrLn "\n> Qual será o verso da carta?"
  back <- getLine

  let newCard = Card front back
  let editedDeck = addCard deck newCard
  editDeckAndSave (name editedDeck) (cards editedDeck) 
  putStrLn "\nCarta adicionada com sucesso!\n"
  mainMenu
  

removeDeckMenu:: Deck -> IO ()
removeDeckMenu deck = do
  putStrLn "\n> Tem certeza que deseja remover o deck? [Y]"
  option <- getLine 
  case (map toUpper option) == "Y" of
    True -> do
      removeAndSave deck
      putStrLn "\nO deck foi removido com sucesso!\n"
      mainMenu
    False -> do
      mainMenu


editCardMenu:: Deck -> Card -> IO ()
editCardMenu deck card = do
  putStrLn "> Qual será a frente da carta?"
  newFront <- getLine

  putStrLn "\n> Qual será o verso da carta?"
  newBack <- getLine

  let editedDeck = editCard deck card newFront newBack
  editDeckAndSave (name editedDeck) (cards editedDeck) 
  putStrLn "\nCarta editada com sucesso!\n"
  mainMenu


removeCardMenu:: Deck -> Card -> IO ()
removeCardMenu deck card = do
  putStrLn "> Tem certeza que deseja remover a carta? [Y]"
  option <- getLine 
  case (map toUpper option) == "Y" of
    True -> do
      let editedDeck = removeCard deck card
      editDeckAndSave (name editedDeck) (cards editedDeck) 
      putStrLn "\nA carta foi removida com sucesso!\n"
      mainMenu
    False -> do
      mainMenu


errorMenu:: IO()
errorMenu = do
  putStrLn "################# Opção inválida! #################\n"
  mainMenu


quitMenu:: IO()
quitMenu = do
  putStrLn "Adeus!"
