module Util.CLIController where
import Util.TxtController
import Util.DeckController
import Models.Deck
import Models.Card
import Data.List (group)


menuDecks:: IO String
menuDecks = do
  namesDecks <- getDecksNames
  case length namesDecks == 0 of
    True -> do
      return "Você não possui decks"
    False -> do
      let numberedDecks = zipWith (\n line -> show n ++ " - " ++ line) [1..] namesDecks 
      let list = ("Seus decks:\n" : numberedDecks)
      let menuString = unlines list
      return menuString

gummyReminderLogo:: String
gummyReminderLogo =
    "───────────────────────────────────────────────────\n"
  ++"       .--.                                        \n"
  ++"      : .--'                                       \n"
  ++"      : : _ .-..-.,-.,-.,-.,-.,-.,-..-..-.         \n"
  ++"      : :; :: :; :: ,. ,. :: ,. ,. :: :; :         \n"
  ++"      `.__.'`.__.':_;:_;:_;:_;:_;:_;`._. ;         \n"
  ++"                                     .-. :         \n"
  ++"                                     `._.'         \n"
  ++"  .---.                 _          .-.             \n"
  ++"  : .; :               :_;         : :             \n"
  ++"  :   .' .--. ,-.,-.,-..-.,-.,-. .-' : .--. .--.   \n"
  ++"  : :.`.' '_.': ,. ,. :: :: ,. :' .; :' '_.': ..'  \n"
  ++"  :_;:_;`.__.':_;:_;:_;:_;:_;:_;`.__.'`.__.':_;    \n"

initialMenu:: String
initialMenu = 
    "──────────────────── Bem-vindo ────────────────────\n\n"
  ++"             Aprenda com o auxílio de             \n"
  ++"              cartões de memorização             \n\n"
  ++"         > Pressione ENTER para iniciar <          "

putLine:: String
putLine = "───────────────────────────────────────────────────\n" 

putLineCard:: String
putLineCard = "- - - - - - - - - - - - - - - - - - - - - - - - - -\n" 
