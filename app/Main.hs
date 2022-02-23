module Main where
import Util.TxtController
import Models.Deck
import Models.Card
import Data.List (delete)

main :: IO()
main = do    
  decks <- loadDB
  print decks

  -- main:: IO()
  -- main = do
    -- let deck1 = Deck 1 "Deck1" []
    -- let newDeck = addCard deck1 card1 
    -- let card2 = Card 2 "asdasdasdas" "czxczxczx" 
    -- let newDeck2 = addCard newDeck card2 
    -- putStrLn $ show card1 
    -- putStrLn $ show deck1 
    -- putStrLn $ show newDeck
    -- putStrLn $ show newDeck2
    


  -- gummyReminderLogo::String
  -- gummyReminderLogo =
  -- 	"\n"
  -- 	++"  ==================================================  \n"
  -- 	++"                                                      \n"		
  -- 	++"   _______                                            \n"
  -- 	++"  |   _   .--.--.--------.--------.--.--.             \n"
  -- 	++"  |.  |___|  |  |        |        |  |  |             \n"
  -- 	++"  |.  |   |_____|__|__|__|__|__|__|___  |             \n"
  -- 	++"  |:  1   |                       |_____|             \n"
  -- 	++"  |::.. . |                                           \n"
  -- 	++"  `-------'                                           \n"
  -- 	++"   _______                __          __              \n"
  -- 	++"  |   _   .-----.--------|__.-----.--|  .-----.----.  \n"
  -- 	++"  |.  l   |  -__|        |  |     |  _  |  -__|   _|  \n"
  -- 	++"  |.  _   |_____|__|__|__|__|__|__|_____|_____|__|    \n"
  -- 	++"  |:  |   |                                           \n"
  -- 	++"  |::.|:. |                                           \n"
  -- 	++"  `--- ---'                                           \n"
  -- 	++"                                                      \n"		
  -- 	++"  ==================================================  \n"	


  -- gummyReminderLogo:: String
  -- gummyReminderLogo =
  -- 	"\n"
  -- 	++"  .--.                                           \n"
  -- 	++" : .--'                                          \n"
  -- 	++" : : _ .-..-.,-.,-.,-.,-.,-.,-..-..-.            \n"
  -- 	++" : :; :: :; :: ,. ,. :: ,. ,. :: :; :            \n"
  -- 	++" `.__.'`.__.':_;:_;:_;:_;:_;:_;`._. ;            \n"
  -- 	++"                                .-. :            \n"
  -- 	++"                                `._.'            \n"
  -- 	++" .---.                 _          .-.            \n"
  -- 	++" : .; :               :_;         : :            \n"
  -- 	++" :   .' .--. ,-.,-.,-..-.,-.,-. .-' : .--. .--.  \n"
  -- 	++" : :.`.' '_.': ,. ,. :: :: ,. :' .; :' '_.': ..' \n"
  -- 	++" :_;:_;`.__.':_;:_;:_;:_;:_;:_;`.__.'`.__.':_;   \n"
                                               

                                                       


                                               

                                                       
                                                       

                                              

                                               
                                               

                                                   

                                              

                                         
                  



