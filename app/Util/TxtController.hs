module Util.TxtController where
  import System.IO
  import System.Directory
  import Prelude as P

  showContent:: String -> IO([String])
  showContent path = do
    file <- openFile path ReadMode
    contentFile <- hGetContents file
    let contentArray = P.lines contentFile
    return contentArray