-- Converts .lhs (literary Haskell files) to .hs (plain Haskell files)
-- Keeps only the statements which are normally compiled, plus blank lines.

-- To use:
--    ghc --make lhs2hs.hs
-- to get an executable file lhs2hs.  
-- Then 
--    lhs2hs filename
-- will open filename.lhs and save the converted file in filename.hs

-- by Scot Drysdale on 28/07/07, based on SOE program on p. 241
-- changed by Walter Schulze 15/08/15

module Main where
import System.Environment   -- to allow getArgs
import System.IO
import System.IO.Error

-- Opens a file, given name and mode
openGivenFile :: String -> IOMode -> IO Handle
openGivenFile name mode 
  = do handle <- openFile name mode
       return handle
 
main = do args <- getArgs
          content <- readFile (args !! 0 ++ ".lhs")
          toHandle <- openGivenFile (args !! 0 ++ ".hs") WriteMode
          convertFile (lines content) toHandle
          hClose toHandle

-- Converts all the lines in a file
convertFile :: [String] -> Handle -> IO ()
convertFile [] toHandle = return ()
convertFile (line:restOfLines) toHandle 
  = do case line of
                ('>' : ' ' : rest) -> hPutStrLn toHandle rest
                ('>' : rest)       -> hPutStrLn toHandle rest
                ('\n' : rest)      -> hPutStrLn toHandle line
                ('\r' : rest)      -> hPutStrLn toHandle line
                []                 -> hPutStrLn toHandle line
                comment            -> hPutStrLn toHandle ('-':'-':' ':comment)
       convertFile restOfLines toHandle
       
