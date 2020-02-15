module IO where

import Types

import System.IO
import Control.Exception

import Lens.Micro

separator = "|"

formatBoard :: Board -> String
formatBoard (Board d _ _ t f hs _ _) = show d ++ separator ++ show t ++ separator ++ show f ++ separator ++ show hs

saveGS :: FilePath -> GameState -> IO GameState
saveGS fp gs = do
    eitherHandle <- try (openFile fp WriteMode) :: IO (Either SomeException Handle)
    case eitherHandle of
      (Left e) -> return (gs & (dialog . _Just . message) .~ (gs ^. (dialog . _Just . errMsg)))
      (Right handle) -> do
          hPutStrLn handle (formatBoard (gs ^. board))
          hClose handle
          return (gs & dialog .~ Nothing)

-- readBoard :: String -> Board
-- readBoard str = Board d (V2 1 1) Nothing t f hs Nothing ""
--     where
--         [dS, tS, fS, hsS] = splitOn separator str
--         d = read dS
--         t = read tS
--         f = read fS
--         hs = read hsS
