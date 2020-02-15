module IO (
    saveGS,
    loadGS
) where

import           Board
import           Types

import           Control.Exception
import           Data.Maybe
import           System.IO
import           Text.Read

import           Lens.Micro

import           Linear.V2

import           Data.List.Split

separator = "|"

--basic functions---------------------------------------------------------------

formatBoard :: Board -> String
formatBoard (Board d _ _ t f hs _ _) = show d ++ separator ++ show t ++ separator ++ show f ++ separator ++ show hs

readBoard :: String -> Maybe Board
readBoard str = if length s /= 4
                   || isNothing md
                   || isNothing mt
                   || isNothing mf
                   || isNothing mhs
                   then Nothing
                   else Just $ checkWin $ Board (fromJust md) (V2 1 1) Nothing (fromJust mt) (fromJust mf) (fromJust mhs) Nothing ""
    where
        s = splitOn separator str
        [dS, tS, fS, hsS] = s
        md = readMaybe dS :: Maybe Int
        mt = readMaybe tS :: Maybe Team
        mf = readMaybe fS :: Maybe (V2 Int)
        mhs = readMaybe hsS :: Maybe [V2 Int]

--IO capabilities---------------------------------------------------------------

saveGS :: FilePath -> GameState -> IO GameState
saveGS fp gs = do
    eitherHandle <- try (openFile fp WriteMode) :: IO (Either SomeException Handle)
    case eitherHandle of
      (Left e) -> return (gs & (dialog . _Just . dialogMsg) .~ (gs ^. (dialog . _Just . dialogErrMsg)))
      (Right handle) -> do
          hPutStrLn handle (formatBoard (gs ^. board))
          hClose handle
          return (gs & dialog .~ Nothing)

loadGS :: FilePath -> GameState -> IO GameState
loadGS fp gs = do
    eitherHandle <- try (openFile fp ReadMode) :: IO (Either SomeException Handle)
    case eitherHandle of
      (Left e) -> return (gs & (dialog . _Just . dialogMsg) .~ (gs ^. (dialog . _Just . dialogErrMsg)))
      (Right handle) -> (flip finally) (hClose handle) $ do
          lines <- lines <$> hGetContents handle
          if length lines /= 1
             then return $ gs & (dialog . _Just . dialogMsg) .~ (gs ^. (dialog . _Just . dialogErrMsg))
             else case readBoard (head lines) of
                    (Just newBoard) -> return $ gs & dialog .~ Nothing & board .~ newBoard & focus .~ OnBoard
                    Nothing -> return $ gs & (dialog . _Just . dialogMsg) .~ (gs ^. (dialog . _Just . dialogErrMsg))
