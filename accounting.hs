import Data.Char
import Data.List
import Data.List.Split
import Data.Map.Strict
import Data.Maybe
import Data.Monoid
import Data.Time
import System.IO
import Text.Regex

data Charge = Charge Float String
instance Show Charge where
  show (Charge f s) = "<tr><td>" ++ s ++ "</td><td>" ++ show f ++ "</td></tr>"

makeMemTuple :: String -> (String, (String, String))
makeMemTuple x = (head l, (l !! 1, l !! 2))
  where l = words x

makeMemTuples :: String -> [(String, (String, String))]
makeMemTuples x = fmap makeMemTuple (lines x)

insertCharge :: Map String [Charge] -> Maybe (String, Charge) -> Map String [Charge]
insertCharge e (Just (s, c)) = insertWith (++) s [c] e
insertCharge e Nothing = e

entriesToMap :: [Maybe (String, Charge)] -> Map String [Charge]
entriesToMap = Data.List.foldl insertCharge empty

take_5_tail :: String -> String
take_5_tail = reverse . take 5 . reverse

makeEntry :: [String] -> Maybe (String, Charge)
makeEntry [x,y,z] = Just (take_5_tail x, Charge (read y) z)
makeEntry _ = Nothing

processInput :: String -> Map String [Charge]
processInput = entriesToMap 
             . fmap makeEntry 
             . filterLongLines 
             . fmap (breakLineUp . trimEnd) 
             . lines 

makeEmail :: Map String (String, String) -> String -> Day -> String
makeEmail members key time = "TO: " ++ email ++ "\n"
                          ++ "From: keegan@twinoaks.org"
                          ++ "\n" ++ "MIME-version: 1.0"
                          ++ "\n" ++ "Content-Type: text/html"
                          ++ "\n" ++ "Subject: Monthly Transactions ("
                          ++ show time ++ ")"
  where Just (_,email) = Data.Map.Strict.lookup key members

makeHTMLDoc :: Map String (String, String) -> String -> Map String [Charge] -> String
makeHTMLDoc members key m = "<html><body>" ++ prettyCharges members key m ++ "</body></html>"

outputHTMLToFile :: String -> String -> IO()
outputHTMLToFile html key = writeFile (key ++ ".html") html

unmaybe :: Maybe String -> String
unmaybe (Just x) = x
unmaybe Nothing = ""

prettyCharges :: Map String (String, String) -> String -> Map String [Charge] -> String
prettyCharges members k m = "<p><b>" 
                          ++ unmaybe (fmap fst (Data.Map.Strict.lookup k members)) 
                          ++ ":</b></p>" ++ "\n" 
                          ++ "<table border = 1>" ++ charges ++ "</table>\n"
    where v = Data.Map.Strict.lookup k m
          l = fromMaybe [] v
          charges = Data.List.foldl (\x y -> x ++ show y ++ "\n") "" l

breakLineUp :: String -> [String]
breakLineUp = splitRegex (mkRegex "  +")

trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse

filterLongLines :: [[String]] -> [[String]]
filterLongLines = Data.List.filter (\y -> length y == 3) 

combineProcessedList :: [[String]] -> String
combineProcessedList x = intercalate "  " combined_list
  where combined_list = intercalate ["\n"] x 

main = do
  c <- getCurrentTime
  let time = utctDay c
  memberRaw <- readFile "members.txt"
  let members = fromList $ makeMemTuples memberRaw
  res <- readFile "EASYT.TXT"
  let d = processInput res
  let html = fmap (\k -> (k, makeEmail members k time ++ "\n\n" ++ makeHTMLDoc members k d)) (keys members)
  mapM_ (\h -> outputHTMLToFile (snd h) (fst h))  html
