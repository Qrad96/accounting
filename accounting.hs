import System.IO
import Data.Monoid
import Data.List.Split
import Text.Regex
import Data.List
import Data.Map.Strict
import Data.Char
import Data.Time

data Charge = Charge Float String
instance Show Charge where
  show (Charge f s) = "<tr><td>" ++ s ++ "</td><td>" ++ (show f) ++ "</td></tr>"

makeMemTuple :: String -> (String, (String, String))
makeMemTuple x = (l !! 0, (l !! 1, l !! 2))
  where l = (words x)

makeMemTuples :: String -> [(String, (String, String))]
makeMemTuples x = fmap makeMemTuple (lines x)

--Make it so you can send emails to people. Make code better

insertCharge :: Map String [Charge] -> Maybe (String, Charge) -> Map String [Charge]
insertCharge e (Just (s, c)) = insertWith (++) s [c] e
insertCharge e Nothing = e

entriesToMap :: [Maybe (String, Charge)] -> Map String [Charge]
entriesToMap x = Data.List.foldl insertCharge empty x

take_5_tail :: String -> String
take_5_tail = reverse . (take 5) . reverse

make_entry :: [String] -> Maybe (String, Charge)
make_entry (x:y:z:[]) = Just (take_5_tail x, Charge (read y) z)
make_entry _ = Nothing

processInput :: String -> Map String [Charge]
processInput s = (entriesToMap .
                  (fmap make_entry) .
                  filter_long_lines .
                  (fmap break_line_up) .
                  (fmap trim_end) .
                  lines) s

makeEmail :: Map String (String, String) -> String -> Day -> String
makeEmail members key time = "TO: " ++ email ++ "\n"
                          ++ "From: keegan@twinoaks.org"
                          ++ "\n" ++ "MIME-version: 1.0"
                          ++ "\n" ++ "Content-Type: text/html"
                          ++ "\n" ++ "Subject: Monthly Transactions ("
                          ++ show time ++ ")"
  where Just (_,email) = Data.Map.Strict.lookup key members

makeHTMLDoc :: Map String (String, String) -> String -> Map String [Charge] -> String
makeHTMLDoc members key m = "<html><body>" ++ pretty_charges members key m ++ "</body></html>"

outputHTMLToFile :: String -> String -> IO()
outputHTMLToFile html key = writeFile (key ++ ".html") html

unmaybe :: Maybe String -> String
unmaybe (Just x) = x
unmaybe Nothing = ""

pretty_charges :: Map String (String, String) -> String -> Map String [Charge] -> String
pretty_charges members k m = "<p><b>" ++ (unmaybe $ fmap fst (Data.Map.Strict.lookup k members)) ++ ":</b></p>" ++ "\n" ++ "<table border = 1>" ++ charges ++ "</table>\n"
    where v = Data.Map.Strict.lookup k m
          l = case v of
                Just c -> c
                Nothing -> []
          charges = Data.List.foldl (\x y -> x ++ (show y) ++ "\n") "" l

break_line_up :: String -> [String]
break_line_up = splitRegex (mkRegex "  +")

trim_end :: String -> String
trim_end = reverse . dropWhile isSpace . reverse

filter_long_lines :: [[String]] -> [[String]]
filter_long_lines x = Data.List.filter (\y -> length y == 3) x

combine_processed_list :: [[String]] -> String
combine_processed_list x = (intercalate "  ") combined_list
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
