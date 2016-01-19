{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative
import           Control.Monad
import           Text.JSON
import           Data.List




newtype Times = Times [[(Int, Int)]] deriving (Show)

get_times (Times t) = t
times_at (Times t) = (t!!)
monday    = (`times_at` 0)
tuesday   = (`times_at` 1)
wednesday = (`times_at` 2)
thursday  = (`times_at` 3)
friday    = (`times_at` 4)

data Campus = North | Central deriving (Show, Eq)

data Section = Section
    { times  :: Times
    , campus :: Campus
    , instructor :: String
    , seats :: String
    , reserved :: String
    , priority :: Int
    } deriving (Show)

newtype Course = Course [(String, Section)] deriving (Show)

newtype Courses = Courses [(String, Course)] deriving (Show)


-- this function was included in a tutorial i followed, i guess it's useful?
(!) :: (JSON a) => JSObject JSValue -> String -> Result a
(!) = flip valFromObj

instance JSON Times where
    showJSON = undefined
    readJSON (JSObject obj) =
        Times      <$>
        ((:) <$>
          obj ! "Mo" <*>
          ((:) <$>
           obj ! "Tu" <*>
           ((:) <$>
            obj ! "We" <*>
            ((:) <$>
             obj ! "Th" <*>
             ((:) <$>
              obj ! "Fr" <*> pure [])))))
    readJSON _ = mzero

instance JSON Campus where
    showJSON = undefined
    readJSON (JSRational False obj)
      | obj == 0  = Ok Central
      | otherwise = Ok North
    readJSON _ = mzero

instance JSON Section where
    showJSON = undefined
    readJSON (JSArray obj) =
        Section             <$>
        (readJSON $ obj!!0) <*>
        (readJSON $ obj!!1) <*>
        (readJSON $ obj!!2) <*>
        (readJSON $ obj!!3) <*>
        (readJSON $ obj!!4) <*>
        (readJSON $ obj!!5)
    readJSON _ = mzero


thing :: (JSON a) => (String, JSValue) -> Result (String, a)
thing (a, b) =
  case readJSON b of
  Ok x    -> Ok (a, x)
  Error x -> Error x

thing2 :: [Result a] -> Result [a]
thing2 (Ok x:xs) =
  case thing2 xs of
  Ok y    -> Ok (x:y)
  Error y -> Error y
thing2 (Error x:_) = Error x
thing2 _    = Ok []

instance JSON Course where
    showJSON = undefined
    readJSON (JSObject obj) =
      Course  <$>
      (thing2 (map thing (fromJSObject obj :: [(String, JSValue)]))
       :: Result [(String, Section)])
    readJSON _ = mzero

instance JSON Courses where
    showJSON = undefined
    readJSON (JSObject obj) =
      Courses  <$>
      (thing2 (map thing (fromJSObject obj :: [(String, JSValue)]))
       :: Result [(String, Course)])
    readJSON _ = mzero




any_overlap :: ([(Int, Int)], [(Int, Int)]) -> Bool
any_overlap (x:xs, yy) = overlap x yy || any_overlap (xs, yy)
                         where
                           overlap a (b:bs) = fst a < snd b && snd a > fst b || overlap a bs
                           overlap _ _ = False
any_overlap _ = False


fits :: Section -> [(String, String, Section)] -> Bool
fits s sch
  | any (\x -> case x of
          (_, _, y) -> any any_overlap (zip
                                        (bus_delay s y (get_times $ times s))
                                        (get_times $ times y))
          ) sch = False
  | otherwise = True
  where bus_delay i j
          | campus i /= campus j = map (map (\z -> (fst z - 50, snd z + 50)))
          | otherwise            = id


with_course :: (String, Course) -> [(String, String, Section)] -> [[(String, String, Section)]]
with_course (n, Course (s:ss)) sch
  | fits (snd s) sch = ((n, fst s, snd s):sch) : w
  | otherwise  = w
    where w = with_course (n, Course ss) sch
with_course _ _ = []


find_schedules :: Courses -> [(String, String, Section)] -> [[(String, String, Section)]]
find_schedules (Courses (c:cs)) sch = concat (map
                                              (find_schedules (Courses cs))
                                              (with_course c sch))
find_schedules _ sch = sch:[]


make_times :: [Section] -> [[(Int, Int, Campus)]]
make_times (s:ss) = case times s of
  Times s' -> zipWith
              (\x -> \y -> (map (\a -> (fst a, snd a, campus s)) x) ++ y)
              s' (make_times ss)
make_times [] = ([]:[]:[]:[]:[]:[])


starting_campus = Central

c_diff :: Campus -> Campus -> Int
c_diff c1 c2
  | c1 /= c2  = 1
  | otherwise = 0

start_time_gte :: [[(Int, Int, Campus)]] -> Int -> Bool
start_time_gte x n = all day_stgte x
  where day_stgte ((y, _, c):_) = y - 50 * (c_diff c starting_campus) >= n
        day_stgte [] = True

num_switches :: [[(Int, Int, Campus)]] -> Int
num_switches (x:xs) = (ns_day starting_campus x) + (num_switches xs)
  where ns_day campus ((_, _, y):ys) = c_diff campus y + ns_day y ys
        ns_day campus _    = c_diff campus starting_campus
num_switches _    = 0

total_priority :: [(String, String, Section)] -> Int
total_priority = foldl (\z (_, _, y) -> z + (priority y)) 0

filter_schedules :: Courses -> [([(String, String, Section)], [[(Int, Int, Campus)]])]
filter_schedules courses = --filter (\x -> last (snd x) == []) $
                           --filter (\x -> total_priority (fst x) >= 0) $
                           --filter (\x -> start_time_gte (snd x) 800) $
                           --filter (\x -> start_time_gte [last $ snd x] 1000) $
                           filter (\x -> num_switches (snd x) <= 6) $
                           res
  where res = map
              (\x -> (x,
                      map
                      (sortBy (\a -> \b -> case (a, b) of
                                ((a', _, _), (b', _, _)) -> compare a' b'))
                      (make_times $ map
                       (\y -> case y of (_, _, y') -> y')
                       x)
                     ))
              (find_schedules courses [])




unwrap :: Result a -> a
unwrap (Ok a) = a
unwrap (Error s) = error s


pretty_section :: [(String, String, Section)] -> String
pretty_section sch = concat (map (\x -> (show x) ++ "\n") sch)

pretty_schedule :: [([(String, String, Section)], [[(Int, Int, Campus)]])]-> String
pretty_schedule sch = concat (map (\x -> "switches: " ++ (show $ num_switches $ snd x) ++ "\n" ++
                                         "priority: " ++ (show $ total_priority $ fst x) ++ "\n" ++
                                         (concat $ map (\y -> (show y) ++ "\n") (snd x)) ++
                                         (pretty_section (fst x)) ++ "\n") sch)

main :: IO ()
main = do
  files <- mapM readFile (["EECS370.json", "LING315.json", "LING313.json", "EECS492.json"])
  let courses = foldl (\x s -> let y = unwrap (decode (s :: String) :: Result Courses) in
                        case (x, y) of
                        (Courses xx, Courses yy) -> Courses (xx ++ yy))
                (Courses [])
                files
  putStrLn (pretty_schedule $ filter_schedules courses)
