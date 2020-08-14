module Query where

import UserInfo
import Rating
import Movie

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

{-
  read table from string with line and column separators
  we need a split function to split the original string
-}
splitBy :: Char -> String -> [String]
splitBy del = foldr op [[]]
                    where op ch l@(x:xs)
                              | ch /= del = (ch:x):xs
                              | otherwise = if null x then l else []:l

read_schema :: ColSeparator -> LnSeparator -> String -> TableSchema
read_schema col_sep ln_sep = splitBy col_sep . head . splitBy ln_sep

read_entry :: ColSeparator -> LnSeparator -> String -> [Entry]
read_entry col_sep ln_sep = map (splitBy col_sep) . tail . splitBy ln_sep

read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table col_sep ln_sep input_str =
            Table (read_schema col_sep ln_sep input_str) 
                  (read_entry col_sep ln_sep input_str)

user_info = read_table '|' '\n' user_info_str 
rating = read_table ' ' '\n' rating_str
movie = read_table '|' '\n' movie_str

{-
  given an Int which represents the column number
  return the maximum length of this column
-}
get_column_len :: Table -> Int -> Int
get_column_len (Table h t) n = maximum $ 
                               map length $ 
                               map head $ 
                               [(drop n h)] ++ (map (drop n) t)
{-
  given a table returns the number of columns
-}
get_table_len :: Table -> Int
get_table_len (Table h _) = length h

{-
  makes a list with the maximum length on each column
-}
get_max_list :: Table -> [Int]
get_max_list t = reverse $ op t ((get_table_len t) - 1)
              where op t 0 = [(get_column_len t 0)]
                    op t n = (get_column_len t n):(op t (n-1))

full_line :: Table -> String
full_line t = replicate (sum (get_max_list t) + (get_table_len t) + 1) '-'

make_pairs :: Table -> [[(Int, String)]]
make_pairs table@(Table h t) = (map (zip (get_max_list table)) t)

add_spaces :: Table -> [Entry]
add_spaces t = map (map (\(a,b) -> b ++ replicate (a - (length b)) ' ')) $
         make_pairs t

show_table_schema :: Table -> String
show_table_schema table@(Table h _) = "|" ++
  (concat $
  map (++ "|") $
  map (\(a,b) -> b ++ replicate (a - (length b)) ' ') $ 
  (zip (get_max_list table) h))

show_table :: Table -> String
show_table t = full_line t ++ "\n" ++ show_table_schema t ++ "\n" ++ full_line t ++ "\n" ++ (concat $
         map (++ "\n") $
         map ('|' :) $
         map (concat) $
         map (map (++ "|")) $
         add_spaces t) ++ full_line t ++ "\n"

instance Show Table where
  show = show_table

{-

-}


data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

eval :: Query -> Table
eval (Atom t) = t

same_zone :: String -> Query
same_zone = undefined

male_within_age :: Integer -> Integer -> Query
male_within_age = undefined

mixed :: [String] -> [String] -> Int -> Query
mixed = undefined
