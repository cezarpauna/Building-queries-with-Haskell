module Query where

import UserInfo
import Rating
import Movie

import Data.List

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
  select from table
  first we need to transform the list of names into a list of number
-}
get_column_index :: Table -> String -> Int
get_column_index (Table h _) str = case elemIndex str h of
                                      Just n -> n
                                      Nothing -> -1 

-- returns an integer list of the needed columns
get_columns_index :: [String] -> Table -> [Int]
get_columns_index list table@(Table h t) = map (get_column_index table) list

-- selecting the columns
select_columns :: [Int] -> Table -> [[Column]]
select_columns (x:[]) (Table h t) = map (\x -> [x]) $ map head $ map (drop (x)) (h:t)
select_columns (x:xs) table@(Table h t) = zipWith (++) (map (\x -> [x]) $ map head $ map (drop x) (h:t)) (select_columns xs table)

-- make columns table
select :: [String] -> Table -> Table
select str table@(Table h t) = Table (head (select_columns (get_columns_index str table) table))
                                      (tail (select_columns (get_columns_index str table) table))

-- select with a limit
select_limit :: [String] -> Integer -> Table -> Table
select_limit str x table@(Table h t) = Table (head (select_columns (get_columns_index str table) table))
                                      (take (fromInteger x) $ tail (select_columns (get_columns_index str table) table))

data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

{-
  different ways of filtering the table
-}
filter_func :: FilterCondition -> Table -> (Entry -> Bool)
filter_func (Lt field no) t@(Table h _) = func_lt
                                        where func_lt entry
                                                | no > (read (head $ drop (get_column_index t field) entry) :: Integer) = True
                                                | otherwise = False

filter_func (Eq field str) t@(Table h _) = func_eq
                                        where func_eq entry
                                                | str == (head $ drop (get_column_index t field) entry) = True
                                                | otherwise = False

filter_func (In field l) t@(Table h _) = func_in
                                        where func_in entry
                                                | elem (head $ drop (get_column_index t field) entry) l = True
                                                | otherwise = False

filter_func (Not f_cond) t = func_not
                              where func_not entry
                                      | filter_func f_cond t entry = False
                                      | otherwise = True

filter_table :: FilterCondition -> Table -> Table
filter_table l t@(Table h e) = Table h (filter (filter_func l t) e)

filter_tables :: Table -> Table -> FilterCondition -> FilterCondition -> Table
filter_tables t1@(Table h1 e1) t2@(Table h2 e2) f1 f2 =
  (Table h1 ((filter (filter_func f1 t1) e1) ++ (filter (filter_func f2 t2) e2)))

data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

eval :: Query -> Table
eval (Atom t) = t
eval (Select str q) = select str $ eval q
eval (SelectLimit str x q) = select_limit str x $ eval q
eval (Filter f q) = filter_table f $ eval q
eval ((Filter f1 q1) :|| (Filter f2 q2)) = filter_tables (eval q1) (eval q2) f1 f2

{-
  some test to see how these queries work
-}
get_zone :: String -> Table -> String
get_zone id t@(Table h e) = concat $ head $ map (drop 4) $ filter (filter_func (Eq "user_id" id) t) e

same_zone :: String -> Query
same_zone id = Select ["user_id", "occupation"] $
               Filter (Eq "zone" (get_zone id user_info)) $
               Filter (Not (Eq "user_id" id)) $
               Atom user_info           

male_within_age :: Integer -> Integer -> Query
male_within_age lower_b upper_b =
  Select ["occupation", "zone"] $
  Filter (Not (Eq "age" (show lower_b))) $
  Filter (Lt "age" upper_b) $
  Filter (Not (Lt "age" lower_b)) $
  Filter (Eq "sex" "M") $
  Atom user_info

mixed :: [String] -> [String] -> Int -> Query
mixed l1 l2 max = Select ["user_id"] $ 
                  Filter (In "zone" l1) $ 
                  Filter (In "occupation" l2) $ 
                  Filter (Lt "age" (toInteger max)) $ 
                  Atom user_info
    