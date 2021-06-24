import System.Random
import System.IO.Unsafe

randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))




items = ["item1", "item2", "item3", "item4", "item5", "item6"]
purchasesHistory = [("user1",[["item1", "item2", "item3"],["item1", "item2", "item4"]]) ,("user2", [["item2", "item5"], ["item4", "item5"]]) ,("user3", [["item3", "item2"]]) ,("user4", [])]

createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList [] = []
createEmptyFreqList (x:xs) = (x,[]):createEmptyFreqList xs

getAllUsersStats :: [(String,[[String]])] -> [(String,[(String,[(String,Int)])])]
getAllUsersStats l = helper9 (createEmptyFreqList items) l

-- takes purchases history and emptyfreqlist and returns list of tupils [(user,[(item,[item,freq])])]
helper9 _ [] = []
helper9 freqlist ((user,l):xs) = (user,getOneUserStats freqlist l):(helper9 freqlist xs)

-- takes emptyfreqlist and a list of lists and returns list of tupils [(item,[item,freq])]
getOneUserStats :: Eq a => [(a,[a])] -> [[a]] -> [(a,[(a,Int)])]
getOneUserStats [] _ = []
getOneUserStats ((item,_):xs) l | helper8 item l = ((helper7 item l):getOneUserStats xs l)
                                | otherwise = (item,[]):getOneUserStats xs l

helper8 :: Eq a => a -> [[a]] -> Bool
helper8 item l = contains item (helper5 l)

-- takes 1 item and a list of lists and returns tupil (item,[(item,freq)])
helper7 :: Eq a => a -> [[a]] -> (a,[(a,Int)])
helper7 item (x:xs) = (item,helper6 (helper1 item (x:xs)))

-- takes a list of lists and returns a list of tupils [(item,Int)]
helper6 :: Eq a => [[a]] -> [(a,Int)]
helper6 (x:xs) = helper4 (helper5 (x:xs)) (x:xs)

-- takes a list of lists and returns a list of all items without duplicates
helper5 [] = []
helper5 (x:xs) = rmdups (x ++ helper5 xs)
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

-- takes a list and a list of lists and returns list of tupils [(item,freq)]
helper4 :: Eq a => [a] -> [[a]] -> [(a,Int)]
helper4 [] _ = []
helper4 (x:xs) l = (helper3 x l):(helper4 xs l)
-- takes an item and a list of lists and returns a tupil (item,freq)
helper3 :: Eq a => a -> [[a]] -> (a,Int)
helper3 item (x:xs) = (item,helper2 item (x:xs))

-- takes an item and a list of lists and returns the number of occurances (used)
helper2 :: Eq a => a -> [[a]] -> Int
helper2 _ [] = 0
helper2 item (x:xs) | contains item x = 1 + helper2 item xs
                    | otherwise = helper2 item xs

-- takes an item and a list of lists and returns the
-- lists that contains the item without the item itself (used)
helper1 :: Eq a => a -> [[a]] -> [[a]]
helper1 _ [] = []
helper1 item (x:xs)  | contains item x = ((filter (/=item) x):(helper1 item xs))
                     | otherwise = helper1 item xs

contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains item (x:xs) | item == x = True
                     | otherwise = contains item xs

removeItem _ []  = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys


freqListItems:: String -> [(String, Int)]
freqListItems user = helperFreqListItems items (concatListOfUser ( getListOfUser user (getAllUsersStats purchasesHistory)))

helperFreqListItems :: [String] -> [(String,Int)] -> [(String,Int)]
helperFreqListItems _ [] = []
helperFreqListItems [] _ = []
helperFreqListItems (itemsx:itemsxs) (concatListOfUserx:concatListOfUserxs) = if (countNumOfItemsInConcatListOfUser itemsx (concatListOfUserx:concatListOfUserxs)) == 0 then helperFreqListItems itemsxs (concatListOfUserx:concatListOfUserxs) else (itemsx,(countNumOfItemsInConcatListOfUser itemsx (concatListOfUserx:concatListOfUserxs))) :  (helperFreqListItems itemsxs (concatListOfUserx:concatListOfUserxs))


getListOfUser :: String -> [(String, [(String, [(String, Int)])])] -> [(String,[(String,Int)])]
getListOfUser _ [] = []
getListOfUser user ((userr,(x:xs)):xss) = if user == userr then (x:xs) else getListOfUser user xss

concatListOfUser :: [(String,[(String,Int)])] -> [(String,Int)]
concatListOfUser [] = []
concatListOfUser ((currItem,([])):xss) = concatListOfUser xss
concatListOfUser ((currItem,((item,int):xs)):xss) =  ((item,int):xs) ++ concatListOfUser xss

countNumOfItemsInConcatListOfUser :: String -> [(String,Int)] -> Int
countNumOfItemsInConcatListOfUser _ [] = 0
countNumOfItemsInConcatListOfUser item ((itemm, int):xs) = if item == itemm then int + countNumOfItemsInConcatListOfUser item xs else countNumOfItemsInConcatListOfUser item xs


countNumOfItemInCart ::  String -> [String] -> Int
countNumOfItemInCart _ [] = 0
countNumOfItemInCart item (x:xs) = if item == x then (1 + (countNumOfItemInCart item xs)) else (countNumOfItemInCart item xs)



-- --------------------------------------

-- freqListCart
getItemsFromOneList:: String -> [String] -> [String]
getItemsFromOneList _ [] = []
getItemsFromOneList item (x:xs) = if (elem item (x:xs)) == False then [] else getItemsFromOneListHelper item (x:xs)
getItemsFromOneListHelper :: String -> [String] -> [String]
getItemsFromOneListHelper _ [] = []
getItemsFromOneListHelper item (x:xs) = if item == x then getItemsFromOneListHelper item xs else x : (getItemsFromOneListHelper item xs)

getItemsFromListOfList :: String -> [[String]] -> [String]
getItemsFromListOfList _ [] = []
getItemsFromListOfList item (x:xs) = (getItemsFromOneList item x) ++ (getItemsFromListOfList item xs)

getListOfUserFromPurchasesHistory :: String -> [(String, [[String]])] -> [[String]]
getListOfUserFromPurchasesHistory _ [] = []
getListOfUserFromPurchasesHistory user ((userr,a):xs) = if user == userr then a else getListOfUserFromPurchasesHistory user xs

getListOfItemsWithoutBeingCounted :: [String] -> [[String]] -> [String]
getListOfItemsWithoutBeingCounted [ ] _ = []
getListOfItemsWithoutBeingCounted (cartx:cartxs) (x:xs) = getItemsFromListOfList cartx (x:xs) ++ getListOfItemsWithoutBeingCounted cartxs (x:xs)

getListOfItemsCounted :: String -> [String] -> [(String, Int)]
getListOfItemsCounted user cart =  getListOfItemsCountedHelper items (getListOfItemsWithoutBeingCounted cart (getListOfUserFromPurchasesHistory user purchasesHistory))
getListOfItemsCountedHelper :: [String] -> [String] -> [(String, Int)]
getListOfItemsCountedHelper [] _ = []
getListOfItemsCountedHelper (itemx:itemxs) list = if countNumOfItemInCart itemx list == 0 then getListOfItemsCountedHelper itemxs list else (itemx,(countNumOfItemInCart itemx list)) : (getListOfItemsCountedHelper itemxs list)

freqListCart :: String ->[String] -> [(String, Int)]
freqListCart _ [] = []
freqListCart user cart = getListOfItemsCounted user cart
 -- --------------------

addItemsNum :: String -> [(String,Int)] -> Int
addItemsNum _ [] = 0
addItemsNum item ((itemm,int):xs) = if item == itemm then int + addItemsNum item xs else addItemsNum item xs

addItems :: [String] -> [(String,Int)] -> [(String,Int)]
addItems [] list = []
addItems (itemx:itemxs) list = if addItemsNum itemx list == 0 then addItems itemxs list else (itemx,(addItemsNum itemx list)) : addItems itemxs list

freqListCartAndItems :: String -> [String] -> [(String, Int)]
freqListCartAndItems _ [] = []
freqListCartAndItems user cart = addItems items ((freqListCart user cart) ++ (freqListItems user))


purchasesIntersection ::  [(String,[(String,Int)])] -> [(String ,[(String,[(String,Int)])])] -> [[(String,[(String,Int)])]]
purchasesIntersection [] []  =[]
purchasesIntersection _ [] = []
purchasesIntersection l ((user,list):xs) = (helper12 l list):(purchasesIntersection l xs)


helper12 :: [(String,[(String,Int)])] -> [(String,[(String,Int)])] -> [(String,[(String,Int)])]
helper12 [] _ = []
helper12 _ [] = []
helper12 ((item,l):xs) ((item2,l2):ys) | (item == item2) && (l/=[] && l2/=[]) = (item,helper10 l l2): helper12 xs ys
                                       | otherwise = helper12 xs ys




-- takes 2 freqlists and returns their sum
helper10 :: [(String,Int)] -> [(String,Int)] -> [(String,Int)]
helper10 [] [] = []
helper10 [] l = l
helper10 l [] = l
helper10 (x:xs) l = helper10 xs (helper11 x l)


helper11 :: (String,Int) -> [(String,Int)] -> [(String,Int)]
helper11 (item,freq) [] = [(item,freq)]
helper11 (item,freq) ((item2,freq2):xs) | item == item2 = (item,freq+freq2):xs
                                        | otherwise = (item2,freq2):helper11 (item,freq) xs


-- :: String -> String

recommendEmptyCart user | (freqListItems user) == [] = ""
                        | otherwise = (makeList (freqListItems user)) !! (randomZeroToX (length (makeList (freqListItems user))-1))




getList user ((users,l):xs) = if user == users then l else getList user xs

recommendBasedOnItemsInCart :: String -> [String] -> String
recommendBasedOnItemsInCart user cart | (getList user purchasesHistory) == [] = ""
                                      |otherwise = (makeList (freqListCartAndItems user cart)) !! (randomZeroToX (length (makeList (freqListCartAndItems user cart))-1))

makeList :: [(String,Int)] -> [String]
makeList [] = []
makeList ((item,freq):xs) = helper13 item freq 0 ++ makeList xs

helper13 item freq counter = if counter<freq then item:helper13 item freq (counter+1) else []

--freqListUsers:: String -> [(String, Int)]

freqListUsers user = helper17 (helper14 (helper16 (purchasesIntersection (getListofUser user (getAllUsersStats purchasesHistory)) (getOtherLists user (getAllUsersStats purchasesHistory)))))

helper17 [] = []
helper17 (x:xs) = helper15 x xs

helper15 list [] = list
helper15 list (x:xs) = helper15 (helper10 list x) xs

helper14 :: [(String,[(String,Int)])] -> [[(String,Int)]]
helper14 [] = []
helper14 ((item,l):xs) = l:(helper14 xs)

getOtherLists _ [] = []
getOtherLists user ((users,l):xs) | user == users = getOtherLists user xs
                                  | otherwise = (users,l):(getOtherLists user xs)

helper16 [] = []
helper16 (x:xs) = x ++ (helper16 xs)

getListofUser _ [] = []
getListofUser user ((users,l):xs) | user == users = l
                                  | otherwise = getListofUser user xs


recommendBasedOnUsers :: String -> String

recommendBasedOnUsers user | (getList user purchasesHistory) == [] = ""
                           | otherwise = (makeList (freqListUsers user)) !! randomZeroToX (length (makeList (freqListUsers user))-1)


recommend :: String -> [String] -> String
recommend user cart | (getRecommendation user cart) == "" = items !! randomZeroToX ((length items)-1)
                    | otherwise = getRecommendation user cart
getRecommendation user cart | cart == [] = recommendBasedOnUsers user
                            | otherwise = recommendBasedOnItemsInCart user cart
