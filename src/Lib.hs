module Lib
    ( someFunc
    ) where

import System.IO
import Control.Applicative
import Data.Char
import Data.List

fileName = "sudoku.txt"
solvedFileName = "solvedSudoku.txt"

someFunc :: IO ()
someFunc = do
    handle <- openFile fileName ReadMode
    text <- slist handle
    let s = myzip text
    let ps = myzip2 text

    writeHandle <- openFile solvedFileName WriteMode
    hPutStrLn writeHandle $ allShowSudoku2 ps $ listCheck_v3 s

    hClose handle
    hClose writeHandle

slist h = do
    eof <- hIsEOF h
    if eof
        then return []
        else do
            x <- hGetLine h
            xs <- slist h
            return (x:xs)

-- 初期化してリスト化
initText [] = []
initText (x:xs)
    | isSpace x = "１２３４５６７８９" : initText xs
    | otherwise = [x] : initText xs

-- 問題文を初期化してリスト化
initText2 [] = []
initText2 (x:xs)
    | isSpace x = "・" : initText2 xs
    | otherwise = [x] : initText2 xs

-- initText で初期化したリストをzip
myzip [] = []
myzip (x:xs) = (zip [1,2..81] (initText x)) : myzip xs

-- initText で初期化した問題文リストをzip
myzip2 [] = []
myzip2 (x:xs) = (zip [1,2..81] (initText2 x)) : myzip2 xs

-- タプルのリストをみやすい形で表示
showSudoku [] = []
showSudoku (x:xs)
    | (fst x) `mod` 27 == 0 = (snd x) ++ '\n' : '\n' : showSudoku xs
    | (fst x) `mod` 9 == 0 = (snd x) ++ '\n' : showSudoku xs
    | ((fst x) `mod` 9) `mod` 3 == 0 = (snd x) ++ ' ' : ' ' : showSudoku xs
    | otherwise = (snd x) ++ ' ' : showSudoku xs

-- タプルリストのリストをみやすい形で表示
allShowSudoku [] = []
allShowSudoku (x:xs) = showSudoku x ++ '\n' : allShowSudoku xs

-- 問題と解答を並べて表示
showSudoku2 [] [] _ _ = []
showSudoku2 (px:pxs) (ax:axs) p a
    | (fst px) `mod` 27 == 0 = reverse ((head $ snd px) : p) ++ "         " ++ reverse ((head $ snd ax) : a) ++ '\n' : '\n' : showSudoku2 pxs axs [] []
    | (fst px) `mod` 9 == 0 = reverse ((head $ snd px) : p) ++ "         " ++ reverse ((head $ snd ax) : a) ++ '\n' : showSudoku2 pxs axs [] []
    | (fst px) `mod` 3 == 0 = showSudoku2 pxs axs (' ' : ' ' : (head $ snd px): p) (' ' : ' ' : (head $ snd ax) : a)
    | otherwise = showSudoku2 pxs axs (' ' : (head $ snd px) : p) (' ' : (head $ snd ax) : a)

allShowSudoku2 [] [] = []
allShowSudoku2 (px:pxs) (ax:axs) = showSudoku2 px ax [] [] ++ '\n' : allShowSudoku2 pxs axs


-- n番目のタプルを返す
getSndFromN _ [] = (0,[])
getSndFromN n (x:xs)
    | n == fst x = x
    | otherwise = getSndFromN n xs


-- それぞれ同じ列、行、グループが何番目かを表すリスト ns[Int] を返す
sameRow n = [x | x <- [(((n-1) `div` 9) * 9 + 1)..(((n-1) `div` 9) * 9 + 9)], not (x==n)]
sameColumn n
    | (n `mod` 9) == 0 = [x | x <- [9,18..81], not (x==n)]
    | not ((n `mod` 9) == 0) = [x | x <- [(n `mod` 9),((n `mod` 9)+9)..((n `mod` 9)+8*9)], not (x==n)]
sameGroup n
    | ((((n-1) `div` 9) + 1) `mod` 3) == 1 && ((n `mod` 9) `mod` 3) == 1 = [n+1,n+2,n+9,n+18,n+10,n+11,n+19,n+20]
    | ((((n-1) `div` 9) + 1) `mod` 3) == 1 && ((n `mod` 9) `mod` 3) == 2 = [n-1,n+1,n+8,n+9,n+10,n+17,n+18,n+19]
    | ((((n-1) `div` 9) + 1) `mod` 3) == 1 && ((n `mod` 9) `mod` 3) == 0 = [n-2,n-1,n+7,n+8,n+9,n+16,n+17,n+18]
    | ((((n-1) `div` 9) + 1) `mod` 3) == 2 && ((n `mod` 9) `mod` 3) == 1 = [n-9,n-8,n-7,n+1,n+2,n+9,n+10,n+11]
    | ((((n-1) `div` 9) + 1) `mod` 3) == 2 && ((n `mod` 9) `mod` 3) == 2 = [n-10,n-9,n-8,n-1,n+1,n+8,n+9,n+10]
    | ((((n-1) `div` 9) + 1) `mod` 3) == 2 && ((n `mod` 9) `mod` 3) == 0 = [n-11,n-10,n-9,n-2,n-1,n+7,n+8,n+9]
    | ((((n-1) `div` 9) + 1) `mod` 3) == 0 && ((n `mod` 9) `mod` 3) == 1 = [n-18,n-17,n-16,n-9,n-8,n-7,n+1,n+2]
    | ((((n-1) `div` 9) + 1) `mod` 3) == 0 && ((n `mod` 9) `mod` 3) == 2 = [n-19,n-18,n-17,n-10,n-9,n-8,n-1,n+1]
    | ((((n-1) `div` 9) + 1) `mod` 3) == 0 && ((n `mod` 9) `mod` 3) == 0 = [n-20,n-19,n-18,n-11,n-10,n-9,n-2,n-1]

-- 場所チェック
-- 文字列(x:xs)から文字cを抜いた文字列を返す
deleteC [] _ = []
deleteC (x:xs) c
    | x == c = deleteC xs c
    | otherwise = x : deleteC xs c

-- 文字cをタプルリスト(x:xs)の(n:ns)番目(複数)のタプルたちから抜いたタプルリストを返す
deleteCList [] _ _ = []
deleteCList (x:xs) [] c = x : deleteCList xs [] c
deleteCList (x:xs) (n:ns) c
    | (fst x) == n = ((fst x), (deleteC (snd x) c)) : deleteCList xs ns c
    | otherwise = x : deleteCList xs (n:ns) c

-- 確定しているタプル(n,x)と同じ列、行、グループの取りうる数字から、確定した数字xを消したタプルリストを返す
deleteFromNs (1,x) s = deleteCList s ([2..12]++[19..21]++[28,37..73]) (head x)
deleteFromNs (n,x) s = deleteCList s (nub(sort ((sameRow n) ++ (sameColumn n) ++ (sameGroup n)))) (head x)

-- 数字チェック
-- 文字cがタプルリスト(x:xs)の (n:ns)=nn 番目(複数)のタプルたちの中に含まれているかをBool型で返す
-- 含まれていたらFalse、含まれていなかったらTrue
-- (n:ns)：同じ列or行orグループの番号のリスト
isConfirm [] _ _ _ = True
isConfirm (x:xs) [] nn c = isConfirm xs nn nn c
isConfirm s nn [] c = isConfirm s nn nn c
isConfirm (x:xs) (n:ns) nn c
    | fst x == n = if elem c (snd x) then False else isConfirm xs ns nn c
    | otherwise = isConfirm (x:xs) ns nn c

-- タプルリスト(x:xs)のn番目のタプルを(n,[c])にしたタプルリストを返す
updateS [] _ _ = []
updateS (x:xs) n c
    | fst x == n = (n,[c]) : updateS xs n c
    | otherwise = x : updateS xs n c

-- 1つの場所(番号n)の取りうる数字(文字列x)に関して、isComfirmがTrueであれば、その場所が確定されたタプルリストを返す
confirmC (_, []) s = s
confirmC (n,(x:xs)) s
    | isConfirm s (sort (sameRow n)) [] x = updateS s n x
    | isConfirm s (sort (sameColumn n)) [] x = updateS s n x
    | isConfirm s (sort (sameGroup n)) [] x = updateS s n x
    | otherwise = confirmC (n,xs) s

-- 場所チェック
checkConfirmed [] s = s
checkConfirmed (x:xs) s
    | length (snd x) == 1 = checkConfirmed xs (deleteFromNs x s)
    | otherwise = checkConfirmed xs s

-- 数字チェック
-- 同じ 1.列 2.行 3.グループ の3パターンにおいて
-- その中で取りうる数字がその場所だけに唯一であれば、その場所はその数字に確定する
-- 数字が確定していない場所を対象にcomfirmCをして、確定させたsを返す
checkN [] s = s
checkN (x:xs) s
    | length (snd x) == 1 = checkN xs s
    | otherwise = checkN xs (confirmC x s)

-- 変化がなくなるまで場所チェックと数字チェックを繰り返し、変化がなくなったら帰ってくる
check pres s
    | pres==s = s
    | otherwise = check s $ checkN (checkConfirmed s s) (checkConfirmed s s)

-- 複数の問題
listCheck [] = []
listCheck (x:xs) = (check [] x) : listCheck xs


-- 組み合わせチェック
isMatch _ [] = []
isMatch (n,x) (nx:nxs)
    | x == snd nx = [n, (fst nx)] ++ isMatch (n,x) nxs
    | otherwise = isMatch (n,x) nxs

-- タプルリスト(x:xs)から(n:ns)番目のタプルだけを抜き取ったタプルリストを返す
pickedS _ [] = []
pickedS [] _ = []
pickedS (x:xs) (n:ns)
    | fst x == n = x : pickedS xs ns
    | otherwise = pickedS xs (n:ns)

-- タプルxと同じグループの中で取り得る数字(acc = snd x)が一致している場所がある場合、一致している場所の番号のリストを返す。ない場合は空リストを返す。
isCombination (n,x) s = nub $ sort $ isMatch (n,x) (pickedS s (sameGroup n))

-- リスト (sn:sns)=ssn の要素から、リスト (n:ns) の要素を削除したリストを返す  (そのグループのそれら以外の場所)
sameGroupWithNs n sn [] = sameGroupWithNs n sn sn
sameGroupWithNs [] _ _ = []
sameGroupWithNs (n:ns) [] ssn = sameGroupWithNs ns ssn ssn
sameGroupWithNs (n:ns) (sn:sns) ssn
    | isContain (n:ns) sn = sameGroupWithNs ns sns ssn
    | otherwise = sn : sameGroupWithNs (n:ns) sns ssn
 where
    isContain [] _ = False
    isContain (n:ns) k
        | n==k = True
        | otherwise = isContain ns k

-- 文字列csをタプルリストxのns番目(複数)のタプルから抜いたタプルリストsを返す
deleteCsList x _ [] = x
deleteCsList x ns (c:cs) = deleteCsList (deleteCList x ns c) ns cs

-- ns番目のタプルの取り得る数字から、combN番目のタプルの取り得る数字を抜いたタプルリストを返す
deleteSameGroupN _ s [] _ = s
deleteSameGroupN [] s (n:ns) combN = deleteSameGroupN s s ns combN
deleteSameGroupN (x:xs) s (n:ns) combN
    | fst x == n = deleteSameGroupN xs (deleteCsList s (n:ns) (snd combN)) (n:ns) combN
    | otherwise = deleteSameGroupN xs s (n:ns) combN

-- あるグループの中で取り得る数字が一致している場所があるとき、そのグループのそれら以外の場所の取り得る数字からそれらの数字を削除する
checkCombination [] s = s
checkCombination (x:xs) s
    | length (snd x) == 1 = checkCombination xs s
    | isCombination x s == [] = checkCombination xs s
    | length (isCombination x s) == length (snd x) = checkCombination xs $ deleteSameGroupN s s (sameGroupWithNs (isCombination x s) (sameGroup (fst x)) []) x
    | otherwise = checkCombination xs s


-- 完成したか（取りうる数字がすべて１つだったらTrue）
isComplete [] = True
isComplete (x:xs)
    | length (snd x) == 1 = isComplete xs
    | otherwise = False

-- 変化がなくなるまで場所チェックと数字チェックと組み合わせチェックを繰り返す
check_v2 prepres pres s
    | isComplete s = s
    | prepres == s = s
    | pres==s = check_v2 pres s (checkCombination s s)
    | otherwise = check_v2 pres s $ checkN (checkConfirmed s s) (checkConfirmed s s)

-- 複数の問題
listCheck_v2 [] = []
listCheck_v2 (x:xs) = (check_v2 [] [] x) : listCheck_v2 xs


-- 
-- 取りうる数字の中から一つの数字を仮で入れてみて、不都合があればその数字は入らないとわかる
-- 不都合がある：同じ列、行、グループそれぞれの中の取り得る数字たちの中に１～９の中で無い数字がある

-- タプルリスト(x:xs)の取り得る数字(snd x)たちの文字列を返す
sameNs [] = []
sameNs (x:xs) = snd x ++ sameNs xs

-- タプルリスト(x:xs)の取り得る数字(snd x)たちの中に、"１２３４５６７８９"=(c:cs)の中で含まれていない数字があったらTrue
isContain _ [] = False
isContain s (c:cs)
    | elem c (nub $ sort $ sameNs s) = isContain s cs
    | otherwise = True

-- n番目のタプルを仮に確定させたタプルリストsを受け取り、それが同じ列、行、グループそれぞれにおいて1つでもisContainだったらTrue
isProblem n s
    | ((isContain (pickedRow n s) cs) || (isContain (pickedColumn n s) cs) || (isContain (pickedGrpup n s) cs)) = True
    | otherwise = False
    where
        pickedRow n s = pickedS s (sort (n : sameRow n))
        pickedColumn n s = pickedS s (sort (n : sameColumn n))
        pickedGrpup n s = pickedS s (sort (n : sameGroup n))
        cs = "１２３４５６７８９"

-- n番目のタプルの取り得る数字を文字cに確定させたタプルリストを返す
confirmOne [] _ _ = []
confirmOne (x:xs) n c
    | fst x == n = (n,[c]) : xs
    | otherwise = x : confirmOne xs n c

-- タプル(n,(c:cs))とタプルリスト(x:xs)を受け取り、n番目のタプルの取り得る数字をcで確定させてcheck_v2したタプルリストを返す
tempList _ [] s = s
tempList (n,(c:cs)) (x:xs) s
    | n == fst x = check_v2 [] [] (confirmOne s n c)
    | otherwise = tempList (n,(c:cs)) xs s

-- タプル(n,(x:xs))とタプルリストsを受け取り、n番目のタプルの取り得る数字をxで確定させてcheck_v2したタプルリストを
-- isProblemに渡し、Trueだったら取り得る数字からxを消したタプルリストを返す
deletedList (_, []) s = s
deletedList (n,(x:xs)) s
    | isProblem n (tempList (n,(x:xs)) s s) = deleteCList s [n] x
    | otherwise = deletedList (n,xs) s

-- いったん確定させたタプルリストを受け取り、check_v2して、不都合があるか判定し、不都合があればその数字を消したタプルリストを返す
checkDeleteC [] s = s
checkDeleteC (x:xs) s = checkDeleteC xs $ deletedList x s

-- 変化がなくなるまで場所チェックと数字チェックと組み合わせチェックと仮入れを繰り返す
check_v3 ppps pps pres s
    | isComplete s = s
    | ppps == s = s
    | pps == s = check_v3 pps pres s (checkDeleteC s s)
    | pres==s = check_v3 pps pres s (checkCombination s s)
    | otherwise = check_v3 pps pres s $ checkN (checkConfirmed s s) (checkConfirmed s s)

-- 複数の問題
listCheck_v3 [] = []
listCheck_v3 (x:xs) = (check_v3 [] [] [] x) : listCheck_v3 xs
