import Data.Char(ord, chr)

-- 2 単純なデータ型

-- 2.1.1
-- BoolがEqのインスタンスであることを使っている
(/\), (\/) :: Bool -> Bool -> Bool
a /\ b = if a==False then False else b
a \/ b = if a==False then b else True

-- 2.1.2
-- =>は予約されている
(==>) :: Bool -> Bool -> Bool
True ==> False = False
_ ==> _ = True

-- 2.1.3
class MyEq a where
    (==.), (/=.) :: a -> a -> Bool
    a /=. b = not (a ==. b)

-- 2.1.4
data Triangle = Failure | Isosceles | Equilateral | Scalene deriving Show

-- given (a,b,c) s.t. a<=b and b<=c
-- TODO: QuickCheck
analyse :: (Int, Int, Int) -> Triangle
analyse (x,y,z)
  | x+y<=z = Failure
  | x==z = Equilateral
  | (x==y) `xor` (y==z) = Isosceles
  | x/=y && y/=z && z/=x = Scalene
  where a `xor` b = not(a==b)

-- 2.1.5
sort3 (a,b,c) = if a<=b
                  then if a<=c then merge a (sort2(b,c)) else merge c (sort2 (a,b))
                  else if b<=c then merge b (sort2(c,a)) else merge c (sort2 (a,b))
    where sort2 (a,b) = if a<=b then (a,b) else (b,a)
          merge a (b,c) = (a,b,c)

analyse' = analyse. sort3

-- 2.1.6
-- 7個．
-- 簡単のために data Triangle = A | B | C | D とすると
-- A<B=True
-- A<C=True
-- A<D=True
-- B<C=True
-- B<D=True
-- C<D=True
-- _<_=False
--
-- (縦)<(横)
--  abcd
-- a ...
-- b  ..
-- c   .
-- d    
--
-- 次のようにしても同じ数になる
-- a<b = not (a<=b)
-- A<=_ = True
-- _<=D = True
-- B<=B = True
-- B<=C = True
-- C<=C = True
-- _<=_ = False
--
-- (縦)<=(横)
--  abcd
-- a....
-- b ...
-- c  ..
-- d   .

-- 2.1.7
-- 複素数

-- 2.1.8
-- 反射律
-- True==True => True
-- False==False => True
-- よりok
--
-- 推移律
-- ならばの左側が成り立つのは次の2つの場合のみ
-- 1. x=y=z=Trueの時 ならばの右側x==zも成り立つ
-- 1. x=y=z=Falseの時 ならばの右側x==zも成り立つ
-- より命題全体も成り立つ ok
--
-- 対称律
-- 1. x=Trueの時
--   a. y=Trueの時 True=>True
--   b. y=Falseの時 False=>False
-- 2. x=Falseの時
--   a. y=Trueの時 False=>False
--   b. y=Falseの時 True=>True
-- いずれも満たす ok

-- 2.1.9
-- 推移律 x<yかつy<zならx<z

-- 2.2.1
nextlet :: Char -> Char
nextlet 'Z' = 'A'
nextlet c = chr(ord c+1)

-- 2.2.2
digitval :: Char -> Int
digitval c = ord c - ord '0'

-- 2.3.1
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving(Eq, Ord, Enum, Show)

dayBefore :: Day -> Day
dayBefore d = toEnum((fromEnum d-1+7) `mod` 7)

-- 2.3.2
data Direction = North | East | South | West deriving(Eq, Ord, Enum, Show)

reverseDirection :: Direction -> Direction
reverseDirection dir = toEnum((fromEnum dir+2) `mod` 4)

-- 2.3.3
data MyBool = MyFalse | MyTrue deriving Show

instance Enum MyBool where
    fromEnum MyFalse = 0
    fromEnum MyTrue = 1
    toEnum 0 = MyFalse
    toEnum 1 = MyTrue

-- 2.4.1
-- 命題cross (f,g) . cross (h,k) == cross (f.h, g.k)
-- a) 引数が(a,b)の場合
-- (cross (f,g) . cross (h,k)) (a,b)
-- = (cross (f,g) . pair (h.fst, k.snd)) (a,b) {crossの定義}
-- = cross (f,g) (pair (h.fst, k.snd) (a,b)) {.の定義}
-- = cross (f,g) ((h.fst) (a,b), (k.snd) (a,b)) {pairの定義}
-- = cross (f,g) ((h.fst) (a,b), (k.snd) (a,b)) {.の定義}
-- = cross (f,g) (h(fst (a,b)), k(snd (a,b))) {.の定義}
-- = cross (f,g) (h a, k b) {fst, sndの定義}
-- = pair (f.fst, g.snd) (h a, k b) {crossの定義}
-- = ((f.fst)(h a, k b), (g.snd)(h a, k b))  {pairの定義}
-- = (f(fst(h a, k b)), g(snd(h a, k b)))  {.の定義}
-- = (f(h a), g(k b))  {fst, sndの定義}
-- cross (f.h, g.k) (a,b)
-- = pair(f.h.fst, g.k.snd)(a,b) {crossの定義}
-- = ((f.h.fst)(a,b), (g.k.snd)(a,b)) {pairの定義}
-- = (f(h(fst(a,b))), g(k(snd(a,b)))) {.の定義}
-- = (f(h a), g(k b)) {fst, sndの定義}
-- 一致
-- b)引数が⊥の場合
-- (cross (f,g) . cross (h,k)) ⊥
-- = cross (f,g) (h(fst ⊥), k(snd ⊥)) {上と同様に}
-- = cross (f,g) (h⊥, k⊥) {fst, snd正格}
-- = (f(h ⊥), g(k ⊥))  {上と同様に}
-- cross (f.h, g.k) (a,b)
-- = ((f.h.fst)⊥, (g.k.snd)⊥) {上と同様に}
-- = (f(h ⊥), g(k ⊥)) {fst, snd正格}
-- いずれもfst, sndの時点で

-- 2.4.2
data Triple a b c = MkTriple a b c

-- 2.4.3
getAge :: (Int,Int,Int) -> (Int,Int,Int) -> Int
getAge (nowD,nowM,nowY) (birD,birM,birY) = if(nowM, nowD)<(birM, birD)
                                                               then nowY-birY-1
                                                               else nowY-birY

-- 2.4.4
-- 可能．Boundedでなくてもいいように次のように割り振る
-- a 0 1 2 3 b
-- 0 0 1 4 9
-- 1 3 2 510
-- 2 8 7 611
-- 315141312
-- TODO: と思ったがこれではBoundedの場合にうまくいかない

instance (Enum a, Enum b)=>Enum(a,b) where
    fromEnum (a,b) = imax*imax-1+if ia<ib then -2*ib+ia+1 else -ib+1
      where ia = fromEnum a+1
            ib = fromEnum b+1
            imax = max ia ib
    toEnum n = if imax*imax-imax<(n+1) then (toEnum (imax-1), toEnum (imax*imax-(n+1))) else (toEnum (n-(imax*imax-2*imax+1)), toEnum (imax-1))
      where imax = ceiling(sqrt(fromIntegral n+1))

-- 2.5.1
q251 :: Either Bool Char -> Bool
q251 (Left _) = True
q251 (Right _) = False

-- 2.5.2
-- case (f,g) . plus (h,k) == case (f.h, g.k)
-- a) 引数がLeft aの場合
-- (case (f,g) . plus (h,k)) (Left a)
-- = case (f,g) (plus (h,k) (Left a)) {.の定義}
-- = case (f,g) (case (Left. h, Right. k) (Left a)) {plusの定義}
-- = case (f,g) ((Left. h) a) {caseの定義}
-- = case (f,g) (Left (h a)) {.の定義}
-- = f (h a) {.の定義}
-- case (f.h, g.k) (Left a)
-- = (f.h) a {caseの定義}
-- = f (h a) {.の定義}
-- 一致
-- b) 引数がRightの場合
-- 同様に一致
-- c) 引数が⊥の場合
-- (case (f,g) . plus (h,k)) (Left a)
-- = case (f,g) (case (Left. h, Right. k) ⊥) {上と同様に}
-- = case (f,g) ⊥ {上と同様に}
-- = bot {上と同様に}
-- case (f.h, g.k) ⊥
-- = ⊥ {上と同様に}
-- 一致

-- 2.6.1
-- 型シノニムなので既存のInteger等の数値どうしを厳密に比較してしまうため，定義できない．

-- 2.6.2
data Jane = MkJane Int

newtype Dick = MkDick Int

jane (MkJane _) = True
dick (MkDick _) = True
q262 = jane undefined /= dick undefined

-- 2.7.1
-- 2.1.5のsort3 :: (Ord a)=>(a,a,a)->(a,a,a)を使用
q271 = sort3 ("McMillan", "Macmillan", "MacMillan")

-- 2.7.2
q272 = [show(show 42), show 42++show 42, show "\n"]
-- それぞれ
-- "42"
-- 4242
-- "\n"
-- となる

-- 2.7.3
showDate :: (Int,Int,Int) -> String
showDate (d, m, y) = show d++' ':showMonth m++',':' ':show y

showMonth :: Int -> String
showMonth 1 = "January"
showMonth 2 = "February"
showMonth 3 = "March"
showMonth 4 = "April"
showMonth 5 = "May"
showMonth 6 = "June"
showMonth 7 = "July"
showMonth 8 = "August"
showMonth 9 = "September"
showMonth 10 = "October"
showMonth 11 = "November"
showMonth 12 = "December"
-- 複雑なバージョン略
