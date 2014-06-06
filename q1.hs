import Prelude hiding(flip, curry) -- 同名の関数を定義するために隠している．隠さない場合はMain.flipなどとスコープを指定する

-- 1 基本概念
-- 1.1 セッションとスクリプト

square x = x * x

-- 1.1.1
quad :: Float -> Float
quad x = square (square x)

-- 1.1.2
greater :: Float -> Float -> Float
greater x y = if x>y then x else y

-- 1.1.3
squareArea :: Float -> Float
squareArea r = (22/7) * r * r

-- 1.2 評価

infinity :: Integer
infinity = infinity + 1


-- 1.2.1
--  square infinity
--  -> infinity * infinity
--  -> (infinity + 1) * infinity
--  -> ((infinity + 1) + 1) * infinity
--  となり，*演算子の左オペランドの評価が正規形にならず停止しないため，式自体の評価も停止しない．

-- 1.2.2
-- 3つ．
-- square (3 + 4)
-- -> square 7 {+}
--   -> 7 * 7 {square}
--     -> 49 停止 {*}
-- -> (3 + 4) * (3 + 4) {square}
--   -> 7 * (3 + 4) {+}
--     -> 7 * 7 {+}
--       -> 49 停止 {*}
--   -> (3 + 4) * 7 {+}
--     -> 7 * 7 {+}
--       -> 49 停止 {*}

-- 1.2.3
-- k番目の簡約規則の適用を(k)と表す．
-- succ (pred (succ (pred (pred (zero)))))
--   -> succ (pred (pred (zero))) {外側の(1)}
--     -> pred (zero) {succ(pred(e))->e}
--   -> succ (pred (pred (zero))) {(2)}
--     -> pred (zero) {succ(pred(e))->e}
--   -> succ (pred (pred (zero))) {内側の(1)}
--     -> pred (zero) {succ(pred(e))->e}
-- 3通り．
-- どれも同じ結果になる．
-- size(zero) = 1
-- size(succ(e)) = 1+size(e)
-- size(pred(e)) = 1+size(e)
-- なるsizeを定義した場合，いずれの簡約においてもsizeの値は小さくなる一方となる．
-- sizeの下限1が存在するため，簡約はいずれ停止する．

-- 1.2.4
-- add (succ (pred (zero)), zero)
--   -> add (zero, zero) {(1)}
--     -> zero {(3)}
--   -> succ (add (pred (zero), zero)) {(4)}
--     -> succ (pred (add (zero, zero))) {(5)}
--       -> add (zero, zero) {(1)}
--         -> zero {(3)}
--       -> succ (pred (zero)) {(3)}
--         -> zero {(1)}
-- 3通り，同じ．

-- 1.2.5
-- succ (pred (e)) -> e
--   size(e) - (1+(1+size(e))) = -2
-- pred (succ (e)) -> e
--   size(e) - (1+(1+size(e))) = -2
-- add(zero, e2) -> e2
--   size(e2) - (1+2*(1+size(e2)) = -s-3
-- add(succ(e1), e2) -> succ(add(e1, e2))
--   1+1+2*(size(e1)+size(e2)) - (1+2*(1+size(e1)+size(e2)) = -1
-- add(pred(e1), e2) -> pred(add(e1, e2))
--   1+1+2*(size(e1)+size(e2)) - (1+2*(1+size(e1)+size(e2)) = -1
-- 全て減少する．
-- 式の大きさの下限1が存在し，簡約するたびに式のサイズが減るため，必ず停止する．

-- 1.3.1
multiply :: (Integer, Integer)->Integer
multiply (x, y) = if x==0 then 0 else x*y

-- multiply(0, infinity) = 0
-- multiply(infinity, 0) = bottom

-- 1.3.2
-- h bottom
-- = f (g bottom) {hの定義}
-- = f bottom {gは正格(g bottom = bottom)}
-- = bottom {fは正格(f bottom = bottom)}
-- よりhは正格

-- 1.4.1
f :: Integer->Integer
f = undefined -- 型は決まっているけど値は決まっていない時に使う値，実際に評価しようとするとエラーになる
g :: Integer->(Integer->Integer)
g = undefined
h :: Integer->Integer->Integer
h x y = f (g x y)

-- f . gは型が合わないので1番目と3番目はおかしい
-- 2番めは
-- h x y 
-- = (f . (g x)) y {2番めの定義}
-- = f ((g x) y) {(.)の定義}
-- より，元のhの定義と一致

-- 1.4.2
delta :: (Float, Float, Float) -> Float
delta (a, b, c) = sqrt(square b - 4 * a * c)

deltac :: Float->Float->Float->Float
deltac a b c = sqrt(square b - 4 * a * c)

-- 1.4.3
-- log :: Float->Float->Float

-- 1.4.4
-- (Float->Float)->Float->Float->Float

-- 1.4.5
apply0 :: (Integer->Integer)->Integer
apply0 f = f 0

applyInteger :: (Integer->Integer)->(Integer->Integer)
applyInteger f x = f x

-- 1.4.6
-- (*) x == (*x) xをかける関数
-- (+) x == (+x) xを足す関数
-- (-) x /= (-x) xの符号を反転させた値

-- 1.4.7
uncurry :: (a->b->c)->(a,b)->c
uncurry f (x, y) = f x y

curry :: ((a,b)->c)->a->b->c
curry f a b = f (a, b)
-- curry (uncurry f) x y
-- = (uncurry f) (a, b) {curryの定義}
-- = f a b {uncurryの定義}
--
-- uncurry (curry f) (x, y)
-- = (curry f) x y {uncurryの定義}
-- = f (x, y) {curryの定義}

-- 1.5.1
fib :: Integer -> Integer
fib n
  | n<0 = error "negative argument"
  | n>=0 = fib(n-1)+fib(n-2)

-- 1.5.2
abs :: Integer -> Integer
abs x = if x>=0 then x else -x

-- 1.6.1
-- いずれも，型シグネチャを書かない状態でghciで:t constなどと型を調べると適切に割り当ててくれる
const :: a->b->a
const x y = x

subst :: (a->b->c)->(a->b)->a->c
subst f g x = f x (g x)

apply :: (a->b)->(a->b)
apply f x = f x

flip :: (a->b->c)->b->a->c
flip f x y = f y x

-- 1.6.2
--q162 :: ((a, b)->c)->Bool
q162 f = [flip(curry f), curry(f . swap)] -- 配列に入れて型が合うことだけ確認

swap :: (a, b)->(b, a)
swap (x, y) = (y, x) 

-- 1.6.3
strange :: ((a->b)->a)->(a->b)->b
strange f g = g (f g)

-- f :: (A -> B) = Aでなくてはならない
-- stranger f = f f

-- 1.6.4
square164 :: Num a=>a->a
square164 x = x * x

-- 1.7.1
increase :: Integer->Integer
increase x = x*x*x+1
