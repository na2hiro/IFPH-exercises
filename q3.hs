-- 3 数値
data Nat = Zero | Succ Nat deriving Show

-- 3.1.1
data PositiveInt = One | SuccInt PositiveInt

-- 3.1.2
convert :: Nat -> Integer
convert Zero = 0
convert (Succ n) = 1+convert n

-- 3.1.3
(+.) :: Nat->Nat->Nat
Zero +. m = m
(Succ n) +. m = Succ (n +. m)

(*.) :: Nat->Nat->Nat
Zero *. m = Zero
(Succ n) *. m = (n *. m) +. n

-- 累乗は定義できない

-- 3.1.4
-- m+nのコストをP(m, n)とすると
-- P(m, 0) = 1
-- P(m, n) = P(m, n-1) + 1
-- より関数P(m, n)はnに関する等差数列
-- P(m, n) = n+1
--
-- m*nのコストをM(m, n)とすると
-- M(m, 0) = 1
-- M(m, n) = M(m, n-1) + P({m*(n-1)の結果}, m) + 1
--         = M(m, n-1) + m + 2
-- よりM(m, n)はnに関する等差数列
-- M(m, n) = (m+2)*n+1
--
-- (模範解答では(m+1)(m+2)になっていたので問い合わせ中)
--  → 模範解答が間違っており，こちらで用意した解答が採用された．

-- 3.1.5
(-.) :: Nat->Nat->Nat
m -. Zero = m
Zero -. n = Zero
Succ m -. Succ n = m -. n

-- 3.2.1
-- Succ Zero * n = n のnに関する帰納法
-- Zeroの場合
--   Succ Zero * Zero
--    = Zero {*の定義}
-- Succ nの場合
--   Succ Zero * Succ n
--    = (Succ Zero * n) + Succ Zero {*の定義}
--    = n + Succ Zero {帰納法の仮定}
--    = Succ (n + Zero) {+の定義}
--    = Succ n {+の定義}
-- より，全ての有限な自然数nにおいてSucc Zero * n = nが成り立つ．
-- Succ Zeroは*に関して左単位元である．

-- 3.2.2
-- a 
--  = a + b {bは右単位元}
--  = b {aは左単位元}

-- 3.2.3
-- p.62で有限の場合は示した．
-- nがbottomの場合
--   x ^ (m + bottom)
--   = x ^ bottom {+の定義}
--   = bottom {^の定義}
--   (x ^ m) * (x ^ bottom)
--   = (x ^ m) * bottom {^の定義}
--   = bottom {*の定義}

-- 3.2.4
-- pに関する帰納法
-- Zeroの場合
--   (m+n)+Zero
--    = m+n {+の定義}
--   m+(n+Zero)
--    = m+n {+の定義}
-- Succ kの場合
--   (m+n)+Succ k
--    = Succ ((m+n)+k) {+の定義}
--    = Succ (m+(n+k)) {帰納法の仮定}
--   m+(n+Succ k)
--    = m+Succ (n+k) {+の定義}
--    = Succ (m+(n+k)) {+の定義}

-- 3.2.5
-- nに関する帰納法
-- Zeroの場合
--   k*(m+Zero)
--    = k*m {+の定義}
--   k*m+k*Zero
--    = k*m+Zero {*の定義}
--    = k*m {+の定義}
-- Succ iの場合
--   k*(m+Succ i)
--    = k*(Succ (m+i)) {+の定義}
--    = k*(m+i)+k {*の定義}
--   k*m+k*Succ i
--    = k*m+k*i+k {*の定義}
--    = k*(m+i)+k {帰納法の仮定}

-- 3.2.6
-- Zero `op` n = Zero
-- Succ m `op` Zero = Zero
-- Succ m `op` Succ n = Succ (m `op` n)
-- `op`は2引き数のうち大きくない方を返す関数である
-- m `op` infinity = m におけるmに関する帰納法
-- Zeroの場合
--   Zero `op` infinity
--    = Zero {opの定義}
-- Succ nの場合
--   Succ n `op` infinity
--    = Succ n `op Succ infinity {infinityの定義}
--    = Succ (n `op` infinity) {opの定義}
--    = Succ n {帰納法の仮定}
-- bottomの場合
--   bottom `op` infinity
--    = bottom

-- 3.2.7
-- P(n) = n (-) m = bottom
-- bottomの場合
--   bottom (-) m = bottom
-- Succ nの場合
--   Succ n (-) m
--    = Succ n (-) Succ m' {帰納法の仮定より，n(-)m'=bottomなるm'が存在}
--    = n (-) m' {(-)の定義}
--    = bottom {2つ上より}
--
-- infinity (-) m = infinityのmに関する帰納法
-- Zeroの場合
--   infinity (-) Zero = infinity
-- Succ mの場合
--   infinity (-) Succ m
--    = Succ infinity (-) Succ m {infinityの定義}
--    = infinity (-) m {infinityの定義}
--    = infinity {帰納法の仮定}

-- 3.3.1
-- foldn Succ Zero n = nのnに関する帰納法
-- bottomの場合
--   foldn Succ Zero bottom
--    = bottom
-- Zeroの場合
--   foldn Succ Zero Zero
--    = Zero {foldnの定義}
-- Succ nの場合
--   foldn Succ Zero (Succ n)
--    = Succ (foldn Succ Zero n) {foldnの定義}
--    = Succ n {帰納法の仮定}

-- 3.3.2
-- m+n = n+m のnに関する帰納法
-- Zeroの場合
--   m+Zero
--    = m {+の定義}
--    = Zero+m {Zeroは+の左単位元}
-- Succ nの場合
--   m+Succ n
--    = Succ (m+n) {+の定義}
--    = Succ (n+m) {帰納法の仮定}
--    = Succ (foldn Succ n m) {p68の+の定義}
--  ここで，Succ . foldn Succ n = foldn Succ (Succ n)を満たすには，
--  融合則より Succ bottom = bottom, Succ n = Succ n, Succ . Succ = Succ . Succであればよい
--  Succ bottom = bottom なのか???

-- 3.3.3
_ `lt` Zero = False
Zero `lt` _ = True
Succ m `lt` Succ n = m `lt` n

m // n = if m `lt` n
          then Zero
          else Succ ((m-.n)//n)
-- (n*m)/n=m のmに関する帰納法(n=Zeroを除く)
-- Zeroの場合
--   (n*Zero)/n = Zero/n = Zero
-- Succ mの場合
--   (n*Succ m)/n
--    = ((n*m)+n)/n
--    = Succ ((((n*m)+n)-n)/n)
--    = Succ ((n*m)/n)
--    = Succ m

-- 3.3.4
log2 (Succ Zero) = Zero
log2 m = Succ (log2 (m // Succ (Succ Zero)))
-- log(2^m)=mのmに関する帰納法(便宜上Succ(Succ Zero)を2と表す)
-- Zeroの場合
--   log(2^Zero) = log(Succ Zero) = Zero
-- Succ mの場合
--   log(2^(Succ m))
--    = log(2^m*2)
--    = Succ (log (2^m*2 // 2))
--    = Succ (log (2*2^m // 2)) {TODO: *交換則}
--    = Succ (log (2^m))
--    = Succ m

-- 3.4.1
-- x = (x`div`y)*y + (x`mod`y)より
-- x`mod`y
--  = 3 - (3`div`(-4))*(-4)
--  = 3 - (-1)*(-4)
--  = -1

-- 3.4.2
-- x = (x`div`y)*y + (x`mod`y)
-- x = floor(x/y)*y + (x`mod`y)
-- (x`mod`y) = x - floor(x/y)*y
-- {
--   x/y-1<floor(x/y)<=x/y
--   -x/y+1> -floor(x/y)>=-x/y
--   y-x=(-x/y+1)*y< -floor(x/y)*y<=(-x/y)*y=-x
-- }
-- x + (y-x) <= (x`mod`y) <= x - x
-- y <= (x`mod`y) <= 0

-- 3.4.3
-- 全ての整数xに対してx=floor(x)であり，全ての実数yに対してfloor(y)は整数で
-- ある．よって，最初の式にx=floor(y)を代入しfloor(y) = floor(floor(y))

-- 3.5.1
-- 有理数a/bの表現を(x1,y1), (x2,y2)とする
-- x1/y1=x2/y2
-- x1*y2=x2*y1
-- ここでx1,y1は互いに素なのでy2はy1の倍数
-- また，x2,y2は互いに素なのでy1はy2の倍数
-- よってy1=y2．同様にx1=x2．
-- よって表現は唯一．

-- 3.5.2
-- xの1の位Xに関して
-- 0から4の場合
--   z=Xしかなく，y=(x-X)/10も唯一に定まる．
-- 5-9の場合
--   z=X-10しかなく，y=(x-(X-10))/10=(x-X)/10+1も唯一に定まる．
-- 逆に，これらの表現が元のxを表していることは自明．
repint x = if xx<=4 then (xx, zz) else (xx-10, zz+1)
    where xx = x-zz*10
          zz = x`div`10
