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

-- 3.1.5
(-.) :: Nat->Nat->Nat
m -. Zero = m
Zero -. n = Zero
Succ m -. Succ n = m -. n
