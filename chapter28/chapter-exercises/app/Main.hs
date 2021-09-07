module Main where
import Criterion.Main

newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL ([]++)
{-# INLINE empty #-}

singleton :: a -> DList a
singleton  a = DL (a:) 
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList (DL f)= f []
{-# INLINE toList #-}


-- Prepend a single element to a dlist.
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}


-- Append a single element to a dlist.
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL$  (++[x]).(unDL xs )
{-# INLINE snoc #-}


-- Append dlists.
append :: DList a -> DList a -> DList a
append xsf ysf = DL$ (unDL xsf) .  (unDL ysf)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
    where go 0 xs = xs
          go n xs = go (n-1) ([n] ++ xs)


constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
    where go 0 xs = xs
          go n xs = go (n-1) (singleton n `append` xs)


data Queue a =Queue { enqueue :: [a]
                    , dequeue :: [a]
                    } deriving (Eq, Show)
-- adds an item
push :: a -> Queue a -> Queue a
push a (Queue enq dq) =  (Queue (a:enq) dq)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue enq []) = pop$ Queue []  (reverse enq)
pop (Queue enq (x:xs) )= Just$ (x ,Queue enq xs)



main :: IO ()
main = defaultMain
    [ bench "concat list" $
    whnf schlemiel 123456
    , bench "concat dlist" $
    whnf constructDlist 123456
    ]


