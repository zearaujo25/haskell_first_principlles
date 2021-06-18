{-# LANGUAGE InstanceSigs #-} 
module State  where 


newtype Moi s a = Moi { runMoi :: s -> (a, s) }


instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi$ (\(a,s)-> ((f a),s)).g    

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi$ \s -> (a,s)
    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi$ \s -> ( (\(a,_)-> (((fst.f$ s) a) ,s) ).g$s)

instance Monad (Moi s) where
    return = pure
    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi f) >>= g = Moi$ \s->  (fst (runMoi  (g.fst.f$ s) s), s)


get :: Moi s s
get = Moi$ \s->(s,s)  

put :: s -> Moi s ()
put s = Moi$ \_-> ((),s)  

exec :: Moi s a -> s -> s
exec (Moi sa) = snd.sa 

eval :: Moi s a -> s -> a
eval (Moi sa) = fst.sa 

modify :: (s -> s) -> Moi s ()
modify f = Moi$ \s-> ((),(f s))