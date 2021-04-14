{-# LANGUAGE FlexibleInstances #-}

module ChapterExercises where 

data Sum a b = First b | Second a

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b


data Company a b c = DeepBlue a b | Something c

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c


data More a b = L b a b | R a b a deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'


data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)


-- data K a b = K a

-- instance Functor (K a) where
--     fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K a b = K a

instance Functor (Flip K a) where 
    fmap f (Flip (K a))=  Flip (K (f a))


data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where 
    fmap f (GoatyConst b)=  GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where 
    fmap f (LiftItOut (fa)) =  LiftItOut (f <$> fa)

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where 
    fmap f (DaWrappa fa ga)  =  DaWrappa (f <$> fa) (f <$> ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where 
    fmap f (IgnoringSomething fa ga) = IgnoringSomething (fa) (f <$> ga)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where 
    fmap f (Notorious go ga gt) = Notorious (go) (ga) (f <$> gt)

data List a = Nil | Cons a (List a)

instance  Functor List where 
    fmap _ Nil = Nil
    fmap f (Cons h t) = Cons (f h) (f <$> t)

data GoatLord a = 
      NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a)
                (GoatLord a)
                (GoatLord a)

instance  Functor GoatLord where 
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a) 
    fmap f (MoreGoats a b c) = MoreGoats (f<$>a) (f<$>b) (f<$>c)

data TalkToMe a =
      Halt
    | Print String a
    | Read (String -> a)

instance  Functor TalkToMe where 
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a) 
    fmap f (Read sa) = Read (f<$>sa)