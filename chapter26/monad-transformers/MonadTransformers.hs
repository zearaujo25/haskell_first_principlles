module MonadTransformers where 

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT mea)= EitherT$ (fmap.fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
    pure a = EitherT$ (pure ( pure a))
    (EitherT mef) <*> (EitherT mea) = EitherT$ (<*>)<$>mef<*>mea 

instance Monad m => Monad (EitherT e m) where
    return = pure
    (EitherT mea) >>= f = EitherT$ do 
            ea <- mea
            case ea of 
                (Right a) -> runEitherT (f a)
                (Left e) -> return (Left e)


swapEither :: Either e a -> Either a e 
swapEither (Left e) = Right e 
swapEither (Right a) = Left a


-- transformer version of swapEither.
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT$ swapEither<$>mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mea) = do 
    ea <- mea
    case ea of 
        (Left a) -> f a
        (Right b) -> g b

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT$ (fmap.fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
    pure a = ReaderT$ \r -> pure a
    (ReaderT rmf) <*> (ReaderT rma) = ReaderT$ (<*>) <$> rmf <*> rma

instance (Monad m) => Monad (ReaderT r m) where
    return = pure
    (ReaderT rma) >>= f = ReaderT$ \r -> do
        a <- (rma r)
        let rmb  = runReaderT.f$a
        rmb r

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
    fmap f (StateT smas) = StateT$ (fmap.fmap) (\(a,s) -> (f a,s)) smas

instance (Monad m) => Applicative (StateT s m) where
    pure a = StateT$ \s -> pure (a,s) 
    (StateT smfs) <*> (StateT smas) = StateT$ \s -> do  
        (f,s') <- (smfs s)
        (a,s'') <- (smas s')
        pure (f a, s'') 

instance (Monad m) => Monad (StateT s m) where
    return = pure
    (StateT smas) >>= f = StateT$ \s -> do 
        (a,s') <- (smas s)
        (runStateT (f a)) s'

class MonadTrans t where
    lift :: (Monad m) => m a -> t m a

instance MonadTrans (EitherT e) where
    lift ma = EitherT$ Right<$>ma  

instance MonadTrans (StateT s) where
    lift ma= StateT$ \s -> (flip (,) s)<$>ma