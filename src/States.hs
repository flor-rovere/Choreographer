module States where

import Control.Monad (liftM,ap)

import DataType

instance Functor State where
    fmap = liftM

instance Applicative State where
    pure = return
    (<*>) = ap

newtype State a = St {runState :: ActualPosition -> (a,ActualPosition)}

instance Show (State a) where
    show = show

instance Monad State where
    return x = St (\s -> (x, s))
    m >>= f  = St (\s -> let (v, s') = runState m s
                          in runState (f v) s')

class Monad m => MonadState m where
    update :: Step -> m ()

instance MonadState State where
    update xs = St (\s -> ((), update' xs s))
                where update' [] s                     = s
                      update' ((Step v i):xs) s        = case v of 
                           RightArm  -> update' xs (s {rightArm=i})
                           LeftArm   -> update' xs (s {leftArm=i})
                           RightLeg  -> update' xs (s {rightLeg=i})
                           LeftLeg   -> update' xs (s {leftLeg=i})
                           StartPace -> update' xs (s {pace=i})
                           EndPace   -> update' xs (s {pace=i})
                           Head      -> update' xs (s {bhead=i})
                      update' ((End):xs) s             = s
                      update' ((StartMany n v i):xs) s = s
                      update' ((EndMany n v i):xs) s   = s
