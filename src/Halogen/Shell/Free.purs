module Halogen.Shell.Free where

import Prelude

import Control.Monad.Cont (class MonadTrans, lift)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (class MonadRec)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Terminal as Terminal
import Halogen.Terminal.Free (TerminalM)

type Args = Array String 
data Stdin a = Stdin String a
newtype Stdout s o m = Stdout (ShellM s o m Unit)

newtype ProcessHandle = ProcessHandle Int
derive newtype instance Eq ProcessHandle
derive newtype instance Ord ProcessHandle
derive newtype instance Semiring ProcessHandle

data ShellF s o m a =
    Terminal (TerminalM a)
  | Lift (m a)
  | GetShell (s -> a)
  | PutShell s a
  | Interpreter (Terminal.Output -> ShellM s o m Unit) a
  | Output o a
  | Exec (H.Component Stdin Args (Stdout s o m) m) Args (ProcessHandle -> a)
  | Kill ProcessHandle a 
  | Tell ProcessHandle String a

instance Functor m => Functor (ShellF s o m) where
  map f (Terminal t) = Terminal (f <$> t)
  map f (Lift q) = Lift (f <$> q)
  map f (GetShell s) = GetShell (f <<< s)
  map f (PutShell s a) = PutShell s (f a)
  map f (Interpreter s a) = Interpreter s (f a)
  map f (Output o a) = Output o (f a)
  map f (Exec c a e) = Exec c a (f <<< e)
  map f (Kill h a) = Kill h (f a)
  map f (Tell h s a) = Tell h s (f a)


type Shell s o m = Free (ShellF s o m)

newtype ShellM s o m a = ShellM (Shell s o m a)

instance MonadTrans (ShellM s o) where
  lift = ShellM <<< liftF <<< Lift

instance MonadEffect m => MonadEffect (ShellM s o m) where
  liftEffect = lift <<< liftEffect

instance MonadAff m => MonadAff (ShellM s o m) where
  liftAff = lift <<< liftAff

derive newtype instance MonadRec (ShellM s o m)
derive newtype instance Functor (ShellM s o m)
derive newtype instance Apply (ShellM s o m)
derive newtype instance Applicative (ShellM s o m)
derive newtype instance Bind (ShellM s o m)
derive newtype instance Monad (ShellM s o m)

terminal :: forall s o m a . TerminalM a -> ShellM s o m a
terminal = ShellM <<< liftF <<< Terminal

getShell :: forall s o m . ShellM s o m s
getShell = ShellM $ liftF $ GetShell identity 

putShell :: forall s o m . s -> ShellM s o m Unit
putShell s = ShellM $ liftF $ PutShell s unit

modifyShell :: forall s o m . (s -> s) -> ShellM s o m Unit
modifyShell f = do
  cmd <- getShell
  putShell (f cmd)

interpreter :: forall s o m . (Terminal.Output -> ShellM s o m Unit) -> ShellM s o m Unit
interpreter i = ShellM $ liftF $ Interpreter i unit

output :: forall s o m . o -> ShellM s o m Unit
output o = ShellM $ liftF $ Output o unit

exec :: forall s o m . H.Component Stdin Args (Stdout s o m) m -> Args -> ShellM s o m ProcessHandle
exec c a = ShellM $ liftF $ Exec c a identity

kill :: forall s o m . ProcessHandle -> ShellM s o m Unit
kill h = ShellM $ liftF $ Kill h unit

tell :: forall s o m . ProcessHandle -> String -> ShellM s o m Unit
tell h s = ShellM $ liftF $ Tell h s unit


