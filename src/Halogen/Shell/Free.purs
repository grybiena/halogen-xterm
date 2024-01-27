module Halogen.Shell.Free where

import Prelude

import Control.Monad.Cont (class MonadTrans, lift)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (State, execState)
import Data.Array (cons)
import Data.Symbol (class IsSymbol)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Data.Slot (Slot, SlotStorage, foreachSlot)
import Halogen.Data.Slot as Slots
import Halogen.HTML as HH
import Halogen.Terminal as Terminal
import Halogen.Terminal.Free (TerminalM)
import Prim.Row (class Cons)
import Type.Prelude (Proxy)

type Slots w =
  ( terminal :: H.Slot TerminalM Terminal.Output Unit
  | w
  )



newtype WindowSlot :: forall k1 k2. Type -> Row Type -> (Type -> Type) -> k1 -> k2 -> Type
newtype WindowSlot o w m query output = 
  WindowSlot {
    window :: H.ComponentHTML (Action o) (Slots w) m
  }

data Action o =
    Initialize
  | TerminalOutput Terminal.Output
  | RaiseOutput o


renderWindows :: forall o w m. WindowSlots o w m -> Array (H.ComponentHTML (Action o) (Slots w) m)
renderWindows slots = execState (foreachSlot slots renderWindows') []
  where
    renderWindows' :: forall query output.
                      WindowSlot o w m query output -> State (Array (H.ComponentHTML (Action o) (Slots w) m)) Unit 
    renderWindows' (WindowSlot { window }) = H.modify_ (cons window)

type WindowSlots a w m = SlotStorage w (WindowSlot a w m)




data ShellF w s o m r =
    Terminal (TerminalM r)
  | Lift (m r)
  | GetShell (s -> r)
  | PutShell s r
  | Interpreter (Terminal.Output -> ShellM w s o m Unit) r
  | Output o r

  | GetWindowSlots (WindowSlots o w m -> r)
  | ModifyWindowSlots (WindowSlots o w m -> WindowSlots o w m) r


instance Functor m => Functor (ShellF w s o m) where
  map f (Terminal t) = Terminal (f <$> t)
  map f (Lift q) = Lift (f <$> q)
  map f (GetShell s) = GetShell (f <<< s)
  map f (PutShell s a) = PutShell s (f a)
  map f (Interpreter s a) = Interpreter s (f a)
  map f (Output o a) = Output o (f a)
  map f (GetWindowSlots s) = GetWindowSlots (f <<< s)
  map f (ModifyWindowSlots s a) = ModifyWindowSlots s (f a)


type Shell w s o m = Free (ShellF w s o m)

newtype ShellM w s o m r = ShellM (Shell w s o m r)

instance MonadTrans (ShellM w s o) where
  lift = ShellM <<< liftF <<< Lift

instance MonadEffect m => MonadEffect (ShellM w s o m) where
  liftEffect = lift <<< liftEffect

instance MonadAff m => MonadAff (ShellM w s o m) where
  liftAff = lift <<< liftAff

derive newtype instance MonadRec (ShellM w s o m)
derive newtype instance Functor (ShellM w s o m)
derive newtype instance Apply (ShellM w s o m)
derive newtype instance Applicative (ShellM w s o m)
derive newtype instance Bind (ShellM w s o m)
derive newtype instance Monad (ShellM w s o m)

terminal :: forall w s o m r . TerminalM r -> ShellM w s o m r
terminal = ShellM <<< liftF <<< Terminal

getShell :: forall w s o m . ShellM w s o m s
getShell = ShellM $ liftF $ GetShell identity 

putShell :: forall w s o m . s -> ShellM w s o m Unit
putShell s = ShellM $ liftF $ PutShell s unit

modifyShell :: forall w s o m . (s -> s) -> ShellM w s o m Unit
modifyShell f = do
  cmd <- getShell
  putShell (f cmd)

interpreter :: forall w s o m . (Terminal.Output -> ShellM w s o m Unit) -> ShellM w s o m Unit
interpreter i = ShellM $ liftF $ Interpreter i unit

output :: forall w s o m . o -> ShellM w s o m Unit
output o = ShellM $ liftF $ Output o unit

modifyWindowSlots :: forall w s o m . (WindowSlots o w m -> WindowSlots o w m) -> ShellM w s o m Unit
modifyWindowSlots f = ShellM $ liftF $ ModifyWindowSlots f unit

newWindow :: forall q i o _1 sh so sym px s w m.
             Cons sym (Slot q o s) _1 (Slots w)
          => IsSymbol sym
          => Ord s
          => Cons sym (Slot q o s) px w
          => Proxy sym
          -> s
          -> H.Component q i o m
          -> i -> (o -> so) -> ShellM w sh so m Unit
newWindow label i component input actout = do
  let window = HH.slot label i component input (RaiseOutput <<< actout) 
  modifyWindowSlots (Slots.insert label i (WindowSlot { window }))

