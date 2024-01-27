module Halogen.Shell
  ( Query(..)
  , Shell(..)
  , component
  ) where

import Prelude

import Control.Monad.Free (runFreeM)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Data.Slot as Slot
import Halogen.HTML as HH
import Halogen.Shell.Free (Action(..), ShellF(..), ShellM(..), Slots, WindowSlots, renderWindows)
import Halogen.Terminal as Terminal
import Type.Proxy (Proxy(..))
import XTerm.Options (cursorBlink, fontFamily)
import XTerm.Terminal (Terminal, new)

_terminal = Proxy :: Proxy "terminal"


type Shell w q r s o m =
  { init :: ShellM w s o m Unit
  , query :: q -> ShellM w s o m r
  , shell :: s
  }

type State w q r s o m =
  { shell :: Shell w q r s o m
  , interpreter :: Terminal.Output -> ShellM w s o m Unit
  , terminal :: Maybe Terminal
  , windows :: WindowSlots o w m 
  }

data Query q r a = Query q (r -> a)


component :: forall w q r s o m. MonadAff m => H.Component (Query q r) (Shell w q r s o m) o m
component = do
  H.mkComponent
    { initialState: \shell -> { shell, interpreter: const (pure unit), terminal: Nothing, windows: Slot.empty }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , handleQuery = handleQuery
                                     , initialize = Just Initialize
                                     }
    }

render :: forall w q r s o m. MonadAff m => State w q r s o m -> H.ComponentHTML (Action o) (Slots w) m
render { terminal, windows } =
  HH.div_
    ([ case terminal of
         Nothing -> HH.div_ []
         Just te -> HH.slot _terminal unit Terminal.component te TerminalOutput
     ]
     <> renderWindows windows

   )

handleAction :: forall w q r s o m .
                MonadAff m
             => Action o
             -> H.HalogenM (State w q r s o m) (Action o) (Slots w) o m Unit
handleAction = case _ of
  Initialize -> do
    terminal <- H.liftEffect $ new (fontFamily := "\"Cascadia Code\", Menlo, monospace"
                                 <> cursorBlink := true
                                   ) mempty
    H.modify_ (\st -> st { terminal = Just terminal })
    { shell } <- H.get
    void $ runShellM $ shell.init
  TerminalOutput output -> do
     { interpreter } <- H.get
     void $ runShellM $ interpreter output
  RaiseOutput o -> H.raise o
--  ProcessOutput (Stdout sh) -> void $ runShellM sh



handleQuery :: forall w q r s o m a .
                MonadAff m
             => Query q r a
             -> H.HalogenM (State w q r s o m) (Action o) (Slots w) o m (Maybe a)
handleQuery (Query q f) = do
  { shell } <- H.get
  r <- runShellM $ shell.query q
  pure $ f <$> r


runShellM :: forall w q r s o m a .
            MonadAff m
         => ShellM w s o m a
         -> H.HalogenM (State w q r s o m) (Action o) (Slots w) o m (Maybe a)
runShellM (ShellM s) = runMaybeT $ runFreeM go s
  where
    go (Terminal f) = do
      r <- H.lift $ H.query _terminal unit f
      MaybeT $ pure r
    go (Lift m) = MaybeT $ Just <$> H.lift m
    go (GetShell a) = do
      { shell } <- H.lift H.get
      pure $ a shell.shell
    go (PutShell sh a) = do
      H.modify_ (\st -> st { shell = st.shell { shell = sh } })
      pure a
    go (Interpreter i a) = do
      H.modify_ (\st -> st { interpreter = i })
      pure a
    go (Output o a) = do
      H.lift $ H.raise o
      pure a
    go (GetWindowSlots f) = do
      { windows } <- H.get
      pure $ f windows 
    go (ModifyWindowSlots f a) = do
      H.modify_ (\st -> st { windows = f st.windows })
      pure a

