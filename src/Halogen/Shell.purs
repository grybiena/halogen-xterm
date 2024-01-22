module Halogen.Shell
  ( Query(..)
  , Shell(..)
  , component
  ) where

import Prelude

import Control.Monad.Free (runFreeM)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Options ((:=))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Shell.Free (Args, ProcessHandle(..), ShellF(..), ShellM(..), Stdin(..), Stdout(..))
import Halogen.Terminal as Terminal
import Halogen.Terminal.Free (TerminalM)
import Type.Proxy (Proxy(..))
import XTerm.Options (cursorBlink, fontFamily)
import XTerm.Terminal (Terminal, new)

type Slots s o m =
  ( terminal :: H.Slot TerminalM Terminal.Output Unit
  , process :: H.Slot Stdin (Stdout s o m) ProcessHandle
  )

_terminal = Proxy :: Proxy "terminal"
_process = Proxy :: Proxy "process"

type Shell q r s o m =
  { init :: ShellM s o m Unit
  , query :: q -> ShellM s o m r
  , shell :: s
  }

type State q r s o m =
  { shell :: Shell q r s o m
  , interpreter :: Terminal.Output -> ShellM s o m Unit
  , terminal :: Maybe Terminal
  , processes :: Map ProcessHandle (H.Component Stdin Args (Stdout s o m) m /\ Args)
  }

data Action s o m =
    Initialize
  | TerminalOutput Terminal.Output
  | ProcessOutput (Stdout s o m)

data Query q r a = Query q (r -> a)


component :: forall q r s o m. MonadAff m => H.Component (Query q r) (Shell q r s o m) o m
component = do
  H.mkComponent
    { initialState: \shell -> { shell, interpreter: const (pure unit), terminal: Nothing, processes: Map.empty }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , handleQuery = handleQuery
                                     , initialize = Just Initialize
                                     }
    }

render :: forall q r s o m. MonadAff m => State q r s o m -> H.ComponentHTML (Action s o m) (Slots s o m) m
render { terminal, processes } =
  HH.div_
    ([ case terminal of
         Nothing -> HH.div_ []
         Just te -> HH.slot _terminal unit Terminal.component te TerminalOutput
     ]
   <> ((\(h /\ (c /\ a)) -> HH.slot _process h c a ProcessOutput) <$> Map.toUnfoldable processes))


handleAction :: forall q r s o m .
                MonadAff m
             => Action s o m
             -> H.HalogenM (State q r s o m) (Action s o m) (Slots s o m) o m Unit
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
  ProcessOutput (Stdout sh) -> void $ runShellM sh



handleQuery :: forall q r s o m a .
                MonadAff m
             => Query q r a
             -> H.HalogenM (State q r s o m) (Action s o m) (Slots s o m) o m (Maybe a)
handleQuery (Query q f) = do
  { shell } <- H.get
  r <- runShellM $ shell.query q
  pure $ f <$> r


runShellM :: forall q r s o m a .
            MonadAff m
         => ShellM s o m a
         -> H.HalogenM (State q r s o m) (Action s o m) (Slots s o m) o m (Maybe a)
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
    go (Exec c a e) = do
      let getMaxHandle procs = (maybe (ProcessHandle 0) (\x -> x.key) (Map.findMax procs))
      st <- H.modify (\st -> st { processes = Map.insert (one + getMaxHandle st.processes) (c /\ a) st.processes })
      pure (e (getMaxHandle st.processes))
    go (Kill h e) = do
      H.modify_ (\st -> st { processes = Map.delete h st.processes })
      pure e
    go (Tell h c a) = do
      H.lift $ H.tell _process h (Stdin c)
      pure a


