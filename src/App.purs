module App where

import Prelude

import Control.Monad.State (get, gets, modify_)
import Control.Plus (empty)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe')
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff as Aff
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI component_App {} =<< HA.awaitBody)

--------------------------------------------------------------------------------
-- App
--------------------------------------------------------------------------------

data AppAction
  = EngineOutput_AppAction EngineOutput
  | ViewOutput_AppAction ViewOutput

component_App = H.mkComponent { initialState, eval, render }
  where
  initialState _ = {}

  eval = H.mkEval H.defaultEval { handleAction = handleAction }

  handleAction (ViewOutput_AppAction vo) = H.tell (Proxy @"Engine") unit $ EngineQuery vo
  handleAction (EngineOutput_AppAction eo) = H.tell (Proxy @"View") unit $ ViewQuery eo

  render _ =
    HH.div []
      [ HH.slot (Proxy @"Engine") unit component_Engine {} EngineOutput_AppAction
      , HH.slot (Proxy @"View") unit component_View {} ViewOutput_AppAction
      ]

--------------------------------------------------------------------------------
-- Engine
--------------------------------------------------------------------------------

data EngineQuery a = EngineQuery ViewOutput a

data EngineOutput = EngineOutput Int Int

derive instance Generic EngineOutput _

instance Show EngineOutput where
  show x = genericShow x

component_Engine = H.mkComponent { initialState, eval, render }
  where
  initialState _ =
    { active_index: 0
    }

  eval = H.mkEval H.defaultEval
    { handleQuery = case _ of
        EngineQuery (ViewOutput active_index) a -> do
          { active_index: old_active_index } <- get
          modify_ _ { active_index = active_index }
          H.raise $ EngineOutput old_active_index active_index
          pure $ pure a
    }

  render _ = HH.div [ HP.style "display: none" ] []

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

data ViewQuery a = ViewQuery EngineOutput a
--   | ViewConfirm Confirm

data ViewAction = ViewAction MouseEvent Int

data ViewOutput = ViewOutput Int

derive instance Generic ViewOutput _

instance Show ViewOutput where
  show x = genericShow x

component_View = H.mkComponent { initialState, eval, render }
  where
  initialState _ =
    { values: [ true ] <> Array.replicate 10 false
    }

  eval = H.mkEval H.defaultEval
    { handleQuery = case _ of
        ViewQuery (EngineOutput i1 i2) a -> do
          Aff.delay (Aff.Milliseconds 200.0) # liftAff
          modify_ \state -> state
            { values =
                state.values
                  # Array.modifyAt i1 (const false) >>> fromMaybe' (\_ -> unsafeCrashWith "impossible")
                  # Array.modifyAt i2 (const true) >>> fromMaybe' (\_ -> unsafeCrashWith "impossible")
            }
          pure $ pure a
    , handleAction = \(ViewAction e i) -> do
        Aff.delay (Aff.Milliseconds 200.0) # liftAff
        e # MouseEvent.toEvent # Event.stopPropagation # liftEffect
        H.raise $ ViewOutput i
    }

  render state =
    HH.div [ HP.style "display: flex; flex-direction: column; gap: 0.5em;" ] $
      state.values # mapWithIndex \i value ->
        HH.div
          [ HE.onClick \e -> ViewAction e i
          , HP.style if value then "color: blue" else ""
          , HE.onMouseEnter \e -> ViewAction e i
          ]
          [ HH.text $ show value ]

