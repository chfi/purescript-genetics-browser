module Genetics.Browser.UI.View where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Right, Left))
import Data.Newtype (over, unwrap, wrap)
import Data.Pair (Pair(..))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Milliseconds, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Genetics.Browser.Coordinates (CoordSys, CoordSysView(..), normalizeView, scalePairBy, translatePairBy)

foreign import onTimeout
  :: Milliseconds
  -> Effect Unit
  -> Effect { run    :: Effect Unit
            , cancel :: Effect Unit }


data UpdateView =
    ScrollView Number
  | ZoomView Number
  | ModView (Pair BigInt -> Pair BigInt)


instance showUpdateView :: Show UpdateView where
  show (ScrollView x) = "(Scroll by " <> show x <> ")"
  show (ZoomView s) = "(Zoom by " <> show s <> ")"
  show _ = "(ModView)"


    -- TODO idk if this instance makes sense??? whatevs
instance semigroupUpdateView :: Semigroup UpdateView where
  append (ScrollView x1) (ScrollView x2) = ScrollView (x1 + x2)
  append (ZoomView s1)   (ZoomView s2)   = ZoomView   (s1 * s2)
  append _ y = y

instance monoidUpdateView :: Monoid UpdateView where
  mempty = ModView identity


updateViewFold :: UpdateView
               -> CoordSysView
               -> CoordSysView
updateViewFold uv = over CoordSysView case uv of
  ZoomView   x -> (_ `scalePairBy`     x)
  ScrollView x -> (_ `translatePairBy` x)
  ModView f    -> f



data Animation =
    Scrolling Number
  | Zooming   (Pair Number)
  | Jump

-- | A `ViewAnimation` describes either an animation that can be applied
-- | to achieve the *visual* effect required to stay in sync with the
-- | current change in view, or a final `CoordSysView` to apply to
-- | relevant tracks when the change in view is finished.
type ViewAnimation = Either Animation CoordSysView


animateDelta' :: ∀ a a' b.
                 Monoid a'
              => (a' -> a -> { velocity :: a'
                             , position :: a
                             , output :: b })
              -> (b -> Effect Unit)
              -> { position :: a
                 , velocity :: a' }
              -> Milliseconds
              -> Effect (a' -> Effect Unit)
animateDelta' update cb initial timeout = do
  velRef <- Ref.new initial.velocity
  posRef <- Ref.new initial.position

  control <- onTimeout timeout do
    vel <- Ref.read velRef
    pos <- Ref.read posRef
    let step = update vel pos
    Ref.write step.velocity velRef
    Ref.write step.position posRef

    cb step.output

  pure \cmd -> do
    Ref.modify_ (_ <> cmd) velRef
    control.run


uiViewUpdate' :: ∀ c r.
                CoordSys c BigInt
             -> Milliseconds
             -> CoordSysView
             -> (CoordSysView -> Effect Unit)
             -> Effect (UpdateView -> Effect Unit)
uiViewUpdate' cs timeout view cb = do
  let done (Left x)  = cb x
      done (Right a) = log "animating"

  animateDelta' (viewUpdate' cs) done { position: view, velocity: mempty } timeout


viewUpdate' :: CoordSys _ BigInt
            -> UpdateView
            -> CoordSysView
            -> { velocity :: UpdateView
               , position :: CoordSysView
               , output   :: Either CoordSysView Animation }
viewUpdate' cs uv curView =
  let updatePosition vD = normalizeView cs (BigInt.fromInt 200000)
                          <<< updateViewFold vD
  in case uv of
       sv@(ScrollView x) ->
         let x' = x * 0.5
             output =  if x' < 0.001 then Left curView
                                     else Right $ Scrolling x
             velocity = ScrollView x'
             position = updatePosition sv curView
         in { velocity, position, output }

       zv@(ZoomView   z) ->
         let z' = z * 0.5
             output =  if z' < 0.001 then Left curView
                                     else Right $ Zooming (Pair 0.25 0.75)
             velocity = ZoomView z'
             position = updatePosition zv curView
         in { velocity, position, output }

       ModView f ->
         let position = wrap $ f $ unwrap curView
         in { velocity: ModView identity
            , position
            , output: Left position }
