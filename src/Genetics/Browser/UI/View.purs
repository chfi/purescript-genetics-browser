module Genetics.Browser.UI.View where

import Prelude

import Data.BigInt (BigInt)
import Data.Either (Either(Right, Left))
import Data.Newtype (over)
import Data.Pair (Pair)
import Effect (Effect)
import Effect.Aff (Milliseconds)
import Effect.Ref as Ref
import Genetics.Browser.Coordinates (CoordSysView(..), scalePairBy, translatePairBy)

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


animateDelta :: ∀ a a' b.
                    Monoid a'
             => { step    :: a' -> a -> a
                , animate :: a' -> a -> b }
             -> (Either a b -> Effect Unit)
             -> { position :: a
                , velocity :: a' }
             -> { step :: Milliseconds
                , done :: Milliseconds }
             -> Effect (a' -> Effect Unit)
animateDelta update cb initial timeout = do

  posRef <- Ref.new initial.position
  velRef <- Ref.new initial.velocity

  done <- onTimeout timeout.done do
    pos <- Ref.read posRef
    cb $ Left pos

  comms <- onTimeout timeout.step do
    vel <- Ref.read velRef
    Ref.write mempty velRef
    pos <- Ref.read posRef
    Ref.write  (update.step vel pos) posRef
    cb (Right $ update.animate vel pos)
    done.run

  pure \cmd -> do
    Ref.modify_ (_ <> cmd) velRef
    comms.run
