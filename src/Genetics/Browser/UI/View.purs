module Genetics.Browser.UI.View where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Right, Left))
import Data.Lens ((^.))
import Data.List (List)
import Data.List as List
import Data.Newtype (over)
import Data.Pair (Pair(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Milliseconds)
import Effect.Ref as Ref
import Genetics.Browser.Canvas (BrowserContainer, TrackAnimation(..), animateTrack, forTracks_)
import Genetics.Browser.Coordinates (CoordSys, CoordSysView(..), _TotalSize, normalizeView, scalePairBy, translatePairBy)


foreign import onTimeout
  :: Milliseconds
  -> Effect Unit
  -> Effect { run    :: Effect Unit
            , cancel :: Effect Unit }

foreign import onFrame
  :: Effect Unit
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




-- | TODO: instead of some low delay on the `step` timeout, use `requestAnimationFrame`
animateDelta :: ∀ a a' b.
                    Monoid a'
             => { step    :: a' -> a -> a
                , animate :: a' -> a -> b }
             -> (Either a b -> Effect Unit)
             -> { position :: a
                , velocity :: a' }
             -> Milliseconds
             -> Effect { update :: a' -> Effect Unit
                       , position :: Effect a
                       , velocity :: Effect a' }
animateDelta motion cb initial timeout = do

  posRef <- Ref.new initial.position
  velRef <- Ref.new initial.velocity

  done <- onTimeout timeout do
    pos <- Ref.read posRef
    cb $ Left pos

  comms <- onFrame do
    vel <- Ref.read velRef
    Ref.write mempty velRef
    pos <- Ref.read posRef
    Ref.write  (motion.step vel pos) posRef
    cb (Right $ motion.animate vel pos)
    done.run

  let update cmd = do
          Ref.modify_ (_ <> cmd) velRef
          comms.run

      position = Ref.read posRef
      velocity = Ref.read velRef

  pure { update, position, velocity }


type ViewManager = { updateView  :: UpdateView -> Effect Unit
                   , browserView :: Effect CoordSysView
                   , addCallback :: (CoordSysView -> Effect Unit) -> Effect Unit
                   }

browserViewManager :: ∀ c.
                      CoordSys c BigInt
                   -> Milliseconds
                   -> { initialView :: CoordSysView }
                      -- , minimumBpPerPixel :: Number }
                   -> BrowserContainer
                   -> Effect ViewManager
browserViewManager cSys timeout options bc = do

  cbs <- Ref.new (mempty :: List (CoordSysView -> Effect Unit))

  let initial = { position: options.initialView
                , velocity: mempty }


      step :: UpdateView -> CoordSysView -> CoordSysView
      step uv = normalizeView cSys (BigInt.fromInt 200000)
                 <<< updateViewFold uv

      animate :: UpdateView -> CoordSysView -> TrackAnimation
      animate uv csv = case uv of
        ScrollView x -> Scrolling (toLengths csv x)
        ZoomView   x -> Zooming   (toZoomRange csv x)
        ModView _    -> Jump


      toLengths :: CoordSysView -> Number -> Number
      toLengths (CoordSysView (Pair l r)) x =
        if x < zero then (if l <= zero then 0.0 else x)
                    else (if r >= (cSys ^. _TotalSize) then 0.0 else x)

      toZoomRange :: CoordSysView -> Number -> Pair Number
      toZoomRange (CoordSysView (Pair l r)) x =
        let dx = (x - 1.0) / 2.0
            l' = if l <= zero then 0.0
                              else (-dx)
            r' = if r >= (cSys ^. _TotalSize) then 1.0
                                              else 1.0 + dx
        in Pair l' r'

      callback :: Either CoordSysView TrackAnimation -> Effect Unit
      callback = case _ of
        Right a -> forTracks_ bc (flip animateTrack a)
        Left  v -> Ref.read cbs >>= traverse_ (_ $ v)

  anim <- animateDelta { step, animate } callback initial timeout

  pure { updateView:  anim.update
       , browserView: anim.position
       , addCallback: \cb -> Ref.modify_ (List.Cons cb) cbs
       }
