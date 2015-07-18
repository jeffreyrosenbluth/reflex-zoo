{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Fix      (MonadFix)

import Reflex
import Graphics.Gloss (Picture, Display(..), white)

import Buttons
import GlossInterface

-- Utilities

if_then_else :: Bool -> a -> a -> a
if_then_else True  t _ = t
if_then_else False _ f = f

replaceWith :: Reflex t => a -> Event t b -> Event t a
replaceWith = fmap . const

filterEq :: (Eq a, Reflex t) => a -> Event t a -> Event t ()
filterEq x = replaceWith () . ffilter (== x)

eachE :: Reflex t => Event t () -> a -> Event t a
eachE = flip replaceWith

accumB :: (Reflex t, MonadHold t m, MonadFix m)
       => a -> Event t (a -> a) -> m (Behavior t a)
accumB a ef = do
  d <- foldDyn ($) a ef
  return $ current d

-- FRP network

mainReflex ::(Reflex t, MonadHold t m, MonadFix m)
           => Event t Float
           -> Event t InputEvent
           -> m (Behavior t Picture)
mainReflex _ glossEvent = do
    -- Part 1: static version

    -- Input

    let click0  = filterEq (Just Click) $ filter0  <$> glossEvent
        click5  = filterEq (Just Click) $ filter5  <$> glossEvent
        click10 = filterEq (Just Click) $ filter10 <$> glossEvent

        toggle0  = filterEq (Just Toggle) $ filter0  <$> glossEvent
        toggle5  = filterEq (Just Toggle) $ filter5  <$> glossEvent
        toggle10 = filterEq (Just Toggle) $ filter10 <$> glossEvent

    -- Behaviour

    mode0  <- accumB True (eachE toggle0  not)
    mode5  <- accumB True (eachE toggle5  not)
    mode10 <- accumB True (eachE toggle10 not)

    count0  <- accumB 0 $ leftmost
                 [ eachE toggle0 (const 0)
                 , eachE click0  (+1) ]
    count5  <- accumB 0 $ eachE (gate mode5 click5) (+1)
    count10 <- accumB 0 $ eachE click10 (+1)

    let minus1   = constant (-1)

        output0  = pull $ if_then_else  <$> sample mode0  <*> sample count0  <*> sample minus1
        output5  = pull $ if_then_else  <$> sample mode5  <*> sample count5  <*> sample minus1
        output10  = pull $ if_then_else <$> sample mode10 <*> sample count10 <*> sample minus1

        picture = pull $  renderButtons
                      <$> sample output0  <*> pure Nothing
                      <*> sample output5  <*> pure Nothing
                      <*> sample output10 <*> pure Nothing

    return picture



main :: IO ()
main = playReflex (InWindow "Reflex Example" (320, 240) (800, 200))
                  white
                  30
                  mainReflex
