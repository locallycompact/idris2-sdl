module Main

import Control.Linear.LIO
import Data.IORef
import Data.Nat
import Data.Vect
import System.Clock
import System.FFI
import System.Random

import System
import SDL
import SDL.Foreign

import Data.SortedMap
import GameOfLife

-- Press n to compute the next step in the game
-- Press s to start/stop the simulation
-- Press r to generate a new random game
-- Press +/- to speed up/down
-- Press arrow keys to pan

%auto_implicit_depth 256
black : SDLColor
black = RGBA 0 0 0 255

red : SDLColor
red = RGBA 255 0 0 255
%auto_implicit_depth 50

data Ref : (l : label) -> Type -> Type where
  [search l]
  MkRef : IORef a -> Ref x a

export
newRef : HasIO io => (x : label) -> t -> io (Ref x t)
newRef x val = do ref <- liftIO $ newIORef val
                  pure (MkRef ref)

export %inline
get : HasIO io => (x : label) -> (ref : Ref x a) => io a
get x {ref = MkRef io} = liftIO $ readIORef io

export %inline
put : HasIO io => (x : label) -> (ref : Ref x a) => a -> io ()
put x {ref = MkRef io} val = liftIO $ writeIORef io val

export %inline
modify : HasIO io => (x : label) -> (ref : Ref x a) => (a -> a) -> io ()
modify x f = do ref <- get x
                put x (f ref)

putError : LinearIO io => (err : SDLError) -> L io ()
putError = putStrLn . show

data Life : Type where -- Label for State

record LifeState where
  constructor MkLifeState
  {radius : Nat}
  game : Game (S radius) Cell
  scale : Nat
  frequency : Nat
  elapsedTicks : Nat
  lastTick : Clock Monotonic
  isPlaying : Bool
  averageUpdateTime : (Integer, Integer)

drawBoard : LinearIO io
         => (scale : Nat)
         -> (cells : List (Nat, Nat, Cell))
         -> (1 _ : SDL WithRenderer)
         -> L {use = 1} io (SDL WithRenderer)
drawBoard scale [] s = pure1 s
drawBoard scale ((_, _, Dead) :: cs) s = drawBoard scale cs s
drawBoard scale ((col, row, Alive) :: cs) s = do
  let rect = MkRect (cast $ row * scale) (cast $ col * scale) (cast scale) (cast scale)
  Success s <- fillRect rect s
    | Failure s err => do putError err
                          pure1 s
  drawBoard scale cs s

data TxtM : Type where

K : Type
K = Ref TxtM (SortedMap String (SDLTexture, SDLRect))

lookupTexture : (HasIO io, K) => String -> io (Maybe (SDLTexture, SDLRect))
lookupTexture x = do
  k <- get TxtM
  pure (lookup x k)

f : LinearIO io => Maybe (SDLTexture, SDLRect) -> (1 _ : SDL WithRenderer) -> L {use = 1} io (SDLErrorPath WithRenderer WithRenderer)
f (Just (a, l)) s = copySurface a l l s
f Nothing s = ?p

withSimpleImage : (LinearIO io, K) => String -> String -> SDLRect -> ((1 _ : SDL WithRenderer) -> L { use = 1} io (SDLErrorPath WithRenderer WithRenderer)) -> (1 _ : SDL WithRenderer) -> L { use = 1 }  io (SDLErrorPath WithRenderer WithRenderer)
withSimpleImage f x r a s = withImage f (\(Texture h) => \s => do
                             modify TxtM (insert x (Texture h, r))
                             a s) s

withImageLookup : (LinearIO m, K) => String -> ((SDLTexture, SDLRect) -> m a) -> m a
withImageLookup x f = do
  b < get TxtM
  let x = lookup x b
  f x

drawGame : (LinearIO io, K)
        => (1 _ : SDL WithRenderer)
        -> L {use = 1} io (SDL WithRenderer)
drawGame x = do
  Success s <- withSimpleImage "logo.png" "foo" (MkRect 0 0 100 100) (\s => do
                 b <- get TxtM
                 let x = lookup "foo" b
                 f x s
                ) x
    | Failure s err => do putError err
                          pure1 s
  pure1 s

eventLoop : (LinearIO io, K)
         => (1 _ : SDL WithRenderer)
         -> L {use = 1} io (SDL WithRenderer)
eventLoop s = do
    now <- liftIO $ clockTime Monotonic
    usleep 100000
    evt <- pollEvent
    s <- drawGame s
    s <- render s
    eventLoop s


defaultWindowOpts : SDLWindowOptions
defaultWindowOpts =
  MkSDLWindowOptions { name = "Example"
                     , x = SDLWindowPosCentered
                     , y = SDLWindowPosCentered
                     , width = 500
                     , height = 500
                     , flags = []
                     }

win : (LinearIO io, K) => L io ()
win = initSDL [SDLInitVideo] (\err => putStrLn "Fatal error: \{show err}") $ \s => do
  putStrLn "=> SDL Inited"

  let winops : SDLWindowOptions = record { flags = [SDLWindowMaximized] } defaultWindowOpts
  Success s <- newWindow winops s
    | Failure s err => handleInitedError s (putError err)
  putStrLn "=> Window created"

  Success s <- newRenderer Nothing [SDLRendererSoftware] s
    | Failure s err => handleWindowedError s (putError err)
  putStrLn "=> Renderer operational"

  s <- drawGame s
  s <- render s
  s <- eventLoop s

  s <- closeRenderer s
  putStrLn "=> Renderer closed"
  s <- closeWindow s
  putStrLn "=> Window closed"
  quitSDL s
  putStrLn "=> SDL quitted"

main : IO ()
main = do
  clock <- clockTime Monotonic
  rnd <- randomGame {radius = 14} -- total dim is (14 + 1) * 2 + 1
  run $ do ref <- newRef TxtM empty
           win
