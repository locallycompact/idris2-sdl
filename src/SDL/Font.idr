module SDL.Font

import Control.Linear.LIO

import public SDL.Types
import public SDL.Font.Types
import SDL.Font.Foreign

%default total

public export
data SDLTTFState : Type where
  Inited : SDLTTFState

public export
data SDLTTF : SDLTTFState -> Type where
  Initial : SDLTTF Inited

public export
data SDLTTFErrorPath : (ok : SDLTTFState) -> (err : SDLTTFState) -> Type where
  Success : (1 _ : SDLTTF ok) -> SDLTTFErrorPath ok err
  Failure : (1 _ : SDLTTF err) -> SDLFontError -> SDLTTFErrorPath ok err

export
initSDLTTF : LinearIO io => (onerr : SDLFontError -> L io ret) -> (onok : (1 _ : SDLTTF Inited) -> L io ret) -> L io ret
initSDLTTF onerr onok = case !(init) of
  Left err => onerr err
  Right () => onok Initial

export
withFont : LinearIO io
        => (path : String)
        -> (pt   : Int)
        -> (handler : (1 _ : SDLFont) -> (1 _ : SDLTTF Inited) -> L io { use = 1 } (SDLTTFErrorPath Inited Inited))
        -> (1 _ : SDLTTF Inited)
        -> L io {use = 1} (SDLTTFErrorPath Inited Inited)
withFont path pt handler Initial = case !(openFont path pt) of
  Left err => pure1 $ Failure Initial err
  Right fnt => handler fnt Initial

export
quitSDLTTF : LinearIO io => (1 _ : SDLTTF Inited) -> L io ()
quitSDLTTF Initial = quit

export
handleInitedError : LinearIO io => (1 _ : SDLTTF Inited) -> (1 fn : L io a) -> L io a
handleInitedError s fn = do quitSDLTTF s
                            fn
