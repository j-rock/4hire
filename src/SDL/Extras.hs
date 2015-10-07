{-# LANGUAGE RecordWildCards #-}

module SDL.Extras
    ( ScreenInfo(..)
    , SDLContext(..)
    , MilliDelta
    , InitialState
    , FinishedPlaying(..)
    , IterateStateFunc
    , runWithSDLContext
    ) where

import Data.Word (Word32)
import           Control.Monad          (unless, when)
import qualified Linear                 as L
import           SDL                    (($=))
import qualified SDL
import qualified Foreign.C.Types        as Foreign
import qualified Data.Text              as Text

data ScreenInfo = ScreenInfo { width  :: Foreign.CInt
                             , height :: Foreign.CInt
                             , title  :: Text.Text
                             }

data SDLContext = SDLContext { renderer :: SDL.Renderer
                             , window   :: SDL.Window
                             , events   :: [SDL.Event]
                             }

type MilliDelta = Word32
type InitialState state = state
data FinishedPlaying = StillPlaying
                     | DonePlaying
                       deriving (Eq, Ord, Show, Enum, Bounded)


type IterateStateFunc m state = SDLContext -> MilliDelta -> state -> m (state, FinishedPlaying)


runWithSDLContext :: ScreenInfo -> InitialState a -> IterateStateFunc IO a -> IO ()
runWithSDLContext ScreenInfo{..} initialState iterator =
    let initialize = SDL.initialize [SDL.InitVideo]

        setupRenderQuality = do SDL.HintRenderScaleQuality $= SDL.ScaleLinear
                                renderQuality <- SDL.get SDL.HintRenderScaleQuality
                                when (renderQuality /= SDL.ScaleLinear) $
                                    putStrLn "Warning: Linear texture filtering not enabled"

        winArg = SDL.defaultWindow {SDL.windowInitialSize = L.V2 width height}
        createWindow = do win <- SDL.createWindow title winArg
                          SDL.showWindow win
                          pure win

        renConf = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedRenderer
                                     , SDL.rendererTargetTexture = False
                                     }
        createRenderer win = SDL.createRenderer win (-1) renConf

    in do initialize
          setupRenderQuality
          window   <- createWindow
          renderer <- createRenderer window
          SDL.rendererDrawColor renderer $= black
          loopOn initialState iterator $ SDLContext renderer window []
          SDL.destroyRenderer renderer
          SDL.destroyWindow window
          SDL.quit
    where black = let m = minBound in L.V4 m m m m


loopOn :: InitialState state -> IterateStateFunc IO state -> SDLContext -> IO ()
loopOn initial f context = iterateWithTime initial $ \dt a' ->
                                  do events <- collectEvents
                                     SDL.clear (renderer context)
                                     (a'', done) <- f context{events = events} dt a'
                                     SDL.present (renderer context)
                                     pure (a'', done ||: quit events)
    where quit = elem SDL.QuitEvent . map SDL.eventPayload

          collectEvents = do ev <- SDL.pollEvent
                             maybe (return []) (\e -> (e:) <$> collectEvents) ev

          (||:) :: FinishedPlaying -> Bool -> FinishedPlaying
          DonePlaying ||: _    = DonePlaying
          _           ||: True = DonePlaying
          _           ||: _    = StillPlaying


iterateWithTime :: InitialState state -> (MilliDelta -> state -> IO (state, FinishedPlaying)) -> IO ()
iterateWithTime initial f = go initial 0 =<< SDL.ticks
  where go a dt t = do (a',stop) <- f dt a
                       keepPlaying stop $ SDL.ticks >>= \t' -> go a' (t' - t) t'
        keepPlaying x = unless (x == DonePlaying)
