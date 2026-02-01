-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Monad               (replicateM_)
import qualified Data.Map.Strict as M
-----------------------------------------------------------------------------
import           Miso
import           Miso.Lens
import           Miso.Html
import           Miso.Html.Property
import           Miso.Canvas
import qualified Miso.Canvas as Canvas
import           Miso.String hiding (count)
import qualified Miso.Svg as S
import qualified Miso.CSS as CSS
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
data Model = Model
  { _time   :: (Double, Double)
  , _count  :: Int
  , _loaded :: Int
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
data Action
  = GetTime
  | SetTime (Double, Double)
  | Add
  | Remove
  | Loaded
-----------------------------------------------------------------------------
baseUrl :: MisoString
baseUrl = "https://7b40c187-5088-4a99-9118-37d20a2f875e.mdnplay.dev/en-US/docs/Web/API/Canvas_API/Tutorial/Basic_animations/"
-----------------------------------------------------------------------------
main :: IO ()
main = startApp events app
  where
    events :: Events
    events = M.insert "load" CAPTURE defaultEvents

    app :: App Model Action
    app = (component (Model (0.0, 0.0) 1 0) updateModel viewModel)
      { mount = Just GetTime
      }

    viewModel Model { _time = m, _count = k, _loaded = x } =
      div_
      [ id_ "Canvas grid" ]
      $
      [ img_ [ CSS.style_ [ CSS.display "none" ]
             , id_ "sun"
             , src_ (baseUrl <> "canvas_sun.png")
             , onLoad Loaded
             ]
      , img_ [ CSS.style_ [ CSS.display "none" ]
             , id_ "moon"
             , src_ (baseUrl <> "canvas_moon.png")
             , onLoad Loaded
             ]
      , img_ [ CSS.style_ [ CSS.display "none" ]
             , id_ "earth"
             , src_ (baseUrl <> "canvas_earth.png")
             , onLoad Loaded
             ]
      , h1_
        [ CSS.style_
          [ CSS.fontFamily "monospace"
          ]
        ]
        [ "🍜 "
        , a_ [ href_ "https://github.com/haskell-miso/miso-canvas2d" ] [ "miso-canvas2d" ]
        ]
      , div_
        [ ]
        [ button_
          [ CSS.style_
            [ CSS.fontSize (CSS.px 26)
            , CSS.margin "5px"
            ]
          , onClick Add
          ]
          [ "Add" ]
        , button_
          [ CSS.style_
            [ CSS.fontSize (CSS.px 26), CSS.margin "5px"
            ]
          , onClick Remove
          ]
          [ "Remove" ]
        ]
      ] ++
      if x == 3 -- dmj: 3 img loaded
      then
        [ Canvas.canvas
          [ width_ "300"
          , height_ "300"
          ]
          initCanvas
          (canvasDraw m n)
        | n <- [ 1 :: Int .. k ]
        ]
      else
        [ "Loading..." ]
-----------------------------------------------------------------------------
initCanvas :: DOMRef -> Canvas (Image, Image, Image)
initCanvas _ = liftIO $ do
  sun <- newImage (baseUrl <> "canvas_sun.png")
  moon <- newImage (baseUrl <> "canvas_moon.png")
  earth <- newImage (baseUrl <> "canvas_earth.png")
  pure (sun, moon, earth)
-----------------------------------------------------------------------------
canvasDraw
  :: (Double, Double)
  -> Int
  -> (Image, Image, Image)
  -> Canvas ()
canvasDraw (millis', secs') n (sun, moon, earth) = do
   let
     secs = secs' + fromIntegral n
     millis = millis' + fromIntegral n
   globalCompositeOperation DestinationOver
   clearRect (0,0,300,300)
   fillStyle $ Canvas.color (CSS.rgba 0 0 0 0.6)
   strokeStyle $ Canvas.color (CSS.rgba 0 153 255 0.4)
   save ()
   translate (150, 150)
   rotate ((((2 * pi) / 60) * secs) + (((2 * pi) / 60000) * millis))
   translate (105,0)
   fillRect (0 ,-12, 50, 24)
   drawImage (earth, -12, -12)
   save ()
   rotate ((((2 * pi) / 6) * secs) + (((2 * pi) / 6000) * millis))
   translate (0,28.5)
   drawImage (moon, -3.5, -3.5)
   replicateM_ 2 (restore ())
   beginPath ()
   arc (150, 150, 105, 0, pi * 2)
   stroke ()
   drawImage' (sun, 0, 0, 300, 300)
-----------------------------------------------------------------------------
newTime :: IO (Double, Double)
newTime = liftIO $ do
  date <- newDate
  (,) <$> getMilliseconds date <*> getSeconds date
-----------------------------------------------------------------------------
updateModel
  :: Action
  -> Transition Model Action
updateModel = \case
  GetTime ->
    io (SetTime <$> newTime)
  SetTime m -> do
    time .= m
    issue GetTime
  Add -> count += 1
  Remove -> count %= \x -> if x == 0 then 0 else x - 1
  Loaded -> loaded += 1
-----------------------------------------------------------------------------
time = lens _time (\m k -> m { _time = k })
-----------------------------------------------------------------------------
count = lens _count (\m k -> m { _count = k })
-----------------------------------------------------------------------------
loaded = lens _loaded (\m k -> m { _loaded = k })
-----------------------------------------------------------------------------
