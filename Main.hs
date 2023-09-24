-- Front-end of point to plane(polar opposite of point) transformation program.
-- in polarity a point in our ordinary space corresponds to a plane.
-- so, plane is our element of geometrical object.

import Data.String
import Data.Text
--import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Time
import System.Environment
import System.IO
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Gdk.DrawWindow
import Graphics.UI.Gtk.Gdk.Drawable
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.Gdk.Pixmap
--import PictContext
-- App
import ProjGeom2D

data Palette = Palette {
  red :: GCValues,
  orange :: GCValues,
  yellow :: GCValues,
  green :: GCValues,
  blue :: GCValues,
  indigo :: GCValues,
  violet :: GCValues
  }

width :: Double
width = 1000.0
height :: Double
height = 1000.0
radius :: Double
radius = sqrt(width^2 + height^2)

main :: IO ()
main = do
  args <- System.Environment.getArgs
  if Prelude.length args /= 8 then
    putStrLn "Usage: projgeom a b c d x y times color(0..6)"
  else
      do
        initGUI

        builder <- builderNew 
        builderAddFromFile builder "Pict.glade"

        window <- builderGetObject builder castToWindow "window1"

        onDestroy window mainQuit

        canvas <- builderGetObject builder castToDrawingArea "drawingarea1"

        widgetShowAll window
        dw <- widgetGetDrawWindow canvas

        pixmap <- pixmapNew (Just dw) 1000 1000 Prelude.Nothing

        onExpose canvas $ \_ -> redraw dw (castToDrawable pixmap)

        saveButton <- builderGetObject builder castToButton "button1"
        let pictType = "conic"
        onClicked saveButton $ savePict (castToDrawable pixmap) pictType args

        closeButton <- builderGetObject builder castToButton "button2"
        onClicked closeButton $ do
          widgetDestroy window

        let a = read (args !! 0) :: Double
        let b = read (args !! 1) :: Double
        let c = read (args !! 2) :: Double
        let d = read (args !! 3) :: Double
        let x = read (args !! 4) :: Double
        let y = read (args !! 5) :: Double
        let times  = read (args !! 6) :: Int
        let startColor = read (args !! 7) :: Int
        let palette = initPalette

        drawPict (castToDrawable pixmap) palette ((a,b),(c,d)) x y times startColor

        mainGUI

initPalette :: Palette
initPalette = Palette {
  red = newGCValues{ foreground = (Color 58339 8995 8738) }, -- 227,38,34
  orange = newGCValues{ foreground = (Color 62965 37265 257) }, -- 245,145,1
  yellow = newGCValues{ foreground = (Color 62708 58853 0) }, -- 244,228,0
  green = newGCValues{ foreground = (Color 0 36494 23387) }, -- 0,142,91
  blue = newGCValues{ foreground = (Color 9766 29041 45746) }, -- 38,113,178
  indigo = newGCValues{ foreground = (Color 17476 20046 39321) }, -- 68,78,153
  violet = newGCValues{ foreground = (Color 28013 14649 35723) } -- 109,57,139
  }

drawPict :: Drawable -> Palette -> Projectivity -> Double -> Double -> Int -> Int -> IO Bool
drawPict drw palette proj x y times startColor =
  do
    let r = 1000.0 * sqrt(2.0)
    let dt = pi/fromIntegral(times)
    let pencils = getPencils proj x y r dt times []
    let (minx,miny,maxx,maxy) = getIncidentRange pencils (0.0,0.0,0.0,0.0)
    let ox = minx - 50
    let oy = miny - 300
    let ratio = width / ((max (maxx-minx) (maxy-miny)) + 100.0)
    drawPencils drw palette startColor pencils ox oy ratio
    return True

drawPencils :: Drawable -> Palette -> Int -> [(Element,Element,Element)] -> Double -> Double -> Double -> IO()
drawPencils drw palette startColor pencils ox oy ratio = do
  drawpencils2 drw palette startColor pencils ox oy ratio
  drawIncidents drw palette startColor pencils ox oy ratio

drawpencils2 :: Drawable -> Palette -> Int -> [(Element,Element,Element)] -> Double -> Double -> Double -> IO()
drawpencils2 _ _ _ [] _ _ _ = do return()
drawpencils2 drw palette color ((l1,l2,_):pencils) ox oy ratio = do
  draw drw palette color l1 ox oy ratio
  draw drw palette color l2 ox oy ratio
  drawpencils2 drw palette (mod (color+1) 7) pencils ox oy ratio

drawIncidents :: Drawable -> Palette -> Int -> [(Element,Element,Element)] -> Double -> Double -> Double -> IO()
drawIncidents _ _ _ [] _ _ _ = do return()
drawIncidents drw palette color ((_,_,p):pencils) ox oy ratio = do
  draw drw palette color p ox oy ratio
  drawIncidents drw palette (mod (color+1) 7) pencils ox oy ratio

getIncidentRange :: [(Element,Element,Element)] -> (Double,Double,Double,Double) -> (Double,Double,Double,Double)
getIncidentRange [] range = range
getIncidentRange ((_,_,Point (_,_,0)):pencils) range = getIncidentRange pencils range
getIncidentRange ((_,_,Point (x,y,z)):pencils) (minx,miny,maxx,maxy) =
  getIncidentRange pencils (minx',miny',maxx',maxy')
  where
    x' = x/z
    y' = y/z
    minx' = min minx x'
    miny' = min miny y'
    maxx' = max maxx x'
    maxy' = max maxy y'

draw :: Drawable -> Palette -> Int -> Element -> Double -> Double -> Double -> IO()
draw _ _ _ Plane _ _ _ = do return ()
draw _ _ _ ProjGeom2D.Nothing _ _ _ = do return ()
draw d palette color (Line (p1,p2,p3)) ox oy ratio
  | p1 Prelude.== 0 && p2 Prelude.== 0 = do return ()
  | p1 Prelude.== 0 = do
      gc <- gcNew d
      setColor gc palette color
      let y0 = round((-1)*p3/p2)
      drawLine d gc (0,y0) (round(width), y0)
      return ()
  | p2 Prelude.== 0 = do
      gc <- gcNew d
      setColor gc palette color
      let x0 = round((-1)*p3/p1)
      drawLine d gc (x0,0) (x0,round(height))
      return ()
  | y1 >= 0 && y1 <= round(height) = do
      gc <- gcNew d
      setColor gc palette color
      drawLine d gc (x1+dx, y1+dy) (x1-dx, y1-dy)
      return ()
  | y2 >= 0 && y2 <= round(height) = do
      gc <- gcNew d
      setColor gc palette color
      drawLine d gc (x2+dx, y2+dy) (x2-dx, y2-dy)
      return ()
  | x3 >= 0 && x3 <= round(width) = do
      gc <- gcNew d
      setColor gc palette color
      drawLine d gc (x3+dx, y3+dy) (x3-dx, y3-dy)
      return ()
  | x4 >= 0 && x4 <= round(width) = do
      gc <- gcNew d
      setColor gc palette color
      drawLine d gc (x4+dx, y4+dy) (x4-dx, y4-dy)
      return ()
  | otherwise = do return ()
  where
    theta = atan ((-1)*p1/p2)
    intoFramex x = round((x - ox)*ratio)
    intoFramey y = round((y - oy)*ratio)
    dx = round(radius*cos(theta))
    dy = round(radius*sin(theta))
    x1 = intoFramex 0.0
    y1 = intoFramey ((-1)*p3/p2)
    x2 = intoFramex (width/ratio)
    y2 = intoFramey ((-1)*(p3+p1*(width/ratio))/p2)
    x3 = intoFramex ((-1)*p3/p1)
    y3 = intoFramey 0.0
    x4 = intoFramex ((-1)*(p3+p2*(height/ratio))/p1)
    y4 = intoFramey (height/ratio)
draw d palette color (Point (x,y,z)) ox oy ratio
  | z Prelude.== 0 = do return()
  | otherwise = do
      gc <- gcNew d
      setColor gc palette color
      drawCircle d gc (round((x/z - ox)*ratio),round((y/z - oy)*ratio)) 5
      return ()

drawCircle :: Drawable -> GC -> Point -> Int -> IO()
drawCircle d gc (x, y) s = do
  drawArc d gc True (x-s) (y-s) (2*s) (2*s) 0 (360*64)

getPencils :: Projectivity -> Double -> Double -> Double -> Double -> Int -> [(Element,Element,Element)] -> [(Element,Element,Element)]
getPencils proj x y r dt times accum
  | times <= 0 = accum
  | otherwise =
    getPencils proj x y r dt (times-1) ((l1,l2,p):accum)
    where
      t = dt*fromIntegral(times)
      p1 = Point (x + r*cos(t), y + r*sin(t), 1)
      p2 = Point (x - r*cos(t), y - r*sin(t), 1)
      l1 = join p1 p2
      l2 = projectiveLine proj l1
      p = incident l1 l2

redraw :: DrawWindow -> Drawable -> IO Bool
redraw dw drawable = 
  do
    gc <- gcNew dw
    drawDrawable dw gc drawable 0 0 0 0 (-1) (-1)
    return True

savePict :: Drawable -> String -> [String] -> IO ()
savePict drawable pictType args =
  do  
    Just pixbuf <- pixbufGetFromDrawable drawable (Rectangle 0 0 999 999)
    timestamp <- getTimestamp
    pixbufSave pixbuf ("./" ++ pictType ++ (toString args) ++ ".png") (Data.Text.pack "png") ([]::[([Char],[Char])])

getTimestamp :: IO [Char]
getTimestamp = formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getZonedTime

setColor :: GC -> Palette -> Int -> IO ()
setColor gc palette ncol =
  do
    let color = asColor palette ncol
    gcSetValues gc color

toString :: [String] -> String
toString [] = ""
toString (a:as) = "_" ++ a ++ (toString as)

asColor :: Palette -> Int -> GCValues
asColor palette n =
  case n of
    0 -> red palette
    1 -> orange palette
    2 -> yellow palette
    3 -> green palette    
    4 -> blue palette
    5 -> indigo palette
    6 -> violet palette
