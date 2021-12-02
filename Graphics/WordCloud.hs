{-# LANGUAGE NoImplicitPrelude #-}
-- | Generate word clouds.
--
-- A word cloud is a a graphical display of 30 to around 150 words.
-- They are generated from weighted data sets of words. Weights are
-- represented with size and/or colour.
-- Del.icio.us, Flickr and Technorati are examples of so-called
-- \"tag-clouds\", and Wordle is a fantastic example of word
-- clouds, by which this module is inspired.

module Graphics.WordCloud
    (-- * Types
     Config(..)
    ,Font(..)
    ,Algorithm(..)
    ,def
     -- * Generating
    ,makeCloud
    ,histogramByFreq
    ,boringWords
     -- * Saving
    ,saveCloud
    )
    where

import Prelude hiding (Word)

import Graphics.GD
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import Data.Ord
import Data.Char
import Control.Monad
import Control.Monad.State.Lazy
import Data.Default

import Graphics.WordCloud.Types

-- | Words that are uninteresting like 'the' and 'a' etc.
boringWords :: [String]
boringWords = words " the a to i it is of that you in and not with\
                    \ for but be if do or on can just what have"

-- | Render a word cloud and save it to a file.
saveCloud :: Config -> Histogram -> IO ()
saveCloud _ _ = return ()

-- | Generates a word cloud Image from a Histogram according to the Config.
--
--   Example usage:
--
-- > import Graphics.GD
-- > import Graphics.WordCloud
-- >
-- > main = do img <- makeCloud def [("foo",3),("bar",2),("mu",1)]
-- >           savePngFile "wordcloud.png" img
--
makeCloud :: Config     -- ^ A configuration detailing how to render the cloud.
          -> Histogram  -- ^ A dataset from which to build a word cloud.
          -> IO Image   -- ^ The rendered Image for manipulation by Graphics.GD.
makeCloud co' histo = flip evalStateT (defCloudSt{cloudConf = co'}) $ do
  canvasSize <- config confCanvasSize
  img <- io $ newImage canvasSize;
  modify (\st -> st { cloudImg = img })
  bg <- config (toGdColor . confBGColor)
  io $ fillImage bg img
  maxWords <- config confMaxWords
  modify (\st -> st { cloudMax = snd $ head histo })
  drawWords (take maxWords histo)
  return img

-- | Generate a Histogram of words according to frequency.
histogramByFreq :: [String]  -- ^ Words to filter out.
                -> String    -- ^ Any string.
                -> Histogram -- ^ Returned histogram.
histogramByFreq badws = list . table where
    table = filterByGood badws . histogram . words . map toLetter
    list = sortBy (flip (comparing snd)) . M.toAscList

toLetter :: Char -> Char
toLetter c | isLetter c = c
           | otherwise  = ' '

-- | Make a histogram of a list.
histogram :: (Ord a, Num n) => [a] -> Map a n
histogram = foldl' (flip $ flip (M.insertWith $ const (+1)) 1) M.empty

-- | Draw the histogram onto the Image.
drawWords :: Histogram -> Cloud ()
drawWords = foldM_ tryDrawWord []

-- | Draw a word onto the Image.
tryDrawWord :: [Rect] -> WordEntry -> Cloud [Rect]
tryDrawWord rs (w,c) = do
  cmax <- gets cloudMax
  defaultPos <- config confDefaultPos
  let ratio = fromIntegral c / fromIntegral cmax
      word = Word w ratio defaultPos
  rect <- liftM regionToRect $ placeOrCalc word
  tryDrawAt rs rect word
  where placeOrCalc | null rs   = drawWord
                    | otherwise = calcWord

-- | Try to place a word somewhere on the canvas.
tryDrawAt :: [Rect] -> Rect -> Word -> Cloud [Rect]
tryDrawAt [] rect _ = return [rect]
tryDrawAt rs rect w = do
  attempt <- tryPlaceWord rs rect
  case attempt of
    Just pt -> do r <- liftM regionToRect $ drawWord (w{wordPoint=pt})
                  return (rs ++ [r])
    Nothing -> return rs

-- | Try to place a word around another word.
tryPlaceWord :: [Rect] -> Rect -> Cloud (Maybe Point)
tryPlaceWord rs s = do
  algo <- config confCloudAlgo
  mapM (aroundRect s rs) rs >>= return . algoToFunc algo rs s

algoToFunc :: Algorithm -> [Rect] -> Rect -> [Maybe Point] -> Maybe Point
algoToFunc Original _  _ = foldr1 mplus
algoToFunc Circular rs s = foldr1 (best (head rs) s)

-- | Return the closest point to c.
best :: Rect -> Rect -> Maybe Point -> Maybe Point -> Maybe Point
best c s r1 r2 | isNothing r1 || isNothing r2 = mplus r1 r2
               | distance c (offset s r1') < distance c (offset s r2') = r1
               | otherwise = r2
               where r1' = fromJust r1
                     r2' = fromJust r2

distance :: Rect -> Rect -> Int
distance c r = abs (x1-x1') + abs (x2-x2') + abs (y1-y1') + abs (y2-y2')
             where ((x1,y1),(x2,y2)) = r
                   ((x1',y1'),(x2',y2')) = c

-- | Try to place a word around a specific word.
aroundRect :: Rect -> [Rect] -> Rect -> Cloud (Maybe Point)
aroundRect s rs r = do
  d <- config confCanvasSize
  return $ foldr1 mplus $ map (tryPlace d) (around r s)
  where tryPlace :: (Int,Int) -> Point -> Maybe Point
        tryPlace d p  | not (tr `isInside` d)  = Nothing
                      | any (isOverlap tr) rs  = Nothing
                      | otherwise              = Just p
                where tr = offset s p

-- | Is a Rect inside a set of dimensions?
isInside :: Rect -> Point -> Bool
isInside r (w,h) | x1 < 0 || x1 > w || x2 > w = False
                 | y1 < 0 || y1 > h || y2 > h = False
                 | y2 < 0                     = False
                 | otherwise                  = True
    where ((x1,y1),(x2,y2)) = r

-- | Get a Rect from a Region.
regionToRect :: Region -> Rect
regionToRect (tl,_,br,_) = (tl,br)

-- | Put a rect set of points at a certain offset.
offset :: Rect -> Point -> Rect
offset r p = ((ox,y1+oy),(x2+ox,y2+oy)) where
    (ox,oy) = p
    ((_,y1),(x2,y2)) = r

-- | Does one Rect overlap another?
isOverlap :: Rect -> Rect -> Bool
isOverlap r r' = x1' < x2 && x2' > x1 && y1' > y2 && y2' < y1
    where ((x1 ,y1 ),(x2 ,y2 )) = r
          ((x1',y1'),(x2',y2')) = r'

-- | All the points around a word at which another word can be placed.
around :: Rect -> Rect -> [Point]
around r s = [(x,y) | x <- [min x1 x2..max x1 x2],
                      y <- [min y1 y2..max y1 y2],
                      x == x1 || x == x2 || y == y1 || y == y2]
    where ((x1',y1'),(x2,y2)) = r
          x1 = x1' - xm2
          y1 = y1' + (ym1 - ym2)
          ((_,ym1),(xm2,ym2)) = s

-- | Draw a word onto the image.
drawWord :: Word -> Cloud Region
drawWord w = do
  fontFamily <- config confFontFamily
  fontSize <- config confFontSize
  min' <- config confFontSizeMin
  img <- gets cloudImg
  color <- colorWord (wordSize w)
  let draw font = io $
          drawString font
                     (fontSizeMod fontSize (wordSize w) min')
                     0
                     (wordPoint w)
                     (wordString w)
                     color
                     img
  case fontFamily of
    FontPath p -> do io_ $ useFontConfig True
                     draw p
    FontName f -> do io_ $ useFontConfig False
                     draw f

-- | Calculate the region that a Word would take up.
calcWord :: Word -> Cloud Region
calcWord w = do
  fontFamily <- config confFontFamily
  fontSize   <- config confFontSize
  min' <- config confFontSizeMin
  let calc font = io $
        measureString font
                      (fontSizeMod fontSize (wordSize w) min')
                      0
                      (0,0)
                      (wordString w)
                      0
  case fontFamily of
    FontPath p -> do io_ $ useFontConfig True
                     calc p
    FontName f -> do io_ $ useFontConfig False
                     calc f

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

-- | Color a word based on size.
colorWord :: Double -> Cloud Color
colorWord n = do
  conf <- config id
  let colors =  confColors conf
      color  = fromMaybe defaultWcColor $ headMay $ colors
  -- Rotate the colors.  `confColors` should be an infinite list.
  modify (\s -> s { cloudConf = conf { confColors = safeTail colors } })
  return . toGdColor $ shadeWcColor n color
  where
-- | Filter only the kind of words we want.
filterByGood :: [String] -> Map String Int -> Map String Int
filterByGood badws = M.filterWithKey (\x _ -> goodWord x) where
    goodWord [_] = False
    goodWord w   = not $ any (==(map toLower w)) badws -- No articles.

-- | Short-hand utility.
io :: (MonadIO m) => IO a -> m a
io = liftIO

-- | Short-hand utility.
io_ :: (MonadIO m) => IO a -> m ()
io_ x = liftIO x >> return ()

fontSizeMod :: Double -> Double -> Double -> Double
fontSizeMod fs ws min' = max (fs*ws) min'
