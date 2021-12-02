{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
module Graphics.WordCloud.Types where

import Graphics.GD
import Data.Default
import Control.Monad.State.Lazy
import Data.Typeable
import Data.Generics
import GHC.Word (Word8)

data Word = Word {
      wordString :: String
    , wordSize   :: Double
    , wordPoint  :: Point }
type Rect = ((Int,Int),(Int,Int))
type Region = (Point, Point, Point, Point)
type Histogram = [WordEntry]
type WordEntry = (String,Int)

-- | Which font to use.
data Font = FontPath FilePath -- ^ Specify by filepath.
          | FontName String   -- ^ Specify by FontConfig name.
            deriving (Data,Typeable)

-- | Configuration for a word cloud.
--
-- Default configuration:
--
--   * confFontFamily = FontName \"URW Bookman L\"
--
--   * confFontSize   = 72
--
--   * confMaxWords   = 100
--
--   * confCanvasSize = (800,600)
--
--   * confDefaultPos = (400,400)
--
--   * confColors     = [red, gree, blue]
--
--   * confBGColor    = ivory
--
--   * confCloudAlgo  = Original
data Config = Config {
      confFontFamily  :: Font       -- ^ The font to use.
    , confFontSize    :: Double     -- ^ Largest font size on which to base the other sizes.
    , confFontSizeMin :: Double
    , confMaxWords    :: Int        -- ^ Maximum words to draw in the cloud.
    , confCanvasSize  :: (Int,Int)  -- ^ Canvas (image) size.
    , confDefaultPos  :: (Int,Int)  -- ^ Specify a default position for the largest word.
    , confColors      :: [WcColor]  -- ^ The colours to use for the cloud words.
    , confBGColor     :: WcColor    -- ^ The background colour.
    , confCloudAlgo   :: Algorithm  -- ^ The algorithm to use for rendering the cloud.
    }-- deriving (Data,Typeable)

instance Default Config where
    def = Config {
            confFontFamily  = FontName "URW Bookman L"
          , confFontSize    = 60
          , confMaxWords    = 70
          , confFontSizeMin = 10
          , confCanvasSize  = (800,600)
          , confDefaultPos  = (400,400)
          , confColors      = defaultWcColors
          , confBGColor     = ivory
          , confCloudAlgo   = Original }

-- | The algorithm to use for the cloud generation.
data Algorithm
  = Original  -- ^ Generates a mostly horizontal cloud.
  | Circular  -- ^ Generates a mostly circular cloud.
--    deriving (Data,Typeable)

-- Apply a function to the config; handy function.
config :: (Config -> a) -> Cloud a
config f = gets (f . cloudConf)

--deriving instance Data Image

-- Cloud creation monad
data CloudSt = CloudSt { cloudImg   :: Image
                       , cloudMax   :: Int
                       , cloudConf  :: Config }
  --             deriving (Data,Typeable)
type Cloud a = StateT CloudSt IO a

-- Default state
defCloudSt :: CloudSt
defCloudSt = CloudSt { cloudConf = undefined
                     , cloudMax  = undefined
                     , cloudImg  = undefined }

-- Colors

data WcColor = RGB Word8 Word8 Word8 | RGBA Word8 Word8 Word8 Word8
  deriving (Eq, Show)

toGdColor :: WcColor -> Color
toGdColor (RGB r g b) = rgb (fromEnum r) (fromEnum g) (fromEnum b)
toGdColor (RGBA r g b a) = rgba (fromEnum r) (fromEnum g) (fromEnum b) (fromEnum a)

shadeWcColor :: Double -> WcColor -> WcColor
shadeWcColor n c = case c of
  RGB r g b -> RGB (color r) (color g) (color b)
  RGBA r g b a -> RGBA (color r) (color g) (color b) a
  where color = mm . floor . (*n) . fromIntegral
        mm = max 20 . min 255


-- Common Colors

defaultWcColor :: WcColor
defaultWcColor = black

defaultWcColors :: [WcColor]
defaultWcColors = cycle [red, green, blue]

red :: WcColor
red = RGB 0xff 0 0

green :: WcColor
green = RGB 0 0xff 0

blue :: WcColor
blue = RGB 0 0 0xff

pink :: WcColor
pink = RGB 0xff 0xc0 0xcb

raspberry :: WcColor
raspberry = RGB 0x86 0x26 0x57

orchid :: WcColor
orchid = RGB 0xda 0x70 0xd6

darkOrchid :: WcColor
darkOrchid = RGB 0x99 0x32 0xcc

violet :: WcColor
violet = RGB 0x94 0x00 0xd3

indigo :: WcColor
indigo = RGB 0x4b 0x00 0x82

navyBlue :: WcColor
navyBlue = RGB 0x00 0x00 0x80

royalBlue :: WcColor
royalBlue = RGB 0x41 0x69 0xe1

turqoise :: WcColor
turqoise = RGB 0x00 0xf5 0xff

springGreen :: WcColor
springGreen = RGB 0x00 0xee 0x76

paleGreen :: WcColor
paleGreen = RGB 0x98 0xfb 0x98

ivory :: WcColor
ivory = RGB 0xff 0xff 0xf0

yellow :: WcColor
yellow = RGB 0xff 0xff 0x00

khaki :: WcColor
khaki = RGB 0xcd 0xc6 073

goldenrod :: WcColor
goldenrod = RGB 0xff 0xec 0x8b

gold :: WcColor
gold = RGB 0xff 0xd7 0x00

brick :: WcColor
brick = RGB 0x9c 0x66 0x1f

black :: WcColor
black = RGB 0 0 0

white :: WcColor
white = RGB 0xff 0xff 0xff

orange :: WcColor
orange = RGB 0xff 0x80 0x00

chocolate :: WcColor
chocolate = RGB 0x8b 0x45 0x13

brown :: WcColor
brown = RGB 0xa5 0x2a 0x2a
