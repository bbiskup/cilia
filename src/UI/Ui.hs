import Data.Monoid((<>))
import Data.Text.Markup((@@))

import qualified Graphics.Vty as V
import qualified Brick.Types as BT
import qualified Brick.Widgets.Core as BWC
import Brick.Markup(markup, (@?))
import Brick.Util (on, fg)
import Brick.Widgets.Core(
      (<+>)
    , (<=>)
    )

ui :: BT.Widget ()
ui = (m1 <=> m2) <+> (BWC.padLeft (BT.Pad 1) m3)
    where
        m1 = markup $ ("Hello1" @@ fg V.blue) <> ", " <> ("world1!" @@ fg V.red)


theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("keyword1",      fg V.yellow)
    , ("keyword2",      V.white `on` V.blue)
    ]
