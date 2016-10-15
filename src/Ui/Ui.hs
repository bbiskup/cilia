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
        m2 = markup $ ("Hello2" @? "keyword1") <> ", " <> ("world2!" @? "keyword2")
        m3 = markup $ ("Hello3," @? "keyword1") <> "\n" <> ("world3!" @? "keyword2")