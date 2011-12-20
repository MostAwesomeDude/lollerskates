import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withLollerSite)
import Prelude              (IO)

main :: IO ()
main = defaultMain fromArgs withLollerSite