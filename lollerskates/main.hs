import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withLollerWeb)
import Prelude              (IO)

main :: IO ()
main = defaultMain fromArgs withLollerWeb