import qualified Portager.DSLSpec (spec)
import qualified Portager.FlattenSpec (spec)
import qualified Portager.WritesSpec (spec)

main :: IO ()
main = do
  Portager.DSLSpec.spec
  Portager.FlattenSpec.spec
  Portager.WritesSpec.spec
