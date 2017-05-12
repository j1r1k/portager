import qualified Portager.DSLSpec (spec)
import qualified Portager.FlattenSpec (spec)

main :: IO ()
main = do
  --Portager.DSLSpec.spec
  Portager.FlattenSpec.spec
