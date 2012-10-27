
import Criterion.Main
import Text.Parsec

import Data.Dates
import Data.Dates.Formats

formatStr = "YYYY/MM/DD"

main = do
  format <- case runParser pFormat () formatStr formatStr of
              Left err -> fail (show err)
              Right f -> return f
  let parser = formatParser format
      parse str = case runParser parser () str str of
                    Left err -> error (show err)
                    Right x -> x
  defaultMain [
      bench "parser" $ whnf parse "2012/09/12"
    ]
