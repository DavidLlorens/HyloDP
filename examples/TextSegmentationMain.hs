import TextSegmentation
import Data.Map(fromList)

main :: IO ()
main = do
  let dictionary = fromList [ ("a", 0.05)
                          , ("acas", 0.01)
                          , ("as", 0.03)
                          , ("ca", 0.01)
                          , ("cas", 0.05)
                          , ("casaca", 0.05)
                          , ("de", 0.05)
                          , ("e", 0.02)
                          , ("la", 0.12)
                          , ("lacas", 0.05)
                          , ("nada", 0.07)
                          , ("sacad", 0.02)
                          , ("aca", 0.01)
                          , ("ad", 0.01)
                          , ("asa", 0.02)
                          , ("cadena", 0.06)
                          , ("casa", 0.08)
                          , ("da", 0.05)
                          , ("den", 0.02)
                          , ("en", 0.11)
                          , ("laca", 0.05)
                          , ("na", 0.01)
                          , ("saca", 0.05)
                          ]

  putStrLn "For 'ended':"
  print $ ts dictionary "ended"
  print $ tsCount dictionary "ended"

  putStrLn "For 'lacasacadenada':"
  print $ ts dictionary "lacasacadenada"
  print $ tsCount dictionary "lacasacadenada"
  print $ tsAllSolutions dictionary "lacasacadenada"
