import ChoreWheel

import System.Process

main :: IO ()
main = do
  callCommand "notify-send 'chorewheel loaded' -t 1500"
  runApp
