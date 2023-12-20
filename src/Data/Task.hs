module Data.Task (Task (..), TaskInput (..), mkTasks) where

import Data.Text

data Task a = Task {flag :: Text, input :: TaskInput a, weigh :: Int}

data TaskInput a = TextInput Text | LinkInput a

mkTasks :: (Text -> a) -> [Task a]
mkTasks _ =
  [ Task
      "flag{hEll0_planetcalc.ru}"
      (TextInput "zfua{bYff0_jfuhynwufw.lo}")
      1,
    Task
      "flag{f1R3w0rK_tW1nKl3s}"
      (TextInput "uyea{r1f3j0fe_ep1ekx3a}")
      2
  ]
