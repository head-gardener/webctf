module Data.Task (Task (..), TaskInput (..), Flag (..), mkTasks) where

import Data.Text
import System.CPUTime

data Task a = Task
  { flag :: Flag,
    input :: TaskInput a,
    tip :: Text,
    weigh :: Int
  }

data Flag = TextFlag Text | MonadFlag (IO Text)

data TaskInput a = TextInput Text | LinkInput a | MonadInput (IO Text)

mkTasks :: (Text -> a) -> [Task a]
mkTasks f =
  [ Task
      (TextFlag "flag{hEll0_planetcalc.ru}")
      (TextInput "zfua{bYff0_jfuhynwufw.lo}")
      ( "This one is simple. "
          <> "Notice that the cipher only affects latin characters."
      )
      1,
    Task
      (TextFlag "flag{f1R3w0rK_tW1nKl3s}")
      (TextInput "uyea{r1f3j0fe_ep1ekx3a}")
      "Might take some time."
      2,
    Task
      (TextFlag "FLAG{OMG_YOU_OPEN_IMAGE_IN_HEX_WOW!!!}")
      (LinkInput $ f "photo1.jpg")
      "Hiding arbitrary text in binary files is pretty easy."
      2,
    Task
      (TextFlag "flag{Th1sC0nn3ct10n1sV3ryD@ng3r0u$}")
      (LinkInput $ f "secret.pcapng")
      "Notice file extension."
      2,
    Task
      (TextFlag "flag{XOR_5tr0n9_4nd_51mpl3}")
      (TextInput "b8c1df88a5f5f1bd8198ca9deec387b0eac3dab0eb9cd39fb29ec3")
      "Notice the encoding."
      3,
    Task
      (TextFlag "flag{R1v35t_5h4m1r_4dl3m4n}")
      (LinkInput $ f "keychain.txt")
      "3 letters. You know this one."
      2,
    Task
      (TextFlag "flag{r3pt!l1ans_@re_3vil}")
      (LinkInput $ f "morse.txt")
      ( "Start with the morse. Encryption used might be case-unstable,"
          <> "so convert the flag to lowercase."
      )
      5,
    Task
      (TextFlag "голосования")
      (LinkInput $ f "comms.zip")
      ( "Наши агенты перехватили секретные переговоры межнациональных "
          <> "элит. Расшифруйте запись и ответьте, день чего упоминается "
          <> "в записи. Ответ - одно слово, родительный падеж, строчными буквами."
      )
      4,
    Task
      (TextFlag "373963868372838")
      (LinkInput $ f "db.sqlite3")
      ( "Gregory forgot his password, and luckily we store them unencrypted. "
          <> "Can you help him?"
      )
      2,
    Task
      (TextFlag "223.134.40.75")
      (LinkInput $ f "log.txt")
      ( "We would like to know our customer base. Can you tell us the IP address "
          <> "of our most frequent visitor?"
      )
      3,
    Task
      (TextFlag "24576")
      (LinkInput $ f "charade.jpg")
      "Answer is a number."
      3,
    Task
      (TextFlag "CRYPTO{1m4g3_w1th_h1dd3n_m3554g3}")
      (LinkInput $ f "secret.png")
      "Flag is in form CRYPTO{...}"
      5,
    Task
      (MonadFlag $ fmap (\(a, b) -> pack $ show $ a + b) generateExpression)
      ( MonadInput $
          fmap
            (\(a, b) -> pack (show a) <> " + " <> pack (show b))
            generateExpression
      )
      "Get quick."
      7
  ]

generateExpression :: IO (Int, Int)
generateExpression = do
  time <- fromIntegral <$> getCPUTime
  let a = pseudorand time
  let b = pseudorand a
  return (a, b)

pseudorand :: Int -> Int
pseudorand s = (s * 21357 + 52834) `mod` 2358
