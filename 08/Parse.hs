import           Prelude hiding (mapM_)

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ _ [] = return ()
mapM_ f (x:xs) = do
  _ <- f x
  mapM_ f xs

main :: IO ()
main =
  mapM_ (putStrLn . show) ([1..10] :: [Int])
  --return ()
