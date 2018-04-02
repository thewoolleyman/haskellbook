module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness = 
    if cool then putStrLn "Cool" else putStrLn "not cool"
    where cool = coolness == "downright frosty yo"

