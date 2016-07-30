module Print3Flipped where

myGreeting :: String
myGreeting = (++) "Hello" " world!"

hello :: String
hello = "hello"

world :: String
world = "World"

main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where secondGreeting = (++) hello ((++) " " world)
 
