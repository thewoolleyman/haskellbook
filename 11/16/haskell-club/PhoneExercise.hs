module PhoneExercise where

import Data.List
import Data.Maybe

type Symbol = Char
type Letters = [Char]

data Button
  = Button { symbol :: Symbol, letters :: Letters }

data Input
  = Input Char Int

data DaPhone = DaPhone [Button]

defaultPhone :: DaPhone
defaultPhone =
  DaPhone [ Button '1' ['1']
          , Button '2' ['a', 'b', 'c', '2']
          , Button '3' ['d', 'e', 'f', '3']
          , Button '4' ['g', 'h', 'i', '4']
          , Button '5' ['j', 'k', 'l', '5']
          , Button '6' ['m', 'n', 'o', '6']
          , Button '7' ['p', 'q', 'r', 's', '7']
          , Button '8' ['t', 'u', 'v', '8']
          , Button '9' ['w', 'x', 'y', 'z', '9']
          , Button '*' ['^', '*']
          , Button '0' ['+', '_', '0']
          , Button '#' ['.', ',', '#']
          ]


charToButton :: DaPhone -> Char -> Maybe Button
charToButton (DaPhone buttons) keypress =
  find (\button -> symbol button == keypress) buttons

forwardEncode :: DaPhone -> String -> String
forwardEncode phone keypresses =
  let validKeypresses =
        map (\elem ->
              case elem of
                Just x -> x
                Nothing -> undefined
            ) $
        filter isJust $
          map (\keypress -> charToButton phone keypress) keypresses

      validKeypressToInput :: [Button] -> [Input]
      validKeypressToInput keypresses | [] = []
                                      | x : [] = buttonToInput x : []
                                      | x : y : xs if x == y = buttonToInputs x 2 : []
  in
    foldr () [] validKeypresses

backwardEncode :: DaPhone -> String -> [Input]
backwardEncode phone = undefined

buttonToInput = undefined

