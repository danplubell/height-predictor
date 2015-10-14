{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Data.Maybe
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX


main :: IO ()
main = start $ do
  frm <- frame [text := "Height Predictor", tabTraversal := True]
  pan <- panel frm []
  mHeightIn  <- entry pan []
  dHeightIn  <- entry pan []
  pmHeight   <- staticText frm []
  pfHeight   <- staticText frm []

  set pan [layout:=margin 10 $ column 10 [ grid 10 10 [[label "Mother's Height in inches:",widget mHeightIn],
                                                       [label "Father's Height in inches",widget dHeightIn],
                                                       [label "Female Predicted Height:" ,widget pfHeight],
                                                       [label "Male Predicted Height:", widget pmHeight]]]]
  focusOn mHeightIn
  
  let networkDescription:: forall t. Frameworks t => Moment t ()      
      networkDescription = do
      bmHeightIn <- behaviorText mHeightIn ""
      bdHeightIn <- behaviorText dHeightIn ""

      sink pfHeight [text :== maybe "--" show <$> (calcHeight ((\x' y' -> (x'+y'-5.118)/2))<$> bdHeightIn <*> bmHeightIn)]
      sink pmHeight [text :== maybe "--" show <$> (calcHeight ((\x' y' -> (x'+y'+5.118)/2))<$> bdHeightIn <*> bmHeightIn)]
      where
        readNumber s = listToMaybe [x | (x,"")<-reads s]  
        calcHeight f d m  = liftA2 f (readNumber d) (readNumber m)
  network <- compile networkDescription
  actuate network

