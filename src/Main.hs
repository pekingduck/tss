{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad             (void, when)
import           Data.Bifunctor            (first)
import           Data.List                 (find)
import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Maybe                (fromJust, isJust)
import qualified Data.Text                 as T
import           Graphics.UI.Pashua
import           Graphics.UI.Pashua.Parser
import           System.Environment        (getArgs)
import           Text.Read                 (readEither)

data WidgetID
  = Vertical | HrTSS | Weight
  | Strength | Duration
  | OK | Cancel
  deriving (Show, Eq)

data StrengthType = General | Max deriving (Show, Eq, Enum, Read)

main :: IO ()
main = do
  getArgs >>=
    \case
      [ "hike" ]     -> mainHike
      [ "strength" ] -> mainStrength
      _              -> putStrLn "Invalid command"

mainStrength :: IO ()
mainStrength = do
  let radioList = mkOptionListFromEnum (Just General)
      form :: Maybe (Form WidgetID)
      form = mkForm Nothing
             [ radioButton Strength radioList
             , (defaultButton OK) { label_ = Just "Calculate" }
             , (textField Duration) { label_ = Just "Duration in minutes"
                                    , mandatory = Just True }
             , cancelButton Cancel
             ]

  case form of
    Nothing -> putStrLn "Invalid form (stength)"
    Just form' -> do
      result <- runPashua form'
      let
        parseStrengthType = mkEnumParser ("Invalid Strength type: " <>)
        x :: Either (Err WidgetID) Float
        x = do
          t <- parseStrengthType Strength result
          d <- parseFloat Duration result
          case t of
            General -> pure $ d * 70.0 / 60.0
            Max     -> pure $ d * 80.0 / 60.0
      case x of
        Left FormCancelled -> pure ()
        Left (ParseError w s) ->
          simpleMessage "Error: " $ T.pack $ show w <> " - " <> s
        Right v -> simpleMessage "TSS" $ "TSS is " <> T.pack (show v)

mainHike :: IO ()
mainHike = do
  let form :: Maybe (Form WidgetID)
      form = mkForm Nothing
             [ (textField Vertical) { mandatory = Just True
                                      , label_ = Just "Elevation in m" }
             , (textField HrTSS) { mandatory = Just True
                                   , label_ = Just "HrTSS" }
             , checkbox Weight "With 10%+ BW"
             , (defaultButton OK) { label_ = Just "Calculate" }
             , cancelButton Cancel
             ]

  case form of
    Nothing -> putStrLn "Invalid form (hike)"
    Just form' -> do
      result <- runPashua form'
      let
        x :: Either (Err WidgetID) Float
        x = do
          v <- parseFloat Vertical result
          h <- parseFloat HrTSS result
          w <- parseBool Weight result
          pure $ if w
            then h + v * 20.0 / 304.8
            else h + v * 10.0 / 304.8
      case x of
        Left FormCancelled -> pure ()
        Left (ParseError w s) ->
          simpleMessage "Error: " $ T.pack $ show w <> " - " <> s
        Right v  -> simpleMessage "TSS" $ "Adjusted TSS is " <> T.pack (show v)
