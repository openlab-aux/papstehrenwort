{-# LANGUAGE LambdaCase, MultiParamTypeClasses, FlexibleInstances, InstanceSigs, OverloadedStrings #-}
module Papstehrenwort.I18n
  ( renderMessage
  , Markup(..), FromMarkup(..)
  , Default(..), Lang(..)
  , UIMessages(..)
  ) where

import Protolude
import Data.String (fromString)
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import qualified Text.Blaze.Html5 as Blaze
import Network.HTTP.Media (mapAcceptLanguage)
import Web.HttpApiData (FromHttpApiData(..))

-- | the 'RenderMessage' is used to provide translations for a message types
--
-- The 'master' argument exists so that it is possible to provide more
-- than one set of translations for a 'message' type. This is useful
-- if a library provides a default set of translations, but the user
-- of the library wants to provide a different set of translations.
-- Shamelessly copied from https://hackage.haskell.org/package/shakespeare-2.0.11/docs/Text-Shakespeare-I18N.html
class RenderMessage master message where
  renderMessage :: master  -- ^ type that specifies which set of translations to use
                -> Lang    -- ^ language that should be rendered
                -> message -- ^ message to translate
                -> Markup

instance RenderMessage master Text where
  renderMessage _ _ = T

-- | Intermediate markup type for translated content.
data Markup = M   (NonEmpty Markup) -- ^ combining markup
            | Em  Markup            -- ^ emphasized
            | Str Markup            -- ^ strong
            | T   Text              -- ^ plain text
instance IsString Markup where
  fromString s = T $ toS s
instance Semigroup Markup where
  M ms <> M ms' = M $ ms <> ms'
  M ms <> m     = M $ ms <> (m :| [])
  m    <> M ms  = M $ m  <| ms
  T t  <> T t'  = T $ t  <> t'
  m    <> m'    = M $ m  :| [m']
instance Monoid Markup where
  mappend = (<>)
  mempty  = T mempty

class FromMarkup a where
  fromMarkup :: Markup -> a

-- TODO escape HTML in strings
instance FromMarkup Blaze.Html where
  fromMarkup = \case
    (M  ms) -> mconcat . NE.toList $ fmap fromMarkup ms
    (T   t) -> Blaze.toHtml   t
    (Em  m) -> Blaze.em     $ fromMarkup m
    (Str m) -> Blaze.strong $ fromMarkup m

data Default = Default
data Lang = EN | DE

instance FromHttpApiData Lang where
  parseHeader :: ByteString -> Either Text Lang
  parseHeader bs = note ("No Lang for '" <> toS bs <> "'.")
    $ mapAcceptLanguage [ ("de", DE)
                        , ("en", EN) ] bs
  parseUrlPiece = parseHeader . toS

data UIMessages = Title
                | Tagline
                | Introduction
                | MailAddress
                | DisplayName
                | TaskTitle
                | TaskDescription
                | TaskUrl
                | TaskNextOccur
                | CommitButton

instance RenderMessage Default UIMessages where
  renderMessage _ EN = \case
    Title           -> "Papstehrenwort"
    Tagline         -> "If you don’t do it, who will?"
    Introduction    -> "Below you see a list of all "
                       <> Str "tasks that should be done regularly" <> ". "
                       <> "If you want to help, simply fill in your details, "
                       <> Str "check those you want to do"
                       <> " and click “" <> renderMessage Default EN CommitButton <> "”!"
    MailAddress     -> "Your Mail Address:"
    DisplayName     -> "Your Display Name:"
    TaskTitle       -> "Title"
    TaskDescription -> "Description"
    TaskUrl         -> "Url"
    TaskNextOccur   -> "Next"
    CommitButton    -> "Commit"

  renderMessage _ DE = \case
    Title           -> "Papstehrenwort"
    Tagline         -> "Wenn du es nicht tust, wer dann?"
    Introduction    -> "Hier siehst du die Liste "
                       <> Str "aller Aufgaben, die regelmäßig erledigt werden müssen" <> ". "
                       <> "Wenn du helfen willst, gib deine persönlichen Daten an und  "
                       <> Str "markiere die Aufgaben, die du erledigen willst"
                       <> " und drücke “" <> renderMessage Default DE CommitButton <> "”!"
    MailAddress     -> "Deine Mailadresse:"
    DisplayName     -> "Dein Anzeigename:"
    TaskTitle       -> "Titel"
    TaskDescription -> "Beschreibung"
    TaskUrl         -> "Url"
    TaskNextOccur   -> "Nächstes Mal"
    CommitButton    -> "Abschicken"
