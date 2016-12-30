{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Writers.ElmHtml ( writeElmHtmlString ) where

import qualified Data.ByteString          as S
import qualified Data.ByteString.Char8    as SBC
import           Data.List                (isInfixOf)
import qualified Data.Text                as T
import           Text.Blaze.Html          (Html)
import           Text.Blaze.Internal
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.Writers.HTML (writeHtml)

-- | Conver Pandoc document to Elm-Html String.
writeElmHtmlString :: WriterOptions -> Pandoc -> String
writeElmHtmlString opts d =
  let html = writeHtml opts{ writerHtml5 = True } d
  in  htmlToElm html

htmlToElm :: Html -> String
htmlToElm html =
  renderElm html ""

renderElm :: Markup -> String -> String
renderElm = go id
  where
    go :: (String -> String) -> MarkupM b -> String -> String
    go attrs (Parent tag _ _ content) =
      ("node \"" ++) . getString tag . ("\" ( " ++) . attrs .
      ("[] ) ( " ++) . go id content . ("[] )" ++)
    go attrs (CustomParent tag content) =
      ("node \"" ++) . fromChoiceString tag . ("\" ( " ++) . attrs .
      ("[] ) ( " ++) . go id content . ("[] )" ++)
    go attrs (Leaf tag _ _) =
      getString tag . (" [" ++) . attrs . ("] []" ++)
    go attrs (CustomLeaf tag _) =
      fromChoiceString tag . ('[' :) . attrs . ("] []" ++)
    go attrs (AddAttribute rawKey _ value h) =
      flip go h $
        ("(attribute \"" ++) . getString rawKey . ("\" \"" ++) .
        fromChoiceString value . ("\") :: " ++) . attrs
    go attrs (AddCustomAttribute key value h) =
      flip go h $
        ("(attribute \"" ++) . fromChoiceString key . ("\" \"" ++) .
        fromChoiceString value . ("\") :: " ++) . attrs
    go _ (Content content) =
      ("text \"" ++) . fromChoiceString content . ('\"' :)
    go _ (Comment comment) =
      id
    go attrs (Append h1 h2) =
      go attrs h1 . (" :: " ++) . go attrs h2
    go _ Empty =
      id
    {-# NOINLINE go #-}

-- | Escape predefined XML entities in a string
--
escapeMarkupEntities :: String  -- ^ String to escape
                   -> String  -- ^ String to append
                   -> String  -- ^ Resulting string
escapeMarkupEntities []     k = k
escapeMarkupEntities (c:cs) k = case c of
    '<'  -> '&' : 'l' : 't' : ';'             : escapeMarkupEntities cs k
    '>'  -> '&' : 'g' : 't' : ';'             : escapeMarkupEntities cs k
    '&'  -> '&' : 'a' : 'm' : 'p' : ';'       : escapeMarkupEntities cs k
    '"'  -> '&' : 'q' : 'u' : 'o' : 't' : ';' : escapeMarkupEntities cs k
    '\'' -> '&' : '#' : '3' : '9' : ';'       : escapeMarkupEntities cs k
    x    -> x                                 : escapeMarkupEntities cs k

-- | Render a 'ChoiceString'.
--
fromChoiceString :: ChoiceString  -- ^ String to render
                 -> String        -- ^ String to append
                 -> String        -- ^ Resulting string
fromChoiceString (Static s)     = ("Static " ++) . getString s
fromChoiceString (String s)     = ("String " ++) . escapeMarkupEntities s
fromChoiceString (Text s)       = escapeMarkupEntities $ ("Text " ++ T.unpack s)
fromChoiceString (ByteString s) = ("ByteString " ++) . (SBC.unpack s ++)
fromChoiceString (PreEscaped x) = ("PreEscaped " ++) . case x of
    String s -> (s ++)
    Text   s -> (\k -> T.foldr (:) k s)
    s        -> fromChoiceString s
fromChoiceString (External x) = ("External " ++) . case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then id else (s ++)
    Text   s     -> if "</" `T.isInfixOf` s then id else (\k -> T.foldr (:) k s)
    ByteString s -> if "</" `S.isInfixOf` s then id else (SBC.unpack s ++)
    s            -> fromChoiceString s
fromChoiceString (AppendChoiceString x y) =
    fromChoiceString x . fromChoiceString y
fromChoiceString EmptyChoiceString = id
{-# INLINE fromChoiceString #-}
