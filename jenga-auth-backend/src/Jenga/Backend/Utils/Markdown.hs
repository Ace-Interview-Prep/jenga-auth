-- | Markdown.hs

module Jenga.Backend.Utils.Markdown where --(runMmarkWithInput) where

import qualified Data.Text as T
import Data.Bifunctor
import Lucid.Base (Html, renderText)
import Text.MMark as MMark
import Text.MMark.Extension.GhcSyntaxHighlighter
import Text.MMark.Extension.LinkTarget
import Text.MMark.Extension.Skylighting
import qualified Text.Megaparsec.Error as MP
import qualified Data.Text.Lazy as LT

runMMarkLucid :: T.Text -> Either (MP.ParseErrorBundle T.Text MMarkErr) (Html ())
runMMarkLucid = fmap (MMark.render . useExtensions extensions) . MMark.parse "markdown message"

runMMark :: T.Text -> Either String T.Text
runMMark = bimap (MP.errorBundlePretty) (LT.toStrict . renderText) . runMMarkLucid

runMMarkDefaulting :: T.Text -> T.Text
runMMarkDefaulting content = either (const content) (id) (runMMark content)

-- | If we are running it line by line then we dont care
-- | why it fails
runMMarkLines :: T.Text -> [T.Text]
runMMarkLines = fmap runMMarkDefaulting . reMergeCodeBlocks . T.split (== '\n')

reMergeCodeBlocks :: [T.Text] -> [T.Text]
reMergeCodeBlocks [] = []
reMergeCodeBlocks (block:blocks) =
  if not (T.isPrefixOf "```" block)
  then block : reMergeCodeBlocks blocks
  else
    let
      (consumed,terminator:left) = span (not . T.isPrefixOf "```") blocks

      consumed' = removeTrailingSpaces <$> consumed
      newBlock = block : consumed' <> [terminator]
    in (T.intercalate (T.singleton '\n') newBlock) : reMergeCodeBlocks left

removeTrailingSpaces :: T.Text -> T.Text
removeTrailingSpaces = T.reverse . T.dropWhile (== ' ') . T.reverse

extensions :: [MMark.Extension]
extensions =
  [ ghcSyntaxHighlighter
  , linkTarget
  , skylighting
  ]

runMmarkWithInput :: T.Text -> Either String T.Text
runMmarkWithInput = runMMark
