module Hp.PollQuestion
  ( PollQuestion(..)
  , isPollQuestionValid
  ) where

import Data.Char (isSpace)

import qualified Data.Text as Text


-- | A single question in a poll.
data PollQuestion
  = CheckboxQuestion Text [Text]
  -- | DropdownQuestion Text [Text]
  -- | NumberQuestion Text Int
  -- | RadioQuestion Text [Text]
  -- | TextQuestion Text Bool Text -- Bool means: is text area?
  deriving stock (Generic, Show)

-- | Is this question valid? (disallow empty strings, etc)
isPollQuestionValid :: PollQuestion -> Bool
isPollQuestionValid = \case
  CheckboxQuestion header choices ->
    and
      [ sensible header
      , not (null choices)
      , all sensible choices
      ]

  where
    -- Not null, no leading or trailing whitespace
    sensible :: Text -> Bool
    sensible s =
      and
        [ not (Text.null s)
        , not (isSpace (s ^?! _head))
        , not (isSpace (s ^?! _last))
        ]
