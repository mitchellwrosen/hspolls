module Hp.PollQuestion
  ( PollQuestion(..)
  ) where


-- | A single question in a poll.
data PollQuestion
  = CheckboxQuestion Text [Text]
  -- | DropdownQuestion Text [Text]
  -- | NumberQuestion Text Int
  -- | RadioQuestion Text [Text]
  -- | TextQuestion Text Bool Text -- Bool means: is text area?
  deriving stock (Generic, Show)
