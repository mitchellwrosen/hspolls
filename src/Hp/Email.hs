module Hp.Email
  ( Email(..)
  , PersonalEmail(..)
  , TransactionalEmail(..)
  ) where

-- | An email.
data Email
  = EmailPersonal PersonalEmail
  | EmailTransactional TransactionalEmail

-- | An email to one person.
data PersonalEmail
  = PersonalEmail
  { body :: Text
  , from :: Text
  , subject :: Text
  , to :: Text
  } deriving stock (Generic)

-- | An email to a bunch of people (using BCC).
data TransactionalEmail
  = TransactionalEmail
  { bcc :: [Text]
  , body :: Text
  , from :: Text
  , subject :: Text
  } deriving stock (Generic)
