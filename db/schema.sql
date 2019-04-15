CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE users (
  id
    uuid
    DEFAULT uuid_generate_v4(),

  email
    text,

  -- The user's GitHub username, or NULL if they haven't authenticated with
  -- GitHub. Once set, may be overwritten if the user decides to authenticate
  -- as a different GitHub user (we only want to track/display one).
  github
    text,

  -- Send the user an email when a new poll is created?
  subscribed_to_poll_created
    boolean
    DEFAULT false,

  PRIMARY KEY (id),
  UNIQUE (email),
  UNIQUE (github)
);

CREATE TABLE polls (
  id
    uuid
    DEFAULT uuid_generate_v4(),

  created_at
    timestamp with time zone
    NOT NULL
    DEFAULT current_timestamp,

  duration
    interval
    NOT NULL,

  form
    jsonb
    NOT NULL,

  userId
    uuid,

  FOREIGN KEY (userId) REFERENCES users (id),
  PRIMARY KEY (id)
);

-- TODO track some identiy of responder, to kinda sorta prevent duplicate voting
CREATE TABLE poll_responses (
  id
    uuid
    DEFAULT uuid_generate_v4(),

  created_at
    timestamp with time zone
    NOT NULL
    DEFAULT current_timestamp,

  pollId
    uuid
    NOT NULL,

  response
    jsonb
    NOT NULL,

  userId
    uuid,

  FOREIGN KEY (pollId) REFERENCES polls (id),
  FOREIGN KEY (userId) REFERENCES users (id),
  PRIMARY KEY (id)
);
