CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- TODO poll created by nullable user
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

  response
    jsonb
    NOT NULL,

  poll
    uuid
    NOT NULL,

  FOREIGN KEY (poll) REFERENCES polls(id),
  PRIMARY KEY (id)
);

CREATE TABLE users (
  id
    uuid
    DEFAULT uuid_generate_v4(),

  -- The user's GitHub username, or NULL if they haven't authenticated with
  -- GitHub. Once set, may be overwritten if the user decides to authenticate
  -- as a different GitHub user (we only want to track/display one).
  github
    text,

  PRIMARY KEY (id),
  UNIQUE (github)
);
