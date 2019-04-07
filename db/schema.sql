CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE polls (
  id                              uuid DEFAULT uuid_generate_v4(),
  form                            jsonb NOT NULL,
  end_time                        timestamp with time zone NOT NULL,
  created_at                      timestamp with time zone NOT NULL DEFAULT current_timestamp,
  PRIMARY KEY (id)
);

CREATE TABLE poll_responses (
  id                              uuid DEFAULT uuid_generate_v4(),
  response                        jsonb NOT NULL,
  created_at                      timestamp with time zone NOT NULL DEFAULT current_timestamp,
  poll                            uuid NOT NULL,
  FOREIGN KEY (poll) REFERENCES polls(id),
  PRIMARY KEY (id)
);
