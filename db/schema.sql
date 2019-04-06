PRAGMA foreign_keys = ON;

create table if not exists polls(
    id INTEGER PRIMARY KEY AUTOINCREMENT
  , questions BLOB not null
  , startTime TEXT not null
  , endTime TEXT not null
);

create table if not exists answers(
    id INTEGER PRIMARY KEY AUTOINCREMENT
  , answers BLOB not null
  , poll INTEGER not null
  , time TEXT not null
  , FOREIGN KEY (poll) REFERENCES polls(id)
);