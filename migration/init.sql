-------------------------------------------------------------------------------
-- User

CREATE TABLE IF NOT EXISTS "user" (
  id SERIAL PRIMARY KEY,
  name text UNIQUE NOT NULL,
  email TEXT UNIQUE NOT NULL
);

CREATE UNIQUE INDEX IF NOT EXISTS user_name_idx ON "user" (name);

-------------------------------------------------------------------------------
-- Auth

CREATE TABLE IF NOT EXISTS password (
  user_id INT PRIMARY KEY REFERENCES "user",
  password_hash text NOT NULL
);

CREATE TABLE IF NOT EXISTS refresh_token (
  user_id INT PRIMARY KEY REFERENCES "user",
  token_string TEXT UNIQUE NOT NULL,
  expiry timestamp NOT NULL
);

CREATE UNIQUE INDEX IF NOT EXISTS refresh_token_token_string_idx
  ON refresh_token (token_string);

-------------------------------------------------------------------------------
-- Household

CREATE TABLE IF NOT EXISTS household (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS household_member (
  household_id INT NOT NULL REFERENCES household,
  user_id INT NOT NULL REFERENCES "user"
);

CREATE UNIQUE INDEX IF NOT EXISTS household_member_id
  ON household_member (household_id, user_id);

-------------------------------------------------------------------------------
-- Chore

CREATE TABLE IF NOT EXISTS chore (
  id SERIAL PRIMARY KEY,
  household_id INT NOT NULL REFERENCES household ON DELETE CASCADE,
  name TEXT NOT NULL
);

-- CREATE TYPE chore_event_type AS ENUM (
--   'complete',
--   'skip'
-- );

CREATE TABLE IF NOT EXISTS chore_event (
  chore_id INT NOT NULL,
  day DATE NOT NULL,
  type text NOT NULL,

  FOREIGN KEY (chore_id) REFERENCES chore
);

-------------------------------------------------------------------------------
-- Schedule

-- CREATE TYPE schedule_constr AS ENUM (
--   'strict_days',
--   'flex_days',
--   'weekly_pattern',
--   'monthly_pattern'
-- );

CREATE TABLE IF NOT EXISTS schedule (
  id    SERIAL          NOT NULL,
  chore_id INT NOT NULL,
  --type  schedule_constr   NOT NULL,
  type  text   NOT NULL,

  PRIMARY KEY (id, type),
  FOREIGN KEY (chore_id) REFERENCES chore
);

CREATE TABLE IF NOT EXISTS flex_days (
  id INTEGER PRIMARY KEY,
  type
      text NOT NULL
      DEFAULT 'flex_days'
      CHECK (type = 'flex_days'),
  days INT NOT NULL,
  scheduled DATE,
  FOREIGN KEY (id, type) REFERENCES schedule (id, type) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS strict_days (
  id INTEGER PRIMARY KEY,
  type
      text NOT NULL
      DEFAULT 'strict_days'
      CHECK (type = 'strict_days'),
  days INT NOT NULL,
  scheduled DATE,
  FOREIGN KEY (id, type) REFERENCES schedule (id, type) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS weekly_pattern (
  id INTEGER PRIMARY KEY,
  type
      text NOT NULL
      DEFAULT 'weekly_pattern'
      CHECK (type = 'weekly_pattern'),
  iterations INT NOT NULL,
  --elem_index INT NOT NULL,
  elem_index INT,
  scheduled DATE,
  FOREIGN KEY (id, type) REFERENCES schedule (id, type) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS weekly_pattern_elem (
  --id SERIAL PRIMARY KEY,
  weekly_pattern_id INT NOT NULL REFERENCES weekly_pattern ON DELETE CASCADE,
  iteration INT NOT NULL,
  point INT NOT NULL
);

CREATE UNIQUE INDEX weekly_pattern_elem_uq
ON weekly_pattern_elem (weekly_pattern_id, iteration, point);

CREATE TABLE IF NOT EXISTS monthly_pattern (
  id INTEGER PRIMARY KEY,
  type TEXT NOT NULL
  DEFAULT 'monthly_pattern'
  CHECK (type = 'monthly_pattern'),
  iterations INT NOT NULL,
  --elem_index INT NOT NULL,
  elem_index INT,
  scheduled DATE,
  FOREIGN KEY (id, type) REFERENCES schedule (id, type)
);

CREATE TABLE IF NOT EXISTS monthly_pattern_elem (
  id SERIAL PRIMARY KEY,
  monthly_pattern_id INT NOT NULL REFERENCES monthly_pattern ON DELETE CASCADE,
  iteration INT NOT NULL,
  point INT NOT NULL
);

CREATE UNIQUE INDEX monthly_pattern_elem_uq
ON monthly_pattern_elem (monthly_pattern_id, iteration, point);
