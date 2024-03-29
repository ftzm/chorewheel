-------------------------------------------------------------------------------
-- User

CREATE TABLE IF NOT EXISTS "user" (
  id UUID PRIMARY KEY,
  name text UNIQUE NOT NULL,
  email TEXT UNIQUE NOT NULL
);

CREATE UNIQUE INDEX IF NOT EXISTS user_name_idx ON "user" (name);

-------------------------------------------------------------------------------
-- Auth

CREATE TABLE IF NOT EXISTS password (
  user_id UUID PRIMARY KEY REFERENCES "user",
  password_hash text NOT NULL
);

CREATE TABLE IF NOT EXISTS refresh_token (
  user_id UUID PRIMARY KEY REFERENCES "user",
  token_string TEXT UNIQUE NOT NULL,
  expiry timestamp NOT NULL
);

CREATE UNIQUE INDEX IF NOT EXISTS refresh_token_token_string_idx
  ON refresh_token (token_string);

CREATE TABLE IF NOT EXISTS session_token (
  user_id UUID PRIMARY KEY REFERENCES "user",
  token_string TEXT UNIQUE NOT NULL,
  expiry timestamp NOT NULL
);

CREATE UNIQUE INDEX IF NOT EXISTS session_token_token_string_idx
  ON session_token (token_string);

-------------------------------------------------------------------------------
-- Household

CREATE TABLE IF NOT EXISTS household (
  id UUID PRIMARY KEY,
  name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS household_member (
  household_id UUID NOT NULL REFERENCES household,
  user_id UUID NOT NULL REFERENCES "user"
);

CREATE UNIQUE INDEX IF NOT EXISTS household_member_id
  ON household_member (household_id, user_id);

-------------------------------------------------------------------------------
-- Chore

CREATE TABLE IF NOT EXISTS chore (
  id UUID PRIMARY KEY,
  household_id UUID NOT NULL REFERENCES household ON DELETE CASCADE,
  name TEXT NOT NULL
);

CREATE UNIQUE INDEX chore_name_unique ON chore (name);


-- CREATE TYPE chore_event_type AS ENUM (
--   'complete',
--   'skip'
-- );

CREATE TABLE IF NOT EXISTS chore_event (
  --id UUID PRIMARY KEY,
  chore_id UUID NOT NULL,
  day DATE NOT NULL,
  type text NOT NULL,
  user_id UUID REFERENCES "user",

  PRIMARY KEY (chore_id, day),
  FOREIGN KEY (chore_id) REFERENCES chore
);

CREATE UNIQUE INDEX chore_event_day_unique ON chore_event (chore_id, day);

CREATE TABLE IF NOT EXISTS chore_participant_type (
  chore_id UUID NOT NULL REFERENCES chore,
  type text NOT NULL
);

CREATE TABLE IF NOT EXISTS chore_participant (
  chore_id UUID NOT NULL REFERENCES chore,
  user_id UUID NOT NULL REFERENCES "user",
  type
      text NOT NULL
      DEFAULT 'some'
      CHECK (type = 'some')
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
  id UUID NOT NULL,
  type  text   NOT NULL,

  PRIMARY KEY (id, type),
  FOREIGN KEY (id) REFERENCES chore
);

CREATE TABLE IF NOT EXISTS flex_days (
  id UUID PRIMARY KEY,
  type
      text NOT NULL
      DEFAULT 'flex_days'
      CHECK (type = 'flex_days'),
  days INT NOT NULL,
  scheduled DATE,
  FOREIGN KEY (id, type) REFERENCES schedule (id, type) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS strict_days (
  id UUID PRIMARY KEY,
  type
      text NOT NULL
      DEFAULT 'strict_days'
      CHECK (type = 'strict_days'),
  days INT NOT NULL,
  scheduled DATE,
  FOREIGN KEY (id, type) REFERENCES schedule (id, type) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS weekly_pattern (
  id UUID PRIMARY KEY,
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
  weekly_pattern_id UUID NOT NULL REFERENCES weekly_pattern ON DELETE CASCADE,
  iteration INT NOT NULL,
  point INT NOT NULL
);

CREATE UNIQUE INDEX weekly_pattern_elem_uq
ON weekly_pattern_elem (weekly_pattern_id, iteration, point);

CREATE TABLE IF NOT EXISTS monthly_pattern (
  id UUID PRIMARY KEY,
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
  --id SERIAL PRIMARY KEY,
  monthly_pattern_id UUID NOT NULL REFERENCES monthly_pattern ON DELETE CASCADE,
  iteration INT NOT NULL,
  point INT NOT NULL
);

CREATE UNIQUE INDEX monthly_pattern_elem_uq
ON monthly_pattern_elem (monthly_pattern_id, iteration, point);
