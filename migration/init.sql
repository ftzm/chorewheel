CREATE TABLE IF NOT EXISTS "user" (
  id SERIAL PRIMARY KEY,
  name TEXT UNIQUE NOT NULL,
  email TEXT UNIQUE NOT NULL
);

CREATE UNIQUE INDEX IF NOT EXISTS user_name_idx ON "user" (name);

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

CREATE TABLE IF NOT EXISTS chore (
  id SERIAL PRIMARY KEY,
  household_id INT NOT NULL REFERENCES household,
  name TEXT NOT NULL,
);

CREATE TABLE IF NOT EXISTS chore_instance (
  first_scheduled TIMESTAMPTZ,
  completed_at TIMESTAMPTZ
);
