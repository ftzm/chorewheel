CREATE TABLE IF NOT EXISTS "user" (
  id SERIAL PRIMARY KEY,
  name TEXT UNIQUE NOT NULL,
  email TEXT UNIQUE NOT NULL
);

CREATE TABLE IF NOT EXISTS password (
  user_id INT UNIQUE NOT NULL REFERENCES "user",
  password_hash text NOT NULL
);

CREATE TABLE IF NOT EXISTS refresh_token (
  user_id INT UNIQUE NOT NULL REFERENCES "user",
  token_string text UNIQUE NOT NULL,
  expiry timestamp NOT NULL
);
