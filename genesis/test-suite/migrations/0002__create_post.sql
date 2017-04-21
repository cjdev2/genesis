CREATE TABLE post (
  id integer NOT NULL PRIMARY KEY AUTOINCREMENT,
  title text NOT NULL,
  body text NOT NULL,
  blog_id uuid NOT NULL REFERENCES blog ON DELETE CASCADE
)
