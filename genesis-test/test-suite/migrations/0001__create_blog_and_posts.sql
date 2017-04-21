CREATE TABLE blog (
  id serial NOT NULL PRIMARY KEY,
  name text NOT NULL
);

CREATE TABLE post (
  id serial NOT NULL PRIMARY KEY,
  title text NOT NULL,
  body text NOT NULL,
  blog_id integer NOT NULL REFERENCES blog ON DELETE CASCADE
);
