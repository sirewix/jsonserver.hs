DROP TABLE IF EXISTS posts CASCADE;
DROP TABLE IF EXISTS tags CASCADE;
DROP TABLE IF EXISTS categories CASCADE;
DROP TABLE IF EXISTS users CASCADE;
DROP TABLE IF EXISTS tag_post_relations CASCADE;

CREATE TABLE users (
    name             varchar (20) PRIMARY KEY
  , lastName         varchar (30)
  , avatar           varchar (20)
  , registrationDate date
  , admin            bool DEFAULT false
  , password         varchar (20)
);

INSERT INTO users  (name, lastname, registrationdate, admin, password) VALUES ('admin', 'Juohanpeohanovitch', current_timestamp, true, '123');

CREATE TABLE authors (
    author_id   serial PRIMARY KEY
  , description text
) INHERITS (users);

CREATE TABLE categories (
    id            serial PRIMARY KEY
  , name          varchar (50)
  , subcategoryOf serial REFERENCES categories
);

CREATE TABLE tags (
    id  serial PRIMARY KEY
  , tag varchar (20)
);

CREATE TABLE posts (
    id        serial PRIMARY KEY
  , title     varchar (50)
  , date      date
  , author    serial REFERENCES authors
  , category  serial REFERENCES categories
  , content   text
  , mainImage varchar (20)
  , images    varchar (20) []
  , draft     bool
);

CREATE TABLE tag_post_relations (
    tag  serial REFERENCES tags
  , post serial REFERENCES posts
);
