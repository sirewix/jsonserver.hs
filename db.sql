DROP TABLE IF EXISTS posts CASCADE;
DROP TABLE IF EXISTS tags CASCADE;
DROP TABLE IF EXISTS categories CASCADE;
DROP TABLE IF EXISTS users CASCADE;
DROP TABLE IF EXISTS authors CASCADE;
DROP TABLE IF EXISTS tag_post_relations CASCADE;

CREATE TABLE users (
    name             varchar (20) PRIMARY KEY
  , lastName         varchar (30)
  , avatar           varchar (20)
  , registrationDate date
  , admin            bool DEFAULT false
  , password         varchar (30)
);

CREATE TABLE authors (
    username    varchar (20) REFERENCES users
  , description text
);

INSERT INTO users  (name, lastname, registrationdate, admin, password)
     VALUES ('admin', 'Juohanpeohanovitch', current_timestamp, true, '123');

CREATE TABLE categories (
    name          varchar (50) PRIMARY KEY
  , subcategoryOf varchar (50) REFERENCES categories
);

CREATE TABLE tags (
    tag varchar (20) PRIMARY KEY
);

CREATE TABLE posts (
    id        serial PRIMARY KEY
  , title     varchar (50)
  , date      date
  , author    varchar (20) REFERENCES users
  , category  varchar (50) REFERENCES categories
  , content   text
  , mainImage varchar (20)
  , images    varchar (20) []
  , draft     bool
);

CREATE TABLE tag_post_relations (
    tag  varchar (20) REFERENCES tags
  , post serial REFERENCES posts
);
