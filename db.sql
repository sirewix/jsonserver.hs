DROP TABLE IF EXISTS posts              CASCADE;
DROP TABLE IF EXISTS tags               CASCADE;
DROP TABLE IF EXISTS categories         CASCADE;
DROP TABLE IF EXISTS users              CASCADE;
DROP TABLE IF EXISTS authors            CASCADE;
DROP TABLE IF EXISTS tag_post_relations CASCADE;

CREATE TABLE users (
    name             varchar (20)  PRIMARY KEY
  , lastName         varchar (30)
  , avatar           varchar (20)
  , registrationDate date
  , admin            bool DEFAULT false
  , password         varchar (30)
);

CREATE TABLE authors (
    id          serial PRIMARY KEY
  , username    varchar (20) REFERENCES users
  , description text
);

INSERT INTO users (name, lastname, admin, avatar, registrationDate, password)
    VALUES ('admin', 'Juohanpeohanovitch', true, '/admin.png', current_timestamp, '123');

INSERT INTO authors (username, description) VALUES ('admin', 'godfather');

CREATE TABLE categories (
    id        serial PRIMARY KEY
  , name      varchar (50)
  , parent_id int NULL REFERENCES categories (id) ON DELETE CASCADE
    --FOREIGN KEY (manager_id) REFERENCES categories (id) ON DELETE CASCADE
);

INSERT INTO categories (name, parent_id) VALUES
    ('All', NULL),
    ('Flat', 1),
    ('Volumetric', 1),
    ('Square', 2),
    ('Sphere', 3);

CREATE TABLE tags (
    tag varchar (20) PRIMARY KEY
                     ON DELETE CASCADE
                     ON UPDATE CASCADE
);

INSERT INTO tags VALUES ('tag1');
INSERT INTO tags VALUES ('tag2');
INSERT INTO tags VALUES ('tag3');

CREATE TABLE posts (
    id        serial PRIMARY KEY
  , title     varchar (50)
  , date      date
  , author    int REFERENCES authors
  , category  int REFERENCES categories
  , content   text
  , mainImage varchar (20)
  , images    varchar (20) []
  , published bool
);

CREATE TABLE tag_post_relations (
    tag  varchar (20) REFERENCES tags ON DELETE CASCADE ON UPDATE CASCADE
  , post int REFERENCES posts ON DELETE CASCADE
);
