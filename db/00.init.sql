DROP TABLE IF EXISTS posts              CASCADE;
DROP TABLE IF EXISTS tags               CASCADE;
DROP TABLE IF EXISTS categories         CASCADE;
DROP TABLE IF EXISTS users              CASCADE;
DROP TABLE IF EXISTS authors            CASCADE;
DROP TABLE IF EXISTS tag_post_relations CASCADE;
DROP TABLE IF EXISTS comments           CASCADE;

CREATE TABLE users (
    id               serial          PRIMARY KEY
  , name             varchar (20)    NOT NULL UNIQUE
  , lastName         varchar (30)    NOT NULL
  , avatar           varchar (20)    NULL
  , registration_date date           NOT NULL DEFAULT current_timestamp
  , admin            bool            NOT NULL DEFAULT false
  , password         varchar (30)    NOT NULL
);

INSERT INTO users (name, lastname, admin, avatar, password) VALUES
    ('admin', 'Ivanov', true,  '/admin.png',  '123');

CREATE TABLE authors (
    id               serial          PRIMARY KEY
  , user_id          int             NOT NULL UNIQUE REFERENCES users ON DELETE CASCADE
  , description      text            NOT NULL
);

INSERT INTO authors (user_id, description) VALUES
    (1, 'godfather');

CREATE TABLE categories (
    id               serial          PRIMARY KEY
  , name             varchar (50)    NOT NULL UNIQUE
  , parent_id        int             NULL REFERENCES categories (id) ON DELETE CASCADE
  , CHECK (parent_id <> id)
);

INSERT INTO categories (name, parent_id) VALUES
    ('Root', NULL);

CREATE TABLE tags (
    id               serial          PRIMARY KEY
  , tag              varchar (20)    NOT NULL UNIQUE
);

CREATE TABLE posts (
    id               serial          PRIMARY KEY
  , title            varchar (50)    NOT NULL
  , date             date            NOT NULL DEFAULT current_timestamp
  , author           int             NOT NULL REFERENCES authors
  , category         int             NOT NULL REFERENCES categories
  , content          text            NOT NULL
  , main_image       varchar (20)    NOT NULL
  , images           varchar (20) [] NOT NULL
  , published        bool            NOT NULL DEFAULT false
);

CREATE TABLE tag_post_relations (
    tag              int             NOT NULL REFERENCES tags ON DELETE CASCADE
  , post             int             NOT NULL REFERENCES posts ON DELETE CASCADE
  , PRIMARY KEY (tag, post)
);

CREATE TABLE comments (
    id               serial          PRIMARY KEY
  , post             int             NOT NULL REFERENCES posts ON DELETE CASCADE
  , user_id          int             NOT NULL REFERENCES users ON DELETE CASCADE
  , comment          text            NOT NULL
);

DROP FUNCTION IF EXISTS category_root;
CREATE FUNCTION category_root(int)
RETURNS json
LANGUAGE SQL IMMUTABLE
AS $$
WITH RECURSIVE x AS (
    SELECT categories.* FROM categories WHERE id = $1
    UNION ALL
    SELECT categories.* FROM categories JOIN x ON x.parent_id = categories.id
) SELECT json_agg(x) FROM x WHERE parent_id IS NOT NULL;
$$;

DROP FUNCTION IF EXISTS tags_by_post;
CREATE FUNCTION tags_by_post(int)
RETURNS json
LANGUAGE SQL IMMUTABLE
AS $$
    SELECT json_agg(tags)
    FROM tag_post_relations
    JOIN tags ON tag_post_relations.tag = tags.id
    WHERE post = $1
$$;

DROP FUNCTION IF EXISTS author_id_by_username;
CREATE FUNCTION author_id_by_username(text)
RETURNS int
LANGUAGE SQL IMMUTABLE
AS $$
    SELECT authors.id
    FROM authors
    JOIN users ON users.id = authors.user_id
    WHERE users.name = $1
$$;

DROP VIEW IF EXISTS posts_view;
CREATE MATERIALIZED VIEW posts_view AS
    SELECT
        posts.id AS id,
        published,
        categories.name AS categoryname,
        categories.id AS categoryid,
        coalesce(array_length(posts.images, 1), 0) AS numberofimages,
        array_agg (tag_post_relations.tag) AS tag_ids,
        date AS date,
        json_build_object (
            'id',            posts.id,
            'title',         posts.title,
            'date',          posts.date,
            'content',       posts.content,
            'main_image',    posts.main_image,
            'images',        posts.images,
            'author',        json_build_object (
                                'username', users.name,
                                'description', authors.description
                             ),
            'category_tree', COALESCE (category_root(posts.category), '[]'::json),
            'tags',          COALESCE (tags_by_post(posts.id), '[]'::json)
        ) AS json
    FROM posts
    JOIN authors                 ON authors.id = posts.author
    JOIN users                   ON authors.user_id = users.id
    JOIN categories              ON categories.id = posts.category
    LEFT JOIN tag_post_relations ON post = posts.id
    LEFT JOIN tags               ON tag_post_relations.tag = tags.id
    GROUP BY
        posts.id,
        authors.id,
        users.id,
        categoryid,
        categoryname;
