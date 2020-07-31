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

CREATE TABLE categories (
    id        serial PRIMARY KEY
  , name      varchar (50)
  , parent_id int NULL REFERENCES categories (id) ON DELETE CASCADE
    --FOREIGN KEY (manager_id) REFERENCES categories (id) ON DELETE CASCADE
);

CREATE TABLE tags (
    id serial PRIMARY KEY
  , tag varchar (20) UNIQUE
);

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
    tag  int REFERENCES tags ON DELETE CASCADE
  , post int REFERENCES posts ON DELETE CASCADE
);

DROP FUNCTION IF EXISTS category_root;
CREATE FUNCTION category_root(int)
RETURNS json
LANGUAGE SQL IMMUTABLE
AS $$
SELECT json_agg(u) FROM (
    WITH RECURSIVE x AS (
        SELECT categories.* FROM categories WHERE id = $1
        UNION ALL
        SELECT categories.* FROM categories JOIN x ON x.parent_id = categories.id
    ) SELECT id, name FROM x
) AS u
$$;

DROP VIEW IF EXISTS posts_view;
CREATE MATERIALIZED VIEW posts_view AS
    SELECT
        posts.id,
        published,
        posts.date,
        posts.title,
        posts.content,
        authors.username AS authorname,
        categories.name AS categoryname,
        categories.id AS categoryid,
        coalesce(array_length(posts.images, 1), 0) AS numberofimages,
        array_agg (tag_post_relations.tag) AS tags,
        json_build_object (
            'id',            posts.id,
            'title',         posts.title,
            'date',          posts.date,
            'content',       posts.content,
            'mainImage',     posts.mainImage,
            'images',        posts.images,
            'author',        to_json(authors),
            'category_tree', category_root(posts.category),
            'tags',          json_agg (tags)
        ) AS json
    FROM posts
    JOIN authors ON authors.id = posts.author
    JOIN categories ON categories.id = posts.category
    LEFT JOIN tag_post_relations ON post = posts.id
    LEFT JOIN tags ON tag_post_relations.tag = tags.id
    GROUP BY
        posts.id,
        authors.id,
        tag_post_relations.post,
        categoryid,
        categoryname;
