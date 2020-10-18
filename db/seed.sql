INSERT INTO users (name, lastname, admin, avatar, password) VALUES
    ('lupa',  'Kawasaki',           false, '/author.png', '123'),
    ('pupa',  'Erricson',           false, '/user.png',   '123');

INSERT INTO authors (user_id, description) VALUES
    (2, 'someauthor');

INSERT INTO categories (name, parent_id) VALUES
    ('Flat',       1),
    ('Volumetric', 1),
    ('Square',     2),
    ('Sphere',     3);

INSERT INTO tags (tag) VALUES ('tag1'), ('tag2'), ('tag3');

INSERT INTO posts (title, author, category, content, main_image, images, published) VALUES
    ('title1', 1, 4, 'asdfe',    'img.png', array['hi', 'by'], true),
    ('title2', 1, 2, 'nubvryu',  'img.png', array['hi'],       true),
    ('title3', 2, 4, 'ovnet',    'img.png', array[]::text[],   true),
    ('title4', 2, 2, 'weroihzb', 'img.png', array[]::text[],   true),
    ('title5', 2, 2, 'lkmug',    'img.png', array[]::text[],   true),
    ('title6', 1, 1, 'lkmug',    'img.png', array[]::text[],   true);

INSERT INTO tag_post_relations (tag, post) VALUES
    (1, 1),
    (1, 2),
    (1, 3),
    (1, 4),
    (2, 1),
    (3, 1);

INSERT INTO comments (post, user_id, comment) VALUES
    (1, 3, 'haha'),
    (1, 2, 'hehe'),
    (1, 1, 'hihi'),
    (2, 3, 'hoho'),
    (2, 2, 'heh'),
    (3, 3, 'hah'),
    (3, 1, 'meh'),
    (4, 2, 'ooo'),
    (3, 3, 'eh'),
    (3, 2, 'wow'),
    (2, 3, 'ops'),
    (4, 2, 'beep');

REFRESH MATERIALIZED VIEW posts_view;

