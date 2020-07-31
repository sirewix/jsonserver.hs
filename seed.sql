INSERT INTO users (name, lastname, admin, avatar, registrationDate, password) VALUES
    ('admin', 'Juohanpeohanovitch', true, '/admin.png', current_timestamp, '123'),
    ('lupa', 'Kawasaki', false, '/author.png', current_timestamp, '123'),
    ('pupa', 'Erricson', false, '/user.png', current_timestamp, '123');

INSERT INTO authors (username, description) VALUES
    ('admin', 'godfather'),
    ('lupa', 'someauthor');

INSERT INTO categories (name, parent_id) VALUES
    ('All', NULL),
    ('Flat', 1),
    ('Volumetric', 1),
    ('Square', 2),
    ('Sphere', 3);

INSERT INTO tags (tag) VALUES ('tag1'), ('tag2'), ('tag3');

INSERT INTO posts (title, date, author, category, content, mainImage, images, published) VALUES
    ('title0', current_timestamp, 1, 4, 'asdfe',    'img.png', array['hi', 'by'], true),
    ('title1', current_timestamp, 1, 2, 'nubvryu',  'img.png', array['hi'],       true),
    ('title2', current_timestamp, 2, 4, 'ovnet',    'img.png', array[]::text[],   true),
    ('title3', current_timestamp, 2, 2, 'weroihzb', 'img.png', array[]::text[],   true),
    ('title4', current_timestamp, 2, 2, 'lkmug',    'img.png', array[]::text[],   true);

INSERT INTO tag_post_relations (tag, post) VALUES
    (1, 1),
    (1, 2),
    (1, 3),
    (1, 4),
    (2, 1),
    (3, 1);

INSERT INTO comments (post, username, comment) VALUES
    (1, 'pupa', 'haha'),
    (1, 'lupa', 'hehe'),
    (1, 'admin', 'hihi'),
    (2, 'pupa', 'hoho'),
    (2, 'lupa', 'heh'),
    (3, 'pupa', 'hah'),
    (3, 'admin', 'meh'),
    (4, 'lupa', 'ooo'),
    (3, 'pupa', 'eh'),
    (3, 'lupa', 'wow'),
    (2, 'pupa', 'ops'),
    (4, 'lupa', 'beep');

REFRESH MATERIALIZED VIEW posts_view;

