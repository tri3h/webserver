--tags
CREATE TEMP TABLE temp_tags AS (
SELECT t.name, t.tag_id, mt.min
FROM (SELECT name, MIN (tag_id) AS min
    FROM tags
    GROUP BY name
    HAVING COUNT(name) > 1) mt 
INNER JOIN tags t ON t.name = mt.name
WHERE t.tag_id != mt.min);

ALTER TABLE post_tags DROP CONSTRAINT post_tag_id;

UPDATE post_tags pt
SET tag_id = tt.min 
FROM temp_tags tt 
WHERE pt.tag_id = tt.tag_id;

DELETE FROM post_tags pt1 USING post_tags pt2 
WHERE pt1.post_tag_id > pt2.post_tag_id AND pt1.post_id = pt2.post_id AND pt1.tag_id = pt2.tag_id;

ALTER TABLE post_tags ADD CONSTRAINT post_tag_id PRIMARY KEY (post_tag_id);

ALTER TABLE draft_tags DROP CONSTRAINT draft_tag_id;

UPDATE draft_tags dt 
SET tag_id = tt.min 
FROM temp_tags tt 
WHERE dt.tag_id = tt.tag_id;

DELETE FROM draft_tags dt1 USING draft_tags dt2 
WHERE dt1.draft_tag_id > dt2.draft_tag_id AND dt1.draft_id = dt2.draft_id AND dt1.tag_id = dt2.tag_id;

ALTER TABLE draft_tags ADD CONSTRAINT draft_tag_id PRIMARY KEY (draft_tag_id);

DELETE FROM tags t USING temp_tags tt
WHERE t.tag_id = tt.tag_id;

DROP TABLE temp_tags;

ALTER TABLE tags ADD CONSTRAINT tag_name_unique UNIQUE (name);

--categories
UPDATE categories c 
SET name = mc.name || ' ' || mc.rn
FROM (SELECT category_id, name, ROW_NUMBER() OVER (PARTITION BY name ORDER BY name) rn
    FROM categories) mc 
WHERE mc.rn > 1 AND mc.category_id = c.category_id;

ALTER TABLE categories ADD CONSTRAINT category_name_unique UNIQUE (name);

--authors
CREATE TEMP TABLE temp_authors AS (
SELECT a.author_id, a.description, a.user_id, ma.min_author_id
FROM (SELECT user_id, MIN (author_id) AS min_author_id
    FROM authors
    GROUP BY user_id
    HAVING COUNT(user_id) > 1) ma
INNER JOIN authors a ON a.user_id = ma.user_id);

UPDATE posts p
SET author_id = ta.min_author_id
FROM temp_authors ta
WHERE p.author_id = ta.author_id;

UPDATE drafts d 
SET author_id = ta.min_author_id
FROM temp_authors ta 
WHERE d.author_id = ta.author_id;

UPDATE authors a 
SET description = ta.new_description
FROM (SELECT min_author_id, STRING_AGG(description, ', ') AS new_description 
FROM temp_authors
GROUP BY min_author_id) ta
WHERE a.author_id = ta.min_author_id;

DELETE FROM authors a USING temp_authors ta
WHERE a.author_id = ta.author_id AND a.author_id != ta.min_author_id;

DROP TABLE temp_authors;

ALTER TABLE authors ADD CONSTRAINT user_id_unique UNIQUE (user_id);

ALTER TABLE users ADD CONSTRAINT login_unique UNIQUE (login);

ALTER TABLE users ADD CONSTRAINT token_unique UNIQUE (token);