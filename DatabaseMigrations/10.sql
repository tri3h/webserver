ALTER TABLE comments
DROP CONSTRAINT post_id,
ADD CONSTRAINT post_id 
    FOREIGN KEY(post_id)
        REFERENCES posts(post_id)
            ON DELETE CASCADE;

ALTER TABLE post_minor_photos
DROP CONSTRAINT post_id,
ADD CONSTRAINT post_id 
    FOREIGN KEY(post_id)
        REFERENCES posts(post_id)
            ON DELETE CASCADE;

ALTER TABLE post_tags
DROP CONSTRAINT post_id,
ADD CONSTRAINT post_id 
    FOREIGN KEY(post_id)
        REFERENCES posts(post_id)
            ON DELETE CASCADE;