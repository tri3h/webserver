CREATE TABLE pictures (
    picture_id SERIAL,
    picture BYTEA,
    CONSTRAINT picture_id PRIMARY KEY(picture_id)
);

ALTER TABLE users RENAME COLUMN avatar TO picture_id;

ALTER TABLE users ALTER COLUMN picture_id TYPE INT USING (NULL);

ALTER TABLE users 
ADD CONSTRAINT picture_id 
    FOREIGN KEY(picture_id)
        REFERENCES pictures(picture_id)
            ON DELETE SET NULL;

ALTER TABLE posts RENAME COLUMN main_photo TO picture_id;

ALTER TABLE posts ALTER COLUMN picture_id TYPE INT USING (NULL);

ALTER TABLE posts
ADD CONSTRAINT picture_id 
    FOREIGN KEY(picture_id)
        REFERENCES pictures(picture_id)
            ON DELETE SET NULL;

ALTER TABLE minor_photos RENAME COLUMN minor_photo TO picture_id;

ALTER TABLE minor_photos ALTER COLUMN picture_id TYPE INT USING (NULL);

ALTER TABLE minor_photos
ADD CONSTRAINT picture_id 
    FOREIGN KEY(picture_id)
        REFERENCES pictures(picture_id)
            ON DELETE SET NULL;
