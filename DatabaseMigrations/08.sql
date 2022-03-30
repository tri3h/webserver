ALTER TABLE pictures RENAME TO images;

ALTER TABLE images RENAME COLUMN picture_id TO image_id;

ALTER TABLE images RENAME COLUMN picture TO image;

ALTER TABLE users RENAME COLUMN picture_id TO image_id;

ALTER TABLE posts RENAME COLUMN picture_id TO image_id;

CREATE TABLE drafts (
    draft_id SERIAL,
    post_id INT,
    author_id INT,
    category_id INT,
    name TEXT,
    text TEXT,
    image_id INT,
    CONSTRAINT draft_id PRIMARY KEY(draft_id),
    CONSTRAINT post_id
      FOREIGN KEY(post_id) 
	    REFERENCES posts(post_id)
	        ON DELETE SET NULL,
    CONSTRAINT author_id
      FOREIGN KEY(author_id) 
	    REFERENCES authors(author_id)
	        ON DELETE SET NULL,
    CONSTRAINT category_id
      FOREIGN KEY(category_id) 
	    REFERENCES categories(category_id)
	        ON DELETE SET NULL,
    CONSTRAINT image_id
      FOREIGN KEY(image_id) 
	    REFERENCES images(image_id)
	        ON DELETE SET NULL
);

ALTER TABLE minor_photos RENAME COLUMN minor_photo_id TO post_minor_photo_id;

ALTER TABLE minor_photos RENAME TO post_minor_photos;

ALTER TABLE post_minor_photos RENAME COLUMN picture_id TO image_id;

CREATE TABLE draft_tags (
    draft_tag_id SERIAL,
    tag_id INT,
    draft_id INT,
    CONSTRAINT draft_tag_id PRIMARY KEY(draft_tag_id),
    CONSTRAINT tag_id
      FOREIGN KEY(tag_id) 
	    REFERENCES tags(tag_id)
	        ON DELETE CASCADE,
    CONSTRAINT draft_id
      FOREIGN KEY(draft_id) 
	    REFERENCES drafts(draft_id)
	        ON DELETE CASCADE
);

CREATE TABLE draft_minor_photos (
    draft_minor_photo_id SERIAL,
    image_id INT,
    draft_id INT,
    CONSTRAINT draft_minor_photo_id PRIMARY KEY(draft_minor_photo_id),
    CONSTRAINT image_id
      FOREIGN KEY(image_id) 
	    REFERENCES images(image_id)
	        ON DELETE CASCADE,
    CONSTRAINT draft_id
      FOREIGN KEY(draft_id) 
	    REFERENCES drafts(draft_id)
	        ON DELETE CASCADE
);