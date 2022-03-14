CREATE TABLE posts (
    post_id SERIAL,
    author_id INT,
    category_id INT,
    name TEXT,
    date DATE,
    text TEXT,
    main_photo TEXT,
    CONSTRAINT post_id PRIMARY KEY(post_id),
    CONSTRAINT author_id
      FOREIGN KEY(author_id) 
	    REFERENCES authors(author_id)
	        ON DELETE SET NULL,
    CONSTRAINT category_id
      FOREIGN KEY(category_id) 
	    REFERENCES categories(category_id)
	        ON DELETE SET NULL
);

CREATE TABLE post_tags (
    post_tag_id SERIAL,
    post_id INT,
    tag_id INT,
    CONSTRAINT post_tag_id PRIMARY KEY(post_id, tag_id),
    CONSTRAINT post_id
      FOREIGN KEY(post_id) 
	    REFERENCES posts(post_id)
	        ON DELETE SET NULL,
    CONSTRAINT tag_id
      FOREIGN KEY(tag_id) 
	    REFERENCES tags(tag_id)
	        ON DELETE SET NULL
);

CREATE TABLE minor_photos (
    minor_photo_id SERIAL,
    minor_photo TEXT,
    post_id INT,
    CONSTRAINT minor_photo_id PRIMARY KEY(minor_photo_id),
    CONSTRAINT post_id
      FOREIGN KEY(post_id) 
	    REFERENCES posts(post_id)
	        ON DELETE SET NULL
);
