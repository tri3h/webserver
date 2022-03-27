CREATE TABLE comments (
    comment_id SERIAL,
    post_id INT,
    user_id INT,
    text TEXT,
    CONSTRAINT comment_id PRIMARY KEY(comment_id),
    CONSTRAINT post_id
      FOREIGN KEY(post_id) 
	    REFERENCES posts(post_id)
	        ON DELETE SET NULL,
    CONSTRAINT user_id
      FOREIGN KEY(user_id) 
	    REFERENCES users(user_id)
	        ON DELETE SET NULL
);