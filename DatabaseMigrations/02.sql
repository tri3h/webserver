ALTER INDEX users_pkey RENAME TO user_id;

CREATE TABLE authors (
    author_id SERIAL,
    user_id INT,
    description TEXT, 
    CONSTRAINT author_id PRIMARY KEY(author_id),
    CONSTRAINT user_id
      FOREIGN KEY(user_id) 
	    REFERENCES users(user_id)
	        ON DELETE SET NULL
);