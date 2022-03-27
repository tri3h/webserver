CREATE TABLE categories (
    category_id SERIAL,
    parent_id INT,
    name TEXT, 
    CONSTRAINT category_id PRIMARY KEY(category_id)
);