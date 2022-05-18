ALTER TABLE categories 
    ADD CONSTRAINT parent_id 
        FOREIGN KEY(parent_id) 
            REFERENCES categories(category_id);

ALTER TABLE drafts 
    ALTER COLUMN author_id SET NOT NULL;