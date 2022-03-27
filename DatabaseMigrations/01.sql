CREATE TABLE users (
    user_id serial PRIMARY KEY,
    name text,
    surname text,
    avatar text,
    login text,
    password text,
    registration_date date,
    admin boolean,
    token text
);