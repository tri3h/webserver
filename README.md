# webserver

## How to run
Before the first run, 2 config files should be filled. They are both in the folder **Configs**.
The first file is **Server.config** that contains information about a port, a host and a log verbosity.
The second file is **Database.config** that contains information about a database.
Also it may be necessary to use a command *stack setup*.

After these two files are filled, the project may be started by typing *stack run* into command line while being in the project folder. If there are no admins in the database, the program will ask about creating default admin.

Scripts from folder *scripts* may be run with bash like: *bash script_name.sh -parameter1 "value1" -parameter2 "value2"*. Some of scripts take an image, they take it in the form of two parameters: a path to the file and a type of image, for example to add a minor photo to a draft: *bash add_minor_photo.sh -t "567980" -d "8" -p "/home/user/Documents/tiger.jpg" -y "jpg"*. 

## Basic structure 
The project has 3 main parts:
<details>
<summary>Server part</summary>

The file **Server.hs** gets a user request, considers a path and a method within a request and then calls a requested function from one of the server part files. These files are found in the folder */src*.
Each of these functions gets a query and, if necessary, a body of request from the **Server.hs**. Then they try to parse these data using functions from **Utility.hs** and then check if all needed data are presented. If not, they give a negative answer to user. Otherwise they call functons from *Handler part* and give an answer to user according to results.
</details>

<details>
<summary>Handler part</summary>

Files from this part are in the folder */Handler*. These files contain the main logic of the project. They get data from the server part and perform actions with a help of *Database part*, then return results to the server part. 
</details>

<details>
<summary>Database part</summary>

This part is responsible for work with database and contains queries to a database. They are stored in the folder */Database/Queries*. They are used to insert, update, delete or get data from a database.
**Connection.hs** is responsible for openning/closing connection to a database.
**Migration.hs** is responsible for applying migrations to a database.
</details>

All three parts are splitted into files with the same names. For example, there are 4 files with a name *Tag.hs*: one in the Server part, one in the Handler part, one in the Database part and one in the Types. All of these 4 files are responsible for actions with tags. But each of them do his part of the job.

<details>
<summary>Additional parts</summary>

Additional parts are folders */scripts*, */DatabaseMigrations* and */test*:

- *Scripts* contains sh scripts with curl requests in main folder and additional utility scripts in *utility* folder. 

- *DatabaseMigrations* contains sql files that needed to make migrations.

- *Test* contains tests for the Handler part.
</details>

## List of endpoints

<details>
<summary> Available for all </summary>

- POST /users 
    * Create a user
    * Required parameters: 
        * name
        * surname
        * login
        * password
    * Also there should be an image "avatar"
    * Return a token in case of success

- /tokens 
    * Make a new token
    * Required parameters:
        * login
        * password
    * Return a new token in case of success

- GET /images
    * Get an image
    * Required parameters:
        * image_id
    * Return an image in case of success

- GET /tags 
    * Get a tag
    * Required parameters:
        * tag_id
    * Return a tag in case of success

- GET /categories
    * Get a category
    * Required parameters:
        * category_id
    * Return a category in case of success

- GET /posts
    * Get posts
    * Required parameters: none
    * Optional parameters:
        * author_name
        * category_id
        * tag
        * tag_id
        * tag_in
        * tag_all
        * post_name
        * text
        * substring
        * date_after
        * date_at
        * date_before
        * sort_by
        * offset
        * limit
    * "sort_by" can be: "by_date" / "by_author" / "by_category" / "by_photos_number"
    * Return all posts (max at a time = 10) if there are no optional parameters. Return posts (max at a time = 10 or less if there is a limit parameter) only with corresponding parameters if there are optional paramaters. Return no post if there are no posts with such optional parameters
</details>

<details>
<summary>Available for users (and admins)</summary>

- GET /users
    * Get a user
    * Required parameters:
        * token
    * Return a user in case of success

- GET /comments
    * Get all comments to a post
    * Required parameters:
        * token
        * post_id
    * Return a list of comments to a post in case of success

- POST /comments
    * Create a comment to a post
    * Required parameters:
        * token
        * post_id
        * user_id
        * text
    * Return nothing in case of success

- GET /drafts
    * Get a draft
    * Required parameters:
        * token
        * draft_id
    * Return a draft in case of success

- POST /drafts
    * Create a draft
    * Required parameters:
        * token
        * category_id
        * description
        * tag_id
        * name
    * Also there should be an image "main_photo"
    * Return nothing in case of success

- PUT /drafts
    * Edit a draft
    * Required parameters:
        * token
        * draft_id
    * Optional parameters:
        * category_id
        * tag_id
        * name
        * description
    * Also there may be an image "main_photo"
    * Return nothing in case of success

- DELETE /drafts
    * Delete a draft
    * Required parameters:
        * token
        * draft_id
    * Return nothing in case of success

- POST drafts/minor_photo
    * Add a minor photo to a draft
    * Required parameters:
        * token
        * draft_id
    * Also there should be an image "minor_photo"
    * Return nothing in case of success

- DELETE drafts/minor_photo
    * Delete a minor photo from a draft
    * Required parameters:
        * token
        * draft_id
        * minor_photo_id
    * Return nothing in case of success

- /publish
    * Make a post from a draft or if a post already exists then update it
    * Required parameters:
        * token
        * draft_id
    * Return nothing in case of success
</details>

<details>
<summary>Available only for admins</summary>

- DELETE /users
    * Delete a user
    * Required parameters:
        * token
        * user_id
    * Return nothing in case of success

- POST /authors
    * Create an author
    * Required parameters:
        * token
        * user_id
        * description
    * Return nothing in case of success

- PUT /authors
    * Edit an author
    * Required parameters:
        * token
        * author_id
        * description
    * Return nothing in case of success

- GET /authors
    * Get an author
    * Required parameters:
        * token
        * author_id
    * Return an author in case of success

- DELETE /authors
    * Delete an author
    * Required parameters:
        * token
        * author_id
    * Return nothing in case of success

- POST /tags
    * Create a tag
    * Required parameters:
        * token
        * name
    * Return nothing in case of success

- PUT /tags
    * Edit a tag
    * Required parameters:
        * token
        * tag_id
        * name
    * Return nothing in case of success

- DELETE /tags
    * Delete a tag
    * Required parameters:
        * token
        * tag_id
    * Return nothing in case of success

- POST /categories
    * Create a category
    * Required parameters:
        * token
        * name
    * Optional parameters:
        * parent_id
    * Return nothing in case of success

- PUT /categories 
    * Edit a category
    * Required parameters:
        * token
        * category_id
    * Optional parameters:
        * parent_id
        * name
    * Return nothing in case of success

- DELETE /categories
    * Delete a category
    * Required parameters:
        * token
        * category_id
    * Return nothing in case of success

- DELETE /comments
    * Delete a comment
    * Required parameters:
        * token
        * comment_id
    * Return nothing in case of success
</details>