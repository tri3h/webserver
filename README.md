# webserver

## How to run
Before the first run, a config file should be filled. It is **Server.config.template** in the folder *Configs*. 
You need to fill information for connection to a database and server. 
Also you can fill information about default admin. Then the program will create default admin in the database when it starts (but only if there is no admin in the database yet). The token of the created admin will be displayed in the logger on level info.
Then this file should be renamed to **Server.config**.

Also before the first run it may be necessary to use a command *stack setup*.

After that the project may be started by typing *stack run* into command line while being in the project folder. 

## How to use scripts
Scripts from folder *scripts* may be run with bash in two ways: with *user* parameters and with *default* parameters. 

To run a script with default parameters: *bash script_name.sh*. In this case scripts will take a token from the file *token.txt* and take the file *"image.png"* for user avatar and draft photos. Both of these files are in the folder *scripts/utility*. Before using scripts it is necessary either to create a user by running *"create_user.sh"* script (then a token will be set automatically) or to put a token into the file *"token.txt"* manually. Scripts can create only a usual user so admin functionallity will not be available when using token from *"create_user.sh"*.

To run a script with user parameters: *bash script_name.sh -parameter1 "value1" -parameter2 "value2"*. All required parameters that will not be filled explicitly, will use default values. All optional parameters that will not be filled, will be empty. Some of scripts take an image, they take it in the form of a path to the file, for example to add a minor photo to a draft: *bash add_minor_photo.sh -t "567980" -d "8" -p "/home/user/Documents/tiger.jpg"*. 

## Basic structure 

The file *src/Server.hs* starts server and gets a user request, then calls *Handlers/Server.hs* to consider a path and a method within a request. Depending on received data it calls other functions from *Handlers* to try to parse user data using functions from *src/Utility.hs* and then check if all needed data is presented. If not, they give a negative answer to a user. Otherwise they call functons from *Database/Queries* to insert, update, delete or get data and give an answer to a user according to results.

There are also:

- *Database/Connection.hs* that is responsible for openning/closing connection to a database

- *Database/Migration.hs* that is responsible for applying migrations to a database

- *Types* that contains types

- *src/Error.hs* that contains possible errors

- *Scripts* that contains sh scripts with curl requests in main folder and additional utility scripts in *utility* folder. 

- *DatabaseMigrations* that contains sql files that needed to make migrations.

- *Test* that contains tests for the *Handlers* files.

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
    * Also there *may* be an image "avatar"
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
    * Get a tag or tags
    * Required parameters: none
    * Optional parameters:
        * tag_id
        * limit
        * offset
    * Limit and offset matter only when getting a list of tags
    * Return a tag if there is "tag_id" parameter or a list of tags otherwise in case of success

- GET /categories
    * Get a category or categories
    * Required parameters: none
    * Optional parameters:
        * category_id
        * limit
        * offset
    * Limit and offset matter only when getting a list of categories
    * Return a category if there is "category_id" parameter or a list of categories otherwise in case of success

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
    * "tag_in" and "tag_all" can have several values, they should be separated by ","
    * Return all posts (max at a time = 10) if there are no optional parameters. Return posts (max at a time = 10 or less if there is a limit parameter) only with corresponding parameters if there are optional paramaters. Return no post if there are no posts with such optional parameters

- GET /comments
    * Get comments to a post
    * Required parameters:
        * post_id
    * Optional parameters:
        * limit
        * offset
    * Return a list of comments to a post in case of success
</details>

<details>
<summary>Available for users (and admins)</summary>

- GET /users
    * Get a user
    * Required parameters:
        * token
    * Return a user in case of success

- POST users/avatar
    * Add an avatar to a user (or change it in case there is already an avatar)
    * Required parameters:
        * token 
    * Also there should be an image "avatar"
    * Return nothing in case of success

- POST /comments
    * Create a comment to a post
    * Required parameters:
        * token
        * post_id
        * text
    * Return nothing in case of success

- GET /drafts
    * Get a draft
    * Required parameters:
        * token
        * draft_id
    * Return a draft in case of success

- GET /drafts/id
    * Get all draft id by this author
    * Required parameters:
        * token 
    * Optional parameters:
        * limit
        * offset
    * Return a list of draft id by the author in case of success

- POST /drafts
    * Create a draft
    * Required parameters:
        * token
        * category_id
        * description
        * name
    * Optional parameters:
        * tag_id
    * Also there *may* be an image "main_photo"
    * "tag_id" can have serveral values, they should be separated by ","
    * Return a draft id in case of success

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
    * "tag_id" can have serveral values, they should be separated by ","
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