# webserver

## How to run
Before the first run, 2 config files should be filled. They are both in the folder **Configs**.
The first file is **Server.config** that contains information about a port and a host.
The second file is **Database.config** that contains information about a database.
Also it may be necessary to use a command stack setup.

After these two files are filled, the project may be started by typing *stack run 1* into command line while being in the project folder. The number 1 is needed to run database migrations that will be executed according to settings in the file Database.config.

For the next runs when database migration is not needed, the project starts by typing *stack run* without additional arguments.

## Basic structure 
The project has 3 main parts:
- Server part
- Handler part
- Database part

All three parts are splitted into files with the same names. For example, there are 4 files with a name *Tag.hs*: one in the Server part, one in the Handler part, one in the Database part and one in the Types. All of these 4 files are responsible for actions with tags. But each of them do his part of the job.


*Server part*

The file **Server.hs** gets a user request, considers a path and a method within a request and then calls a requested function from one of the server part files. These files are found in the folder */src*.
Each of these functions gets a query and, if necessary, a body of request from the **Server.hs**. Then they try to parse these data using functions from **Utility.hs** and then check if all needed data are presented. If not, they give a negative answer to user. Otherwise they call functons from *Handler part* and give an answer to user according to results.


*Handler part*

Files from this part are in the folder */Handler*. These files contain the main logic of the project. They get data from the server part and perform actions with a help of *Database part*, then return results to the server part. 


*Database part*

This part is responsible for work with database and contains queries to a database. They are stored in the folder */Database/Queries*. They are used to insert, update, delete or get data from a database.
**Connection.hs** is responsible for getting database config and openning/closing connection to a database.
**Migration.hs** is responsible for applying migrations to a database.


Additional parts are folders */Types*, */scripts*, */DatabaseMigrations* and */test*:
- *Types* files contain created types that used in this project.
- *Scripts* contains sh scripts with curl requests.
- *DatabaseMigrations* contains sql files that needed to make migrations.
- *Test* contains tests for the Handler part.
