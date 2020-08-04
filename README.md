# JSON api server for blog-like platform
This repo contains my attempt to learn haskell by creating simple json api server using postgreSQL as database.
```shell
stack build
stack test
stack exec server-exe -- config.yml
```

## Configuration
This server can be configured via YAML config file, that is read as the first command line argument.
The file may contain an object with such fields:

| Parameter               |  Default  | Type                                       | Description                                      |
|-------------------------|:---------:|--------------------------------------------|--------------------------------------------------|
| database                |     —     | `ConnectInfo`                              | Object with info about database connection       |
| port                    |    3000   | `Port`                                     | Port to listen to                                |
| number_of_secrets       |     2     | `Int`                                      | The pool of random secrets to generate tokens    |
| secrets_update_interval |     60    | `Int` in minutes                           | Interval to refresh the pool and expiration time |
| backdoor                |  `false`  | `Bool`                                     | Bypass token verification entirely               |
| log_level               | `Warning` | `Debug` \| `Info` \| `Warning`  \| `Error` | Log level                                        |
| log_file                |     —     | Optional `String`                          | Log file, stdout otherwise                       |
| db_refresh_interval     |    1000   | Optional `Int` in ms                       | Update materialized views in database            |

Where `ConnectInfo` is
| Parameter |  Default  | Type     |
|-----------|:---------:|----------|
| host      | localhost | `String` |
| port      |    5432   | `Int`    |
| user      |  postgres | `String` |
| password  |     —     | `String` |
| database  |  postgres | `String` |

## Authorization
This server uses [JWT](https://jwt.io/) based one level token authentication. Registration is done with `register` method.
These credentials are used with `login` method to receive a token. That token contains user name, admin and author rights
and is valid for `secrets_update_interval` minutes.
Pass it as additional query parameter (`/method?token=934fh93h7f`) for methods that require it.
For the testing purposes you should enable `backdoor` option to bypass authentication and appear as admin in the system.


## Methods
All methods are GET requests returning JSON object

On success:
```json
{
   "ok" : true,
   "response" : "response object"
}
```

On error:
```json
{
   "ok" : false,
   "code" : 400,
   "error" : "Bad Request"
}
```

### All methods
| Method              |  Access  | Parameters                                                 | Returns                             |
|---------------------|:--------:|------------------------------------------------------------|-------------------------------------|
| register            | *public* | `username`, `lastname`, `password`                         | `null`                              |
| login               | *public* | `username`, `password`                                     | Token                               |
| makeAuthor          |  *admin* | `username`, `description`                                  | `null`                              |
| getAuthors          |  *admin* | [`page`]                                                   | Page of all authors                 |
| editAuthor          |  *admin* | `username`, `description`                                  | `null`                              |
| deleteAuthor        |  *admin* | `username`                                                 | `null`                              |
| getTags             | *public* | [`page`]                                                   | Page of all tags                    |
| createTag           |  *admin* | `name`                                                     | Tag id (`tag`)                      |
| editTag             |  *admin* | `tag`, `name`                                              | `null`                              |
| deleteTag           |  *admin* | `tag`                                                      | `null`                              |
| getCategories       | *public* | [`cid`], [`page`]                                          | Page of subcategories of a category |
| createCategory      |  *admin* | `name`, [`cid`]                                            | Category id (`cid`)                 |
| editCategory        |  *admin* | `name`, `cid`                                              | `null`                              |
| deleteCategory      |  *admin* | `cid`                                                      | `null`                              |
| getUsers            | *public* | [`page`]                                                   | Page of all users                   |
| createUser          |  *admin* | `username`, `lastname`, `password`                         | `null`                              |
| deleteUser          |  *admin* | `username`                                                 | `null`                              |
| getPost             | *author* | `pid`                                                      | Author's post or draft              |
| getPosts            | *author* | [`page`]                                                   | Page of author's posts and drafts   |
| createPost          | *author* | `title`, `cid`, `text`, `image`, `images` (array)          | Post id (`pid`)                     |
| editPost            | *author* | `pid`, [`title`], [`cid`], [`text`], [`image`], [`images`] | `null`                              |
| publishPost         | *author* | `pid`                                                      | `null`                              |
| deletePost          | *author* | `pid`                                                      | `null`                              |
| attachTag           | *author* | `cid`, `pid`                                               | `null`                              |
| deattachTag         | *author* | `cid`, `pid`                                               | `null`                              |
| post/{pid}          | *public* |                                                            | Published post                      |
| post/{pid}/comments | *public* | [`page`]                                                   | Page of comments                    |
| posts               | *public* | [`page`], see others in search parameters table            | Page of published posts             |
| addComment          |  *user*  | `pid`, `text`                                              | Comment id (`cid`)                  |
| deleteComment       |  *user*  | `cid`                                                      | `null`                              |

### Search parameters
| Parameter      | Type                                                         | Description                                         |
|----------------|--------------------------------------------------------------|-----------------------------------------------------|
| sort           | `ByDate` \| `ByAuthor` \| `ByCategory` \| `ByNumberOfImages` | Sort by                                             |
| sort_reversed  | `Bool`                                                       | Reverse sort order                                  |
| tags__in       | Array of `tag`                                               | At least one tag present                            |
| tags__all      | Array of `tag`                                               | All tags are present                                |
| tag            | `tag`                                                        | Exact tag                                           |
| created_at     | Date (2020-07-31)                                            | Creation date                                       |
| created_at__gt | Date (2020-07-31)                                            | Creation date lower bound                           |
| created_at__lt | Date (2020-07-31)                                            | Creation date upper bound                           |
| author_name    | String                                                       | Author name                                         |
| cid            | `cid`                                                        | Category id                                         |
| title          | String                                                       | Search in title                                     |
| text           | String                                                       | Search in content                                   |
| search         | String                                                       | Search in title, content, tags, category and author |


## Queries to test yourself
You can test methods using shorthand curl scripts in `queries/`. Don't forget to enable server backdoor for methods that require token. Configuration is available via environment variables.

| Environment variable | Default               | Description                                              |
|----------------------|-----------------------|----------------------------------------------------------|
| SERVER_URL           | http://localhost:3000 | Server host url                                          |
| JSON_PRETTIFIER      | json_pp               | Json pretty printer to format output (use `cat` for raw) |

Here is the usage
```sh
./register username lastName password
./login username password

./makeAuthor
./getAuthors
./editAuthor
./deleteAuthor

./getAuthors [page]
./makeAuthor username description
./editAuthor username description
./deleteAuthor username

./getTags [page]
./createTag tagname
./editTag tagid tagname
./deleteTag tagid

./getCategories [cid]
./createCategory categoryname [cid]
./editCategory categoryname cid
./deleteCategory cid

./getUsers [page]
./createUser username lastName password
./deleteUser username

./post pid
./getPost pid
./getPosts [page]
./createPost title cid content image images
./attachTag tid pid
./deattachTag tid pid
./editPost pid title cid content image images
./publishPost pid
./deletePost pid
./posts

./getComments pid [page]
./addComment pid text
./deleteComment cid
```
