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
| pageSizes               |     —     | `PageSizes`                                | Page sizes                                       |

### `ConnectInfo`
| Parameter |  Default  | Type     |
|-----------|:---------:|----------|
| host      | localhost | `String` |
| port      |    5432   | `Int`    |
| user      |  postgres | `String` |
| password  |     —     | `String` |
| database  |  postgres | `String` |

### `PageSizes`
| Parameter  | Default | Type  |
|------------|:-------:|-------|
| users      |    20   | `Int` |
| authors    |    20   | `Int` |
| posts      |    20   | `Int` |
| comments   |    20   | `Int` |
| categories |    20   | `Int` |
| tags       |    20   | `Int` |


## Authentication
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
| Method             |  Access  | Parameters                                                             | Returns                             |
|--------------------|:--------:|------------------------------------------------------------------------|-------------------------------------|
| register           | *public* | `username`, `lastname`, `avatar`, `password`                           | `null`                              |
| login              | *public* | `username`, `password`                                                 | Token                               |
| makeAuthor         |  *admin* | `username`, `description`                                              | `null`                              |
| getAuthors         |  *admin* | [`page`]                                                               | Page of all authors                 |
| editAuthor         |  *admin* | `previous_username`, [`username`], [`description`]                     | `null`                              |
| deleteAuthor       |  *admin* | `username`                                                             | `null`                              |
| getTags            | *public* | [`page`]                                                               | Page of all tags                    |
| createTag          |  *admin* | `tag`                                                                  | Tag id                              |
| editTag            |  *admin* | `id`, `tag`                                                            | `null`                              |
| deleteTag          |  *admin* | `id`                                                                   | `null`                              |
| getCategories      | *public* | `id`, [`page`]                                                         | Page of subcategories of a category |
| createCategory     |  *admin* | `name`, [`parent_id`]                                                  | Category id                         |
| editCategory       |  *admin* | `id`, [`name`], [`parent_id`]                                          | `null`                              |
| deleteCategory     |  *admin* | `id`                                                                   | `null`                              |
| getUsers           | *public* | [`page`]                                                               | Page of all users                   |
| deleteUser         |  *admin* | `username`                                                             | `null`                              |
| getDraft           | *author* | `id`                                                                   | Author's post or draft              |
| getDrafts          | *author* | [`page`]                                                               | Page of author's posts and drafts   |
| createPost         | *author* | `title`, `category_id`, `content`, `main_image`, `images` (array)      | Post id                             |
| editPost           | *author* | `id`, [`title`], [`category_id`], [`text`], [`main_image`], [`images`] | `null`                              |
| publishPost        | *author* | `id`                                                                   | `null`                              |
| deletePost         | *author* | `id`                                                                   | `null`                              |
| attachTag          | *author* | `tag_id`, `post_id`                                                    | `null`                              |
| deattachTag        | *author* | `tag_id`, `post_id`                                                    | `null`                              |
| post/{id}          | *public* |                                                                        | Published post                      |
| post/{id}/comments | *public* | [`page`]                                                               | Page of comments                    |
| posts              | *public* | [`page`], see others in search parameters table                        | Page of published posts             |
| addComment         |  *user*  | `post_id`, `comment`                                                   | Comment id                          |
| deleteComment      |  *admin* | `id`                                                                   | `null`                              |

### Search parameters
| Parameter      | Type                                                         | Description                                         |
|----------------|--------------------------------------------------------------|-----------------------------------------------------|
| sort           | `ByDate` \| `ByAuthor` \| `ByCategory` \| `ByNumberOfImages` | Sort by                                             |
| sort_reversed  | `Bool`                                                       | Reverse sort order                                  |
| tags__in       | Array of `Int`                                               | At least one tag present                            |
| tags__all      | Array of `Int`                                               | All tags are present                                |
| tag            | `Int`                                                        | Exact tag                                           |
| created_at     | `Date` (YYYY-MM-DD)                                          | Creation date                                       |
| created_at__gt | `Date` (YYYY-MM-DD)                                          | Creation date lower bound                           |
| created_at__lt | `Date` (YYYY-MM-DD)                                          | Creation date upper bound                           |
| author_name    | `String`                                                     | Author name                                         |
| category_id    | `Int`                                                        | Category id                                         |
| title          | `String`                                                     | Search in title                                     |
| text           | `String`                                                     | Search in content                                   |
| search         | `String`                                                     | Search in title, content, tags, category and author |


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

./getCategories [parent_id]
./createCategory categoryname [parent_id]
./editCategory categoryname cid
./deleteCategory cid

./getUsers [page]
./deleteUser username

./post pid
./getDraft pid
./getDrafts [page]
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
