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
| make_author         |  *admin* | `username`, `description`                                  | `null`                              |
| get_authors         |  *admin* | [`page`]                                                   | Page of all authors                 |
| edit_author         |  *admin* | `username`, `description`                                  | `null`                              |
| delete_author       |  *admin* | `username`                                                 | `null`                              |
| get_tags            | *public* | [`page`]                                                   | Page of all tags                    |
| create_tag          |  *admin* | `name`                                                     | Tag id (`tag`)                      |
| edit_tag            |  *admin* | `tag`, `name`                                              | `null`                              |
| delete_tag          |  *admin* | `tag`                                                      | `null`                              |
| get_categories      | *public* | [`cid`], [`page`]                                          | Page of subcategories of a category |
| create_category     |  *admin* | `name`, [`cid`]                                            | Category id (`cid`)                 |
| edit_category       |  *admin* | `name`, `cid`                                              | `null`                              |
| delete_category     |  *admin* | `cid`                                                      | `null`                              |
| get_users           | *public* | [`page`]                                                   | Page of all users                   |
| create_user         |  *admin* | `username`, `lastname`, `password`                         | `null`                              |
| delete_user         |  *admin* | `username`                                                 | `null`                              |
| get_post            | *author* | `pid`                                                      | Author's post or draft              |
| get_posts           | *author* | [`page`]                                                   | Page of author's posts and drafts   |
| create_post         | *author* | `title`, `cid`, `text`, `image`, `images` (array)          | Post id (`pid`)                     |
| edit_post           | *author* | `pid`, [`title`], [`cid`], [`text`], [`image`], [`images`] | `null`                              |
| publish_post        | *author* | `pid`                                                      | `null`                              |
| delete_post         | *author* | `pid`                                                      | `null`                              |
| attach_tag          | *author* | `cid`, `pid`                                               | `null`                              |
| deattach_tag        | *author* | `cid`, `pid`                                               | `null`                              |
| post/{pid}          | *public* |                                                            | Published post                      |
| post/{pid}/comments | *public* | [`page`]                                                   | Page of comments                    |
| posts               | *public* | [`page`], see others in search parameters table            | Page of published posts             |
| add_comment         |  *user*  | `pid`, `text`                                              | Comment id (`cid`)                  |
| delete_comment      |  *user*  | `cid`                                                      | `null`                              |

### Search parameters
| Parameter      | Type                                                         | Description                                         |
|----------------|--------------------------------------------------------------|-----------------------------------------------------|
| sort           | `ByDate` \| `ByAuthor` \| `ByCategory` \| `ByNumberOfImages` | Sort by                                             |
| sort_reversed  | `ByDate` \| `ByAuthor` \| `ByCategory` \| `ByNumberOfImages` | Sort by reversed                                    |
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

./make_author
./get_authors
./edit_author
./delete_author

./get_authors [page]
./make_author username description
./edit_author username description
./delete_author username

./get_tags [page]
./create_tag tagname
./edit_tag tagid tagname
./delete_tag tagid

./get_categories [cid]
./create_category categoryname [cid]
./edit_category categoryname cid
./delete_category cid

./get_users [page]
./create_user username lastName password
./delete_user username

./post pid
./get_post pid
./get_posts [page]
./create_post title cid content image images
./attach_tag tid pid
./deattach_tag tid pid
./edit_post pid title cid content image images
./publish_post pid
./delete_post pid
./posts

./get_comments pid [page]
./add_comment pid text
./delete_comment cid
```
