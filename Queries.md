# Queries to test yourself
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
