# tonatona-sample

A example app using `tonatona` and its plugins.

```
$ ENV="Production" TONA_DB_CONN_STRING="postgresql://tonatonasample:foobar@localhost:5432/tonatonasample" make run
...
```

Run the example app using sqlite:

```
$ ENV="Production" TONA_DB_CONN_STRING=":memory:" DB_TO_USE="sqlite" make run
...
```

## Steps to install and set up postgres

Use the following steps to install postgres and set it up to be used with the sample app.

```sh
# Install postgres through your system's package manager.

# become postgres user
$ sudo -i -u postgres

# as postgres user, initialize the database
# (maybe only needed on arch linux?)
$ initdb --locale en_US.UTF-8 -E UTF8 -D '/var/lib/postgres/data'

# As the postgres user, comment out all lines in the following file and add the
# following line.  This disables local trust-based authentication (any local
# user can login as any database user without a password), and enables
# password-based authentication (but only from localhost).
$ echo 'host all all 127.0.0.1/32 md5' >> /var/lib/postgres/data/pg_hba.conf

# create the tonatonasample user for developement and testing
$ sudo -u postgres -- psql --command "CREATE ROLE tonatonasample NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN ENCRYPTED PASSWORD 'foobar'"

# create the tonatonasample db for developement
$ sudo -u postgres -- createdb tonatonasample

# grant access to tonatonasample db for tonatonasample user
$ sudo -u postgres -- psql --command "GRANT ALL PRIVILEGES ON DATABASE tonatonasample TO tonatonasample"

# restart postgres service
$ sudo systemctl restart postgresql
($ sudo service postgresql restart # Ubuntu)

# as normal user, try accessing database
$ psql -U tonatonasample -d tonatonasample -h 127.0.0.1
```

## Try sending requests to the server

A normal route that doesn't do anything special:

```ssh
$ curl localhost:8000/foo
1
```

A route that sends a redirect:

```sh
$ curl -v localhost:8000/redirect-example
...
< HTTP/1.1 302 Found
< Transfer-Encoding: chunked
< Date: Wed, 13 Jun 2018 06:45:09 GMT
< Server: Warp/3.2.22
< Location: https://google.com
```

A route that adds a tag to the database.  This adds the tag `hello` with the value `foobar`.

```sh
$ $ curl -v -X POST localhost:8000/tag/hello/foobar
[]
```

A route that gets all of a specific tag from the database.  This gets all the values for tags called `hello`:

```sh
$ curl localhost:8000/tag/hello
["foobar"]
```
