
#### Run Ephemeral MySQL

Note that some of the options here might be undesirable for a given use-case. Especially
`--lower_case_table_names=1`, which is useful for restoring dumps taken from a mysql installation
on a machine without a case-sensitive file-system (Windows).

Start the image:
```sh
docker run --rm -i --env MYSQL_ALLOW_EMPTY_PASSWORD=TRUE -p 3306:3306 mysql:5.7.27 mysqld --lower_case_table_names=1 --character-set-server=utf8 --collation-server=utf8_general_ci
```

Access the image from the docker host:
```sh
mysql -u root -h 0.0.0.0
```

#### Create DB Dump
```sh
mysqldump \
    --single-transaction \
    -h localhost \
    -P 3306 \
    -u root \
    -p'root' \
    --databases first_db_to_dump second_if_you_like \
    > uat-dump-$(date -u "+%Y-%m-%d-%H-%M").sql
```

#### Restore Dump
```sh
mysql -h 0.0.0.0 -P 3306 -u root < uat-dump-$name.sql
```