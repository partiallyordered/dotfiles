
#### Connecting
```sh
mysql \
    --password=password-here \
    -h hostname-here \
    -u username-here \
    -D database-name
```

#### Running a sql script, output in terminal
```sh
mysql \
    -t \ # print the table outline
    --password=password-here \
    -h hostname-here \
    -u username-here \
    -D database-name < script.sql
```

Pretty print with table outline:
```
•  --table, -t
   Display output in table format. This is the default for interactive use, but can be used to produce table output in batch mode.
```

Vertical mode (very expanded display for results with many columns):
```
•  --vertical, -E
   Print query output rows vertically (one line per column value). Without this option, you can specify vertical output for individual statements by terminating them with \G.
```

#### List databases
```
MySQL > show databases;
```

#### List tables
```
MySQL [database-name]> show tables;
```

#### Help (question mark)
```
MySQL [database-name]> ?
```

#### Server-side help
```
MySQL [database-name]> help contents
```

#### Show table with column comments
```sql
SHOW FULL COLUMNS FROM $table_name;
```

#### Show table create instruction (useful to see constraints)
```sql
SHOW CREATE TABLE $table_name;
```

#### Show indexes for table
```sql
SHOW INDEXES IN $table_name;
```

#### Upsert
Do nothing if a unique key already exists:
```sql
INSERT IGNORE INTO users (name, age) VALUES ('ole jimmy', 93);
```
Update existing data in-place:
```sql
INSERT INTO users (name, age) VALUES ('ole jimmy', 93) ON DUPLICATE KEY UPDATE age=age+1;
```
Delete the existing row, then insert the new row, when a unique key already exists:
```sql
REPLACE INTO users (name, age) VALUES ('ole jimmy', 93);
```

#### Grant privileges on a schema
```sql
GRANT ALL PRIVILEGES ON schema_name.* TO 'username'@'host';
```
when 'username' is logged in via 'host'. For username from any host:
```sql
GRANT ALL PRIVILEGES ON schema_name.* TO 'username'@'%';
```

#### Which tables reference this one?
```sql
select table_name
from information_schema.KEY_COLUMN_USAGE
where table_schema = 'my_database'
and referenced_table_name = 'my_table_here';
```

#### Get/set default character set
From: https://stackoverflow.com/a/1049958
And: https://www.a2hosting.co.uk/kb/developer-corner/mysql/convert-mysql-database-utf-8
For schemas:
```sql
-- get
SELECT default_character_set_name FROM information_schema.SCHEMATA
WHERE schema_name = "schemaname";
-- set
ALTER DATABASE DBNAME CHARACTER SET utf8 COLLATE utf8_general_ci;
```
For tables:
```sql
SELECT CCSA.character_set_name FROM information_schema.`TABLES` T,
       information_schema.`COLLATION_CHARACTER_SET_APPLICABILITY` CCSA
WHERE CCSA.collation_name = T.table_collation
  AND T.table_schema = "schemaname"
  AND T.table_name = "tablename";
```
For columns:
```sql
SELECT character_set_name FROM information_schema.`COLUMNS` 
WHERE table_schema = "schemaname"
  AND table_name = "tablename"
  AND column_name = "columnname";
```


### mycli

#### Copy query results to clipboard
In the `mycli` CLI, set the pager to `xclip`. The query will be sent to the clipboard per the
`xclip` configuration.
```
> pager xclip
> select * from user;

33 rows in set
> pager less
```