
#### Update
```sql
UPDATE tbl SET col = 5;
```

#### Update in-place using existing value
```sql
UPDATE tbl SET col = col + 5;
```

#### Insert or update with result of query
```sql
INSERT INTO
    tbl1 (col1, col2)
    SELECT
        col3 AS col1, col4 AS col2
    FROM
        tbl2
    WHERE
        col3 < 5, col4 > 10;
```

#### Insert data computed from static input and existing data
```sql
-- Create a temporary table to hold your data.
CREATE TEMPORARY TABLE tmpTbl (id INT(10), value VARCHAR(255));
INSERT INTO tmpTbl VALUES
(1, 'valueOne'),
(2, 'valueTwo');

-- Insert normally.
INSERT INTO existingTblOne et1
SELECT *
FROM existingTblTwo et2
CROSS JOIN tmpTbl;

-- Discard the temporary table. Not strictly necessary.
DROP TABLE tmpTbl;
```

#### String functions

##### Substring
```sql
SUBSTRING(str, pos, len)
```
or
```sql
SUBSTRING(str FROM pos FOR len)
```

##### Replace
```sql
REPLACE(str, find_string, replace_with)
```

##### Locate
```sql
LOCATE (search str, str, [position])
```
