
Spin up a mysql instance:
```sh
podman run -p 3306:3306 -e MYSQL_ROOT_PASSWORD=wasspord --rm -it mysql:8.0.26
```
Create the database:
```sh
mycli -u root -h localhost --password wasspord -P 3306 -e 'create database testdb'
```
Run the application:
```sh
./gradlew bootRun
```

### TODO
- add a Makefile to automate the above steps
