
```sh
nix-shell -p spring-boot-cli
spring init --build=gradle --dependencies=web,data-jpa,mysql my-project
cd my-project
```

Use `spring init --list` to see the list of available dependencies, build options, etc.

Add the following to application.properties:
```properties
spring.jpa.hibernate.ddl-auto=update
spring.datasource.url=jdbc:mysql://localhost/testdb
spring.datasource.username=root
spring.datasource.password=wasspord
spring.datasource.driver-class-name=com.mysql.jdbc.Driver
```

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

### References
- https://spring.io/guides/gs/accessing-data-mysql/
