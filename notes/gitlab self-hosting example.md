
Create a docker network so the runner and server can communicate:
```sh
docker network create --driver=bridge gitlab
```

Start Gitlab
```sh
#!/usr/bin/env sh
GITLAB_HOME="/home/msk/projects/github.com/modusintegration/wso2is-populate/gitlab_home"
mkdir $GITLAB_HOME/{config,logs,data,runner}
docker run --detach \
  --hostname gitlab.example.com \
  --publish 8080:80 \
  --name gitlab \
  --network gitlab \
  --volume $GITLAB_HOME/config:/etc/gitlab \
  --volume $GITLAB_HOME/logs:/var/log/gitlab \
  --volume $GITLAB_HOME/data:/var/opt/gitlab \
  gitlab/gitlab-ee:latest
  #--publish 22:22 \
```
Wait a while for the server to come up.

Log in and chang password to _rootroot_ in browser at `http://localhost:8080`

Register the runner: https://docs.gitlab.com/runner/register/index.html#docker
```sh
docker run --rm -it \
  -v $GITLAB_HOME/runner:/etc/gitlab-runner \
  --network gitlab \
  gitlab/gitlab-runner register \
  --url http://gitlab \
  --executor docker \
  --docker-volumes /var/run/docker.sock:/var/run/docker.sock \
  --docker-image docker:latest \
  --docker-network-mode gitlab
```

When prompted, use the following (get the connection token from the top-right panel here:
http://localhost:8080/admin/runners):
```
Enter the GitLab instance URL (for example, https://gitlab.com/):
http://gitlab
Enter the registration token:
m_2zjnHuzBofx-3v4t6p
Enter a description for the runner:
[89507b4a3f3c]:
Enter tags for the runner (comma-separated):
Registering runner... succeeded                     runner=m_2zjnHu
Registering runner... succeeded                     runner=m_2zjnHu
Enter an executor: parallels, docker+machine, docker-ssh+machine, docker, docker-ssh, shell, ssh, virtualbox, kubernetes, custom:
docker
Enter the default Docker image (for example, ruby:2.6):
docker
Runner registered successfully. Feel free to start it, but if it's running already the config should be automatically reloaded!
```

Start the runner
```sh
docker run -d --name gitlab-runner \
  -v $GITLAB_HOME/runner:/etc/gitlab-runner \
  --mount type=bind,source=/var/run/docker.sock,destination=/var/run/docker.sock \
  --network gitlab \
  gitlab/gitlab-runner:latest
```

Created a test project with the following `.gitlab-ci.yml`:
```yaml
Show running docker images:
  stage: deploy
  image:
    name: docker
  script:
    - docker ps
```
