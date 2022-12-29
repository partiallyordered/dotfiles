For example, when a long-running process is still running in another session that's not accessible
and it's desirable to provide input to it, or view its stdout or stderr.

Use `reptyr`
```sh
nix-shell -p reptyr
reptyr $(pgrep tinyproxy)
```

References:
- https://www.baeldung.com/linux/attach-terminal-detached-process
- https://unix.stackexchange.com/questions/58550/how-to-view-the-output-of-a-running-process-in-another-bash-session
- https://stackoverflow.com/questions/715751/attach-to-a-processes-output-for-viewing
