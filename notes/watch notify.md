For example on a Rust project:
```sh
# The sleep allows files to be closed by the previous process- I'm not sure why they remain open
find src -type f | entr -rs 'sleep 0.5; notify-send "$(cargo check 2>&1)"'
```
