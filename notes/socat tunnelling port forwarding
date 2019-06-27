
#### Listen on port 8000, forward all connections to localhost port 3000
```bash
socat -d -d 'TCP-LISTEN:8000,reuseaddr,fork' 'TCP:localhost:3000'
```

#### Have a chat over your new tunnel
```bash
# In one terminal:
nc -4 -l 3000
# In another terminal:
nc -4 localhost 8000
# Now type into either terminal and press return
```
