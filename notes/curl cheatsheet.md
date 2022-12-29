
# Send a file
curl -X POST --data='@./file' localhost:3000

# Reissue requests when a redirect is supplied
curl -L localhost:3000

# Do not perform certificate validation
curl -k localhost:3000

# Save output to file with name of input
curl -O localhost:3000

# Print request/response parameters and arbitrary text in the output
# Prints newline
curl -w '\n' localhost:3000
# Prints http status code to stderr, followed by newline. %{stderr} changes output to stderr. Silent otherwise (-s).
curl -s -w '%{stderr}%{http_code}\n'
# See man curl for more parameters to -w

# Output control
# Print request/response headers
curl -i localhost:3000
# Print request/response headers and additional information from curl
curl -v localhost:3000

# Weather
IP geolocated:
```sh
curl wttr.in
```
Queries:
```sh
curl wttr.in/fr                         # France
curl wttr.in/lhr                        # airport codes
curl wttr.in/:help                      # help
curl wttr.in/courchevel
curl wttr.in/saint-martin-de-belleville
curl wttr.in/aotearoa
curl v2d.wttr.in/courchevel             # different visualisation
```
More: https://github.com/chubin/wttr.in
