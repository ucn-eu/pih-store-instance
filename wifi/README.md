In order to have a locally running data store (assuming that mirage tool stack is installed):

1. put server certificates under tls/
2. from a terminal, type
```
mirage configure
mirage build
./mir-wifi
```
3. a unikernel should run locally with tls ports 4433, you can change them in the file server_config.ml