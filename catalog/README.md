#### To build this unikernel

Using the script `build.sh`, inside this script you could change the configuration for its network stack and the ip address and port number where it will persist its data to.

During runtime, the unikernel will also need some information about other data holding unikernels and the MUSE service endpoint that it could talk to. To do this, you may need to create your own configuration file with the name `dom_configs.ml` as suggested [here](https://github.com/sevenEng/pih-store-instance/blob/master/catalog/unikernel.ml#L196). Here is a simplest file used during test:

```ocaml
let domain_lst = [
  "review", "https://192.168.252.11:8443";
]

let muse_endpoint = "http://10.0.0.1:8888"

```

The catalog unikernel needs `domain_lst` to get the meat data from each of the listed unikernels. And `muse_endpoint` is the address and port number that the MUSE service listens to.


#### RESTful APIs

`GET /users` returns a list of user IDs that the data owner could choose to delegate to or revoke from the search right of his/her meat data.

`POST /upload` asks to upload meta data to the MUSE cloud, in the request body there need to be a json object with fields `file_id` and `data`.

`GET /list` gets the list of meta data that could be uploaded.

`GET /read/meta/<domain>` reads the meta data from the domain `<domain>`.

`POST /delegate` delegates the search right to a user for a uploaded file, the request body needs to contain a json object with fields `file_id` and `user_id`.

`POST /revoke` revokes the search right from a user for a uploaded file, the request body needs to contain a json object with fields `file_id` and `user_id`.
