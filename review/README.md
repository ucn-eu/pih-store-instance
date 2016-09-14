#### To build this unikernel

Using the script `build.sh`, inside this script you could change the configuration for its network stack and the ip address and port number where it will persist its data to.


#### RESTful APIs

`POST /create/<id>` creates a movie reviwe in the data store and makes its key as `<id>`, the request body needs to have a json object with the fields `id`, `title`, `rating`, and `comment`.

`GET /read/<id>` reads the movie review whose id is `<id>`.

`POST /update/<id>` updates the moview review whose id is `<id>`, the request body has to have the json object with the same format as for the creaation API.

`POST /delete/<id>` deletes the movie review whose id is `<id>`.

`GET /meta` reads the meta data for movie reviews.

`GET /list` reads all the IDs of the reviews currently in the data store.
