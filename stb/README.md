### API Exposed

#### For Box

`GET /` returns string `<xml>true</xml>`

`GET /<yy>/<mm>/<dd>` concatenates all the data received on the same day, and return them as a json array

`GET /<yy>/<mm>/<dd>/<tunetime>` returns the data indexed by the specific `tunetime`, if there is no such data, return a `404 NOT FOUND`

`POST /endpoint` configures the endpoint to publish the data as soon as they reaches the PIH, in the body, there should be a json object who has a field `uri`, and the value for `uri` will be used to get the endpoint

`POST /<yy>/<mm>/<dd>/<tunetime>` writes the data to the store, the server will try to get the value for the header `UA-DeviceId`, and also collect all the form data from the body. Before writing to the store, if the endpoint has been configured, the data will also be published to it.


#### For Mobile

`GET /<yy>/<mm>/<dd>` concatenates all the data received on the same day, and return them as a json array

`GET /<yy>/<mm>/<dd>/<tunetime>` returns the data indexed by the specific `tunetime`, if there is no such data, return a `404 NOT FOUND`

`POST /endpoint` configures the endpoint to publish the data as soon as they reaches the PIH, in the body, there should be a json object who has a field `uri`, and the value for `uri` will be used to get the endpoint

`POST /<yy>/<mm>/<dd>/<tunetime>` writes the data to the store, the server will put the whole body directly into the store
