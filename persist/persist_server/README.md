* build this irmin server with tools of your choice

   if using ocamlbuild, add a new file named `_tags` with the content
   ```
   true: package(irmin.http), package(irmin.unix), package(cohttp.lwt)
   ```
   then run `ocamlbuild -use-ocamlfind main.native`

* the server takes port number as the only argument, it will use the current working directory as the one to persist data, invocation could be like
   `./main.native <port> &`
