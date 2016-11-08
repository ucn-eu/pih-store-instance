#!/bin/bash -ex

mirage clean -f config_box.ml || true
mirage configure -f config_box.ml -t xen --no-opam --ip 192.168.252.21 --network=192.168.252.0/24 --gateway=192.168.252.2 --logs=ucn.stb.box:info --persist-host 10.0.0.1 --persist-port 20003
mirage build -f config_box.ml
