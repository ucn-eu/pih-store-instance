#!/bin/bash -ex

mirage clean -f config_mobile.ml || true
mirage configure -f config_mobile.ml -t xen --no-opam --ip 192.168.252.22 --network=192.168.252.0/24 --gateway=192.168.252.2 --logs=ucn.stb.mobile:info --persist-host 10.0.0.1 --persist-port 20004
mirage build -f config_mobile.ml
