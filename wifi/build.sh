#!/bin/bash -ex

mirage clean || true
mirage configure -t xen --no-opam --ip 192.168.252.10 --network 192.168.252.10/24  --gateway 192.168.252.2 --logs ucn.wifi:info --persist-host 10.0.0.1 --persist-port 20005
mirage build
