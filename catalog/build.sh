#!/bin/bash -ex

mirage clean || true
mirage configure -t xen --no-opam --persist-host 10.0.0.1 --persist-port 10003 --ip=192.168.252.12 --netmask=255.255.255.0 --gateways=192.168.252.2 --logs=ucn.catalog:info
mirage build
