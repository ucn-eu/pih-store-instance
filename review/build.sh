#!/bin/bash -ex

mirage clean || true
mirage configure -t xen --no-opam --ip=192.168.252.11 --netmask=255.255.255.0 --gateways=192.168.252.2 --logs=ucn.review:info --persist-host 10.0.0.1 --persist-port 10002
mirage build
