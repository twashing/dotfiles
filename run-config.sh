#!/bin/sh
image_path="$(guix system image --network --image-type=docker $1)"
image_id="$(docker load < $image_path | cut -f3 -d ' ')"

# docker run -ti $image_id /run/current-system/profile/bin/bash --login

container_id="$(docker create -p 8080:8080/tcp --privileged $image_id)"
docker start $container_id
docker exec -ti $container_id /run/current-system/profile/bin/bash --login
