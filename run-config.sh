#!/bin/sh
image_path="$(guix system image --network --image-type=docker $1)"
image_id="$(sudo docker load < $image_path | cut -f3 -d ' ')"

# sudo docker run -ti $image_id /run/current-system/profile/bin/bash --login

container_id="$(sudo docker create -p 8080:8080/tcp --privileged $image_id)"
sudo docker start $container_id
sudo docker exec -ti $container_id /run/current-system/profile/bin/bash --login
