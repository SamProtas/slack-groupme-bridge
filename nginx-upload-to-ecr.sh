#!/usr/bin/env bash

docker build -t slack-groupme-nginx nginx-docker/
docker tag slack-groupme-nginx:latest 936149274534.dkr.ecr.us-east-1.amazonaws.com/slack-groupme-nginx:latest
docker push 936149274534.dkr.ecr.us-east-1.amazonaws.com/slack-groupme-nginx:latest