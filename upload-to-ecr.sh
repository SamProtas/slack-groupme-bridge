#!/usr/bin/env bash

docker build -t slack-groupme-bridge app-docker/
docker tag slack-groupme-bridge:latest 936149274534.dkr.ecr.us-east-1.amazonaws.com/slack-groupme-bridge:latest
docker push 936149274534.dkr.ecr.us-east-1.amazonaws.com/slack-groupme-bridge:latest