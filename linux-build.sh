#!/usr/bin/env bash

cp ./stack.yaml ./linux-stack.yaml
printf "\n\ndocker:\n    enable: true\n    auto-pull: true\n" >> ./linux-stack.yaml

stack --stack-yaml linux-stack.yaml build

DIST_DIR=$(stack --stack-yaml linux-stack.yaml path --dist-dir)
cp "${DIST_DIR}/build/slack-groupme-bridge-exe/slack-groupme-bridge-exe" app-docker/slack-groupme-bridge-exe
cp .env app-docker/.env