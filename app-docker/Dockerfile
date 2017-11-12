FROM ubuntu:16.04

LABEL author="Sam Protas" email="sam.protas@gmail.com"

RUN apt-get update && apt-get install -y libgmp-dev netbase ca-certificates

COPY slack-groupme-bridge-exe /bin/slack-groupme-bridge-exe
COPY .env /var/local/.env

EXPOSE 5000 5000

WORKDIR /var/local/
ENTRYPOINT ["slack-groupme-bridge-exe"]
