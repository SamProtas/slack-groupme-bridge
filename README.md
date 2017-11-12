# slack-groupme-bridge

A messaging bridge connecting a GroupMe Group with a Slack Channel.

As part of an attempt to migrate my friends' group message from GroupMe to Slack I'm building this. The hope is this
will make it possible for a portion of the group to migrate to Slack without fragmenting the conversation. Then we can
peer pressure the hold outs 😏.



## Local Dev
To test small changes, use the repl:

```bash
stack repl
```

To build for prod and run a production build locally (via docker):

```bash
./linux-build.sh && docker-compose up --build
```

## Deployment

App deployment:
```bash
./linux-build.sh && ./upload-to-ecr.sh
```

Nginx deployment:
```bash
./nginx-upload-to-ecr.sh
```