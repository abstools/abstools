---
title: "Running the Collaboratory using Docker"
date: 2018-08-15T09:20:37+02:00
---

The collaboratory is a browser-based development environment for ABS.  It
integrates an editor, the syntax checker and simulator, and the SACO resource
analysis tool.

## Running the Collaboratory using Docker

To run the collaboratory, first install docker from https://www.docker.com.  Then, run the following command in a terminal window:

```bash
docker run -d --rm -p 8080:80 --name collaboratory abslang/collaboratory:latest
```

When the command has finished, connect a browser to http://localhost:8080 and
start using ABS!

To stop the collaboratory, use the following command:

```bash
docker stop collaboratory
```

