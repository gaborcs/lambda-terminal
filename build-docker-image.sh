#!/usr/bin/env bash

stack image container --docker &&
docker build -t lambdaland/lambda-terminal .
