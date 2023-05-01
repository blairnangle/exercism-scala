#!/bin/zsh

sbt clean compile test
sbt scalafmt
