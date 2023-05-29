# Oys Command Line

## Build

Use GraaVM
> sdk use java 22.3.r19-grl

Build a fat jar
> scala-cli --power package --assembly oys.scala -f

Install the native image (and its prerequisites. See https://www.graalvm.org/22.0/reference-manual/native-image/)
> gu install native-image

Compile the native binary
> native-image --no-fallback -jar oys.jar oys

## Run

### Project
 1. Describe
 > oys project --describe

### Git 

 1. Git status
 > oys git --status

 
