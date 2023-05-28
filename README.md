# Oys Command Line

## Build

Use GraaVM
> sdk use java 22.3.r19-grl

Build a fat jar
> scala-cli --power package --assembly oys.scala -f

Compile the native binary
> native-image --no-fallback -jar oys.jar oys

## Run

### Project
 1. Describe
 > oys project --describe

### Git 

 1. Git status
 > oys git --status

 
