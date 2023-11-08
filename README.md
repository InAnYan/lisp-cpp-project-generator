# C++ project generator
A simple program written in Lisp that generates C++ project files.

## Usage:
```sh
sbcl --script cpp-project-generator.lisp path
```
Where: `path` - path to the description of the project.

## Grammar of description file
```
node
```
Where `node` is one of the following:
```
(namespace NAME  ; NAME is a string.
  node*)
```
or
```
(klass NAME)  ; NAME is a string.
```
or
```
(exception NAME  ; NAME is a string.
  var*)
```
Where `var` is:
```
(TYPE NAME)  ; TYPE and NAME are strings.
```

## Generation of project tree
- Every `namespace` corresponds to a new folder and a C++ namespace.
- Every `klass` corresponds to two files in the same directory: header (`.hpp`) and implementation (`.cpp`). The class will have namespaces as specified in the description.
- Every `exception` is like a `klass` but it is tuned to exception. It generates appropriate getters and setters for user-defined fields. For more information try running `example.lisp` file.

## Files:
- `cpp-project-generator.lisp`: main executable.
- `files-generator.lisp`: functions that generate files.
- `macro-reader.lisp`: transforms description files into correct Lisp program that calls functions from `files-generator.lisp`.

## Extra
This repository contatins file `start-project.lisp` which initializes an empty C++ project.
