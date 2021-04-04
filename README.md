# Engage Search Filter Parser

This application enables you to parse an Engage Search Filter and pretty-print it. This then enables you to inspect the query in a readable form.


## Building

```sh
$ stack build
```


## Running

Just parsing and outputting the tree:

```sh
$ pbpaste | stack exec -- queryparser-exe
```

Pretty printing:

```sh
$ pbpaste | stack exec -- queryparser-exe --pretty
```

Outputting Disjunct Normal Form:

```sh
$ pbpaste | stack exec -- queryparser-exe --dnf
```
