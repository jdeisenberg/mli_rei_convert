# mli\_rei\_convert

A few days ago in the Reason discord chat, I was pointed to the documentation for [Belt](https://bucklescript.github.io/bucklescript/api/Belt.html). Belt is a stdlib shipped with BuckleScript and, as of this writing, is still in beta.

When I noted that the documentation was in OCaml/Bucklescript format, I asked about Reason format, and was told that it would be done. Someone else said that the examples would have to be parsed and converted. So I decided to try do something along those lines.

This is more of a proof-of-concept and a way for me to learn Reason than production code. When you run the code, you provide the name of a file ending in `.mli`; the output file will be the same name except ending in `.rei`.

The algorithm, in general, is that anything in `[...]` is a single line of Bucklescript code, and anything enclosed in `{[...]}` is a multi-line sequence of code. The code examples are presumed to be code rather than interface. If conversion to Reason fails, the original string is kept, surrounded by `!!!`.

The `mli_files` folder in this repository contains the `.mli` files for Belt, which you may use as input to the program.

# Build
```
bsb -make-world
```

# Run

```
node src/MLI_REI_convert.bs.js inputfile.mli
```
