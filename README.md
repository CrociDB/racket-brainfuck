# A Brainfuck Interpreter written in Racket

A simple brainfuck interpreter written in Racket for study purposes. It doesn't use the parsing features of Racket, though.

## How to Run

There are a few examples inside the `examples/` dir, and it can be run as:

```
$ racket main.rkt examples/helloworld.bf
```

## Command Line Parameters

Additionally to the path of the file to execute, you can also set the number of cells available for the interpreter. The default value is 100.

```
$ racket main.rkt --help
usage: brainfuck interpreter [ <option> ... ] <filename>

<option> is one of

  -c <c>, --num-cells <c>
     set the number of cells. the default is 100
  --help, -h
     Show this help
  --
     Do not treat any remaining argument as a switch (at this level)

 /|\ Brackets indicate mutually exclusive options.

 Multiple single-letter switches can be combined after
 one `-`. For example, `-h-` is the same as `-h --`.
```
