HOBiT
======

A prototype implementation of a higher-order bidirectional
programming language HOBiT.

How to Build
-------------

Install [`stack`](https://docs.haskellstack.org/en/stable/README/) first. Then, type the following command.

```
stack build
```


How to Use
----------

Just run `hobit`. Then, one will see the following prompt of HOBiT's
read-eval-print loop.

    HOBiT>

You can type expressions to evaluate.

    HOBiT> 1 + 2
    3

The read-eval-print loop also accepts some commands, where valid commands are displayed by typing `:h`.

    HOBiT> :h
    :quit
        Quit REPL.
    :load FILEPATH
        Load a program.
    :reload
        Reload the last program.
    :put EXP [EXP [EXP]]
        Evaluate a function as "put".
    :get Exp [Exp]
        Evaluate a function as "get".
    :set verbosity INT
        Set verbosity to the specified number.
    :help
        Show this help.
    :type EXP
        Show expression's type.

The ":get" and ":put" are most interesting parts of HOBiT's functionality.  To explain the behavior, we first load the file `./example/Unlines.hobit`.

     HOBiT> :l examples/Unlines.hobit
     ...
     unlinesB :: BX [[Char]] -> BX [[Char]]
     ...

The type constructor "BX" indicates that the values can be updated.  To
run the `unlinesB` function forward, just use the `:get` command.

     HOBiT> :get unlinesB ["a", "b", "c"]
     "a\nb\nc\n"

The command `:put` is used to invoke the backward execution of
`unlinesB`.

     HOBiT> :put unlinesB ["a", "b", "c"] "AA\nBB\nCC\n"
     ...
     ["AA","BB","CC"]
     HOBiT> :put unlinesB ["a", "b", "c"] "AA\nBB\n"
     ...
     ["AA","BB"]
     HOBiT> :put unlinesB ["a", "b", "c"] "AA\nBB\nCC\nDD\n"
     ...
     ["AA","BB","CC","DD"]

The directory `examples` contains several examples. Enjoy!

Differences from the Paper
-------------------------

* `where` clauses are not implemented.

* Some arithmetic operators are not implemented (such as `/=`, `<`, `*`...).

* Bidirectional case expressions and let expressions are `case*` and `let*`.

* We do not provide direct ways to write bidirectional constructors.
  Instead, we adopt the syntactic notion called the *bidirectional context*, under which constructors are turned into bidirectional ones automatically. 

    * There are three forms of expressions that open bidirectional contexts: (1) `let*`, `case*` (except exit-conditions and reconcilers), and constructors in bidirectional contexts. 

    * Bidirectional contexts are explicitly opened by `(|` and `|)`.

    * The bidirectional context is shallow in the sense that arguments of functions are unaffected.  For example, `[]` in `(| f [] |)` is not
      promoted.
  
Note
-----

The executable creates the file named `.HOBiT_history` in
the home directory.

If the error
  `gcc' failed in phase `Linker'. (Exit code: 1)
is showen, 
  sudo apt install libtinfo-dev
may resolve it.
