# ELVM Compiler Infrastructure

**THIS PROJECT IS A FORK PROJECT FROM [ORIGINAL ELVM](https://github.com/shinh/elvm)**. I added compile-time type-level haskell backend.

## HOW TO USE

```
$ make
$ ./out/8cc -S -I. -Ilibc -o test/fizzbuzz_fast.eir test/fizzbuzz_fast.c
$ ./out/elc -tlhs test/fizzbuzz_fast.eir > fizzbuzz_fast.hs
```

and combine [type level cpu](https://github.com/eliza0x/type-level-cpu). You can run C source code in haskell compile time.

