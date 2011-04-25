placeholders
============

While working on some Haskell code, it is often useful to develop
incrementally by inserting `undefined` as a placeholder for missing code. 

This approach has a couple of drawbacks.

* If you have several occurrences of `undefined` in your code, it can be hard
to track down the one reponsible for the error at run-time. 

* It is too easy to forget to replace `undefined` with the proper code, which
might cause unexpected errors.

This library provides placeholders that produce better messages when
evaluated at run-time and also generate compile-time warnings so that they do
not get forgotten so easily.

example
=======
 
    {-# LANGUAGE TemplateHaskell #-}
    
    import Development.Placeholders
    
    theUltimateAnswer :: Int
    theUltimateAnswer = $notImplemented
    
    main = do
        putStrLn "The ultimate answer:"
        print theUltimateAnswer

This will compile with a warning about the unimplemented function:

    $ ghc --make Simple.hs
    ...
    Simple.hs:6:21: Unimplemented feature
    ...

At runtime, an exception will be thrown when the placeholder is evaluated,
indicating the location of the placeholder.

    $ ./Simple
    The ultimate answer:
    Simple: PlaceholderExcption "Unimplemented feature at Simple.hs:6:21"

If compiled with the GHC flag `-Werror`, the warning will get turned into
an error and compilation will fail. `-Werror` can therefore be used to
verify that you haven't left any unintended placeholders behind.

