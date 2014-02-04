# mjolnir

Mjolnir is a Clojure library designed to simplify native code generation. It is useful for writing on-the-fly high performance code, writing programming languages, or simply for exploring new how code performs on different platforms. 

Internally, Mjolnir wraps the LLVM library. It then provides several layers of abstractions on top of LLVM. See the examples in the repository for in-depth examples of the library at work. 

NOTE: the real work lately has been going on in the datomic branch. See recent additions to that branch for up-to-date examples. 

## Layers

Constructors - various Clojure functions that wrap expressions and can emulate let, defn, etc. To use these, use the following pattern

    (ns example
      (:require [mjolnir.constructors-init :as cinit])
      (:alias c mjolnir.constructors))
      
The alias line performs some magic that allows code like the following from within any clojure file:

    (c/defn square [Int64 a -> Int64]
      (c/* a a))


Expressions - Constructors emit Mjolnir expressions. These live in `mjolnir.types` and `mjolnir.expressions`. These expressions are simply Clojure records that implement several common protocols. Once constructed, these expressions can be built via `mjolnir.expressions/build`. But most of the time this function will only be invoked against `mjolnir.expressions/Module` as this record contains a lot of setup code that is neede for the other expressions to compile. 

LLVMC - Expressions invoke the many functions found in `mjolnir.llvmc`. This namespace simply wraps the many functions found in LLVM. The wrapping is done via JNA. 

LLVM-c - Internally, LLVM exposes the C++ api as a C library known as llvm-c.

LLVM - And finally, at the bottom we have the llvm library

## defnf (Def Native Fn)

Mjolnir supports a fairly basic, but powerful macro known as defnf. This macro acts much like Clojure's defn macro, but with C-like semantics:

    (defnf fib [Int64 x -> Int64]
        (if (< x 2)
            x
            (+ (fib (- x 1))
               (fib (- x 2)))))
               
The code inside the macro will be translated to mjolnir constructors (via pre-fixing c- to a symbol if possible). Then the entire function will be type infered. 

If a given variable is a struct, .- can be used to get a member:

    (defnf myfn [Point* pnt -> Int64]
        (.-x pnt))  
        
In addition, pointer types support IFn, and when called, will create a cast operation:

    (myfn (Point* (malloc (sizeof Point))))
    
    

## Supported Platforms

At this time only OSX (64-bit) and NVidia PTX (on OSX) is supported. Adding new targets is easy, so if you want to add support for a platform, take a crack at it!. 

## Usage

FIXME

## License

Copyright (c) 2012-2013 Timothy Baldridge

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
