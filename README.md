# Where Lions Roam: Haskell & Hardware on the VELDT

## Table of Contents
1. [Section 0: Introduction & Setup](https://github.com/standardsemiconductor/VELDT-getting-started#section-0-introduction--setup)
2. [Section 1: Fiat Lux](https://github.com/standardsemiconductor/VELDT-getting-started#section-1-fiat-lux)
   1. [Learning to Count](https://github.com/standardsemiconductor/VELDT-getting-started#learning-to-count)
   2. [Its a Vibe: PWM](https://github.com/standardsemiconductor/VELDT-getting-started#its-a-vibe-pwm)
   3. [Fiat Lux: Blinker](https://github.com/standardsemiconductor/VELDT-getting-started#fiat-lux-blinker)
3. [Section 2: Roar](https://github.com/standardsemiconductor/VELDT-getting-started#section-2-roar)
4. [Section 3: Pride](https://github.com/standardsemiconductor/VELDT-getting-started#section-3-pride)
5. [Section 4: Where Lions Roam](https://github.com/standardsemiconductor/VELDT-getting-started#section-4-where-lions-roam)
   
**Clicking on any header within this document will return to Table of Contents** 

## [Section 0: Introduction & Setup](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
This is an opinionated guide to hardware design from first principles using Haskell and VELDT. However, much of the information within this document can be useful independent of the HDL or FPGA development platform. We assume you have VELDT FPGA development board; if not, go order one from [Amazon](https://www.amazon.com/dp/B08F9T8DFT?ref=myi_title_dp). We also assume you are using Linux, but this is only for getting the tools setup and running the examples. 
  
Much of the code included in the examples is written in Haskell and compiled to Verilog using [Clash](https://clash-lang.org/). We find desiging hardware with Haskell to be an enriching experience, and if you are experimenting with HDLs or just starting out with hardware, give it a shot; over time the dividends pay well. Visit the [VELDT-info](https://github.com/standardsemiconductor/VELDT-info#clash) repo for instructions on installation and setup of Haskell and Clash tools. If you design hardware in a language other than Haskell, feel free to skip over the language specific aspects. We hope to translate the examples to other HDLs as this guide develops.
  
We use the Project IceStorm flow for synthesis, routing, and programming. These are excellent, well-maintained open source tools. For installation and setup instructions visit the [VELDT-info](https://github.com/standardsemiconductor/VELDT-info#project-icestorm) repo.

This guide is split into several sections. Each section begins with construction of sub-components then culminates with an application which utilizes the sub-components. [Section 1](https://github.com/standardsemiconductor/VELDT-getting-started#section-1-fiat-lux) constructs a simple blinker, the "hello-world" of FPGAs. [Section 2](https://github.com/standardsemiconductor/VELDT-getting-started#section-2-roar) covers serializers and deserializers which are used to construct a UART. In [Section 3](https://github.com/standardsemiconductor/VELDT-getting-started#section-3-pride) we learn how to interact with the memory provided by VELDT. Finally, in [Section 4](https://github.com/standardsemiconductor/VELDT-getting-started#section-4-where-lions-roam) we design a simple CPU with a custom ISA, then integrate our LED, UART, and memory peripherals to create a stored-program computer. By the end of the guide, you will have a library of commonly used sub-components along with a directory of applications demonstrating their useage. The library and demos explained in this guide are available in this repo, see the [veldt](https://github.com/standardsemiconductor/VELDT-getting-started/tree/master/veldt) and [test] directories.

Finally, if you have any suggestions, comments, discussions, edits additions etc. please open an issue in this repo. We value any and all contributions. Lets get started!
## [Section 1: Fiat Lux](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
In this section we start by building a counter. Then using the counter, construct a PWM. Equipped with our counter and PWM, we integrate a RGB LED Driver IP to create our first running application on VELDT; a blinker!
### [Learning to Count](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
Lets start by making a haskell library:
```console
foo@bar:~$ mkdir veldt && cd veldt
foo@bar:~/veldt$ cabal init
Guessing dependencies...

Generating LICENSE...
Warning: unknown license type, you must put a copy in LICENSE yourself.
Generating Setup.hs...
Generating CHANGELOG.md...
Generating Main.hs...
Generating veldt.cabal...

Warning: no synopsis given. You should edit the .cabal file and add one.
You may want to edit the .cabal file and add a Description field.
```
We can remove the `Main.hs` file:
```console
foo@bar:~/veldt$ rm Main.hs
```
Next edit the `veldt.cabal`. We remove the executable section and add our own library. We leave everything else untouched. You may customize attributes like `synopsys` but it is not necessary. Your `veldt.cabal` file should look similar:
```
cabal-version:       >=1.10
-- Initial package description 'veldt.cabal' generated by 'cabal init'.
-- For further documentation, see http://haskell.org/cabal/users-guide/
                                                                                                         
name:                veldt
version:             0.1.0.0
-- synopsis:
-- description:
-- bug-reports:
-- license:
license-file:        LICENSE
author:              Standard Semiconductor
maintainer:          standard.semiconductor@gmail.com
-- copyright:
-- category:
build-type:          Simple
extra-source-files:  CHANGELOG.md

library
	exposed-modules: Veldt.Counter
        build-depends: clash-prelude,
        	       mtl,
                       lens,
		       interpolate,
                       base,
		       ghc-typelits-extra,
		       ghc-typelits-knownnat,
                       ghc-typelits-natnormalise
       	default-extensions: NoImplicitPrelude,
                            DeriveGeneric,
                            DeriveAnyClass,
                            DataKinds,
                            TemplateHaskell,
                            LambdaCase,
                            TupleSections,
                            TypeOperators
        default-language: Haskell2010
	ghc-options: -Wall -fexpose-all-unfoldings -fno-worker-wrapper -fplugin=GHC.TypeLits.Extra.Solve\
r -fplugin=GHC.TypeLits.KnownNat.Solver	-fplugin=GHC.TypeLits.Normalise
```
We won't go through everything about this cabal file, but here are the highlights. `exposed-modules` are the modules we export from the library to be used in our demos. So far we see `Veldt.Counter`, we will create a directory `Veldt` with a file `Counter.hs`. This will have our counter source code. The `build-depends` section lists our library dependencies. Notably the `clash-prelude` package provides important functions and types that are crucial for hardware. We use `lens` zoom and mutate substates. `interpolate` is used for inline primitives when we need Yosys to infer hardware IP. `base` provides standard haskell functions and types. The `ghc-typelits...` packages are plugins to help the clash compiler infer types. The next section is `default-extensions`, these help us reduce boilerplate and clean up syntax. `NoImplicitPrelude` is especially important, it says we don't want the standard Haskell prelude imported implicitly, we want to explicitly import the Clash prelude. `ghc-options` turns on warnings and activates plugins.

Create a directory `Veldt` with a file `Counter.hs`.
```console
foo@bar:~/veldt$ mkdir Veldt && cd Veldt
foo@bar:~/veldt/Veldt$ touch Counter.hs
```

Open `Counter.hs` in your favorite editor. Lets name the module, list the exports and import some useful packages:
```haskell
module Veldt.Counter
  ( Counter
  , mkCounter
  , increment
  , incrementWhen
  , incrementUnless
  , decrement
  , set
  , get
  , gets
  ) where

import Clash.Prelude
import Control.Monad.RWS (RWST)
import qualified Control.Monad.RWS as RWS
```
The exported types and functions define the API for our counter. We want to be able to increment, decrement, set, or get the counter value. Additionally, we provide conditional increment functions. Its a good exercise to do the same for decrement.

Lets define the counter type, it will be polymorphic so we may use the counter in different contexts with different underlying counter types e.g. `Counter (Index 25)`, `Counter (Unsigned 32)`, or `Counter (BitVector 8)`
```haskell
newtype Counter a = Counter { unCounter :: a }
  deriving (NFDataX, Generic)
```
Clash requires we derive `NFDataX` and `Generic` for any type which will be stored as state in a register.

Next lets define a function which creates a counter. Its very simple, but it highlights a style we use throughout the library; functions prefixed with "mk" are state type constructors.
```haskell
mkCounter :: a -> Counter a
mkCounter = Counter
```

The rest of the API is monadic. This is an opinion, you don't have to use monads to make a counter, but this guide does. We advise you to explore other styles, techniques and representations. That being said, we choose to represent mealy machines as Reader-Writer-State monads or RWS for short. In order to easily compose monadic functions we use the concrete RWS transformer from [mtl](http://hackage.haskell.org/package/mtl). It has the type `RWST r w s m a`. Although the counter does not use the Reader or Writer types `r` and `w`, other components in the library will and this eases composition, e.g. when we want to make a UART with both a counter and serializer. Lets define some simple functions to access or mutate our counter.
```haskell
set :: (Monoid w, Monad m) => a -> RWST r w (Counter a) m ()
set = RWS.put . Counter

get :: (Monoid w, Monad m) => RWST r w (Counter a) m a
get = RWS.gets unCounter

gets :: (Monoid w, Monad m) => (a -> b) -> RWST r w (Counter a) m b
gets f = f <$> get
```
They wrap/unwrap the newtype and use the RWST `RWS.get` and `RWS.put` functions to provide access to the underlying counter value with type `a`. We use these accessors to implement the `increment` and `decrement` functions.
```haskell
increment :: (Monoid w, Monad m, Bounded a, Enum a, Eq a) => RWST r w (Counter a) m ()
increment = do
  c <- get
  set $ if c == maxBound
    then minBound
    else succ c

decrement :: (Monoid w, Monad m, Bounded a, Enum a, Eq a) => RWST r w (Counter a) m ()
decrement = do
  c <- get
  set $ if c == minBound
    then maxBound
    else pred c
```
Heres the gist: first `get` the current value of the counter. If the value is equal to its maximum (minimum) bound then set the counter to the minimum (maximum) bound. Otherwise, `set` the counter to the value's successor (predecessor). 

The typeclass constraint `Bounded` says our counter has a minimum and maximum value which gives us `minBound` and `maxBound`. Likewise `Eq` lets us compare equality `==` and `Enum` provides `succ` (successor) and `pred` (predecessor) functions on our polymorphic type `a`. Without these constraints the compiler would complain that it could not deduce the required typeclass. Additionally, the RWS Monad `RWST r w s m a` requires `w` to be a `Monoid` and `m` a `Monad`, this will be important later when we "run" our monadic action.

When designing your own counters be careful when using `succ` or `pred`. For example `succ 0 == (1 :: BitVector 8)` and `pred 4 == (3 :: Index 6)`, but `succ (4 :: Index 5)` is undefined and out of bounds (**DO NOT DO THIS**) because the type `Index 5` only has inhabitants `0`,`1`,`2`,`3`, and `4`; that is why we check for `maxBound` and `minBound` states in `increment` and `decrement`.

Finally, let us now use our new `increment` function to implement a conditional increment `incrementWhen` and `incrementUnless`. The former will increment when a predicate is `True`, the latter when `False`.
```haskell
incrementWhen
  :: (Monoid w, Monad m, Bounded a, Enum a, Eq a)
  => (a -> Bool)
  -> RWST r w (Counter a) m ()
incrementWhen f = do
  b <- gets f
  if b
    then increment
    else set minBound

incrementUnless
  :: (Monoid w, Monad m, Bounded a, Enum a, Eq a)
  => (a -> Bool)
  -> RWST r w (Counter a) m ()
incrementUnless f = incrementWhen (not . f)
```
Within `incrementWhen`, we get the counter value and apply our predicate then bind to `b`. Thus, if the predicate evaluates to `True`, `b` is bound to `True` and we increment the counter. Otherwise, we set the value of the counter to its minimum bound. To reduce and reuse code, we implement `incrementUnless` using `incrementWhen` and post-compose `not` to our predicate. Suppose we have `incrementUnless (== 3) :: RWST r w (Counter (Index 8)) m ()`, then the states of the counter would be: ... 0 1 2 3 0 1 2 3 0 1 2 3 ...

To end this section, lets clean then rebuild the library. You should not see any errors.
```console
foo@bar:~/veldt$ cabal clean
foo@bar:~/veldt$ cabal build
Resolving dependencies...
Build profile: -w ghc-8.8.3 -O1
In order, the following will be built (use -v for more details):
 - veldt-0.1.0.0 (lib) (configuration changed)
Configuring library for veldt-0.1.0.0..
Warning: The 'license-file' field refers to the file 'LICENSE' which does not
exist.
Preprocessing library for veldt-0.1.0.0..
Building library for veldt-0.1.0.0..
[1 of 1] Compiling Veldt.Counter    ( Veldt/Counter.hs, /home/davos/eda/proj/VELDT-getting-started/veldt/dist-newstyle/build/x86_64-linux/ghc-8.8.3/veldt-0.1.0.0/build/Veldt/Counter.o ) [flags changed]
```
You can find the full counter source code [here](https://github.com/standardsemiconductor/VELDT-getting-started/blob/master/veldt/Veldt/Counter.hs). We can now use our counter to create a PWM.
### [Its a Vibe: PWM](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
### [Fiat Lux: Blinker](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
## [Section 2: Roar](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
## [Section 3: Pride](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
## [Section 4: Where Lions Roam](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
