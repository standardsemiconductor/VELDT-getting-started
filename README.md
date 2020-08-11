# Where Lions Roam: Haskell & Hardware on VELDT

## Table of Contents
1. [Section 1: Introduction & Setup](https://github.com/standardsemiconductor/VELDT-getting-started#section-1-introduction--setup)
2. [Section 2: Fiat Lux](https://github.com/standardsemiconductor/VELDT-getting-started#section-2-fiat-lux)
   1. [Learning to Count](https://github.com/standardsemiconductor/VELDT-getting-started#learning-to-count)
   2. [Its a Vibe: PWM](https://github.com/standardsemiconductor/VELDT-getting-started#its-a-vibe-pwm)
   3. [Drive: RGB Primitive](https://github.com/standardsemiconductor/VELDT-getting-started#drive-rgb-primitive)
   4. [Fiat Lux: Blinker](https://github.com/standardsemiconductor/VELDT-getting-started#fiat-lux-blinker)
3. [Section 3: Roar](https://github.com/standardsemiconductor/VELDT-getting-started#section-3-roar)
4. [Section 4: Pride](https://github.com/standardsemiconductor/VELDT-getting-started#section-4-pride)
5. [Section 5: Where Lions Roam](https://github.com/standardsemiconductor/VELDT-getting-started#section-5-where-lions-roam)
   
**Clicking on any header within this document will return to Table of Contents** 

## [Section 1: Introduction & Setup](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
This is an opinionated guide to hardware design from first principles using Haskell and VELDT. However, much of the information within this document can be useful independent of the HDL or FPGA development platform. We assume you have VELDT FPGA development board; if not, go order one from [Amazon](https://www.amazon.com/dp/B08F9T8DFT?ref=myi_title_dp). We also assume you are using Linux, but this is only for getting the tools setup and running the examples. 
  
Much of the code included in the examples is written in Haskell and compiled to Verilog using [Clash](https://clash-lang.org/). We find desiging hardware with Haskell to be an enriching experience, and if you are experimenting with HDLs or just starting out with hardware, give it a shot; over time the dividends pay well. Visit the [VELDT-info](https://github.com/standardsemiconductor/VELDT-info#clash) repo for instructions on installation and setup of Haskell and Clash tools. If you design hardware in a language other than Haskell, feel free to skip over the language specific aspects. We hope to translate the examples to other HDLs as this guide develops.
  
We use the Project IceStorm flow for synthesis, routing, and programming. These are excellent, well-maintained open source tools. For installation and setup instructions visit the [VELDT-info](https://github.com/standardsemiconductor/VELDT-info#project-icestorm) repo.

This guide is split into several sections. Each section begins with construction of sub-components then culminates with an application which utilizes the sub-components. [Section 2](https://github.com/standardsemiconductor/VELDT-getting-started#section-2-fiat-lux) constructs a simple blinker, the "hello-world" of FPGAs. [Section 3](https://github.com/standardsemiconductor/VELDT-getting-started#section-3-roar) covers serializers and deserializers which are used to construct a UART. In [Section 4](https://github.com/standardsemiconductor/VELDT-getting-started#section-4-pride) we learn how to interact with the memory provided by VELDT. Finally, in [Section 5](https://github.com/standardsemiconductor/VELDT-getting-started#section-5-where-lions-roam) we design a simple CPU with a custom ISA, then integrate our LED, UART, and memory peripherals to create a stored-program computer. By the end of the guide, you will have a library of commonly used sub-components along with a directory of applications demonstrating their usage. The library and demos explained in this guide are available in this repo, see the [veldt](https://github.com/standardsemiconductor/VELDT-getting-started/tree/master/veldt) and [demo] directories.

Finally, if you have any suggestions, comments, discussions, edits additions etc. please open an issue in this repo. We value any and all contributions. Let's get started!
## [Section 2: Fiat Lux](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
In this section we start by building a counter. Then using the counter, construct a PWM. Equipped with our counter and PWM, we use the RGB LED Driver IP to create our first running application on VELDT; a blinker!
### [Learning to Count](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
We begin by creating a haskell library:
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
Next, edit the `veldt.cabal`. We remove the executable section and add our own library. We leave everything else untouched. You may customize attributes like `synopsys` but it is not necessary. Your `veldt.cabal` file should look similar:
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
                            TypeOperators,
			    QuasiQuotes
        default-language: Haskell2010
	ghc-options: -Wall -fexpose-all-unfoldings -fno-worker-wrapper -fplugin=GHC.TypeLits.Extra.Solve\
r -fplugin=GHC.TypeLits.KnownNat.Solver	-fplugin=GHC.TypeLits.Normalise
```
We won't go through everything about this cabal file, but here are the highlights. `exposed-modules` are the modules we export from the library to be used in our demos. So far we see `Veldt.Counter`, we will create a directory `Veldt` with a file `Counter.hs`. This will have our counter source code. The `build-depends` section lists our library dependencies. Notably the `clash-prelude` package provides important functions and types that are crucial for hardware. We use `lens` to zoom and mutate substates. `interpolate` is used for inline primitives when we need Yosys to infer hardware IP. `base` provides standard haskell functions and types. The `ghc-typelits...` packages are plugins to help the clash compiler infer types. The next section is `default-extensions`, these help us reduce boilerplate and clean up syntax. `NoImplicitPrelude` is especially important, it says we don't want the standard Haskell prelude imported implicitly, we want to explicitly import the Clash prelude. `ghc-options` turns on warnings and activates plugins.

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
The exported types and functions define the API for our counter. We want to be able to `increment`, `decrement`, `set`, or `get` the counter value. Additionally, we provide `gets` (`get` with a projection) and conditional increment functions `incrementWhen` and `incrementUnless`.

Let's define the counter type, it will be polymorphic so we may use the counter in different contexts with different underlying counter types e.g. `Counter (Index 25)`, `Counter (Unsigned 32)`, or `Counter (BitVector 8)`
```haskell
newtype Counter a = Counter { unCounter :: a }
  deriving (NFDataX, Generic)
```
Clash requires we derive `NFDataX` and `Generic` for any type which will be stored as state in a register.

Next we define a function which constructs a counter. Although very simple, it highlights a style we use throughout the library; functions prefixed with "mk" are state type constructors, sometimes called "smart constructors".
```haskell
mkCounter :: a -> Counter a
mkCounter = Counter
```

The rest of the API is monadic. This is an opinion, you don't have to use monads to make a counter, but this guide does. We advise you to explore other styles, techniques and representations. That being said, we choose to represent mealy machines as Reader-Writer-State monads or RWS for short. In order to easily compose monadic functions we use the concrete RWS transformer from [mtl](http://hackage.haskell.org/package/mtl). It has the type `RWST r w s m a`. Although the counter does not use the Reader or Writer types `r` and `w`, other components in the library will and this eases composition, e.g. when we want to make a UART with both a counter and serializer. Let's define some simple functions to access or mutate our counter.
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
Here's the gist: first `get` the current value of the counter. If the value is equal to its maximum (minimum) bound then set the counter to the minimum (maximum) bound. Otherwise, `set` the counter to the value's successor (predecessor). 

The typeclass constraint `Bounded` says our counter has a minimum and maximum value which gives us `minBound` and `maxBound`. Likewise `Eq` lets us compare equality `==` and `Enum` provides `succ` (successor) and `pred` (predecessor) functions on our polymorphic type `a`. Without these constraints the compiler would complain that it could not deduce the required typeclass. Additionally, the RWS Monad `RWST r w s m a` requires `w` to be a `Monoid` and `m` a `Monad`, this will be important later when we "run" our monadic action.

When designing your own counters be careful when using `succ` or `pred`. For example `succ 0 == (1 :: BitVector 8)` and `pred 4 == (3 :: Index 6)`, but `succ (4 :: Index 5)` is undefined and out of bounds (**DO NOT DO THIS**) because the type `Index 5` only has inhabitants `0`,`1`,`2`,`3`, and `4`; that is why we check for `maxBound` and `minBound` states in `increment` and `decrement`.

Finally, we use our new `increment` function to implement a conditional increment `incrementWhen` and `incrementUnless`. The former will increment when a predicate is `True`, the latter when `False`.
```haskell
incrementWhen
  :: (Monoid w, Monad m, Bounded a, Enum a, Eq a)
  => (a -> Bool)
  -> RWST r w (Counter a) m ()
incrementWhen p = do
  b <- gets p
  if b
    then increment
    else set minBound

incrementUnless
  :: (Monoid w, Monad m, Bounded a, Enum a, Eq a)
  => (a -> Bool)
  -> RWST r w (Counter a) m ()
incrementUnless p = incrementWhen (not . p)
```
Within `incrementWhen`, we get the counter value and apply our predicate. If the predicate evaluates to `True`, `b` is bound to `True` and we increment the counter. Otherwise, `b` is bound to `False` and we set the value of the counter to its minimum bound. To reduce and reuse code, we implement `incrementUnless` using `incrementWhen` and post-compose `not` to our predicate. Suppose we have `incrementUnless (== 3) :: RWST r w (Counter (Index 8)) m ()`, then the states of the counter would be: ... 0 1 2 3 0 1 2 3 0 1 2 3 ...

Here is our completed counter:
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

-------------
-- Counter --
-------------
newtype Counter a = Counter { unCounter :: a }
  deriving (NFDataX, Generic)

mkCounter :: a -> Counter a
mkCounter = Counter

set :: (Monoid w, Monad m) => a -> RWST r w (Counter a) m ()
set = RWS.put . Counter

get :: (Monoid w, Monad m) => RWST r w (Counter a) m a
get = RWS.gets unCounter

gets :: (Monoid w, Monad m) => (a -> b) -> RWST r w (Counter a) m b
gets f = f <$> get

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

incrementWhen
  :: (Monoid w, Monad m, Bounded a, Enum a, Eq a)
  => (a -> Bool)
  -> RWST r w (Counter a) m ()
incrementWhen p = do
  b <- gets p
  if b
    then increment
    else set minBound

incrementUnless
  :: (Monoid w, Monad m, Bounded a, Enum a, Eq a)
  => (a -> Bool)
  -> RWST r w (Counter a) m ()
incrementUnless p = incrementWhen (not . p)
```
To end this part, we clean and rebuild the library. You should not see any errors.
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
[1 of 1] Compiling Veldt.Counter    ( Veldt/Counter.hs, /home/foo/veldt/dist-newstyle/build/x86_64-linux/ghc-8.8.3/veldt-0.1.0.0/build/Veldt/Counter.o ) [flags changed]
```
You can find the full counter source code [here](https://github.com/standardsemiconductor/VELDT-getting-started/blob/master/veldt/Veldt/Counter.hs). We can now use our counter to create a PWM.
### [Its a Vibe: PWM](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
Pulse Width Modulation or PWM is used to drive our LED. We use a technique called [time proportioning](https://en.wikipedia.org/wiki/Pulse-width_modulation#Time_proportioning) to generate the PWM signal with our counter. To begin let's create a `PWM.hs` file in the `Veldt` directory.
```console
foo@bar:~/veldt/Veldt$ touch PWM.hs
```
We also need to expose the PWM module with cabal by editing the `exposed-modules` section of `veldt.cabal` to include `Veldt.PWM`.
```
......
library
        exposed-modules: Veldt.Counter,
	                 Veldt.PWM
......
```
Now begin editing the `PWM.hs` file. We start by naming the module, defining our exports, and importing useful packages.
```haskell
module Veldt.PWM
  ( PWM
  , mkPWM
  , pwm
  , setDuty
  ) where

import Clash.Prelude
import Control.Lens
import Control.Monad.RWS
import qualified Veldt.Counter as C
```
We export the type `PWM` and its smart constructor `mkPWM`. The monadic API consists of `pwm`, a PWM action, and a setter `setDuty` to mutate the duty cycle. In this module we will be using [lens](https://hackage.haskell.org/package/lens) to mutate and zoom sub-states. Again, we use the RWS monad. Finally we import our counter as qualified so as not to conflict `RWS`'s `get` with `Counter`'s `get`. This means whenever we want to use an exported function from `Veldt.Counter` we must prefix it with `C.`.

Next we define the `PWM` type and its constructor. Note how we use `makeLenses` to automatically create lenses for our `PWM` type.
```haskell
data PWM a = PWM
  { _ctr  :: C.Counter a
  , _duty :: a
  } deriving (NFDataX, Generic)
makeLenses ''PWM

mkPWM :: Bounded a => a -> PWM a
mkPWM d = PWM (C.mkCounter minBound) d
```
The PWM state consists of a counter and a value used to control the duty cycle. Also, note that we keep `PWM` polymorphic, just like our counter. Our smart constructor creates a PWM with an initial duty cycle and a counter with an initial value set to the minimum bound. 

Let's define and implement `setDuty` which will update the `duty` cycle and reset the counter.
```haskell
setDuty :: (Monoid w, Monad m, Bounded a) => a -> RWST r w (PWM a) m ()
setDuty d = do
  duty .= d
  zoom ctr $ C.set minBound
```
We use the `.=` lens operator to set the `duty` cycle, then we use `zoom` to `set` the counter to its minimum bound. We use `setDuty` to change the duty cycle of the PWM. For example, suppose we have `setDuty 25 :: RWST r w (PWM (Index 100)) m ()`, then the PWM will operate at 25% duty cycle.

Finally, we tackle the `pwm` function.
```haskell
pwm :: (Monoid w, Monad m, Ord a, Bounded a, Enum a) => RWST r w (PWM a) m Bit
pwm = do
  d <- use duty
  c <- zoom ctr C.get
  zoom ctr C.increment
  return $ boolToBit $ c < d
```
First we bind `duty` to `d` and the counter value to `c`. Next we `increment` the counter. Last, we compare `c < d`, convert the `boolToBit`, and `return` the bit. `boolToBit` simply maps `True` to `1 :: Bit` and `False` to `0 :: Bit`. Because we compare the `duty` `d` to the counter `c` with `<`, our type signature requires the underlying counter type `a` to be a member of the `Ord` typeclass. For example, if we have `pwm :: RWST r w (PWM (Index 4)) m Bit` and `duty` is bound to `3 :: Index 4`, (75% duty cycle, remeber `Index 4` has inhabitants 0, 1, 2, 3), the output of `pwm` when run as a mealy machine would be: ... 1, 1, 1, 0, 1, 1, 1, 0, ... .

Here is the complete `PWM.hs` source code:
```haskell
module Veldt.PWM
  ( PWM
  , mkPWM
  , pwm
  , setDuty
  ) where

import Clash.Prelude
import Control.Lens
import Control.Monad.RWS
import qualified Veldt.Counter as C

---------
-- PWM --
---------
data PWM a = PWM
  { _ctr  :: C.Counter a
  , _duty :: a
  } deriving (NFDataX, Generic)
makeLenses ''PWM

mkPWM :: Bounded a => a -> PWM a
mkPWM d = PWM (C.mkCounter minBound) d

setDuty :: (Monoid w, Monad m, Bounded a) => a -> RWST r w (PWM a) m ()
setDuty d = do
  duty .= d
  zoom ctr $ C.set minBound

pwm :: (Monoid w, Monad m, Ord a, Bounded a, Enum a) => RWST r w (PWM a) m Bit
pwm = do
  d <- use duty
  c <- zoom ctr C.get
  zoom ctr C.increment
  return $ boolToBit $ c < d
```
To end this part, we clean and rebuild the library. You should not see any errors.
```console
foo@bar:~/veldt$ cabal clean && cabal build
Resolving dependencies...
Build profile: -w ghc-8.8.3 -O1
In order, the following will be built (use -v for more details):
 - veldt-0.1.0.0 (lib) (first run)
Configuring library for veldt-0.1.0.0..
Warning: The 'license-file' field refers to the file 'LICENSE' which does not
exist.
Preprocessing library for veldt-0.1.0.0..
Building library for veldt-0.1.0.0..
[1 of 2] Compiling Veldt.Counter    ( Veldt/Counter.hs, /home/foo/veldt/dist-newstyle/build/x86_64-linux/ghc-8.8.3/veldt-0.1.0.0/build/Veldt/Counter.o )
[2 of 2] Compiling Veldt.PWM        ( Veldt/PWM.hs, /home/foo/veldt/dist-newstyle/build/x86_64-linux/ghc-8.8.3/veldt-0.1.0.0/build/Veldt/PWM.o )
```
You can find the full PWM source code [here](https://github.com/standardsemiconductor/VELDT-getting-started/blob/master/veldt/Veldt/PWM.hs). In the next part, we use a Clash primitive to infer Lattice RGB Driver IP.
### [Drive: RGB Primitive](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
We need one more component before starting our demo. This component is a RGB LED Driver; it takes 3 PWM signals (R, G, B) to drive the LED. Because the RGB Driver is a Lattice IP block, we need our compiled Haskell code to take a certain form in Verilog. When we synthesize the demo, Yosys will infer the Lattice Ice40 RGB Driver IP from the Verilog code. In order to have Clash use a certain Verilog (or VHDL) code, we write a primitive. This primitive tells the Clash compiler to insert Verilog (or VHDL) instead of compiling our function. Let's begin by creating a directory `Ice40` for our Lattice primitives. This will be within the `Veldt` directory. Then we create a `RgbDriver.hs` file which will be our RGB Driver primitive.
```console
foo@bar:~/veldt$ mkdir Veldt/Ice40 && touch Veldt/Ice40/RgbDriver.hs
```
Next add the `Veldt.Ice40.RgbDriver` to our `veldt.cabal` `exposed-modules` list.
```
...
exposed-modules: Veldt.Counter,
                 Veldt.PWM,
                 Veldt.Ice40.RgbDriver
...
```
Now edit `RgbDriver.hs`. We inline the Verilog primitive (meaning we have Verilog and Haskell in the same module), and then wrap it with a function to ease usage. Let's start by naming the module, its exports, and its imports.
```haskell
module Veldt.Ice40.RgbDriver
  ( Rgb
  , rgbDriver
  ) where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
```
We export the `Rgb` type which is the input/output type of our primitive and a wrapper function `rgbDriver` for the primitive. Additionally we import `Clash.Annotations.Primitive` which supplies code for writing primitives. Since the primtive will be inlined we use the [interpolate](https://hackage.haskell.org/package/interpolate) package for string interpolation.

Now we create the primtive.
```haskell
{-# ANN rgbDriverPrim (InlinePrimitive [Verilog] $ unindent [i|
  [ { "BlackBox" :
      { "name" : "Veldt.Ice40.RgbDriver.rgbDriverPrim"
      , "kind" : "Declaration"
      , "type" :
  "rgbDriverPrim
  :: String         -- current_mode ARG[0]
  -> String         -- rgb0_current ARG[1]
  -> String         -- rgb1_current ARG[2]
  -> String         -- rgb2_current ARG[3]
  -> Signal dom Bit -- pwm_r        ARG[4]
  -> Signal dom Bit -- pwm_g        ARG[5]
  -> Signal dom Bit -- pwm_b        ARG[6]
  -> Signal dom (Bit, Bit, Bit)"
      , "template" :
  "//SB_RGBA_DRV begin
  wire ~GENSYM[RED][0];
  wire ~GENSYM[GREEN][1];
  wire ~GENSYM[BLUE][2];

  SB_RGBA_DRV #(
     .CURRENT_MODE(~ARG[0]),
     .RGB0_CURRENT(~ARG[1]),
     .RGB1_CURRENT(~ARG[2]),
     .RGB2_CURRENT(~ARG[3])
  ) RGBA_DRIVER (
     .CURREN(1'b1),
     .RGBLEDEN(1'b1),
     .RGB0PWM(~ARG[4]),
     .RGB1PWM(~ARG[5]),
     .RGB2PWM(~ARG[6]),
     .RGB0(~SYM[0]),
     .RGB1(~SYM[1]),
     .RGB2(~SYM[2])
  );
 
  assign ~RESULT = {~SYM[0], ~SYM[1], ~SYM[2]};
  //SB_RGBA_DRV end"
      }
    } 
  ]
  |]) #-}
```
When writing primitives be sure the function name, module name, and black box name all match. The template is Verilog from the Lattice documentation [iCE40 LED Driver Usage Guide](https://github.com/standardsemiconductor/VELDT-info/blob/master/ICE40LEDDriverUsageGuide.pdf). The documentation for writing primitives is on the [clash-prelude](https://hackage.haskell.org/package/clash-prelude) hackage page in the `Clash.Annotations.Primitive` module. Basically, the `SB_RGBA_DRV` module takes 3 PWM input signals and outputs 3 LED driver signals. We adopt the style to prefix any primitive functions with `Prim`. Let's give a Haskell function stub for the primitive.
```haskell
{-# NOINLINE rgbDriverPrim #-}
rgbDriverPrim
  :: String
  -> String
  -> String
  -> String
  -> Signal dom Bit
  -> Signal dom Bit
  -> Signal dom Bit
  -> Signal dom (Bit, Bit, Bit)
rgbDriverPrim _ _ _ _ _ _ _ = pure (0, 0, 0)
```
Although we do not provide a real implementation for the the primitive in Haskell, it is good practice to do so and helps when testing and modeling. Also, note the type of `rgbDriverPrim` matches exactly to the inlined primitive type and has a `NOINLINE` annotation.

Instead of constantly writing `(Bit, Bit, Bit)` for our RGB tuple, let's define a type synonym with some tags which are useful when constraining pins.
```haskell
type Rgb = ("red" ::: Bit, "green" ::: Bit, "blue" ::: Bit)
```
Finally, using our `Rgb` type, we wrap the primitive and give it some default parameters.
```haskell
rgbDriver :: Signal dom Rgb -> Signal dom Rgb
rgbDriver rgb = let (r, g, b) = unbundle rgb
                in rgbDriverPrim "0b0" "0b111111" "0b111111" "0b111111" r g b
```
`unbundle` is part of a `Signal` isomorphism, the other part being `bundle`. In this case, `unbundle` maps the type `Signal dom (Bit, Bit, Bit)` to `(Signal dom Bit, Signal dom Bit, Signal dom Bit)`. The `String` parameters we give to `rgbDriverPrim` define the current and mode outputs for the driver. It may be prudent to adjust these parameters depending on the power requirements of your application. It is a good exercise to define a custom current/mode data type and use that in the wrapper `rgbDriver` for easy usage.

Here is the complete `RgbDriver.hs` source code:
```haskell
module Veldt.Ice40.RgbDriver
  ( Rgb
  , rgbDriver
  ) where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

{-# ANN rgbDriverPrim (InlinePrimitive [Verilog] $ unindent [i|
  [ { "BlackBox" :
      { "name" : "Veldt.Ice40.RgbDriver.rgbDriverPrim"
      , "kind" : "Declaration"
      , "type" :
  "rgbDriverPrim
  :: String         -- current_mode ARG[0]
  -> String         -- rgb0_current ARG[1]
  -> String         -- rgb1_current ARG[2]
  -> String         -- rgb2_current ARG[3]
  -> Signal dom Bit -- pwm_r        ARG[4]
  -> Signal dom Bit -- pwm_g        ARG[5]
  -> Signal dom Bit -- pwm_b        ARG[6]
  -> Signal dom (Bit, Bit, Bit)"
      , "template" :
  "//SB_RGBA_DRV begin
  wire ~GENSYM[RED][0];
  wire ~GENSYM[GREEN][1];
  wire ~GENSYM[BLUE][2];

  SB_RGBA_DRV #(
     .CURRENT_MODE(~ARG[0]),
     .RGB0_CURRENT(~ARG[1]),
     .RGB1_CURRENT(~ARG[2]),
     .RGB2_CURRENT(~ARG[3])
  ) RGBA_DRIVER (
     .CURREN(1'b1),
     .RGBLEDEN(1'b1),
     .RGB0PWM(~ARG[4]),
     .RGB1PWM(~ARG[5]),
     .RGB2PWM(~ARG[6]),
     .RGB0(~SYM[0]),
     .RGB1(~SYM[1]),
     .RGB2(~SYM[2])
  );
 
  assign ~RESULT = {~SYM[0], ~SYM[1], ~SYM[2]};
  //SB_RGBA_DRV end"
      }
    } 
  ]
  |]) #-}

{-# NOINLINE rgbDriverPrim #-}
rgbDriverPrim
  :: String
  -> String
  -> String
  -> String
  -> Signal dom Bit
  -> Signal dom Bit
  -> Signal dom Bit
  -> Signal dom (Bit, Bit, Bit)
rgbDriverPrim _ _ _ _ _ _ _ = pure (0, 0, 0)

type Rgb = ("red" ::: Bit, "green" ::: Bit, "blue" ::: Bit)

rgbDriver :: Signal dom Rgb -> Signal dom Rgb
rgbDriver rgb = let (r, g, b) = unbundle rgb
                in rgbDriverPrim "0b0" "0b111111" "0b111111" "0b111111" r g b
```

To end this part, we clean and rebuild the library. You should not see any errors.
```console
foo@bar:~/veldt$ cabal clean && cabal build
Resolving dependencies...
Build profile: -w ghc-8.8.3 -O1
In order, the following will be built (use -v for more details):
 - veldt-0.1.0.0 (lib) (first run)
Configuring library for veldt-0.1.0.0..
Warning: The 'license-file' field refers to the file 'LICENSE' which does not
exist.
Preprocessing library for veldt-0.1.0.0..
Building library for veldt-0.1.0.0..
[1 of 3] Compiling Veldt.Counter    ( Veldt/Counter.hs, /home/foo/veldt/dist-newstyle/build/x86_64-linux/ghc-8.8.3/veldt-0.1.0.0/build/Veldt/Counter.o )
[2 of 3] Compiling Veldt.Ice40.RgbDriver ( Veldt/Ice40/RgbDriver.hs, /home/foo/veldt/dist-newstyle/build/x86_64-linux/ghc-8.8.3/veldt-0.1.0.0/build/Veldt/Ice40/RgbDriver.o )
[3 of 3] Compiling Veldt.PWM        ( Veldt/PWM.hs, /home/foo/veldt/dist-newstyle/build/x86_64-linux/ghc-8.8.3/veldt-0.1.0.0/build/Veldt/PWM.o )
```
You can find the full RGB Driver source code [here](https://github.com/standardsemiconductor/VELDT-getting-started/blob/master/veldt/Veldt/Ice40/RgbDriver.hs). We now move onto creating a blinker.
### [Fiat Lux: Blinker](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
This is our first demo, we will use our PWM to blink an LED; starting with the LED off, it will light up red, green, then blue and cycle back to off before repeating. Let's begin by setting up a directory for our demos, then setup a blinker demo with cabal.
```console
foo@bar:~$ mkdir -p demo/blinker && cd demo/blinker && cabal init
```
The `blinker.cabal` file will look similar to `veldt.cabal`, except our top-level module `Blinker` is the only exposed module, and we add `clash-ghc` and `veldt` to our `build-depends` list. Additionally, we tell cabal where to find our `veldt` library with a `cabal.project` file.
## [Section 3: Roar](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
## [Section 4: Pride](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
## [Section 5: Where Lions Roam](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
