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

This guide is split into several sections. Each section begins with construction of sub-components then culminates with an application which utilizes the sub-components. [Section 1](https://github.com/standardsemiconductor/VELDT-getting-started#section-1-fiat-lux) constructs a simple blinker, the "hello-world" of FPGAs. [Section 2](https://github.com/standardsemiconductor/VELDT-getting-started#section-2-roar) covers serializers and deserializers which are used to construct a UART. In [Section 3](https://github.com/standardsemiconductor/VELDT-getting-started#section-3-pride) we learn how to interact with the memory provided by VELDT. Finally, in [Section 4](https://github.com/standardsemiconductor/VELDT-getting-started#section-4-where-lions-roam) we design a simple CPU with a custom ISA, then integrate our LED, UART, and memory peripherals to create a stored-program computer. By the end of the guide, you will have a library of commonly used sub-components along with a directory of applications demonstrating their useage. The library and demos explained in this guide are available in this repo, see the [veldt] and [test] directories.

Finally, if you have any suggestions, comments, discussions, edits additions etc. please open an issue in this repo. We value any and all contributions. Lets get started!
## [Section 1: Fiat Lux](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
In this section we start by building a counter. Then using the counter, construct a PWM. Equipped with our counter and PWM, we integrate a RGB LED Driver IP to create our first running application on VELDT; a blinker!
### [Learning to Count](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
### [Its a Vibe: PWM](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
### [Fiat Lux: Blinker](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
## [Section 2: Roar](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
## [Section 3: Pride](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
## [Section 4: Where Lions Roam](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
