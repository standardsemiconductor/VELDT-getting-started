# Where Lions Roam: Haskell & Hardware on the VELDT

## Table of Contents
1. [Introduction & Setup](https://github.com/standardsemiconductor/VELDT-getting-started#introduction--setup)
2. [Section 1](https://github.com/standardsemiconductor/VELDT-getting-started#section-1)
   1. [Learning to Count](https://github.com/standardsemiconductor/VELDT-getting-started#learning-to-count)
   2. [Its a Vibe: PWM](https://github.com/standardsemiconductor/VELDT-getting-started#its-a-vibe-pwm)
   3. [Fiat Lux: LED Blinker](https://github.com/standardsemiconductor/VELDT-getting-started#fiat-lux-led-blinker)
   
**Clicking on any header within this document will return to Table of Contents** 

## [Introduction & Setup](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
    This is an opinionated guide to hardware design from first principles using Haskell and VELDT. However, much of the information within this document can be useful independent of the HDL or FPGA development platform. We assume you have VELDT FPGA development board; if not, go order one from [Amazon](https://www.amazon.com/dp/B08F9T8DFT?ref=myi_title_dp). We also assume you are using Linux, but this is only for getting the tools setup and running the examples. 
  
    Much of the code included in the examples will be written in Haskell and compiled to Verilog using [Clash](https://clash-lang.org/). We find desiging hardware with Haskell to be an enriching experience, and if you are experimenting with HDLs or just starting out with hardware, give it a shot; over time the dividends pay well. Visit the [VELDT-info](https://github.com/standardsemiconductor/VELDT-info#clash) repo for instructions on installation and setup of Haskell and Clash tools. If you design hardware in a language other than Haskell, feel free to skip over the language specific aspects. We hope to translate the examples to other HDLs as this guide develops.
  
    We will be using the Project IceStorm flow for synthesis, routing, and programming. These are excellent, well-maintained open source tools. For installation and setup visit the [VELDT-info](https://github.com/standardsemiconductor/VELDT-info#project-icestorm) repo.
  
    Finally, if you have any suggestions, comments, discussions, edits additions etc. please open an issue in this repo. We value any and all contributions. Lets get started!
## [Section 1](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
### [Learning to Count](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
### [Its a Vibe: PWM](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
### [Fiat Lux: LED Blinker](https://github.com/standardsemiconductor/VELDT-getting-started#table-of-contents)
