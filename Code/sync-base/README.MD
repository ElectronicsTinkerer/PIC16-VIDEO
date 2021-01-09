# SYNC-BASE

The `sync-base.asm` file has the bare-minimum code required to create a PAL composite video sync. There are comments inside to help you adapt the code to your own projects.

## Info
- The sync code only requires 2 bytes of register space to operate.
- There is a scanline counter.
- Active Video Region (AVR) flag: your code can use this to determine if you are in the vertical viewing region. (The upper and lower blanking regions can be adjusted by line.)
- The PIC must be clocked at 20MHz

## Assembling
To assemble the code, I use the MPLAB X IDE from Microchip. Create a new standalone project, using `pic-as` as the compiler toolchain. Once the project has been created, put the assembly file in the project's source folder. In the IDE, go to `Production > Set Project Configuration > Customize...` Then under `Categories`, go to `pic-as Global Options`. Here is an `Additional options` box, which needs the following arguments: `-Wa,-a -Wl,-pRESET_VEC=0h,-pISR_VEC=4h` to setup the correct locations for the PSECTS.
