# SYNC-BASE

The `sync-base.asm` file has the bare-minimum code required to create a PAL composite video sync. There are comments inside to help you adapt the code to your own projects.

## Info
- The sync code only requires 2 bytes of register space to operate.
- There is a scanline counter.
- Active Video Region (AVR) flag: your code can use this to determine if you are in the vertical viewing region. (The upper and lower blanking regions can be adjusted by line.)
- The PIC must be clocked at 20MHz
