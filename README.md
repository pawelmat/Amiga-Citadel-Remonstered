# Amiga Citadel 1.3 "Remonstered"

## Introduction

This is a 2022 update of the original Citadel from 1995. 
- The 3D rendering engine has been optimised as much as possible within the current game architecture, resulting in a 4-5x + performance increase
- Gameplay has been improved by implementing additional usability features such as WASD+mouse simultaneous control, auto-weapon change etc.
- Level graphics has been significantly updated including textures and enemies
- Level maps have been corrected or re-designed for better playing experience, fast-paths added in huge levels etc.
- Difficulty has been re-balanced
- FPS counter added
- Crosshairs added
- Speed issues fixed on fast machines: 030/040, overclocked CPUs, Vampire, Warp 1260 and other accelerators, full speed WinUAE etc.
- Manual has been re-written with an extended back-story and updated content
- ... and many more. For a more complete list of changes see the WHDLoad/Citadel/ReadMe file.

## System Requirements

Minimum configuration: Any Amiga with a minimum of 0.5MB Chip + 1MB other (prefferably Fast) memory. Additional memory is required for WHDLoad to run.

Suggested configuration: Amiga 1200 with 68020/14MHz or faster with minimum 0.5MB Chip and 1MB+ Fast memory. The faster the better although the game is capped to 50fps.

## Development environment

A system with minimum 1MB chip and 4 MB Fast memory is required. Can be a real Amiga or emulator-based.

The game can be assembled using AsmOne (v1.20 or newer). On starting it allocate a minimum of 600kb of Public or Fast memory for your workspace. The main file to start the game is Citadel_1_3_135.ss and the entry point is labeled 's'. 

### Running the game from AsmOne

In order to run the game from assembler:
1. Navigate to the directory where you cloned this project and where the main files (Citadel_1_3_135.ss etc.) reside - this will be the working directory.
2. Load AsmOne (v1.20 or newer).
3. Select 'p' and then '600' to allocate working memory.
4. Load the main Citadel source code file 'Citadel_1_3_135.ss' into the AsmOne editor (r Citadel_1_3_135.ss)
5. Press ESC to go into the actual assembler editor. Towards the top of the file find the BASEF label, it's the line 'BASEF1:		equ	$00400000'. In many cases this should be fine but if your memory is located at a different address then change this address to where you have a free 1MB of (prefferably Fast) memory. This can be checked using SysInfo or a similar tool, best select 1MB below the top address of the available memory to avoid conflicts with other running software.
6. Press ESC again to go to the AsmOne command window.
7. Assemble (a), load externs (e) and run the game (j s).

### Preparing the CYT.DAT resource file for WHDLoad

This version of the game is meant to work with WHDLoad by patching the original game. WHDLoad 18.5 or higher is recommended. As a starting point, take the files from the WHDLoad/ folder. These include the citadel.slave install file, cyt.dat resource file and all disks. Note that you should use only **these** disks, not any found elsewhere on the Internet, as they contain the right version of other files and in particular Disk 5 contains all updated graphics.

In order to prepare an updated cyt.dat resource file for WHDLoad:
1. Follow steps 1-5 from the previous section.
2. Change the IS_EXE flag in the source code from 'IS_EXE: equ 0' to 'IS_EXE: equ 1'.
3. Press ESC again to go to the AsmOne command window.
4. Type 'wb cyt.dat'.
5. When prompted BEG> type 's', and then when prompted END> type 'end'. This will save cyt.dat in the working directory.
6. Copy cyt.dat to your folder with the WHDLoad install and other files, replacing the old cyt.dat.
7. Run the game: 'whdload citadel.slave'

## Copyright and License

Copyright (C) 1995, 2022 Pawel Matusz, Artur Bardowski. 
This software is free to copy and use for non-commercial purposes under the terms of the GNU GPL-3.0 license. 

## Warranty

This package comes with absolutely no warranty of any kind, either express or implied, statutory or otherwise. The entire risk as to use, results and performance of the package is assumed by you and if the package should prove to be defective, you assume the entire cost of all necessary servicing, repair or other remediation.
Under no circumstances, can the authors be held responsible for any damage caused in any usual, special, or accidental way, also if the owner or a third party has been pointed at such possibilities of damage.

