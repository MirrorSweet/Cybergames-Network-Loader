# Cybergames-Network-Hack

A loader for [Cybergames Network](http://th.playcybergames.com/main/showthread.php?16378-Cybergames-Network).

## How does it works

Behind the scence, we don't break any file, we're just inject the dll to patch some memoery address while the app is still starting or started (this process is called 'Loader').

We have 2 files including

- Cybergames Loader.exe - the loader, works by load the original executable, when it was started we gonna inject the .dll file name 'CybergamesH.dll' to patch some memory address that allows us to use the VIP features without paid.
- CybergamesH.dll - the worker is generally like a timer, it will wake up every 1 seconds and search for every windows that we wanted to destroy (mostly ads window) and close destroy the ads windows using windows' API (TBD).

## Special thanks

DevilPooh for his speed hack tricks.
