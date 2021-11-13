# CSE230 Final Project (Fall 2021)

We will be beginning development of a [fantasy console](https://en.wikipedia.org/wiki/Fantasy_video_game_console) written in Haskell. Our current design won't have strict emulated hardware constraints, but that might change during development. 

### Group

Allison Turner
Joey Rudek
Mark Barbone
James Yuan

### Project roadmap

1. Build the "terminal" from which users can launch applications. This should contain
   1. a simple prompt which displays the working directory and allows users to enter commands;
   2. cd, mkdir, and rm (without allowing users to leave the console's root folder); and
   3. the ability to launch applications written for the console.
2. Build a song-editing application (something similar to a [tracker](https://en.wikipedia.org/wiki/Music_tracker), in particular perhaps inspired by [LSDJ](https://www.littlesounddj.com/lsd/index.php)).
3. Build a graphics-editing application.
4. Build a game code editor, which will likely involve either including a compiler for something like Lua or creating our own language. These games should be launchable from the terminal (#1).
5. Build a sound editor so that more complex waveforms can be used in the song editor (#2).
6. Create a handy export format so users can share applications.

Creating a fantasy console is, admittedly, very ambitious! (That's probably why the only one we could find made in Haskell, [Piyo](https://github.com/opyapeus/piyo/), was abandoned three years ago after one commit.) As such, we've designed the above roadmap with time constraints in mind: if at the deadline we only have a simple tracker application, that's still something nobody seems to have made in Haskell. On the other hand, no matter how fast we work, it's unlikely we'll complete all 6 steps above by the deadline, but there's a good chance development will continue afterward.

### Dependencies

1. for the terminal, [haskeline](https://hackage.haskell.org/package/haskeline) (line editing, tab completion, history, etc).
2. for playing music, [sdl2-mixer](https://hackage.haskell.org/package/sdl2-mixer), and for reading/writing midi files (and other related formats), [HCodecs](https://hackage.haskell.org/package/HCodecs).
3. for graphics editing, brick seems to provide everything.
4. for game code editing, [brick-skylighting](https://hackage.haskell.org/package/brick-skylighting) provides syntax highlighting. If we want to embed lua, [lua](https://hackage.haskell.org/package/hslua).