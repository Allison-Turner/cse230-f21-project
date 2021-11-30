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

## Updates

1. What is the architecture of your application (the key components)?  
Our application has two major components: the user interface, and audio mechanisms. The user interface is built in Brick, and allows for navigation between play and edit modes, shows notes and a staff, and handles key presses and other events. The audio mechanisms are built with SDL, and handle all of the steps between generating a sound wave and pushing that sound wave to an audio device's sound buffer. The user could direct the audio component to interpret a serialized "song" by selecting a file via the interface, or they could "write" a song that they immediately direct the audio component to play.  
  
Our current design leaves a very easy place for us to add other "fantasy console" components; we already have a startup menu, to which we could add more tools as we develop them, and have an initial run vs edit mode model that we could apply to other mediums.  

2. What challenges did you have so far and how did you solve them?  
The biggest challenge has been getting audio synthesis working. To begin with, none of us had experience using typical audio synthesis tools. We quickly found the Synthesizer package, but there are very few example programs which use it and the code base itself is pretty complicated. A simpler method we tried out relied more directly on what Synthesizer uses to produce sound (SoX, the command line utility), but that's not the easiest program to understand either; moreover, SoX has a pretty bad lag issue.  
  
The solution we've opted for, at least at this point in time, is to use SDL2 to directly queue samples in an audio buffer. To be clear, we're constructing the waves ourselves, pushing them into the sound device's queue, unpausing the device, and then SDL magically turns the lengthy integer array we give it into a tone.  
  
Getting to this point was a particularly large challenge since unlike the other main aspects of our project (namely brick), audio synthesis and sequencing seem to be a pretty uncommon task in Haskell. There are waveform synthesizers like Synthesizer, and there are high-level but extremely heavyweight music production suites like Euterpea, but we couldn't really find a simple library which produces a sound given a waveform and a MIDI note (or even a frequency). So we've built that!  

3. Do you expect to meet your goals until the deadline?  
If not, how will you modify your goals?  
We expect to partially meet our goals.  We started out with the very ambitious goal of creating an entire fantasy console, consisting of a music editor, music player, game editor, and game player.  However, we were also clear that this proposal was "shooting for the moon", and even just a music player and editor would be great.  
  
Currently, we're on track to have:  
 - A functioning *tracker*, supporting music playback and interactive music editing  
 - A main menu allowing the user to save and load songs to/from the filesystem  

All in all, despite the unforseen challenges, we're happy to say that we're still within our initial expectations.


## dev notes
```
stack build
stack exec cse230-f21-project-exe
```