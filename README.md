dictation.el
============

Emacs lisp package to play and pause voice recordings, for easier transcription

Version 0.2 uses mplayer, so every audio format which is supported by mplayer,
can be used for dictation.

The audio player is started as a asynchronous subprocess of GNU Emacs.
(I haven't tested on XEmacs).

Keys/Functions:
F4 This is the pause button. It toggles from playback to pause mode.
F3 Jumps back 10 seconds.
F5 Jumps forward 10 seconds.

All the functions are mapped buffer-locally to the keys.
When the playback ends, the previous binding is restored, if there was any.

Bug reports please here on github:
https://github.com/dischoen/dictation.el/issues

Hm. I think that's all.

Installation
As usual. Put it in your load-path and require it.
