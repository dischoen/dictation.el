dictation.el
============

Emacs lisp package to play and pause voice recordings, for easier transcription

Currently (Version 0.1), mp3 and ogg are supported.
To play respective audio files, the application mplayer and/or mpg123
must be installed.
I think this implies a Linux/BSD like OS.

The audio player is started as a asynchronous subprocess of GNU Emacs.
(I haven't tested on XEmacs).
The key F4 is remapped with a local binding to act as a pause toggle key.
When the playback ends, the previous binding is restored, if there was any.

Bug reports please here on github:
https://github.com/dischoen/dictation.el/issues

Hm. I think that's all.
