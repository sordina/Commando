# Commando

Watch a directory and run a command (with optional arguments).

<img src="http://sordina.binaries.s3.amazonaws.com/commando.png" alt="Commando Command Watcher" />

### Usage

    commando [COMMAND] [-q|--quiet] [-c|--consumer] [-i|--stdin] [-p|--persist] [DIRECTORY]

    Available options:
      -h,--help                Show this help text
      COMMAND                  Command run on events
      -q,--quiet               Hide non-essential output
      -c,--consumer            Pass events as argument to command
      -i,--stdin               Pipe events to command
      -p,--persist             Pipe events to persistent command
      DIRECTORY                Directory to monitor

## Examples

Useful in conjunction with [Conscript](https://github.com/sordina/Conscript):

    commando -c echo | grep --line-buffered Add  | conscript ls

Emit "event" everytime a file is changed in the current directory:

    commando

## Binaries

* [commando-1.0.0.3-MacOSX-10.7.5-11G63b.zip](http://sordina.binaries.s3.amazonaws.com/commando-1.0.0.3-MacOSX-10.7.5-11G63b.zip)
