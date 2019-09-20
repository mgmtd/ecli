cli
===

An Erlang/OTP application providing an online command line interface.

Provides a collection of parts designed to make it easy to build telecom
style command line access to a system. Features:

* Small C program 'cli' to access the command line via a unix domain socket

* Unix domain socket server to receive connections from the cli program

* Example implementation - cli_juniper.erl to show how to build a
  Juniper style command line interface

* Expansion assistant - library module to provide tab/spc completion
  of trees of items that include a few key attributes (name, children,
  node type, action).

* Command parser - library module to parse a command line against a
  tree of items

Status
------

This code is under active development. None of the APIs are in any way
stable and are likely to change. But it does something, and could be
fun to play with.

Origin
------

Much of the tricky stuff comes from edlin.erl of the Erlang/OTP
distribution, with more from Stevens and various blog posts.

Why?
----

I don't want to have to build yet another closed source version of
this. Now I don't work for a telecom supplier I have freedom to
hack on telecom stuff of my own.

Build
-----

    $ rebar3 compile

Run in test mode
----------------

    $ rebar3 shell

    Eshell V10.4  (abort with ^G)
    1> application:start(cli).
    ok
    2> cli:open("/var/tmp/example.socket", cli_juniper).
    {ok,<0.131.0>}
    3>

Then attach the cli and poke around:

    ./_build/default/lib/cli/priv/cli

    Welcome to the Juniper style CLI
    Hit TAB, SPC or ? at any time to see available options

    Seans-MacBook> configure

    [ok]
    Seans-MacBook# <TAB>
    show Show configuration
    set  Set a configuration parameter
    exit Exit configuration mode
    Seans-MacBook# exit

    [ok]
    Seans-MacBook>

Many of the usual command editing shortcuts work as they do in the
erlang shell and other typical emacs-ish shells:

    CTRL-a - Start of line
    CTRL-e - end of line
    CTRL-b - backwards character
    CTRL-f - forward character
    CTRL-h - backward delete character
    CTRL-d - forward delete character
    CTRL-l - redraw line
    CTRL-t - transpose characters
    CTRL-k - kill to end of line
    CTRL-y - yank killed text
    CTRL-w - backwards delete word (buggy right now)

    Cursor left right should work
    CTRL-left and right - back / forward word

    TAB - completion

There are more planned, also to fix multiline bugs

Test
----

    $ rebar3 eunit

Todo
----

Many things:

- [ ] Hook up sigwinch events in cli and pass up to server
- [ ] Grab the user details from the unix domain socket and pass to an
      authentication callback of some kind.
- [ ] Completion of value types - strings, leaf lists, enums
- [ ] Use the values extracted from terminfo properly
- [ ] CTRL-c handling in the cli program
- [ ] CTRL-d to close session (or exit configure mode) if at start of line
- [ ] Lots more of the control keys in cli_edlin.erl need to be hooked up
- [ ] cursor back from a second line goes to the wrong place
- [ ] proper logging of user commands
- [ ] skipping over control sequences in output e.g. coloring
