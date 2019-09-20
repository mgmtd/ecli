cli
===

An OTP application providing an online command line interface.

It provides a bunch of components designed to make it easy to build telecom
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

Build
-----

    $ rebar3 compile

Test
----

    $ rebar3 eunit
