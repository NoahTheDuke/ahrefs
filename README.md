# Noah Bogart's Socket Chat App

This is my attempt at Ahrefs' take-home task. A whirlwind learning adventure to get up
to speed with Ocaml.

## Problem Description

(Taken straight from the email.)

Simple one on one chat.

Application should start in two modes:

* as a server, waiting for one client to connect or;
* as a client, taking an IP address (or hostname) of server to connect to.

After connection is established, user on either side (server and client) can send
messages to the other side. After connection is terminated by the client, server
continues waiting for another client. The receiving side should acknowledge every
incoming message (automatically send back a "message received" indication), sending side
should show the roundtrip time for acknowledgment. Wire protocol shouldn't make any
assumptions on the message contents (e.g. allowed byte values, character encoding, etc).

UI is your choice - can be just a console.

Requirements:

* Application is to be compiled and run on Linux
* Implementation language: OCaml
* You may use any 3rd-party general-purpose libraries (extlib, containers, lwt, etc)
* Primary objectives: robustness, code simplicity and maintainability
