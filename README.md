# royal-nova-sword

Royal Nova Sword - Automate allow listing for industrial network.

This repo contains prototype implementation of automated allow listing for
entire industrial network.

## Background

Today, many of vendor proprietary protocols and custom protocols are used in
industry production networks.  In such environment, signature based deep packet
inspection technology doesn't work very well.  Allow listing is sometime an only
viable option to securing such network.  However, creating and applying access
control list by hand is time consuming and error prone method.

That challenge let an idea emerging - let switch create alow list automatically.

Royal Nova Sword expands the idea from a switch to entire network.  In concept,
Royal Nova Sword leans all traffic flowing through whole network and generates
allow list for entire system automatically.  It can apply not only switches but
also routers, wireless LAN controllers and even clouds as long as you can obtain
flow information and you can apply access control to networking gears and
software components.


## Prototyping

This repo contains prototype implementation of the concept Royal Nova Sword.
For simplicity, this prototype only manipulate a switch.

This prototype provides command and web GUI to make a switch learn, lock, and
unlock traffic flows.


## Build

You need [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) to
build backend.  You need [Elm](https://elm-lang.org/) to build frontend.

Pre-compiled frontend code is already contained in this repo as
`/static/index.html`.  If you want to modify frontend source code, modify
`frontend/src/Main.elm`, compile it by Elm compiler, then copy generated
`index.html` under `/static`.


### Building backend

Run `stack build` at top level directory of this repo.

```shell-session
$ stack build
```

You will get executables `rns-learn`, `rns-lock`, `rns-reset`, and `rns-web` under `.stack-work`.  Real path will something look like `.stack-work/dist/x86_64-osx/Cabal-3.0.1.0/build/rns-web/rns-web`.


### Building frontend

The source repo comes with pre-compiled frontend so you don't have to build by
your self but in case you want to build it, follow this instruction.

1. Navigate to `frontend` directory.
1. Run `elm make src/Main.elm` command.
1. Follow error printed by Elm compiler.  You may need to install some
   dependency by `elm install`, then run `elm make src/Main.elm` again.
1. Finally you will get `index.html`.
1. Copy `index.html` to `../static/`.

Below is typical log.

```shell-session
frontend$ elm make src/Main.elm
Starting downloads...

  ● elm/json 1.1.3
  ● elm/url 1.0.0
  ● elm/time 1.0.0
  ● elm/bytes 1.0.8
  ● elm/file 1.0.5
  ● elm/http 2.0.0
  ● elm/html 1.0.0
  ● elm/browser 1.0.2
  ● mdgriffith/elm-ui 1.1.8
  ● elm/virtual-dom 1.0.2
  ● elm/core 1.0.5

Dependencies ready!
Success! Compiled 1 module.

    Main ───> index.html

frontend$
```

## Usage

Prepare your switch accept RESTCONF.  This prototype only tested with Cisco
IE-3400 with expansion module running Cisco IOS XE 17.3.1.

Start web GUI.

```shell-session
$ .stack-work/dist/x86_64-osx/Cabal-3.0.1.0/build/rns-web/rns-web SWITCH-ADDRESS USERNAME PASSWORD
```

Press "Learn" button to start learning flow.  Press "Lock" to lock down the
switch.  Press "Unlock" to remove all access control list from the switch.
Performance is not optimized.  You may need to wait several seconds to complete
your request.


### Learning

When you press "Learn" button (or you run `rns-learn` command), the app deletes
all access control lists and NetFlow configurations, create a Flexible NetFlow
monitor configuration and apply it to some interfaces.  Once it completed, the
switch stats populating a custom NetFlow record cache.


### Locking

When you press "Lock" button, (or you run `rns-learn` command), the app queries
NetFlow cache, convert obtained flow information to access control list requests
in RESTCONF, then add those access control lists and apply them to appropriate
interfaces.


### Unlocking

When you press "Unlock" button (or you run `rns-reset` command), the app removes
access control lists and NetFlow configurations from the switch.


### Important Note

This prototype only learns and locks traffic flows on `GigabitEthernet2/1`
through `GigabitEthernet2/4`.  Any traffic flows on other port is not impacted.