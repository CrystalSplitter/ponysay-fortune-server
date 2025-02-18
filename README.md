# Ponysay Webserver (ponysay-server)

Run ponysay, but as a webserver!

## Running from command line

```shell
$ ponysay-server
```

This runs on port `3000` by default.

## Installation

### Nix Flakes

Try it out with:

```shell
$ nix shell github:CrystalSplitter/ponysay-server
$ ponysay-server
```

And install with:

```shell
$ nix profile install github:CrystalSplitter/ponysay-server
$ ponysay-server
```

### Nix (Non-Flakes)

Less supported, but should still work.

```shell
$ nix-build .
```

### From Source

First you need to install the dependencies. Sadly,
these are outside of cabal, so they need to be installed
separately. These are:


* [ansi2html](https://pypi.org/project/ansi2html/)
* [ponysay fork](https://github.com/erkin/ponysay)
  (The original [ponysay](https://github.com/erkin/ponysay), is no longer
  maintained and has many outstanding issues.)

Once that is done, you can finally install this package via cabal:
Download then download the source and run `cabal install .` locally.

## Writing a systemd service

Here's an example systemd service for your ponysay
webserver:

```
[Unit]
Description=Run ponysay as a webserver
After=network.target
StartLimitIntervalSec=0

[Service]
Environment="PATH=/home/runner/.local/bin:/bin:/usr/bin:/usr/local/bin:/usr/games"
Type=exec
Restart=always
RestartSec=2
User=runner
ExecStart=/home/runner/bin/ponysay-server

[Install]
WantedBy=multi-user.target
```

Replace `runner` with your service runner account. **Note that this example
makes `runner` root equivalent**, as `runner` can write to
`.local/bin` or `/home/runner/bin/ponysay-server`. Ultimately, both `ponysay`
and `ansi2html` must be in the service `PATH` for `ponysay-server` to run.
