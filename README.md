Cider - A network calculator
============================

**cider** is a tool for listing all of the IP addresses contained within a
CIDR block. Given a CIDR block, it will spit out each _assignable_ IP address
in that block.

Building
--------

Cider is written in Haskell, and you must have [Stack] installed to build it.
Follow the instructions on the Stack homepage to install Stack. Then, to build
and test Cider:

    $ stack build
    $ stack test

Installation
------------

To install Cider, run:

    $ stack install

By default, Cider will be installed to `~/.local/bin/cider`.

You can also download a pre-compiled binary for your platform from [release]
page. Unpack the tarball and copy the `cider` binary to somewhere on your
`$PATH`.

Usage
-----

Get the IP addresses by calling `cider` with a CIDR block as an argument. For
example,

    $ cider 192.168.0.3/29
    192.168.0.1
    192.168.0.2
    192.168.0.3
    192.168.0.4
    192.168.0.5
    192.168.0.6

Run

    $ cider --help

for help information.

  [release]: https://github.com/mdippery/cider/releases
  [Stack]:   https://docs.haskellstack.org/en/stable/README/
