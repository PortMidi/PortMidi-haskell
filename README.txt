This is a Haskell module for PortMidi audio library, which supports
real-time MIDI input and output. 

========
ChangLog
========


- Wed Sep 16 EST 2009

  remove conflicts in cabal file.

  fix compilation problem on OS X.

- Wed Sep 9 EST 2009

  Fix the compilation problem with GHC 6.10.2+ on OS X.

- Mon Sep 7 EST 2009

  Package up for Hackage DB.

- Thu Sep 4 EST 2008

  Initial 0.1 release, very experimental, and lack proper documentation.

============
Installation
============

The usual cabal installation steps:

1. To configure the module:

       runhaskell Setup.hs configure
   or

       runhaskell Setup.hs configure --user --prefix=DIR

   if you want to install the package to your user's directory instead
   of the system one (replace DIR with your own directory of choice).
2. To build the module:

       runhaskell Setup.hs build

3. To install the module:

       runhaskell Setup.hs install

   This will install a Sound.PortMidi module for GHC.

===================
Contact Information
===================

You may send your bug report and feature request to the package 
maintainer: Paul H. Liu <paul@thev.net>.

--
Last Update: Wed May 6 PDT 2015
 


