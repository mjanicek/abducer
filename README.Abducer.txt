Author: Miroslav Janicek <miroslav.janicek@dfki.de>
---

HOW TO INSTALL AND RUN THE ABDUCER SERVER

This file describes the process of building and using the abducer
server.

First of all, you need the Mercury compiler. See the file
`INSTALL.Mercury.txt' in this directory and follow the instructions.
Be patient, the compilation may take a while.

After Mercury is installed, compile the abducer server:

  1) make sure that the variable ICE_HOME is set to the *root* of
     your Ice installation,
     e.g. ICE_HOME=/usr/local or ICE_HOME=/opt/local

  2) run
       $ make

To run the abducer server, run the file `abducer-server' in this
directory.
