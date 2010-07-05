From: Miroslav Janicek <mira.janicek@gmail.com>
---

HOW TO INSTALL AND RUN THE ABDUCER SERVER

This file describes the process of building and using the abducer
server.

First of all, you need the Mercury compiler. See the file
`INSTALL.Mercury.txt' in this directory and follow the instructions.
Be patient, the compilation may take several hours. :)

After Mercury is installed, compile the abducer server:

  1) make sure your env variable MERCURY_HOME is set to the root of
     your Mercury installation, and MERCURY_HOME/bin is in $PATH
     set the variable ICE_HOME to the *root* of your Ice installation
     e.g. ICE_HOME=/usr/local or ICE_HOME=/opt/local
  2)   $ cd dfki/subarchitectures/comsys/src/mercury-c++/abduction
  3) run
       $ make

The compilation isn't automated, so to be sure, you should recompile
the abducer after every change in its directory. I'd like to build it
in CMake, but I don't know the language yet, so if you do, you might
help me with that :)

Now, the CCA component requires the abducer server up and running.
Again, there is no automated way to do that yet; you will need to run
it by hand. To do so,

  1) go to dfki/ (site root)
  2) run
       $ subarchitectures/comsys/src/mercury-c++/abduction/abducer-server

That should be it...

Let me know in case of any problems! (The e-mail is on the first line
of this file.)
