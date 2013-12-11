# About

This program tries to find optimal strategy to a simplified variant of Czech
drinking game Macháček ([Liar's Dice]). The simplification consists of two
modifications:

  * only one k-sided dice is used, higher number is ranked better than lower
    number
  * a player is not allowed to announce lesser number than he really rolled
    (this does not make much sense anyway, but helps to simplify the decision
    tree)

[Liar's Dice]: http://en.wikipedia.org/wiki/Liar%27s_dice

To find the strategy the program uses [Koller-Megiddo algorithm], which is
linear in terms of the decision tree of the game. However, the size of the tree
is exponential in terms of the size of dice.

[Koller-Megiddo algorithm]: http://www.maths.lse.ac.uk/Personal/stengel/TEXTE/lemke.html

# Installation

You will need to have [lp_solve] installed. For building, you will also need
GHC compiler and cabal-install (preferably at least version 1.18). The best way
to obtain these is to use the [Haskell platform].

[lp_solve]: http://sourceforge.net/projects/lpsolve/
[Haskell platform]: http://www.haskell.org/platform/

To compile the program, clone the repository and run following commands in the
directory.

    $ git clone https://github.com/lubomir/machacek.git
    $ cd machacek
    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal configure
    $ cabal build

# Usage

Execute

    $ dist/build/machacek/machacek K

where `K` denotes the size of the dice for which you want to find the strategy.

The program assumes `lp_solve` is accessible through `$PATH`. If that is not
the case, modify the command at the bottom of `LPSolve.hs` module and recompile
the program (`cabal build`).
