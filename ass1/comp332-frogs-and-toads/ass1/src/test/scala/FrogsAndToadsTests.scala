/*
 * This file is part of COMP332 Assignment 1.
 *
 * Copyright (C) 2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Tests of the Frogs and Toads puzzle solver.
 * Uses the ScalaTest `FlatSpec` style for writing tests. See
 *
 *      http://www.scalatest.org/user_guide
 *
 * For more info on writing ScalaTest tests.
 */

package org.mq.frogsandtoads

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class FrogsAndToadsTests extends FlatSpec with Matchers {

  import PuzzleState._

//checks amount of cells at initial position
  for (x <- 1 to 10) {
    for (y <- 1 to 10) {
      "A puzzle state with " + x + "frogs and " + y +" toads:" should
        "have " + (x+y+1) + " cells" in {
        assert(PuzzleState(x, y).size == x+y+1)
      }
    }
  }

  //checks amount of cells at terminal position
  for (x <- 1 to 10) {
    for (y <- 1 to 10) {
      "A puzzle state with " + x + "frogs and " + y +" toads:" should
        "have " + x+ " + " + y +" + " +1 + " cells in its terminal state" in {
        assert(solve(PuzzleState(x, y)).takeRight(1).head.size == x+y+1)
      }
    }
  }

  //checks position of empty cell at initial position
  for (x <- 1 to 10) {
    for (y <- 1 to 10) {
      it should "have its empty cell at position" + x +" for frogs " + x + "toads" + y in {
        assertResult(x) {
          PuzzleState(x, y).emptyLoc
        }
      }
    }
  }

  //checks position of empty cell at terminal position
  for (x <- 1 to 10) {
    for (y <- 1 to 10) {
      it should "have its empty cell at position " + (PuzzleState(x,y).size-x-1) +" at terminal state for frogs " + x + " and toads" + y in {
        assertResult(y) {
          solve(PuzzleState(x, y)).head.emptyLoc
        }
      }
    }
  }

  //checks that initial position != terminal position
  for (x <- 1 to 10) {
    for (y <- 1 to 10) {
      it should "not be constructed in the terminal puzzle state for frogs " + x + " and toads " + y in {
        assert(!PuzzleState(x, y).isTerminalState())
      }
    }
  }

//checks that the solve function returns a sequence such that the last PuzzleState in the sequence is in the terminal position. i.e. Did solve work?
  for (x <- 1 to 10){
    for( y <- 1 to 10) {
      it should "finish in terminal state for frogs: " +x+" and toads: "+y in {
        assert(solve(PuzzleState(x, y)).head.isTerminalState())
    }
    }
  }
  //checks if there are multiples PuzzleStates in the solution
  for (x <- 1 to 10){
    for( y <- 1 to 10) {
      it should "return a sequence of more than 3 PuzzleStates: for " +x+" frogs: and "+y+" toads" in {
        assert(solve(PuzzleState(x, y)).size > 3)
      }
    }
  }

}
