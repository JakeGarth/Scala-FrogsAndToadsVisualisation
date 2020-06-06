/* Jake Garth - 44887841
 * This file is part of COMP332 Assignment 1.
 *
 * Copyright (C) 2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.mq.frogsandtoads

import doodle.core._
import doodle.syntax._
import doodle.image._
import org.mq.frogsandtoads

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

/**
  * A puzzle state is given as a 1-dimensional array of cell values.
  */


class PuzzleState private (
    board: Vector[PuzzleState.Cell],
    loc: Int,
) {

  import PuzzleState._

  val size = board.size
  val emptyLoc = loc

  def isTerminalState(): Boolean = {
    board.slice(0, emptyLoc).forall(_ == Toad) &&
    board(emptyLoc) == Empty &&
    board.slice(emptyLoc + 1, size).forall(_ == Frog)
  }

  def isInitialState(): Boolean = {
    board.slice(0, emptyLoc).forall(_ == Frog) &&
    board(emptyLoc) == Empty &&
    board.slice(emptyLoc + 1, size).forall(_ == Toad)
  }


  //returns the board for a Puzzlestate
  def getBoard(): Vector[PuzzleState.Cell] = {
    board
  }


  //returns the Frog/Toad/Empty at position "pos" of the board
  def getBoardState(pos: Int): PuzzleState.Cell = {
    board(pos)
  }


  //the toadLeft method works by:
  def toadLeft(): Option[PuzzleState] = {
    val thisBoard: Vector[PuzzleState.Cell] = getBoard()
    val emptyIndex: Int = thisBoard.indexOf(Empty)
    if( emptyIndex != size-1){                   //checking if the swap would cause an outofbounds exception
    if (thisBoard(emptyIndex + 1) == Toad) {     //checking if there is a Toad directly to the right of Empty
    return  Some(new PuzzleState(                //if so, return a new PuzzleState by swapping Empty and Toad
       thisBoard.take(loc).++(Vector(Toad)).++(Vector(Empty)).++(thisBoard.takeRight(size-loc-2)), loc+1))
  } else {
      return None                               //else, Return None
    }
    } else {
      return None                                //else, Return None
    }
  }


  //the frogRight method works by:
  def frogRight(): Option[PuzzleState] = {

    val thisBoard: Vector[PuzzleState.Cell] = getBoard()
    val emptyIndex: Int = thisBoard.indexOf(Empty)
    if(emptyIndex != 0) {                       //checking that the empty object is not at the left most position, ensuring no outofbounds exception
      if (thisBoard(emptyIndex - 1) == Frog) {  //if there is a Frog before the empty spot, do the swap
        return Some(new PuzzleState(            //returns a new Puzzlestate where the empty spot and frog have swapped
          thisBoard.take(loc - 1).++(Vector(Empty)).++(Vector(Frog)).++(thisBoard.takeRight(size - loc - 1)), loc - 1))
      } else {
        return None                             //if this swap is not possible, return nothing

      }
    } else{
      return None
    }
  }


  //the toadJumpLeft method works by:
  def toadJumpLeft(): Option[PuzzleState] = {
    val thisBoard: Vector[PuzzleState.Cell] = getBoard()
    val emptyIndex: Int = thisBoard.indexOf(Empty)
    if(emptyIndex != size-1 && emptyIndex  != size -2) {   //checking if the swap would cause an outofbounds exception
      if (thisBoard(emptyIndex + 1) == Frog && thisBoard(emptyIndex + 2) == Toad) {    //if the layout of the board is Empty, Toad, Frog, do the jump
        return Some(new PuzzleState(                                                   //do the jump
          thisBoard.take(loc).++(Vector(Toad)).++(Vector(Frog)).++(Vector(Empty)).++(thisBoard.takeRight(size - loc - 3)), loc + 2))

      } else {                                       //if not possible, return None
        return None
      }
    } else{
      return None                                    //if not possible, return None
    }
  }
//the frogJumpRight method works by:
  def frogJumpRight(): Option[PuzzleState] = {
    val thisBoard: Vector[PuzzleState.Cell] = getBoard()
    val emptyIndex: Int = thisBoard.indexOf(Empty)
    if(emptyIndex != 0 && emptyIndex  != 1) {     //checking if there will be an outofbounds issue by using the "thisBoard" method
      if (thisBoard(emptyIndex - 1) == Toad && thisBoard(emptyIndex - 2) == Frog) {  //checking if board layout is Frog, Toad, Empty
        return Some(new PuzzleState(                                                 //if so, return a Puzzlestate where the new board is such that the Frog jumped
          thisBoard.take(loc - 2).++(Vector(Empty)).++(Vector(Toad)).++(Vector(Frog)).++(thisBoard.takeRight(size - loc - 1)), loc - 2))
      } else {
        return None                                                                  //else, return None

      }
    } else{
      return None
    }
  }
}

object PuzzleState {

  sealed abstract class Cell
  case object Frog extends Cell
  case object Toad extends Cell
  case object Empty extends Cell

  def apply(frogs: Int, toads: Int): PuzzleState = {
    if (frogs <= 0 || frogs > 10)
      throw new Exception("The number of frogs must be between 1 and 10.")

    if (toads <= 0 || toads > 10)
      throw new Exception("The number of frogs must be between 1 and 10.")

    new PuzzleState(
      Vector.fill(frogs)(Frog) ++ Vector(Empty) ++
        Vector.fill(toads)(Toad),
      frogs
    )
  }


  def solve(start: PuzzleState): Seq[PuzzleState] ={
    if(start.isTerminalState()){                     //If the terminal state has been achieved, return the sequence of PuzzleStates
      return Seq(start)                              //which have been built upon during the pattern matching/recursion
    }
    start.frogJumpRight() match {
      case Some(newState) => solve(newState) match { //if a jump to the right is possible, return:
        case null =>                                 //null if solve(newState) doesn't reach a terminal state;
        case sequen => return sequen++Seq(start)     //or, a sequence of PuzzleStates made up of a correct sequence of PuzzleStates
      }
      case None =>                                   //if a jump to the right is NOT possible, ignore this route
    }
     start.toadJumpLeft() match {
       case Some(newState) => solve(newState) match { //if a jump to the left is possible, return:
         case null =>                                 //null if solve(newState) doesn't reach a terminal state;
         case sequen => return sequen++Seq(start)}    //or, a sequence of PuzzleStates made up of a correct sequence of PuzzleStates
       case None =>                                   //if a jump to the left is NOT possible, ignore this route
     }
     start.toadLeft() match {
       case Some(newState) => solve(newState) match {  //if a slide to the left is possible, return:
         case null =>                                  //null if solve(newState) doesn't reach a terminal state;
         case sequen => return sequen++Seq(start)      //or, a sequence of PuzzleStates made up of a correct sequence of PuzzleStates
       }
       case None =>                                    //if a slide to the left is NOT possible, ignore this route
     }
    start.frogRight match {
      case Some(newState) => solve(newState) match {  //if a slide to the left is possible, return:
          case null =>                                //null if solve(newState) doesn't reach a terminal state;
          case sequen => return sequen++Seq(start)    //or, a sequence of PuzzleStates made up of a correct sequence of PuzzleStates
      }
      case None =>                                    //if a slide to the left is NOT possible, ignore this route
    }
null                                                  //if no moves are available and a terminal state is not achieved, return null.
  }


  val squareboiGreen =                    //these are the squares which will be combined later to make the board
    Image.square(100)               //this is the frog square
      .fillColor(Color.green)
      .strokeColor(Color.black)
      .strokeWidth(4)

  val squareboiBlue =                    //this is the toad square
    Image.square(100)
      .fillColor(Color.blue)
      .strokeColor(Color.black)
      .strokeWidth(4)

  val squareboiWhite =                   //this is the empty square
    Image.square(100)
      .fillColor(Color.white)
      .strokeColor(Color.black)
      .strokeWidth(4)

  def animate(start: PuzzleState): Seq[Image] = {

    if(solve(start) == null){ //if the solve method returns null because it doesn't find a solution, draw an empty screen
      Seq()
    } else {
      seqImageMaker(solve(start), Seq())
    }//the animate method is essentially the "seqImageMaker" method
  }

  //the seqImageMaker returns the sequence of images for the animate method to display
  def seqImageMaker(PuzzleSeq: Seq[PuzzleState], ImageSeq: Seq[Image]):
  Seq[Image] = PuzzleSeq match{                                                                //given a sequence of PuzzleState's,
  case PuzzleSeq if (PuzzleSeq.isEmpty) => ImageSeq                                            //if the sequence of PuzzleState's is empty, return the sequence of images
  case _ => seqImageMaker(PuzzleSeq.take(PuzzleSeq.size-1),
    ImageSeq:+builder(PuzzleSeq.takeRight(1).head.size-1,PuzzleSeq.takeRight(1).head))                  //if it's not empty, use the "builder" method to make an image of the
  }                                                                                                   //front-most PuzzleState, then, parse in a new sequence of PuzzleStates, with the front Puzzlestate removed

  def builder(count: Int, start: PuzzleState): Image =                                  //this method builds the image from a given Puzzlestate.
    count match {
      case n if(n==0 && start.getBoardState(0) == Toad) => squareboiBlue          //first it checks the base case, it returns the very left-most square and returns that
      case n if(n==0 && start.getBoardState(0) == Frog) => squareboiGreen
      case n if(n==0 && start.getBoardState(0) == Empty) => squareboiWhite
      case n if start.getBoardState(n) == Toad =>                                       //then, if it is not in the left-most square, it returns a square of the colour corresponding
        val here = squareboiBlue                                                        //to frog/toad/empty
        builder(n-1,start).beside(here)                                                 //Then, it moves on to the next left square. This continues until there is only one square left to make
      case n if start.getBoardState(n) == Frog =>                                       //which the base case will take care of
        val here = squareboiGreen
        builder(n-1,start).beside(here)
      case n if start.getBoardState(n) == Empty =>
        val here = squareboiWhite
        builder(n-1,start).beside(here)
    }

  def animate(frogs: Int, toads: Int): Seq[Image] =           //calls the animate function, parsing through the given PuzzleState
      animate((PuzzleState(frogs,toads)))

}
