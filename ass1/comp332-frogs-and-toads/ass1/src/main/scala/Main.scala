/*
 * This file is part of COMP332 Assignment 1.
 *
 * Copyright (C) 2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.mq.frogsandtoads

/**
  * The top level object of the Frogs and Toads application.
  */
object Main {
  import cats.instances.all._
  import doodle.core._
  import doodle.image._
  import doodle.java2d._
  import monix.reactive.Observable
  import doodle.java2d.effect._
  import doodle.interact.syntax._
  import doodle.effect.Writer.Gif

  def runAnimation(title: String, frames: Seq[Image]) {

    val animation: Observable[Picture[Unit]] =
      Observable
        .fromIterable(
          frames.flatMap(Seq.fill(20)(_))
        )
        .map(Image.compile)

    val window: Frame =
      Frame.fitToPicture().background(Color.white).title(title)

    animation.animateFrames(window)
  }

  def main(args: Array[String]) {
    runAnimation("Frogs and Toads", PuzzleState.animate(5,8))
  }
}
