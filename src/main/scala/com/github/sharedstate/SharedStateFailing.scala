package com.github.sharedstate

import cats.effect._
import cats.effect.concurrent.Ref
import cats.instances.list._
import cats.syntax.all._

import scala.concurrent.duration._

object SharedStateFailing extends IOApp {

  var myState: Ref[IO, List[String]] = _

  def putStrLn(str: String): IO[Unit] = IO(println(str))

  val process1: IO[Unit] = {
    putStrLn("Starting process #1") *>
      IO.sleep(5.seconds) *>
      myState.update(_ ++ List("#1")) *>
      putStrLn("Done #1")
  }

  val process2: IO[Unit] = {
    putStrLn("Starting process #2") *>
      IO.sleep(3.seconds) *>
      myState.update(_ ++ List("#2")) *>
      putStrLn("Done #2")
  }

  val process3: IO[Unit] = {
    putStrLn("Starting process #3") *>
      IO.sleep(10.seconds) *>
      myState.update(_ ++ List("#3")) *>
      putStrLn("Done #3")
  }

  def masterProcess: IO[Unit] = {
    myState = Ref.of[IO, List[String]](List.empty[String]).unsafeRunSync()
    val ioa = List(process1, process2, process3).parSequence.void
    ioa *> myState.get.flatMap(rs => putStrLn(rs.toString))
  }

  override def run(args: List[String]): IO[ExitCode] =
    masterProcess.as(ExitCode.Success)
}
