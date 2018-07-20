package com.github.sharedstate

import scalaz.zio.{App, IO, Ref, Void}
import scala.concurrent.duration._

object SharedStateZIO extends App {
  // this can be also achieved with `scalaz.zio.console.putStrLn`
  def putStrLn(str: String): IO[Void, Unit] = IO.sync(println(str))

  def process1(myState: Ref[List[String]]): IO[Void, Unit] = {
    putStrLn("Starting process #1") *>
      IO.sleep(5.seconds) *>
      myState.modify(_ ++ List("#1")) *>
      putStrLn("Done #1")
  }

  def process2(myState: Ref[List[String]]): IO[Void, Unit] = {
    putStrLn("Starting process #2") *>
      IO.sleep(3.seconds) *>
      myState.modify(_ ++ List("#2")) *>
      putStrLn("Done #2")
  }

  def process3(myState: Ref[List[String]]): IO[Void, Unit] = {
    // for comprehension style
    for {
      _ <- putStrLn("Starting process #3")
      _ <- IO.sleep(10.seconds)
      _ <- myState.modify(_ ++ List("#3"))
      _ <- putStrLn("Done #3")
    } yield ()
  }

  def masterProcess: IO[Void, Unit] = {
    // for comprehension style
    for {
      state <- Ref(List.empty[String])
      _  <- process1(state).par(process2(state)).par(process3(state))
      lastState <- state.read
      _ <- putStrLn(lastState.toString())
    } yield ()

    // flatMap and sequence style
    // Ref(List.empty[String]).flatMap(st => {
    //  val f =  process1(st).par[Unit](process2(st)).par[Unit](process3(st))
    //  f *> st.read.flatMap(l => putStrLn(l.toString()))
    // })
  }

  def run(args: List[String]): IO[Void, ExitStatus] =
    masterProcess.attempt[Void]
      .map(_.fold(_ => 1, _ => 0))
      .map(exitCode => ExitStatus.ExitNow(exitCode))
}
