import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import fs2.concurrent.SignallingRef
import scala.concurrent.duration._


object Main extends IOApp {

  val signalExample = fs2.Stream.eval(
    SignallingRef[IO, Boolean](false)
  ).flatMap { signal =>
    val maxWait = 6
    val periodic = fs2.Stream
      .awakeEvery[IO](1.second)
      .map { d => s"${maxWait - d.toSeconds}s remaining" }
      .interruptAfter(maxWait.seconds)
      .interruptWhen(signal) ++
      fs2.Stream.eval(
        signal.get
      ).map { interrBySignal => 
        if (interrBySignal) "Interrupted By Signal" 
        else "Finished!!!"
      }
      

    val update = fs2.Stream.sleep[IO](4.seconds) >> fs2.Stream.eval(
      signal.set(true)
    )
    
    periodic.concurrently(update)
  }

  def run(args: List[String]): IO[ExitCode] = 
    signalExample.evalMap(IO.println).compile.drain as ExitCode.Success
}