import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import fs2.concurrent.SignallingRef
import scala.concurrent.duration._


object Main extends IOApp {


  val signalExample = for {

    signal <- SignallingRef[IO, Int](0)
    
    signalUpdates = signal.discrete.evalMap { i => 
      IO.println(s"Current value of signal: $i") 
    }
    
    updateSignal1 = fs2.Stream.awakeEvery[IO](4.seconds).evalMap { d =>
      IO.println(s"time1: ${d.toSeconds}s") >> 
      signal.update(_ + 1)
    }

    updateSignal2 = fs2.Stream.awakeEvery[IO](7.seconds).evalMap { d =>
      IO.println(s"time2: ${d.toSeconds}s") >> 
      signal.update(_ + 1)
    }

  } yield signalUpdates
    .concurrently(updateSignal1)
    .concurrently(updateSignal2)

  def run(args: List[String]): IO[ExitCode] = 
    signalExample.flatMap { updates => 
      updates.compile.drain as ExitCode.Success
    }
}