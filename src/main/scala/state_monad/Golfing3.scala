package state_monad

object Golfing3 extends App {

    case class GolfState(distance: Int)

    def swing(distance: Int): State[GolfState, Int] = State { (s: GolfState) =>
        val newDistance = s.distance + distance
        (GolfState(newDistance), newDistance)
    }

    val stateWithNewDistance: State[GolfState, Int] = for {
        _             <- swing(20)
        _             <- swing(15)
        totalDistance <- swing(0)
    } yield totalDistance


    // THE ACTION BEGINS

    val beginningState = GolfState(0)

    // `run` is like `unsafeRunSync` in the Cats `IO` monad
    val result: (GolfState, Int) = stateWithNewDistance.run(beginningState)

    println(s"GolfState:      ${result._1}")  //GolfState(35)
    println(s"Total Distance: ${result._2}")  //35

}
