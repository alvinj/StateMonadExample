package state_monad.another_state_monad

/**
  * This source code is for another `State` monad
  * that I modified until I could understand it.
  */
case class State[A,S](run: S => (A,S)) {

    // s1 = state1, s2 = state2, s3 = state3
    def flatMap[B](f: A => State[B,S]): State[B,S] = State { s1: S =>
        val (a, s2) = run(s1)
        val stateChangeToB = f(a)
        val (b, s3) = stateChangeToB.run(s2)
        (b, s3)
    }

    def map[B](f: A => B): State[B,S] =
        flatMap(a => State.lift(f(a)))

}

object State {
    /**
      * "lifts" a value and a state into a State[S,A]
      */
    def lift[A,S](value: A): State[A,S] = State(state => (value, state))
}
