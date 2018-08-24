import StateMachine._
import org.scalatest.{Matchers, WordSpec}

class CandyDispenserSpec extends WordSpec with Matchers {

  "Candy dispenser" should {
    "unlock when coin is inserted" in {
      val initialMachine = Machine(Locked, 10, 0)
      val state = simulateMachine(List(Coin))
      val (_, machine) = state.run(initialMachine)

      machine.locked shouldBe false
    }

    "dispense a candy and lock" in {
      val initialMachine = Machine(Unlocked, 10, 0)
      val state = simulateMachine(List(Turn))
      val ((candies, coins), machine) = state.run(initialMachine)

      machine.locked shouldBe true
      candies shouldBe 9
    }

    "do nothing when knob is turned" in {
      val initialMachine = Machine(Locked, 10, 0)
      val state = simulateMachine(List(Turn))
      val (_, machine) = state.run(initialMachine)

      machine shouldBe initialMachine
    }

    "do nothing when coin is inserted" in {
      val initialMachine = Machine(Unlocked, 10, 0)
      val state = simulateMachine(List(Coin))
      val (_, machine) = state.run(initialMachine)

      machine shouldBe initialMachine
    }

    "ignore any input" in {
      val initialMachine = Machine(Unlocked, 0, 120)
      val state = simulateMachine(List(Coin, Turn, Turn, Coin, Turn, Coin, Coin))
      val (_, machine) = state.run(initialMachine)

      machine shouldBe initialMachine
    }

    "give all candies" in {
      val initialMachine = Machine(Locked, 10, 0)
      val inputs = List(
        Coin, Turn,
        Coin, Turn,
        Coin, Turn,
        Coin, Turn,
        Coin, Turn,
        Coin, Turn,
        Coin, Turn,
        Coin, Turn,
        Coin, Turn,
        Coin, Turn
      )
      val state = simulateMachine(inputs)
      val ((candies, coins), machine) = state.run(initialMachine)

      machine.locked shouldBe true
      candies shouldBe 0
      coins shouldBe 10
    }
  }
}
