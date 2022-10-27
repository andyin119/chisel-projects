package check

import scala.collection.immutable._
//import org.scalatest.freespec.AnyFreeSpec

// class CheckUnitTest extends AnyFreeSpec {


// }
@main
def main() = {
    val isTrue = AtomicProp[AnyVal](i => i == 1)
    val isFalse = AtomicProp[AnyVal](i => i == 0)

    val trueThanFalse: Sequence[AnyVal] = Concat(isTrue, isFalse) // need to double check on type
    val test: Seq[AnyVal] = Seq(1, 0, 0, 1, 0, 0, 1)
    val check = new CheckSequence()
    val result = check.checkSeqWithStates(test, trueThanFalse)
    println(result)

    val twoTrueOneFalse: Sequence[AnyVal] = Concat(isTrue, Concat(isTrue, isFalse))
    val test2: Seq[AnyVal] = Seq(1, 1, 1, 0, 1, 1, 0, 1, 1)
    val check2 = new CheckSequence()
    val result2 = check2.checkSeqWithStates(test2, twoTrueOneFalse)
    println(result2)

    val isTwo = AtomicProp[AnyVal](i => i == 2)
    val delay = Delay(1)
    val isThree = AtomicProp[AnyVal](i => i == 3)
    val twoWaitThanThree: Sequence[AnyVal] = Concat(isTwo, Concat(delay, isThree))
    val check3 = new CheckSequence()
    val test3 = Seq(1, 2, 3, 2, 4, 3, 2, 2, 3, 2)
    val result3 = check3.checkSeqWithStates(test3, twoWaitThanThree)
    println(result3)

    val repeatTrueTwice = Repeated(isTrue, 3)
    val check4 = new CheckSequence()
    val test4 = Seq(1, 1, 1, 0, 1, 1, 1, 1, 0)
    val result4 = check4.checkSeqWithStates(test4, repeatTrueTwice)
    println(result4)
}

// list: Seq[Int], trueThanFalse: Sequence[T]
// check returns ([(index1, index2), (index3, index4)])
// gives the index of where the list follows the sequence, and another term for an outlier that is unsure
// list: [1, 0, 0, 0, 1, 0, 1]
// return ([(0, 1), (4, 5)], 6)

// this is for checking a particular sequence, not checking its property
