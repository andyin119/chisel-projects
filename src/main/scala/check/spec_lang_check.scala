package check

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Property[AnyVal](ante: Sequence[AnyVal], post: Sequence[AnyVal])

sealed trait Sequence[AnyVal]
case class AtomicProp[AnyVal] (fn: AnyVal => Boolean) extends Sequence[AnyVal]
case class Concat(seq1: Sequence[AnyVal], seq2: Sequence[AnyVal]) extends Sequence[AnyVal]
case class Delay(num_txns: Int) extends Sequence[AnyVal]
case class Repeated(seq: Sequence[AnyVal], repeats: Int) extends Sequence[AnyVal]
case class UnboundedRepeat(seq: Sequence[AnyVal], min_repeat: Int) extends Sequence[AnyVal]
case class Or(seq1: Sequence[AnyVal], seq2: Sequence[AnyVal]) extends Sequence[AnyVal]

// sealed trait Operators
// case class Stable[T]


class State(property: Sequence[AnyVal], initial: (AnyVal, Int)) {
    // State class to keep track of the sequence states
    val atomicSeqs = mutable.Queue[Sequence[AnyVal]]()
    var isSuccess = false
    var isFailed = false
    var successIdx = mutable.Seq(initial._2, None)
    var delay = 0
    // maybe establish states outside?
    //expandStates(property.ante)
    //expandStates(property.post)
    expandStates(property)
    
    def expandStates(sequence: Sequence[AnyVal]): Boolean = {
        var success = true
        sequence match {
            case _: AtomicProp[AnyVal] => atomicSeqs += sequence
            case concatSeq: Concat =>  {
                val success1 = expandStates(concatSeq.seq1)
                val success2 = expandStates(concatSeq.seq2)
            }
            case _: Delay => atomicSeqs += sequence
            case repeatSeq: Repeated => {
                for (idx <- 0 until repeatSeq.repeats) {
                    expandStates(repeatSeq.seq)
                }
            }
            // case _: Or => {
            //     // assume only have a Or of two atomic properties
            //     atomicSeqs += sequence
            // }
            case _ => success = false
        }
        success
    }

    def advanceState(txn: AnyVal, idx: Int) = {
        val nextCond = atomicSeqs.front
        if (delay > 1) { // if delay is 1, the last cycle of adding in delay has already been counted
            delay = delay - 1
        } else {
            val isTrue = nextCond match {
                case atomic: AtomicProp[AnyVal] => atomic.fn(txn)
                case delayState: Delay => {
                    delay = delay + delayState.num_txns
                    true
                }
                //case or: Or => or.seq1(txn[0]) || or.seq2(txn)
                case _ => false
            }
            if (isTrue) {
                atomicSeqs.dequeue() // if satisfy condition, remove the seq condition from the lists to check
            } else {
                isFailed = true
            }
        }
        if (atomicSeqs.isEmpty && !isFailed) {
            isSuccess = true
            successIdx(1) = idx
        }
    }
}

class CheckSequence {
    def checkSeqWithStates(txns: Seq[AnyVal], property: Sequence[AnyVal]): (ListBuffer[mutable.Seq[Matchable]], ListBuffer[Matchable]) = {
        val isSuccess = ListBuffer[mutable.Seq[Matchable]]()
        //val ante_cond = property.ante
        //val post_cond = property.post
        val stateLists = ListBuffer[State]()
        
        val state = new State(property, (txns(0), 0))
        txns.zipWithIndex.foreach {
            case (txn, idx) => {
                val state = new State(property, (txn, idx))
                stateLists += state
                for (curr_state <- stateLists) {
                    curr_state.advanceState(txn, idx)
                    if (curr_state.isSuccess) {
                        isSuccess += curr_state.successIdx
                        //stateLists -= curr_state
                    }
                }
                val cloned_state_list = stateLists.clone()
                for (curr_state <- cloned_state_list) {
                    if (curr_state.isSuccess || curr_state.isFailed){
                        stateLists -= curr_state
                    }
                }
                
            }
        }
        // saves the index of the transactions that are not fully checked against the property
        val unchecked = mutable.ListBuffer[Matchable]()
        for (remaining_state <- stateLists) {
            unchecked += remaining_state.successIdx(0)
        }
        (isSuccess, unchecked)
    }
    /*
    def checkSeq(txns: Seq[AnyVal], sequence: Sequence[AnyVal]): (List[(Int, Int)], Option[Int]) = {
        val pending_txn = mutable.Queue[Int]()
        val success: List[(Int, Int)] = mutable.List()
        txns.zipWithIndex.foreach{
            case (txn, idx) => {
                if (pending_txn.contains(idx - 1) && sequence.seq2.fn(txn)) {
                    success :+ (pending_txn.dequeue(), idx)
                } else if (pending_txn.contains(idx - 1)) {
                    pending_txn.dequeue()
                }
                if (sequence.seq1.fn(txn)) pending_txn += idx
            }
        }
        val unchecked: Option[Int] = if (pending_txn.isEmpty) None else Some(pending_txn.dequeue())
        return (success, unchecked)
    }
    */
    /*
    def checkProp(txns: Seq[AnyVal], property: Property[AnyVal]): (List[(Int, Int)], List[Option[Int]]) = {
        // recursive sequence checking
        val transactions: Seq[AnyVal] = mutable.Seq()
        val isSuccess = mutable.List()
        val ante_cond = property.ante
        val post_cond = property.post

        for (var i <- 0 to txns.length - 1) {
            transactions = txns ~>(0, i) // select sequence up till index i
            val cond = checkAntePost(transactions, ante_cond)
            if cond:
                val postCheck = checkAntePost(txns ~> (i, txns.length - 1), post_cond)
                if postCheck[0]:
                    isSuccess = isSuccess :+ (i, postCheck[1])
        }
        return isSuccess
        // txns.zipWithIndex.foreach {
        //     (txn, idx) => {
        //         transactions :+ txn
        //         val cond = checkAntecendent(transactions, ante_cond)
        //     }
        // }
    }
    */
    //  def checkDelay(txns: Seq[AnyVal], sequence: Sequence[AnyVal]) = {
    //     // keeps a counter of the delayed elements
    // }

    // def checkAntePost(txns: Seq[T], sequence: Sequence[T]): (Boolean, Int) = {
    //     // similar to checkSeq, need to include other types of sequences
    //     // return whether true or false and include the end index if true
    //     txns.zipWithIndex.foreach{
    //         case (txn, idx) => {
    //             val result = sequence match {
    //                 case AtomicProp[T](fn) => fn(txn)
    //                 case Concat(seq1, seq2) => checkSeq(txns ~> (0, idx), sequence)
    //                 case Delay()
    //             }      
    //         }

    //     }
    // }
}
