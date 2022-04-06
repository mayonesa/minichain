package io.iog.minichain.models

import scala.annotation.tailrec
import scala.math.min
import zio.Task

object Miner:
  // NOTE: A Hash is also a Number, we use the two interchangeably.
  //
  // Mining is about computing hashes until we get something that is less
  // than a given target number.
  // This target serves, in a way, as the maximum possible number that a
  // proof of work computation should produce.
  final val StdMiningTargetNumber = targetByLeadingZeros(1)

  // Mines the Genesis block.
  // Normally, this is done by the system during bootstrapping
  // and every other block is mined by a miner.
  final val Genesis = Miner.mineNextBlock(
    index = 0, // The very first block
    parentHash = Sha256.ZeroHash, // Let's assume this is by definition for the Genesis block.
    transactions = Seq("Hello Blockchain, this is Genesis :)"),
    StdMiningTargetNumber,
  )

  private val Parallelism = 10
  private lazy val Step = Nonce.MaxValue / Parallelism
  private lazy val LastStart = Step * Parallelism
  private lazy val Betweens = (0 until Nonce.MaxValue by Step).map { start =>
    val inclusiveEnd = if start == LastStart then Nonce.MaxValue else start + Step - 1
    (start, inclusiveEnd)
  }


  // Create a target number with the requirement of having
  // some leading zeros. More leading zeros means smaller target number.
  //
  // NOTE: use less leading zeros if not wanting to cause too many compute cycles
  private def targetByLeadingZeros(zeros: Int) =
    require(zeros < Sha256.NumberOfBytes)

    val bytes: Bytes =
      Array.tabulate[Byte](32) { n =>
        if (n < zeros) {
          0
        }
        else {
          0xff.toByte
        }
      }

    Number(1, bytes)

  // Actual "proof-of-work"-style computation.
  // Compare the parameters of this method with the fields of a Block and
  // you'll see that the only thing missing here is the nonce. Here is why.
  //
  // Initially we have all the fixed elements a block:
  //
  //  - index,
  //  - parentHash,
  //  - transactions,
  //  - miningTargetNumber
  //
  // and by varying the nonce we try to have a block hash that is below the
  // given miningTargetNumber.
  //
  // NOTE Remember that the block hash can be transformed to an actual number,
  //      so we can talk about hash and number interchangeably.
  def mineNextBlock(
    index: Int,
    parentHash: Hash,
    transactions: Seq[Transaction],
    miningTargetNumber: Number,
  ): Task[Block] =
    def mineBetween(start: Nonce, inclusiveEnd: Nonce) =
      Task.blocking(Task.attempt {
        @tailrec
        def loop(nonce: Nonce): Option[Block] =
          if nonce > inclusiveEnd then
            None
          else
            val block = Block(index, parentHash, transactions, miningTargetNumber, nonce)
            if block.cryptoHash.toNumber < miningTargetNumber then Some(block)
            else loop(nonce + 1)

        loop(start)
      })

    val mines = Betweens.map { (start, inclusiveEnd) =>
      mineBetween(start, inclusiveEnd)
    }
    Task.raceAll(mines.head, mines.tail).map(_.get)
