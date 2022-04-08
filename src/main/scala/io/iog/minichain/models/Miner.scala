package io.iog.minichain.models

import scala.annotation.tailrec
import zio.Task

object Miner:
  lazy val StdMiningTargetNumber: Number = targetByLeadingZeros(1)

  lazy val Genesis: Task[Block] = mine(
    index = 0,
    parentHash = Sha256.ZeroHash,
    transactions = Seq("Hello Blockchain, this is Genesis :)"),
    StdMiningTargetNumber,
  )

  private val Parallelism = 10
  private lazy val Step = Nonce.MaxValue / Parallelism
  private lazy val LastStart = Step * Parallelism
  private lazy val Betweens = (0l until Nonce.MaxValue by Step).map { start =>
    val inclusiveEnd = if start == LastStart then Nonce.MaxValue else start + Step - 1
    (start, inclusiveEnd)
  }

  def mine(
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
            if block.cryptoHash.asNumber < miningTargetNumber then Some(block)
            else loop(nonce + 1)

        loop(start)
      })

    val mines = Betweens.map { (start, inclusiveEnd) =>
      mineBetween(start, inclusiveEnd)
    }
    Task.raceAll(mines.head, mines.tail).map(_.get)

  // NOTE: use less leading zeros if not too particular and not wanting to cause too many compute-cycles
  private def targetByLeadingZeros(zeros: Int) =
    require(zeros < Sha256.NumberOfBytes)

    val bytes: Bytes =
      Array.tabulate[Byte](32) { n =>
        if n < zeros then 0
        else 0xff.toByte
      }

    Number(1, bytes)
