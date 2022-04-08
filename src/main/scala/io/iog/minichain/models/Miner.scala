package io.iog.minichain.models

import zio.Task

object Miner:
  // NOTE: A Hash is also a Number, the two are used interchangeably.
  //
  // Mining is about computing hashes until it is less
  // than a given target number.
  // This target serves, in a way, as the maximum possible number that a
  // proof of work computation should produce.
  lazy val StdMiningTargetNumber: Number = targetByLeadingZeros(1)

  // Mines the Genesis block.
  // Normally, this is done by the system during bootstrapping
  // and all subsequent blocks are mined by a miner.
  lazy val Genesis: Task[Block] = mine(
    index = 0, // The very first block
    parentHash = Sha256.ZeroHash, // Let's assume this is by definition for the Genesis block.
    transactions = Seq("Hello Blockchain, this is Genesis :)"),
    StdMiningTargetNumber,
  )

  private val NInitBetweens = 9
  private lazy val Step = Nonce.MaxValue / NInitBetweens
  private lazy val LastStart = Step * NInitBetweens
  private lazy val Betweens = (0l to Nonce.MaxValue by Step).map { start =>
    val inclusiveEnd = if start == LastStart then Nonce.MaxValue else start + Step - 1
    (start, inclusiveEnd)
  }

  // Actual "proof-of-work"-style computation.
  // the parameters of this method is
  // only thing missing the nonce when compared with the fields of a Block
  //
  // all the fixed elements a block:
  //
  //  - index,
  //  - parentHash,
  //  - transactions,
  //  - miningTargetNumber
  //
  // by varying the nonce, a block hash that is below the
  // given miningTargetNumber is attempted.
  //
  // NOTE: the block hash can be transformed to an actual number
  def mine(
    index: Int,
    parentHash: Hash,
    transactions: Seq[Transaction],
    miningTargetNumber: Number,
  ): Task[Block] =
    def mineBetween(start: Nonce, inclusiveEnd: Nonce) =
      Task.blocking(Task.suspend {
        def loop(nonce: Nonce): Task[Option[Block]] =
          if nonce > inclusiveEnd then
            Task.none
          else
            val block = Block(index, parentHash, transactions, miningTargetNumber, nonce)
            for
              hash   <- block.cryptoHash
              result <- if hash.asNumber < miningTargetNumber then Task.some(block)
                          else loop(nonce + 1)
            yield result

        loop(start)
      })

    val mines = Betweens.map { (start, inclusiveEnd) =>
      mineBetween(start, inclusiveEnd)
    }
    Task.raceAll(mines.head, mines.tail).map(_.get)

  // Creates a target number with the requirement of having
  // some leading zeros. More leading zeros means smaller target number.
  //
  // NOTE: use less leading zeros if not too particular and not wanting to cause too many compute cycles
  private def targetByLeadingZeros(zeros: Int) =
    require(zeros < Sha256.NumberOfBytes)

    val bytes: Bytes =
      Array.tabulate[Byte](32) { n =>
        if n < zeros then 0
        else 0xff.toByte
      }

    Number(1, bytes)
