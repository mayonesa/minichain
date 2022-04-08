package io.iog.minichain.models

import io.iog.minichain.adts.IndexedMap
import zio.{Ref, UIO, Task, IO}

// A Blockchain is a sequence of blocks, each one having an index.
// The index of a block is the index of its parent plus one.
// A Blockchain always has a genesis block at index 0, which is the lowest index.
trait Blockchain: // removed `sealed` -- not used as an enumeration or sum type
  // Add a block to the chain.
  def append(block: Block): Task[Unit]

  // Find a block by index.
  def findByIndex(index: Int): UIO[Option[Block]]

  // Find a block by hash.
  def findByHash(hash: Hash): UIO[Option[Block]]

  // Find a common ancestor between this blockchain and that blockchain.
  def latestCommon(that: Blockchain): IO[IllegalStateException, Block]

  def containsHash(hash: Hash): UIO[Boolean]

object Blockchain:
  def empty: UIO[Blockchain] = Ref.make(IndexedMap.empty[Hash, Block]).map(FastBlockchain(_))
end Blockchain

private class FastBlockchain(chainRef: Ref[IndexedMap[Hash, Block]]) extends Blockchain:
  def append(block: Block): Task[Unit] =
    Task.suspend(chainRef.update { chain =>
      require(chain.size == block.index, "append-attempt block index clashes w/ target blockchain")
      chain :+ (block.cryptoHash -> block)
    }).absorb

  def findByIndex(index: Int): UIO[Option[Block]] = get(_.at(index))

  def findByHash(hash: Hash): UIO[Option[Block]] = get(_.from(hash))

  def latestCommon(that: Blockchain): IO[IllegalStateException, Block] =
    chainRef.get.flatMap { chain =>
      def loop(idx: Int): IO[IllegalStateException, Block] =
        if idx < 0 then
          IO.fail(IllegalStateException("No common genesis"))
        else
          val block = chain.at(idx).get
          that.containsHash(block.cryptoHash).flatMap { sameHash =>
            if sameHash then IO.succeed(block)
            else loop(idx - 1)
          }

      loop(chain.size - 1)
    }

  def containsHash(hash: Hash): UIO[Boolean] = get(_.containsKey(hash))

  private def get[A](f: IndexedMap[Hash, Block] => A) = chainRef.get.map(f)
end FastBlockchain
