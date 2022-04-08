package io.iog.minichain.models

import io.iog.minichain.adts.IndexedMap
import zio.{Ref, UIO, Task, IO}

trait Blockchain: // removed `sealed` -- not used as an enumeration or sum type
  def append(block: Block): Task[Unit]
  def findByIndex(index: Int): UIO[Option[Block]]
  def findByHash(hash: Hash): UIO[Option[Block]]
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
    })

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
