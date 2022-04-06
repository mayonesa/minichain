package io.iog.minichain.models

import io.iog.minichain.adts.IndexedMap
import zio.{Ref, UIO, Task}

// A Blockchain is a sequence of blocks, each one having an index.
// The index of a block is the index of its parent plus one.
// A Blockchain always has a genesis block at index 0, which is the lowest index.
trait Blockchain: // removed `sealed` -- not used as an enumeration or sum type
  // Add a block to the chain.
  def append(block: Block): UIO[Unit]

  // Find a block by index.
  def findByIndex(index: Int): UIO[Option[Block]]

  // Find a block by hash.
  def findByHash(hash: Hash): UIO[Option[Block]]

  // Find a common ancestor between this blockchain and that blockchain.
  def commonAncestor(that: Blockchain): UIO[Option[Block]]

  def containsHash(hash: Hash): UIO[Boolean]
end Blockchain

// Implement an in-memory blockchain that internally has an indexing data structure.
// The purpose of this internal data structure is to avoid traversing the linked list
// of blocks when answering queries like findByIndex.
class FastBlockchain(chainRef: Ref[IndexedMap[Hash, Block]]) extends Blockchain:
  def append(block: Block): Task[Unit] =
    Task.suspend(chainRef.update { chain =>
      if chain.size != block.index then throw new IndexOutOfBoundsException(block.index)
      chain :+ (block.cryptoHash -> block)
    })

  def findByIndex(index: Int): UIO[Option[Block]] = get(_.at(index))

  def findByHash(hash: Hash): UIO[Option[Block]] = get(_.from(hash))

  def commonAncestor(that: Blockchain): UIO[Option[Block]] =
    chainRef.get.flatMap { chain =>
      def loop(idx: Int): UIO[Option[Block]] =
        if idx < 0 then
          UIO.none
        else
          val block = chain.at(idx).get
          that.containsHash(block.cryptoHash).flatMap { sameHash =>
            if sameHash then UIO.some(block)
            else loop(idx - 1)
          }

      loop(chain.size - 1)
    }

  def containsHash(hash: Hash): UIO[Boolean] = findByHash(hash).map(_.isDefined)

  private def get(f: IndexedMap[Hash, Block] => Option[Block]) = chainRef.get.map(f)
end FastBlockchain
