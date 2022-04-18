package io.iog.minichain.models

import io.iog.minichain.models.Block.toBytes

type Transaction = String // since it is just a string, simplifying into a `type`
type Nonce = Long
val Nonce = Long

case class Block(
  index: Int,
  parentHash: Hash,
  transactions: Seq[Transaction],
  miningTargetNumber: Number,
  nonce: Nonce,
):

  lazy val cryptoHash: Hash =
    Sha256()(toBytes(index), parentHash.bytes, toBytes(transactions), toBytes(miningTargetNumber), toBytes(nonce))

  lazy val minedProperly: Boolean = cryptoHash.asNumber < miningTargetNumber // assertions can be turned off

object Block:
  private def toBytes(any: Any) = any.toString.getBytes
