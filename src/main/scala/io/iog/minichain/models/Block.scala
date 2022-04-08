package io.iog.minichain.models

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

  lazy val cryptoHash: Hash = Sha256()(index, parentHash, transactions, miningTargetNumber, nonce)

   lazy val minedProperly: Boolean = cryptoHash.asNumber < miningTargetNumber // assertions can be turned off
