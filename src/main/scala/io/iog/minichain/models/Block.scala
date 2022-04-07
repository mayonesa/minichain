package io.iog.minichain.models

type Transaction = String // since it is just a string, simplifying into a `type`
type Nonce = Long
val Nonce = Long

// Every block has an index, starting from zero (0).
// The block at index 0 is called the Genesis block.
// A block links back to the previous (parent) block.
// Of course, we also record the transactions that this block introduces to our mini-chain.
case class Block(
  index: Int,
  parentHash: Hash,
  transactions: Seq[Transaction],
  miningTargetNumber: Number,
  nonce: Nonce,
):

  def cryptoHash: Hash = Sha256(index, parentHash, transactions, miningTargetNumber, nonce)

  // The essence of PoW is that it is a problem whose solution is easy
  // (in computational resources) to verify but difficult to find.
  def minedProperly(): Boolean = cryptoHash.asNumber < miningTargetNumber // assertions can be turned off
