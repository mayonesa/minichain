package io.iog.minichain.models

type Transaction = String // since it is just a string, simplifying into a `type`
type Nonce = Long
val Nonce = Long

// Every block has an index, starting from zero (0).
// The block at index 0 is called the Genesis block.
// A block links back to the previous (parent) block.
// Of course, we also record the transactions that this block introduces to our mini-chain.
// We'll see the meaning of the other fields as we move along.
case class Block(
  index: Int,
  parentHash: Hash,
  transactions: Seq[Transaction],
  miningTargetNumber: Number,
  nonce: Nonce,
):

  // To get the crypto hash of the block, just feed all fields to SHA-256.
  def cryptoHash: Hash = Sha256(index, parentHash, transactions, miningTargetNumber, nonce)

  // The essence of PoW is that it is a problem whose solution is easy
  // (in computational resources) to verify but difficult to find.
  def minedProperly(): Boolean = cryptoHash.toNumber < miningTargetNumber // assertions can be turned off
