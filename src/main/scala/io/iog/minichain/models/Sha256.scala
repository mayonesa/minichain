package io.iog.minichain.models

import io.iog.minichain.models.Sha256.NumberOfBytes

import java.security.MessageDigest

// Hashes are produced by a cryptographic function and in "mini-chain"
// SHA-256  is used, which always generates a 32-byte (256-bit) value.
class Sha256: // object -> class for integrity assurance
  private val digest = MessageDigest.getInstance("SHA-256")

  // hashes a composite structure whose constituents can be given
  // as byte arrays.
  def apply(bytess: Array[Byte]*): Hash =
    for (bytes <- bytess)
      digest.update(bytes)

    val hash = digest.digest()
    assert(hash.length == NumberOfBytes)
    Hash(hash)

object Sha256:
  val NumberOfBytes = 32

  // pre-computes the hash of an empty array of 32 bytes.
  lazy val ZeroHash: Hash = Hash(MessageDigest.getInstance("SHA-256").digest(Array()))
