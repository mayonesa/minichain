package io.iog.minichain.models

import io.iog.minichain.models.Sha256.*

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import java.security.MessageDigest

// Hashes are produced by a cryptographic function and in "mini-chain"
// SHA-256  is used, which always generates a 32-byte (256-bit) value.
class Sha256(): // object -> class for integrity assurance
  private val digest = MessageDigest.getInstance("SHA-256")

  // hashes a composite structure whose constituents can be given
  // as byte arrays.
  def apply(anies: Any*): Hash =  // Bytes -> Any -- don't leak out too much implementation info
    for (any <- anies)
      digest.update(toBytes(any))

    val hash = digest.digest()
    assert(hash.length == NumberOfBytes)
    Hash(hash)

object Sha256:
  val NumberOfBytes = 32

  // pre-computes the hash of an empty array of 32 bytes.
  lazy val ZeroHash: Hash = Hash(MessageDigest.getInstance("SHA-256").digest(Array()))

  private def toBytes(any: Any) =
    val stream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(any)
    oos.close()
    stream.toByteArray
