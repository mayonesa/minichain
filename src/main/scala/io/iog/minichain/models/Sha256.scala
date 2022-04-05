package io.iog.minichain.models

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import java.security.MessageDigest

// Hashes are produced by a cryptographic function and in "mini-chain" we
// use SHA-256, which always generates a 32-byte (256-bit) value.
object Sha256:
  val NumberOfBytes = 32
  private val TheDigest = MessageDigest.getInstance("SHA-256")

  // pre-compute the hash of an empty array of 32 bytes.
  // We call this the "Zero_Hash".
  val ZeroHash: Hash = Hash(TheDigest.digest(Array()))

  // We use this to hash a composite structure whose constituents can be given
  // as byte arrays. We just feed everything to SHA-256.
  def apply(anies: Any*): Hash =  // Bytes -> Any -- don't leak out too much implementation info
    for (any <- anies) {
      TheDigest.update(toBytes(any))
    }
    val hash = TheDigest.digest()
    assert(hash.length == NumberOfBytes)
    Hash(hash)

  private def toBytes(any: Any) =
    val stream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(any)
    oos.close()
    stream.toByteArray
