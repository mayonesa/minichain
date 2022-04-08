package io.iog.minichain.models

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import java.security.MessageDigest

// Hashes are produced by a cryptographic function and in "mini-chain"
// SHA-256  is used, which always generates a 32-byte (256-bit) value.
object Sha256:
  val NumberOfBytes = 32
  private val TheDigest = MessageDigest.getInstance("SHA-256")

  // pre-computes the hash of an empty array of 32 bytes.
  val ZeroHash: Hash = Hash(TheDigest.digest(Array()))

  // hashes a composite structure whose constituents can be given
  // as byte arrays.
  def apply(anies: Any*): Hash =  // Bytes -> Any -- don't leak out too much implementation info
    TheDigest.synchronized {
      for (any <- anies)
        TheDigest.update(toBytes(any))

      val hash = TheDigest.digest()
      assert(hash.length == NumberOfBytes)
      Hash(hash)
    }

  private def toBytes(any: Any) =
    val stream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(any)
    oos.close()
    stream.toByteArray
