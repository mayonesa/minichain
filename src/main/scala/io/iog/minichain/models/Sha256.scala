package io.iog.minichain.models

import io.iog.minichain.models.Sha256.NumberOfBytes

import java.security.MessageDigest

class Sha256(): // object -> class for integrity assurance
  private val digest = MessageDigest.getInstance("SHA-256")

  def apply(bytess: Array[Byte]*): Hash =
    for (bytes <- bytess)
      digest.update(bytes)

    val hash = digest.digest()
    assert(hash.length == NumberOfBytes)
    Hash(hash)

object Sha256:
  val NumberOfBytes = 32
  lazy val ZeroHash: Hash = Hash(MessageDigest.getInstance("SHA-256").digest(Array()))
