package io.iog.minichain.models

// normally Array -> Vector -- immutable per problem description but `Array` is the arg type of `java.security` and
// conversions may cost
type Bytes = Array[Byte]

type Number = BigInt
val Number = BigInt

class Hash(bytes: Bytes) extends Serializable:  // not `case` -- no need to access `bytes`
  lazy val asNumber: Number = Number(1, bytes)

  lazy val asHexString: String = "0x" + bytes.map { byte =>
    String.format("%02X", Byte.box(byte))
  }.mkString

  override def equals(obj: Any): Boolean =
    obj match
      case h: Hash => h.asNumber == asNumber
      case _       => false

  override def hashCode: Int = asNumber.hashCode