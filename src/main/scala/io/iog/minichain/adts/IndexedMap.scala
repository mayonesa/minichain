package io.iog.minichain.adts

class IndexedMap[K, V](vector: Vector[V], map: Map[K, V]):
  lazy val size: Int = vector.length

  def :+ (kv: (K, V)): IndexedMap[K, V] = IndexedMap(vector :+ kv._2, map + kv)

  def at(idx: Int): Option[V] = if idx < 0 || idx >= vector.length then None else Some(vector(idx))

  def from(key: K): Option[V] = map.get(key)
  
  def containsKey(key: K): Boolean = map.contains(key)

object IndexedMap:
  def empty[K, V]: IndexedMap[K, V] = IndexedMap(Vector(), Map())