package dhall.utilities

object MapFunctions {
  implicit class RichMap[K, V](self: Map[K, V]) {
    def unionWith(f: (V, V) => V, other: Map[K, V]): Map[K, V] = {
      val commonKeys = self.keySet intersect other.keySet
      val combinedKeyValues = commonKeys.map(k => k -> f(self(k), other(k))).toMap
      val otherThanCommon = self.filterKeys(!commonKeys.contains(_)) ++ other.filterKeys(!commonKeys.contains(_))
      combinedKeyValues ++ otherThanCommon
    }
  }
}
