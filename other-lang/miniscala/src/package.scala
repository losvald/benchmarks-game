/** Benchmarks written in a _subset_ of Scala with support for 2nd-class values
  * - 2nd-class values enable stack allocation but almost require CPS style
  * - only Int, Boolean, Char and Unit as value types (always stack-allocated)
  * - Pairs and List[T] may be heap-allocated
  * - Array[T] may be fully heap-allocated if T is Int/Boolean/Char/Unit
  * - String is a syntactic sugar around Array[Char]
  * - no user-defined types or classes
  */

package object miniscala2 {
  type Str = Array[Char]
}
