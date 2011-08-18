package abs.backend.scala

package object runtime {
  // Idea: translate ABS-style "let (Int div = n / 10) in ..." to "let(n/10) {div: Int => ...}"
  def let[A, B](v: => A)(f: A => B) = f(v)
}