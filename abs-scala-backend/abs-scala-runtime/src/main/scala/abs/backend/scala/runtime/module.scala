/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.scala

package object runtime {
  // Idea: translate ABS-style "let (Int div = n / 10) in ..." to "let(n/10) {div: Int => ...}"
  def let[A, B](v: => A)(f: A => B) = f(v)
}