/**
  * Copyright (c) 2013 Saddle Development Team
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *     http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
 **/
package org.saddle.index

import org.saddle.Index
import scala.{specialized => spec}

/**
  * Abstract interface for a Joiner instance
  */
private[saddle] trait Joiner[@spec(Int, Long, Double) T] {
  def join(left: Index[T], right: Index[T], how: JoinType): ReIndexer[T]
}
