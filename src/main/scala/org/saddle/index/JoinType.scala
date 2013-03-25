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

/**
 * There are four basic joins which may be performed between
 * two indexed Saddle objects such as [[org.saddle.Series]]
 * or [[org.saddle.Frame]].
 *
 *   - InnerJoin
 *   - OuterJoin (also known as a full join)
 *   - LeftJoin
 *   - RightJoin
 *
 * These are defined analogously to SQL joins.
 *
 * For example, an output key will be generated in the following
 * situations:
 *
 *   - Inner: key exists in both input indexes
 *   - Outer: key exists in either or both input indexes
 *   - Left:  key exists in left or both input indexes
 *   - Right: key exists in right or both input indexes
 */
sealed trait JoinType

object InnerJoin extends JoinType
object OuterJoin extends JoinType
object LeftJoin  extends JoinType
object RightJoin extends JoinType
