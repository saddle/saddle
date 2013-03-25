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

package org.saddle

import org.specs2.mutable.Specification
import time._

class SeriesSpec extends Specification {
  "reindex works on dates" in {
    val s1 = Series(Vec(1d, 2, 3), Index(datetime(2005,1,1), datetime(2005,1,2), datetime(2005,1,3)))
    val s2 = Series(Vec(5d, 7), Index(datetime(2005,1,1), datetime(2005,1,3)))

    s2.reindex(s1.index).index must_== s1.index
  }
}
