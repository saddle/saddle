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

package org.saddle.util

/**
 * This file consists of a few main programs to generate an infinite stream of raw
 * random bytes to stdin; designed to feed into Dieharder battery of tests
 *
 * http://www.phy.duke.edu/~rgb/General/dieharder.php
 *
 * or to the TestU01 battery:
 *
 * http://www.iro.umontreal.ca/~simardr/testu01/tu01.html
 *
 * A useful stdin wrapper for the latter can be found at
 *
 * https://code.google.com/p/csrng/test/TestU01_raw_stdin_input_with_log.c
 *
 * which can be used to replicate the results in "TestU01: A C Library for Empirical
 * Testing of Random Number Generators"; L'Ecuyer and Simard; 2007.
 *
 * For example, after compiling per instructions, can run:
 *
 *   java -cp ... org.saddle.util.Xor64 | ./dieharder -a -g 200
 */

import java.io.BufferedInputStream

/**
 * ========= Summary results of SmallCrush =========
 *  Version:               TestU01 1.2.3
 *  Generator:             STDIN
 *  Number of statistics:  15
 *  Total CPU time:        00:00:19.71
 *  The following tests gave p-values outside [0.001, 0.9990]:
 *  (eps  means a value < 1.0e-300):
 *  (eps1 means a value < 1.0e-15):
 *
 *        Test                          p-value
 *  ----------------------------------------------
 *   8  MatrixRank                       eps
 *  ----------------------------------------------
 *  All other tests were passed
 */
object Xor64 {
  def main(args: Array[String]) {
    val stream = new BufferedInputStream(RandomStream(XorShift(12345678910L)))
    while (true) System.out.write(stream.read())
  }
}

/**
 * ========= Summary results of SmallCrush =========
 * Version:               TestU01 1.2.3
 * Generator:             STDIN
 * Number of statistics:  15
 * Total CPU time:        00:00:19.03
 * The following tests gave p-values outside [0.001, 0.9990]:
 * (eps  means a value < 1.0e-300):
 * (eps1 means a value < 1.0e-15):
 *
 *       Test                          p-value
 * ----------------------------------------------
 * 10  RandomWalk1 J                   0.9997
 * ----------------------------------------------
 * All other tests were passed
 */
object LFib {
  def main(args: Array[String]) {
    val stream = new BufferedInputStream(RandomStream(LFib4(12345678910L)))
    while (true) System.out.write(stream.read())
  }
}

/**
 * ========= Summary results of SmallCrush =========
 * Version:               TestU01 1.2.3
 * Generator:             STDIN
 * Number of statistics:  15
 * Total CPU time:        00:00:18.85
 *
 * All tests were passed
 */
object Ziff {
  def main(args: Array[String]) {
    val stream = new BufferedInputStream(RandomStream(Ziff98(12345678910L)))
    while (true) System.out.write(stream.read())
  }
}