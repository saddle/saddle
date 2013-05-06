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
 * random bytes to stdin; designed to feed into the TestU01 battery of tests:
 *
 * http://www.iro.umontreal.ca/~simardr/testu01/tu01.html
 *
 * A stdin wrapper that I used for the latter can be found within
 *
 * http://code.google.com/p/csrng/
 *
 * In particular the file test/TestU01_raw_stdin_input_with_log.c
 *
 * which can be used to replicate the results in "TestU01: A C Library for Empirical
 * Testing of Random Number Generators"; L'Ecuyer and Simard; 2007.
 *
 * For example, after compiling per instructions, can run:
 *
 *   java -cp ... org.saddle.util.Xor64 | ./TestU01_raw_stdin_input_with_log --normal
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
 *
 * ========= Summary results of Crush =========
 *  Version:          TestU01 1.2.3
 *  Generator:        STDIN
 *  Number of statistics:  144
 *  Total CPU time:   00:57:39.11
 *  The following tests gave p-values outside [0.001, 0.9990]:
 *  (eps  means a value < 1.0e-300):
 *  (eps1 means a value < 1.0e-15):
 *
 *        Test                          p-value
 *  ----------------------------------------------
 *   3  CollisionOver, t = 2           1.4e-10
 *   6  CollisionOver, t = 4             eps
 *  10  CollisionOver, t = 20          1.3e-62
 *  11  BirthdaySpacings, t = 2         5.2e-5
 *  16  BirthdaySpacings, t = 8        1.7e-12
 *  17  BirthdaySpacings, t = 8        3.3e-73
 *  34  Gap, r = 22                      eps
 *  56  MatrixRank, 60 x 60              eps
 *  57  MatrixRank, 60 x 60              eps
 *  58  MatrixRank, 300 x 300            eps
 *  59  MatrixRank, 300 x 300            eps
 *  60  MatrixRank, 1200 x 1200          eps
 *  61  MatrixRank, 1200 x 1200          eps
 *  65  RandomWalk1 H (L = 90)           eps
 *  65  RandomWalk1 M (L = 90)           eps
 *  66  RandomWalk1 H (L = 90)           eps
 *  66  RandomWalk1 M (L = 90)           eps
 *  66  RandomWalk1 J (L = 90)           eps
 *  67  RandomWalk1 H (L = 1000)         eps
 *  67  RandomWalk1 M (L = 1000)        2.0e-9
 *  67  RandomWalk1 C (L = 1000)        1.0e-4
 *  68  RandomWalk1 H (L = 1000)       2.0e-12
 *  68  RandomWalk1 M (L = 1000)         eps
 *  68  RandomWalk1 J (L = 1000)         eps
 *  69  RandomWalk1 C (L = 10000)       9.7e-4
 *  71  LinearComp, r = 0              1 - eps1
 *  72  LinearComp, r = 29             1 - eps1
 *  85  HammingIndep, L = 30             eps
 *  86  HammingIndep, L = 30             eps
 *  87  HammingIndep, L = 300            eps
 *  88  HammingIndep, L = 300            eps
 *  ----------------------------------------------
 *  All other tests were passed
 *
 * ========= Summary results of BigCrush =========
 *  Version:          TestU01 1.2.3
 *  Generator:        STDIN
 *  Number of statistics:  160
 *  Total CPU time:   08:31:19.41
 *  The following tests gave p-values outside [0.001, 0.9990]:
 *  (eps  means a value < 1.0e-300):
 *  (eps1 means a value < 1.0e-15):
 *
 *        Test                          p-value
 *  ----------------------------------------------
 *   2  SerialOver, r = 22               eps
 *   3  CollisionOver, t = 2           6.8e-22
 *   6  CollisionOver, t = 3             eps
 *  12  CollisionOver, t = 21            eps
 *  13  BirthdaySpacings, t = 2        1.7e-18
 *  15  BirthdaySpacings, t = 4          eps
 *  18  BirthdaySpacings, t = 8       7.2e-289
 *  19  BirthdaySpacings, t = 8          eps
 *  21  BirthdaySpacings, t = 16       3.8e-66
 *  37  Gap, r = 20                      eps
 *  68  MatrixRank, L=1000, r=0          eps
 *  69  MatrixRank, L=1000, r=26         eps
 *  70  MatrixRank, L=5000               eps
 *  71  MatrixRank, L=5000               eps
 *  77  RandomWalk1 H (L=1000, r=20)     eps
 *  77  RandomWalk1 M (L=1000, r=20)     eps
 *  77  RandomWalk1 J (L=1000, r=20)     eps
 *  80  LinearComp, r = 0              1 - eps1
 *  81  LinearComp, r = 29             1 - eps1
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
 *  Version:               TestU01 1.2.3
 *  Generator:             STDIN
 *  Number of statistics:  15
 *  Total CPU time:        00:00:19.03
 *  The following tests gave p-values outside [0.001, 0.9990]:
 *  (eps  means a value < 1.0e-300):
 *  (eps1 means a value < 1.0e-15):
 *
 *        Test                          p-value
 *  ----------------------------------------------
 *  10  RandomWalk1 J                   0.9997
 *  ----------------------------------------------
 *  All other tests were passed
 *
 *
 * ========= Summary results of Crush =========
 *  Version:          TestU01 1.2.3
 *  Generator:        STDIN
 *  Number of statistics:  144
 *  Total CPU time:   01:00:08.48
 *
 *  All tests were passed
 *
 * ========= Summary results of BigCrush =========
 *  Version:          TestU01 1.2.3
 *  Generator:        STDIN
 *  Number of statistics:  160
 *  Total CPU time:   08:38:24.56
 *
 *  All tests were passed
 */
object LFib {
  def main(args: Array[String]) {
    val stream = new BufferedInputStream(RandomStream(LFib4(12345678910L)))
    while (true) System.out.write(stream.read())
  }
}

/**
 * ========= Summary results of SmallCrush =========
 *  Version:               TestU01 1.2.3
 *  Generator:             STDIN
 *  Number of statistics:  15
 *  Total CPU time:        00:00:18.85
 *
 *  All tests were passed
 *
 * ========= Summary results of Crush =========
 *  Version:          TestU01 1.2.3
 *  Generator:        STDIN
 *  Number of statistics:  144
 *  Total CPU time:   01:00:08.48
 *
 *  All tests were passed
 *
 * ========= Summary results of BigCrush =========
 *  Version:          TestU01 1.2.3
 *  Generator:        STDIN
 *  Number of statistics:  160
 *  Total CPU time:   08:57:21.49
 *  The following tests gave p-values outside [0.001, 0.9990]:
 *  (eps  means a value < 1.0e-300):
 *  (eps1 means a value < 1.0e-15):
 *
 *        Test                          p-value
 *  ----------------------------------------------
 *  80  LinearComp, r = 0              1 - eps1
 *  81  LinearComp, r = 29             1 - eps1
 *  ----------------------------------------------
 *  All other tests were passed
 */
object Ziff {
  def main(args: Array[String]) {
    val stream = new BufferedInputStream(RandomStream(Ziff98(12345678910L)))
    while (true) System.out.write(stream.read())
  }
}