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
 * This file consistes of a few main programs to generate infinite stream of random
 * output to stdin, designed to feed into Dieharder battery of tests:
 *
 * http://www.phy.duke.edu/~rgb/General/dieharder.php
 */

import java.io.BufferedInputStream

/**
 * #=============================================================================#
 * #            dieharder version 3.31.1 Copyright 2003 Robert G. Brown          #
 * #=============================================================================#
 *    rng_name    |rands/second|   Seed   |
 * stdin_input_raw|  3.54e+06  |1439889471|
 * #=============================================================================#
 *         test_name   |ntup| tsamples |psamples|  p-value |Assessment
 * #=============================================================================#
 *    diehard_birthdays|   0|       100|     100|0.67449395|  PASSED
 *       diehard_operm5|   0|   1000000|     100|0.80274296|  PASSED
 *   diehard_rank_32x32|   0|     40000|     100|0.57369982|  PASSED
 *     diehard_rank_6x8|   0|    100000|     100|0.54585904|  PASSED
 *    diehard_bitstream|   0|   2097152|     100|0.22756959|  PASSED
 *         diehard_opso|   0|   2097152|     100|0.49655993|  PASSED
 *         diehard_oqso|   0|   2097152|     100|0.88196641|  PASSED
 *          diehard_dna|   0|   2097152|     100|0.45041544|  PASSED
 * diehard_count_1s_str|   0|    256000|     100|0.13097840|  PASSED
 * diehard_count_1s_byt|   0|    256000|     100|0.50021576|  PASSED
 *  diehard_parking_lot|   0|     12000|     100|0.95039058|  PASSED
 *     diehard_2dsphere|   2|      8000|     100|0.97620005|  PASSED
 *     diehard_3dsphere|   3|      4000|     100|0.61127811|  PASSED
 *      diehard_squeeze|   0|    100000|     100|0.00873247|  PASSED
 *         diehard_sums|   0|       100|     100|0.03799127|  PASSED
 *         diehard_runs|   0|    100000|     100|0.27043962|  PASSED
 *         diehard_runs|   0|    100000|     100|0.05242603|  PASSED
 *        diehard_craps|   0|    200000|     100|0.35728660|  PASSED
 *        diehard_craps|   0|    200000|     100|0.59189008|  PASSED
 *  marsaglia_tsang_gcd|   0|  10000000|     100|0.77117912|  PASSED
 *  marsaglia_tsang_gcd|   0|  10000000|     100|0.89780328|  PASSED
 *          sts_monobit|   1|    100000|     100|0.72407199|  PASSED
 *             sts_runs|   2|    100000|     100|0.30183032|  PASSED
 *           sts_serial|   1|    100000|     100|0.80061040|  PASSED
 *           sts_serial|   2|    100000|     100|0.23670011|  PASSED
 *           sts_serial|   3|    100000|     100|0.35485712|  PASSED
 *           sts_serial|   3|    100000|     100|0.20217720|  PASSED
 *           sts_serial|   4|    100000|     100|0.74751205|  PASSED
 *           sts_serial|   4|    100000|     100|0.97735076|  PASSED
 *           sts_serial|   5|    100000|     100|0.08160659|  PASSED
 *           sts_serial|   5|    100000|     100|0.58390237|  PASSED
 *           sts_serial|   6|    100000|     100|0.56863095|  PASSED
 *           sts_serial|   6|    100000|     100|0.14219839|  PASSED
 *           sts_serial|   7|    100000|     100|0.92777187|  PASSED
 *           sts_serial|   7|    100000|     100|0.24659032|  PASSED
 *           sts_serial|   8|    100000|     100|0.82124725|  PASSED
 *           sts_serial|   8|    100000|     100|0.97061385|  PASSED
 *           sts_serial|   9|    100000|     100|0.95841573|  PASSED
 *           sts_serial|   9|    100000|     100|0.54702078|  PASSED
 *           sts_serial|  10|    100000|     100|0.89037099|  PASSED
 *           sts_serial|  10|    100000|     100|0.75017893|  PASSED
 *           sts_serial|  11|    100000|     100|0.39655306|  PASSED
 *           sts_serial|  11|    100000|     100|0.66968963|  PASSED
 *           sts_serial|  12|    100000|     100|0.82179670|  PASSED
 *           sts_serial|  12|    100000|     100|0.27236559|  PASSED
 *           sts_serial|  13|    100000|     100|0.06036826|  PASSED
 *           sts_serial|  13|    100000|     100|0.13445453|  PASSED
 *           sts_serial|  14|    100000|     100|0.02117087|  PASSED
 *           sts_serial|  14|    100000|     100|0.13998347|  PASSED
 *           sts_serial|  15|    100000|     100|0.00285799|   WEAK
 *           sts_serial|  15|    100000|     100|0.79337549|  PASSED
 *           sts_serial|  16|    100000|     100|0.19744743|  PASSED
 *           sts_serial|  16|    100000|     100|0.87419992|  PASSED
 *          rgb_bitdist|   1|    100000|     100|0.75604004|  PASSED
 *          rgb_bitdist|   2|    100000|     100|0.08074991|  PASSED
 *          rgb_bitdist|   3|    100000|     100|0.49177433|  PASSED
 *          rgb_bitdist|   4|    100000|     100|0.80606976|  PASSED
 *          rgb_bitdist|   5|    100000|     100|0.88728198|  PASSED
 *          rgb_bitdist|   6|    100000|     100|0.29411819|  PASSED
 *          rgb_bitdist|   7|    100000|     100|0.94009953|  PASSED
 *          rgb_bitdist|   8|    100000|     100|0.98062569|  PASSED
 *          rgb_bitdist|   9|    100000|     100|0.83239503|  PASSED
 *          rgb_bitdist|  10|    100000|     100|0.21570794|  PASSED
 *          rgb_bitdist|  11|    100000|     100|0.63828812|  PASSED
 *          rgb_bitdist|  12|    100000|     100|0.91921648|  PASSED
 * rgb_minimum_distance|   2|     10000|    1000|0.49886268|  PASSED
 * rgb_minimum_distance|   3|     10000|    1000|0.00001880|   WEAK
 * rgb_minimum_distance|   4|     10000|    1000|0.62301084|  PASSED
 * rgb_minimum_distance|   5|     10000|    1000|0.50202688|  PASSED
 *     rgb_permutations|   2|    100000|     100|0.32081459|  PASSED
 *     rgb_permutations|   3|    100000|     100|0.88974598|  PASSED
 *     rgb_permutations|   4|    100000|     100|0.76759728|  PASSED
 *     rgb_permutations|   5|    100000|     100|0.81670947|  PASSED
 *       rgb_lagged_sum|   0|   1000000|     100|0.98260668|  PASSED
 *       rgb_lagged_sum|   1|   1000000|     100|0.07735223|  PASSED
 *       rgb_lagged_sum|   2|   1000000|     100|0.67911274|  PASSED
 *       rgb_lagged_sum|   3|   1000000|     100|0.95841830|  PASSED
 *       rgb_lagged_sum|   4|   1000000|     100|0.33797227|  PASSED
 *       rgb_lagged_sum|   5|   1000000|     100|0.98580717|  PASSED
 *       rgb_lagged_sum|   6|   1000000|     100|0.29429563|  PASSED
 *       rgb_lagged_sum|   7|   1000000|     100|0.76901554|  PASSED
 *       rgb_lagged_sum|   8|   1000000|     100|0.99498849|  PASSED
 *       rgb_lagged_sum|   9|   1000000|     100|0.80774538|  PASSED
 *       rgb_lagged_sum|  10|   1000000|     100|0.70225734|  PASSED
 *       rgb_lagged_sum|  11|   1000000|     100|0.56242359|  PASSED
 *       rgb_lagged_sum|  12|   1000000|     100|0.63092458|  PASSED
 *       rgb_lagged_sum|  13|   1000000|     100|0.97002285|  PASSED
 *       rgb_lagged_sum|  14|   1000000|     100|0.40050300|  PASSED
 *       rgb_lagged_sum|  15|   1000000|     100|0.79660371|  PASSED
 *       rgb_lagged_sum|  16|   1000000|     100|0.09723851|  PASSED
 *       rgb_lagged_sum|  17|   1000000|     100|0.67896676|  PASSED
 *       rgb_lagged_sum|  18|   1000000|     100|0.47813063|  PASSED
 *       rgb_lagged_sum|  19|   1000000|     100|0.82673491|  PASSED
 *       rgb_lagged_sum|  20|   1000000|     100|0.99638952|   WEAK
 *       rgb_lagged_sum|  21|   1000000|     100|0.56614046|  PASSED
 *       rgb_lagged_sum|  22|   1000000|     100|0.34301822|  PASSED
 *       rgb_lagged_sum|  23|   1000000|     100|0.75952204|  PASSED
 *       rgb_lagged_sum|  24|   1000000|     100|0.98885423|  PASSED
 *       rgb_lagged_sum|  25|   1000000|     100|0.26588059|  PASSED
 *       rgb_lagged_sum|  26|   1000000|     100|0.06330721|  PASSED
 *       rgb_lagged_sum|  27|   1000000|     100|0.78264666|  PASSED
 *       rgb_lagged_sum|  28|   1000000|     100|0.12989980|  PASSED
 *       rgb_lagged_sum|  29|   1000000|     100|0.77509665|  PASSED
 *       rgb_lagged_sum|  30|   1000000|     100|0.22018665|  PASSED
 *       rgb_lagged_sum|  31|   1000000|     100|0.54154562|  PASSED
 *       rgb_lagged_sum|  32|   1000000|     100|0.23825485|  PASSED
 *      rgb_kstest_test|   0|     10000|    1000|0.88891497|  PASSED
 *      dab_bytedistrib|   0|  51200000|       1|0.09399770|  PASSED
 *              dab_dct| 256|     50000|       1|0.36208029|  PASSED
 * Preparing to run test 207.  ntuple = 0
 *         dab_filltree|  32|  15000000|       1|0.00175248|   WEAK
 *         dab_filltree|  32|  15000000|       1|0.00003130|   WEAK
 * Preparing to run test 208.  ntuple = 0
 *        dab_filltree2|   0|   5000000|       1|0.53576922|  PASSED
 *        dab_filltree2|   1|   5000000|       1|0.90318889|  PASSED
 * Preparing to run test 209.  ntuple = 0
 *         dab_monobit2|  12|  65000000|       1|1.00000000|  FAILED
 */
object Xor64 {
  def main(args: Array[String]) {
    val stream = new BufferedInputStream(RandomStream(XorShift(new java.util.Random().nextLong)))
    while (true) System.out.write(stream.read())
  }
}

object LFib {
  def main(args: Array[String]) {
    val stream = new BufferedInputStream(RandomStream(LFib4(new java.util.Random().nextLong)))
    while (true) System.out.write(stream.read())
  }
}

object Ziff {
  def main(args: Array[String]) {
    val stream = new BufferedInputStream(RandomStream(Ziff98(new java.util.Random().nextLong)))
    while (true) System.out.write(stream.read())
  }
}
