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

import org.saddle.array

/**
 * JoinHelper takes a factorized representation of a left and right index (ie, the
 * label identifiers are integers).
 *
 * Applying the class will return a left and right indexer each of whose length is
 * equal to the length of the joint index, and each of whose ith entry indicates the
 * location within the original corresponding (left/right) index which contributes to
 * the ith entry of the joint index.
 *
 * Also see [[org.saddle.array.take]] for more info
 */
private[saddle] object JoinHelper {

  def apply(leftLabels: Array[Int], rightLabels: Array[Int], max_groups: Int, how: JoinType): JoinResult = {
    val (marker, counter) = how match {
      case InnerJoin => (ijMarker, ijCounter)
      case OuterJoin => (ojMarker, ojCounter)
      case LeftJoin  => (ljMarker, ljCounter)
      case RightJoin => sys.error("Cannot call directly with RightJoin")
    }

    var count = 0

    // create permutations which recover original input ordering
    val lcounts = labelCount(leftLabels, max_groups)
    val rcounts = labelCount(rightLabels, max_groups)

    val lUnsorter = unsorter(leftLabels, lcounts, max_groups)
    val rUnsorter = unsorter(rightLabels, rcounts, max_groups)

    // count number of output rows in join
    var i = 1
    while (i <= max_groups) {
      val lc = lcounts(i)
      val rc = rcounts(i)
      count = counter(lc, rc, count)
      i += 1
    }

    // exclude NA group
    var lpos = lcounts(0)
    var rpos = rcounts(0)

    val lLabels = Array.ofDim[Int](count)
    val rLabels = Array.ofDim[Int](count)

    // create join factor labels
    i = 1
    var pos = 0
    while (i <= max_groups) {
      var lc = lcounts(i)
      val rc = rcounts(i)
      pos = marker(lLabels, rLabels, lc, rc, lpos, rpos, pos)
      lpos += lc
      rpos += rc
      i += 1
    }

    JoinResult(applyUnsorter(lUnsorter, lLabels), applyUnsorter(rUnsorter, rLabels))
  }

  // Calculates mapping of factor label to count seen in labels array
  private def labelCount(labels: Array[Int], numFactors: Int): Array[Int]= {
    val n = labels.length

    // Create vector of factor counts seen in labels array, saving location 0 for N/A
    val counts = Array.ofDim[Int](numFactors + 1)
    var i = 0
    while(i < n) {
      counts(labels(i) + 1) += 1
      i += 1
    }

    counts
  }

  // Calculate permutation from sorted(labels) -> labels, so we can recover an array of factor labels
  // in the originally provided order.
  private def unsorter(labels: Array[Int], counts: Array[Int], numFactors: Int): Array[Int] = {
    val n = labels.length

    // calculate running sum of label counts
    // - acts as a map from factor label to first offset within hypothetically sorted
    //   label array (in factor-ascending order)
    val where = Array.ofDim[Int](numFactors + 1)
    var i = 1
    while(i < numFactors + 1) {
      where(i) = where(i - 1) + counts(i - 1)
      i += 1
    }

    // Build a permutation that maps from a position in a sorted label array
    // to a position in the original label array.
    val permuter = Array.ofDim[Int](n)
    i = 0
    while(i < n) {
      val w = labels(i) + 1    // ith factor label
      permuter(where(w)) = i   // permuter[loc in sorted array] = i
      where(w) += 1
      i += 1
    }

    permuter
  }

  private def applyUnsorter(unsorter: Array[Int], labels: Array[Int]): Array[Int] = {
    if(unsorter.length > 0)
      array.take(unsorter, labels, -1)
    else {
      val ll = labels.length
      val ar = Array.ofDim[Int](ll)
      var i = 0
      while (i < ll) {
        ar(i) = -1
        i += 1
      }
      ar
    }
  }

  // Wrapper traits for inner logic; not anonymous functions to avoid boxing

  // Input:  L/R label arrays, L/R count of current label, L/R position of current label, position in join
  // Output: new join position
  // Effect: updates label arrays
  private trait LabelMarker {
    def apply(lLabels: Array[Int], rLabels: Array[Int], lc: Int, rc: Int, lpos: Int, rpos: Int, pos: Int): Int
  }

  // Input:  L/R count of current label
  // Output: new count value
  // Effect: None
  private trait JoinCounter {
    def apply(lc: Int, rc: Int, count: Int): Int
  }

  private object ijCounter extends JoinCounter {
    def apply(lc: Int, rc: Int, count: Int): Int = count + lc * rc
  }

  private object ojCounter extends JoinCounter {
    def apply(lc: Int, rc: Int, count: Int): Int = if (rc > 0 && lc > 0) count + lc * rc else count + lc + rc
  }

  private object ljCounter extends JoinCounter {
    def apply(lc: Int, rc: Int, count: Int): Int = if (rc > 0) count + lc * rc else count + lc
  }

  private object ijMarker extends LabelMarker {
    def apply(lLabels: Array[Int], rLabels: Array[Int], lc: Int, rc: Int, lpos: Int, rpos: Int, pos: Int): Int = {
      if (rc > 0 && lc > 0) {
        var j = 0
        while (j < lc) {
          val offset = pos + j * rc
          var k = 0
          while (k < rc) {
            lLabels(offset + k) = lpos + j
            rLabels(offset + k) = rpos + k
            k += 1
          }
          j += 1
        }
      }
      pos + lc * rc
    }
  }

  private object ojMarker extends LabelMarker {
    def apply(lLabels: Array[Int], rLabels: Array[Int], lc: Int, rc: Int, lpos: Int, rpos: Int, pos: Int): Int = {
      if (rc == 0) {
        var j = 0
        while (j < lc) {
          lLabels(pos + j) = lpos + j
          rLabels(pos + j) = -1
          j += 1
        }
        pos + lc
      }
      else if (lc == 0) {
        var j = 0
        while (j < rc) {
          lLabels(pos + j) = -1
          rLabels(pos + j) = rpos + j
          j += 1
        }
        pos + rc
      }
      else {
        var j = 0
        while (j < lc) {
          val offset = pos + j * rc
          var k = 0
          while (k < rc) {
            lLabels(offset + k) = lpos + j
            rLabels(offset + k) = rpos + k
            k += 1
          }
          j += 1
        }
        pos + lc * rc
      }
    }
  }

  private object ljMarker extends LabelMarker {
    def apply(lLabels: Array[Int], rLabels: Array[Int], lc: Int, rc: Int, lpos: Int, rpos: Int, pos: Int): Int = {
      if (rc == 0) {
        var j = 0
        while (j < lc) {
          lLabels(pos + j) = lpos + j
          rLabels(pos + j) = -1
          j += 1
        }
        pos + lc
      }
      else {
        var j = 0
        while (j < lc) {
          val offset = pos + j * rc
          var k = 0
          while (k < rc) {
            lLabels(offset + k) = lpos + j
            rLabels(offset + k) = rpos + k
            k += 1
          }
          j += 1
        }
        pos + lc * rc
      }
    }
  }
}

private[saddle] case class JoinResult(lIdx: Array[Int], rIdx: Array[Int])