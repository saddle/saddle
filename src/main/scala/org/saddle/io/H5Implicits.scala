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

package org.saddle.io

import org.saddle._

object H5Implicits {
  /**
   * Provides enrichment on Frame object for writing to an HDF5 file.
   */
  implicit def frame2H5Writer[RX: ST: ORD, CX: ST: ORD, T: ST](frame: Frame[RX, CX, T]) = new {

    /**
     * Write a frame in HDF5 format to a file at the path provided
     *
     * @param path File to write
     * @param id Name of the HDF group in which to store frame data
     */
    def writeHdfFile(path: String, id: String) {
      H5Store.writeFrame(path, id, frame)
    }

  } // end new

}
