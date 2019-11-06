package org.saddle

/** Binary serialization for Frame[String,String,T] or Mat[T] with primitive T
  *
  * The layout of binary format is as follows:
  * - The first 6 bytes are "SADDLE"
  * - The next unsigned byte is the major version
  * - The next unsigned byte is the minor version
  * - The next 4 bytes form a little endian integer as HEADER_LENGTH
  * - The next HEADER_LENGTH bytes form an UTF-8 string as the header.
  * - The header is a valid JSON object with the following fields:
  *   - v: numeric positive integer is the version of the header structure
  *   - colix : a JSON array of strings, it is the column index of the frame
  *   - rowix : a JSON array of strings, it is the row index of the frame
  *   - numrows: numeric positive integer, number of rows
  *   - numcols: numeric positive integer, number of cols
  *   - Either one of rowix or numrows may be missing
  *   - Either one of colix or numcols may be missing
  *   - rowmajor : a boolean, indicating whether the data is stored in row-major or col-major order
  *   - datatype : string, either "double", "long", "int", "float", "byte"
  * - The header is padded with spaces (0x20) such that HEADER_LENGTH+12 is divisible by 16.
  *   The count of spaces are included in HEADER_LENGTH.
  * - The next width * numRows * numCols bytes form a little endian primitive array in row-major
  *   or col-major order. numRows and numCols are determined from the rowix/numrows and colix/numcols
  *   header fields. width is determined from the datatype field (8 for double and long, 4 for
  *   int and float, 1 for byte)
  */
package object binary {}
