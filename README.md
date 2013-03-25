==========================
Saddle: Scala Data Library
==========================

Introduction
============

Saddle is a data manipulation library for Scala_ that provides array-backed,
indexed, one- and two-dimensional data structures that are judiciously
specialized on JVM primitives to avoid the overhead of boxing and unboxing.

Saddle offers vectorized numerical calculations, automatic alignment of data
along indices, robustness to missing (N/A) values, and facilities for I/O.

Saddle draws inspiration from several sources, among them the R_ programming
language & statistical environment, the numpy_ and pandas_ Python_ libraries,
and the Scala collections library.

License
=======

Saddle is distributed under the Apache License Version 2.0 (see LICENSE file).

Copyright
=========

Copyright (c) 2013 Novus Partners, Inc.
Copyright (c) 2013 The Saddle Development Team

All rights reserved.

Saddle is subject to a shared copyright. Each contributor retains copyright to
his or her contributions to Saddle, and is free to annotate these contributions
via code repository commit messages. The copyright to the entirety of the code
base is shared among the Saddle Development Team, comprised of the developers
who have made such contributions.

The copyright and license of each file shall read as follows:


  Copyright (c) YYYY Saddle Development Team

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.


Individual contributors may, if they so desire, append their names to
the CONTRIBUTORS file.

About the Copyright Holders
===========================

Adam Klein began Saddle development in 2012 while an employee of Novus
Partners, Inc [0]. The code was released by Novus under this license in
2013. Adam Klein remains lead developer. Saddle was inspired by earlier
prototypes developed by Chris Lewis, Cheng Peng, & David Cru. Saddle was
also inspired by pandas [1], a data analysis library written in Python,
whose development is led by Wes McKinney.

[0] http://www.novus.com/
[1] http://pandas.pydata.org/

