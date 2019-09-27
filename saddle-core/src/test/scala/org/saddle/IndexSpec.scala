package org.saddle

import org.specs2.mutable.Specification
import scalar.Scalar

/**
  * User: Adam Klein
  * Date: 2/19/13
  * Time: 7:17 PM
  */
class IndexSpec extends Specification {
  "Index methods" should {
    "next works" in {
      Index(1, 2, 2, 2, 3).next(Scalar(2)) must_== Scalar(3)
      Index(1, 2, 2, 2, 3).next(Scalar(3)) must_== Scalar(3)
      Index(1, 2, 2, 2, 3).next(Scalar(1)) must_== Scalar(2)
    }
    "prev works" in {
      Index(1, 2, 2, 2, 3).prev(Scalar(2)) must_== Scalar(1)
      Index(1, 2, 2, 2, 3).prev(Scalar(3)) must_== Scalar(2)
      Index(1, 2, 2, 2, 3).prev(Scalar(1)) must_== Scalar(1)
    }
    "sliceBy works" in {
      Index(1, 2, 2, 2, 3, 3, 4).sliceBy(1, 4) must_== Index(1, 2, 2, 2, 3, 3,
        4)
      Index(1, 2, 2, 2, 3, 3, 4).sliceBy(1, 4, false) must_== Index(1, 2, 2, 2,
        3, 3)
      Index(1, 2, 2, 2, 3, 3, 4).sliceBy(2, 3, false) must_== Index(2, 2, 2)
      Index(1, 2, 2, 2, 3, 3, 4).sliceBy(2, 3) must_== Index(2, 2, 2, 3, 3)
    }
  }
  "Index Joins" should {
    "Unique sorted left join" in {
      val ix1 = Index(0, 1, 2)
      val ix2 = Index(1, 2, 3)

      val res = ix1.join(ix2, how = index.LeftJoin)

      res.index must_== Index(0, 1, 2)
      res.lTake must_== None
      res.rTake.get must_== Array(-1, 0, 1)
    }

    "Unique sorted right join" in {
      val ix1 = Index(0, 1, 2)
      val ix2 = Index(1, 2, 3)

      val res = ix1.join(ix2, how = index.RightJoin)

      res.index must_== Index(1, 2, 3)
      res.lTake.get must_== Array(1, 2, -1)
      res.rTake must_== None
    }

    "Unique sorted inner join" in {
      val ix1 = Index(0, 1, 2)
      val ix2 = Index(1, 2, 3)

      val res = ix1.join(ix2, how = index.InnerJoin)

      res.index must_== Index(1, 2)
      res.lTake.get must_== Array(1, 2)
      res.rTake.get must_== Array(0, 1)
    }

    "Unique sorted outer join" in {
      val ix1 = Index(0, 1, 2)
      val ix2 = Index(1, 2, 3)

      val res = ix1.join(ix2, how = index.OuterJoin)

      res.index must_== Index(0, 1, 2, 3)
      res.lTake.get must_== Array(0, 1, 2, -1)
      res.rTake.get must_== Array(-1, 0, 1, 2)
    }

    "Unique unsorted left join" in {
      val ix1 = Index(1, 0, 2)
      val ix2 = Index(2, 3, 1)

      val res = ix1.join(ix2, how = index.LeftJoin)

      res.index must_== Index(1, 0, 2)
      res.lTake must_== None
      res.rTake.get must_== Array(2, -1, 0)
    }

    "Unique unsorted right join" in {
      val ix1 = Index(1, 0, 2)
      val ix2 = Index(2, 1, 3)

      val res = ix1.join(ix2, how = index.RightJoin)

      res.index must_== Index(2, 1, 3)
      res.lTake.get must_== Array(2, 0, -1)
      res.rTake must_== None
    }

    "Unique unsorted inner join" in {
      val ix1 = Index(1, 0, 2)
      val ix2 = Index(2, 1, 3)

      val res = ix1.join(ix2, how = index.InnerJoin)

      res.index must_== Index(1, 2)
      res.lTake.get must_== Array(0, 2)
      res.rTake.get must_== Array(1, 0)
    }

    "Unique unsorted outer join" in {
      val ix1 = Index(1, 0, 2)
      val ix2 = Index(2, 1, 3)

      val res = ix1.join(ix2, how = index.OuterJoin)

      res.index must_== Index(1, 0, 2, 3)
      res.lTake.get must_== Array(0, 1, 2, -1)
      res.rTake.get must_== Array(1, -1, 0, 2)
    }

    "Non-unique sorted left join" in {
      val ix1 = Index(0, 1, 1, 2)
      val ix2 = Index(1, 2, 2, 3)

      val res = ix1.join(ix2, how = index.LeftJoin)

      res.index must_== Index(0, 1, 1, 2, 2)
      res.lTake.get must_== Array(0, 1, 2, 3, 3)
      res.rTake.get must_== Array(-1, 0, 0, 1, 2)
    }

    "Non-unique sorted left join [case 2]" in {
      val ix1 = Index(0, 1, 1, 2)
      val ix2 = Index(1)

      val res1 = ix1.join(ix2, how = index.LeftJoin)

      res1.index must_== Index(0, 1, 1, 2)
      res1.lTake must_== None
      res1.rTake.get must_== Array(-1, 0, 0, -1)

      val res2 = ix2.join(ix1, how = index.LeftJoin)

      res2.index must_== Index(1, 1)
      res2.lTake.get must_== Array(0, 0)
      res2.rTake.get must_== Array(1, 2)
    }

    "Non-unique sorted left join [case 3]" in {
      val ix1 = Index(0, 1, 1, 2, 2)
      val ix2 = Index(1, 2, 2, 3)

      val res = ix1.join(ix2, how = index.LeftJoin)

      res.index must_== Index(0, 1, 1, 2, 2, 2, 2)
      res.lTake.get must_== Array(0, 1, 2, 3, 3, 4, 4)
      res.rTake.get must_== Array(-1, 0, 0, 1, 2, 1, 2)
    }

    "Non-unique sorted right join" in {
      val ix1 = Index(0, 1, 1, 2)
      val ix2 = Index(1, 2, 2, 3)

      val res = ix1.join(ix2, how = index.RightJoin)

      res.index must_== Index(1, 1, 2, 2, 3)
      res.lTake.get must_== Array(1, 2, 3, 3, -1)
      res.rTake.get must_== Array(0, 0, 1, 2, 3)
    }

    "Non-unique sorted inner join" in {
      val ix1 = Index(0, 1, 1, 2)
      val ix2 = Index(1, 2, 2, 3)

      val res = ix1.join(ix2, how = index.InnerJoin)

      res.index must_== Index(1, 1, 2, 2)
      res.lTake.get must_== Array(1, 2, 3, 3)
      res.rTake.get must_== Array(0, 0, 1, 2)
    }

    "Non-unique sorted inner join [case 2]" in {
      val ix1 = Index(1, 1, 3, 4)
      val ix2 = Index(1)

      val res1 = ix1.join(ix2, how = index.InnerJoin)

      res1.index must_== Index(1, 1)
      res1.lTake.get must_== Array(0, 1)
      res1.rTake.get must_== Array(0, 0)

      val res2 = ix2.join(ix1, how = index.InnerJoin)

      res2.index must_== Index(1, 1)
      res2.lTake.get must_== Array(0, 0)
      res2.rTake.get must_== Array(0, 1)
    }

    "Non-unique sorted inner join [case 3]" in {
      val ix1 = Index(0, 1, 1, 2, 2)
      val ix2 = Index(1, 2, 2, 3)

      val res = ix1.join(ix2, how = index.InnerJoin)

      res.index must_== Index(1, 1, 2, 2, 2, 2)
      res.lTake.get must_== Array(1, 2, 3, 3, 4, 4)
      res.rTake.get must_== Array(0, 0, 1, 2, 1, 2)
    }

    "Non-unique sorted outer join" in {
      val ix1 = Index(0, 1, 1, 2)
      val ix2 = Index(1, 2, 2, 3)

      val res = ix1.join(ix2, how = index.OuterJoin)

      res.index must_== Index(0, 1, 1, 2, 2, 3)
      res.lTake.get must_== Array(0, 1, 2, 3, 3, -1)
      res.rTake.get must_== Array(-1, 0, 0, 1, 2, 3)
    }

    "Non-unique sorted outer join [case 2]" in {
      val ix1 = Index(0, 1, 1, 2)
      val ix2 = Index(1)

      val res1 = ix1.join(ix2, how = index.OuterJoin)

      res1.index must_== Index(0, 1, 1, 2)
      res1.lTake.get must_== Array(0, 1, 2, 3)
      res1.rTake.get must_== Array(-1, 0, 0, -1)

      val res2 = ix2.join(ix1, how = index.OuterJoin)

      res2.index must_== Index(0, 1, 1, 2)
      res2.lTake.get must_== Array(-1, 0, 0, -1)
      res2.rTake.get must_== Array(0, 1, 2, 3)
    }

    "Non-unique unsorted left join" in {
      val ix1 = Index(1, 1, 2, 0)
      val ix2 = Index(1, 3, 2, 2)

      val res = ix1.join(ix2, how = index.LeftJoin)

      res.index must_== Index(1, 1, 2, 2, 0)
      res.lTake.get must_== Array(0, 1, 2, 2, 3)
      res.rTake.get must_== Array(0, 0, 2, 3, -1)
    }

    "Non-unique unsorted left join [case 2]" in {
      val ix1 = Index(1, 1, 2, 2, 0)
      val ix2 = Index(1, 3, 2, 2)

      val res = ix1.join(ix2, how = index.LeftJoin)

      res.index must_== Index(1, 1, 2, 2, 2, 2, 0)
      res.lTake.get must_== Array(0, 1, 2, 2, 3, 3, 4)
      res.rTake.get must_== Array(0, 0, 2, 3, 2, 3, -1)
    }

    "Non-unique unsorted right join" in {
      val ix1 = Index(1, 1, 2, 0)
      val ix2 = Index(1, 3, 2, 2)

      val res = ix1.join(ix2, how = index.RightJoin)

      res.index must_== Index(1, 1, 3, 2, 2)
      res.lTake.get must_== Array(0, 1, -1, 2, 2)
      res.rTake.get must_== Array(0, 0, 1, 2, 3)
    }

    "Non-unique unsorted inner join" in {
      val ix1 = Index(1, 1, 2, 0)
      val ix2 = Index(1, 3, 2, 2)

      val res = ix1.join(ix2, how = index.InnerJoin)

      res.index must_== Index(1, 1, 2, 2)
      res.lTake.get must_== Array(0, 1, 2, 2)
      res.rTake.get must_== Array(0, 0, 2, 3)
    }

    "Non-unique unsorted outer join" in {
      val ix1 = Index(1, 1, 2, 0)
      val ix2 = Index(1, 3, 2, 2)

      val res = ix1.join(ix2, how = index.OuterJoin)

      res.index must_== Index(1, 1, 2, 2, 0, 3)
      res.lTake.get must_== Array(0, 1, 2, 2, 3, -1)
      res.rTake.get must_== Array(0, 0, 2, 3, -1, 1)
    }
  }
}
