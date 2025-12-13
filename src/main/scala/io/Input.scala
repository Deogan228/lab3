package shop.io

import scala.io.StdIn

object Input:
  val lines: LazyList[String] =
    LazyList.continually(StdIn.readLine()).takeWhile(_ != null)
