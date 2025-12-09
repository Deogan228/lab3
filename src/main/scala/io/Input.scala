package shop.io

import scala.io.StdIn

object Input:
  // Ленивый поток строк с консоли
  val lines: LazyList[String] =
    LazyList.continually(StdIn.readLine()).takeWhile(_ != null)
