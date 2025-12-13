package shop

import shop.io.Input
import shop.io.ConsoleAsk.*
import shop.model.*

object Main:
  def main(args: Array[String]): Unit =
    val in0 = Input.lines

    val (customerName, in1) = askName("Введите имя покупателя:")(in0)
    val customer = Customer(customerName)

    val (count, in2) = askPositiveInt("Сколько элементов на верхнем уровне покупки?")(in1)
    val (roots, _)   = readNodeList(count, in2)

    val purchase = Purchase(customer, roots)

    println()
    println("--- Итоговая покупка ---")
    println(PrettyPrint.renderPurchase(purchase))
