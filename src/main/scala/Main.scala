package shop

import shop.model.*
import shop.io.Input
import shop.io.ConsoleAsk.*

object Main:
  def main(args: Array[String]): Unit =
    val in0 = Input.lines //Берём ленивый поток строк с консоли

    //Читаем имя покупателя
    val (custName, in1) = askString(
      prompt       = "Введите имя покупателя:",
      errorMessage = "Ошибка: имя не может быть пустым.",
      validate     = s => s.nonEmpty && s.exists(_.isLetter)
    )(in0)

    //Создаём покупателя
    val customer = Customer(custName)

    val (count, in2) = askInt(
      prompt       = "Сколько товаров в покупке?",
      errorMessage = "Ошибка: введите натуральное число (> 0).",
      validate     = _ > 0
    )(in1)

    val (items, _) = readItemList(count, in2)

    //Собираем покупку
    val purchase = Purchase(customer, items)

    println()
    println("--- Итоговая покупка ---")
    println(purchase)
    println(f"\nОбщий объём напитков: ${purchase.totalVolume}%.2f л")
