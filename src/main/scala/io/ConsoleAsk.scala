package shop.io

import scala.util.Try
import scala.annotation.tailrec
import java.time.LocalDate

import shop.model.*

// Здесь всё про ввод: safe-ask + чтение Item/списка Item

object ConsoleAsk:

  // Универсальный safe-ввод значения типа T из LazyList
  private def ask[T](
                      prompt: String, //текст вопроса, который печатаем пользователю
                      parse: String => Option[T], //функция парсинга строки
                      errorMessage: String, //что вывести при ошибке
                      validate: T => Boolean = (_: T) => true
                    )(in: LazyList[String]): (T, LazyList[String]) =
    @tailrec
    def loop(stream: LazyList[String]): (T, LazyList[String]) =
      println(prompt)
      stream match
        case h #:: t => //если есть хотя бы одна строка
          val raw = h.trim //берём текущую строку
          parse(raw) match
            case Some(v) if validate(v) =>
              (v, t)
            case _ =>
              println(errorMessage)
              loop(t) //пробуем следующую строку
        case _ => //если список кончился, кидаем исключение
          throw new RuntimeException("Неожиданный конец ввода")
    loop(in)

  def askString(
                 prompt: String,
                 errorMessage: String,
                 validate: String => Boolean = _.nonEmpty
               )(in: LazyList[String]): (String, LazyList[String]) =
    ask[String](prompt, s => Some(s.trim), errorMessage, validate)(in)

  def askInt(
              prompt: String,
              errorMessage: String,
              validate: Int => Boolean = _ => true
            )(in: LazyList[String]): (Int, LazyList[String]) =
    ask[Int](prompt, s => Try(s.toInt).toOption, errorMessage, validate)(in)

  def askDouble(
                 prompt: String,
                 errorMessage: String,
                 validate: Double => Boolean = _ => true
               )(in: LazyList[String]): (Double, LazyList[String]) =
    ask[Double](prompt, s => Try(s.toDouble).toOption, errorMessage, validate)(in)

  def askBigDecimal(
                     prompt: String,
                     errorMessage: String,
                     validate: BigDecimal => Boolean = _ => true
                   )(in: LazyList[String]): (BigDecimal, LazyList[String]) =
    ask[BigDecimal](prompt, s => Try(BigDecimal(s)).toOption, errorMessage, validate)(in)

  // Тип товара для выбора
  enum ItemType:
    case Regular, Food, Drink

  // Чтение одного Item из LazyList
  def readItem(in: LazyList[String]): (Item, LazyList[String]) =
    val (kindInt, in1) = askInt(
      prompt       = "Тип товара (1 - обычный, 2 - продукт, 3 - напиток):",
      errorMessage = "Ошибка: введите 1, 2 или 3.",
      validate     = n => n >= 1 && n <= 3
    )(in)

    //Переводим число в ItemType
    val kind = kindInt match
      case 2 => ItemType.Food
      case 3 => ItemType.Drink
      case _ => ItemType.Regular

    val (name, in2) = askString(
      prompt       = "Название товара:",
      errorMessage = "Ошибка: название не может быть пустым.",
      validate     = s => s.nonEmpty && s.exists(_.isLetter)
    )(in1)

    val (price, in3) = askBigDecimal(
      prompt       = "Цена (положительное число):",
      errorMessage = "Ошибка: введите положительное число.",
      validate     = _ > 0
    )(in2)

    val (weight, in4) = askDouble(
      prompt       = "Вес в килограммах (положительное число):",
      errorMessage = "Ошибка: введите положительное число.",
      validate     = _ > 0
    )(in3)

    kind match
      case ItemType.Regular =>
        val (cat, in5) = askString(
          prompt       = "Категория товара:",
          errorMessage = "Ошибка: категория не может быть пустой.",
          validate     = _.nonEmpty
        )(in4)
        (RegularItem(name, price, weight, cat), in5)

      case ItemType.Food =>
        val (days, in5) = askInt(
          prompt       = "Срок годности (через сколько дней, целое неотрицательное):",
          errorMessage = "Ошибка: введите целое число 0 или больше.",
          validate     = _ >= 0
        )(in4)
        val exp = LocalDate.now().plusDays(days.toLong)
        (FoodItem(name, price, weight, exp), in5)

      case ItemType.Drink =>
        val (vol, in5) = askDouble(
          prompt       = "Объём в литрах (положительное число):",
          errorMessage = "Ошибка: введите положительное число.",
          validate     = _ > 0
        )(in4)

        val (flag, in6) = askInt(
          prompt       = "Газированный? (1 - да, 0 - нет):",
          errorMessage = "Ошибка: введите 1 или 0.",
          validate     = n => n == 0 || n == 1
        )(in5)

        val carbonated = flag == 1
        (DrinkItem(name, price, weight, vol, carbonated), in6)

  // Чтение списка товаров (ADT Items) рекурсивно, без while/var
  @tailrec
  def readItemList(
                    n: Int, //сколько осталось прочитать
                    in: LazyList[String], //текущий поток строк
                    acc: List[Item] = Nil //уже собранные товары (накапливаем в обратном порядке)
                  ): (List[Item], LazyList[String]) =
    if n <= 0 then (acc.reverse, in)
    else
      val (it, inNext) = readItem(in)
      readItemList(n - 1, inNext, it :: acc)
