package shop.io

import scala.annotation.tailrec
import scala.util.Try
import java.time.LocalDate

import shop.model.*

object ConsoleAsk:
  
  type SResult[A] = (A, LazyList[String])

  private type Parser[A]    = String => Option[A]
  private type Validator[A] = A => Boolean

  //базовые парсеры 
  private val parseInt: Parser[Int] =
    s => Try(s.toInt).toOption

  private val parseDouble: Parser[Double] =
    s => Try(s.toDouble).toOption

  private val parseDecimal: Parser[BigDecimal] =
    s => Try(BigDecimal(s)).toOption

  private val parseString: Parser[String] =
    s => Option(s).map(_.trim)

  //универсальный safe input 
  private def ask[A](
                      prompt: String,
                      parser: Parser[A],
                      error: String,
                      validate: Validator[A] = (_: A) => true
                    )(in: LazyList[String]): SResult[A] =

    @tailrec
    def loop(stream: LazyList[String]): SResult[A] =
      println(prompt)
      stream match
        case h #:: t =>
          val raw = h.trim
          parser(raw) match
            case Some(v) if validate(v) => (v, t)
            case _ =>
              println(error)
              loop(t)
        case _ =>
          throw new RuntimeException("Неожиданный конец ввода")

    loop(in)

  //удобные обёртки
  def askName(prompt: String)(in: LazyList[String]): SResult[String] =
    ask[String](
      prompt,
      parseString,
      "Ошибка: введите корректное имя (не пусто, есть буква).",
      s => s.nonEmpty && s.exists(_.isLetter)
    )(in)

  def askNonEmpty(prompt: String, field: String)(in: LazyList[String]): SResult[String] =
    ask[String](
      prompt,
      parseString,
      s"Ошибка: поле '$field' не может быть пустым.",
      _.nonEmpty
    )(in)

  def askPositiveInt(prompt: String)(in: LazyList[String]): SResult[Int] =
    ask[Int](
      prompt,
      parseInt,
      "Ошибка: введите целое число > 0.",
      _ > 0
    )(in)

  def askNonNegativeInt(prompt: String)(in: LazyList[String]): SResult[Int] =
    ask[Int](
      prompt,
      parseInt,
      "Ошибка: введите целое число >= 0.",
      _ >= 0
    )(in)

  def askPositiveDouble(prompt: String)(in: LazyList[String]): SResult[Double] =
    ask[Double](
      prompt,
      parseDouble,
      "Ошибка: введите число > 0.",
      _ > 0
    )(in)

  def askPositiveDecimal(prompt: String)(in: LazyList[String]): SResult[BigDecimal] =
    ask[BigDecimal](
      prompt,
      parseDecimal,
      "Ошибка: введите число > 0.",
      _ > 0
    )(in)

  def askYesNo(prompt: String)(in: LazyList[String]): SResult[Boolean] =
    val (x, rest) = ask[Int](
      prompt + " (1 - да, 0 - нет)",
      parseInt,
      "Ошибка: введите 1 или 0.",
      n => n == 0 || n == 1
    )(in)
    (x == 1, rest)

  //выбор типа узла
  enum NodeKind:
    case Regular, Food, Drink, Box

  private def askNodeKind(in: LazyList[String]): SResult[NodeKind] =
    val (k, rest) = ask[Int](
      "Выберите тип: 1-обычный товар, 2-продукт, 3-напиток, 4-коробка",
      parseInt,
      "Ошибка: введите 1/2/3/4.",
      n => n >= 1 && n <= 4
    )(in)

    val kind = k match
      case 2 => NodeKind.Food
      case 3 => NodeKind.Drink
      case 4 => NodeKind.Box
      case _ => NodeKind.Regular

    (kind, rest)

  //чтение одного Node
  def readNode(in: LazyList[String]): SResult[Node] =
    val (kind, in1) = askNodeKind(in)
    kind match
      case NodeKind.Regular => readRegular(in1)
      case NodeKind.Food    => readFood(in1)
      case NodeKind.Drink   => readDrink(in1)
      case NodeKind.Box     => readBox(in1)

  //чтение конкретных вариантов
  private def readCommonItemFields(in: LazyList[String]): SResult[(String, BigDecimal, Double)] =
    val (name, in1)   = askNonEmpty("Название товара:", "name")(in)
    val (price, in2)  = askPositiveDecimal("Цена:")(in1)
    val (weight, in3) = askPositiveDouble("Вес (кг):")(in2)
    ((name, price, weight), in3)

  private def readRegular(in: LazyList[String]): SResult[Node] =
    val ((name, price, weight), in1) = readCommonItemFields(in)
    val (cat, in2) = askNonEmpty("Категория:", "category")(in1)
    (RegularItem(name, price, weight, cat), in2)

  private def readFood(in: LazyList[String]): SResult[Node] =
    val ((name, price, weight), in1) = readCommonItemFields(in)
    val (days, in2) = askNonNegativeInt("Через сколько дней истечёт срок годности?")(in1)
    val exp = LocalDate.now().plusDays(days.toLong)
    (FoodItem(name, price, weight, exp), in2)

  private def readDrink(in: LazyList[String]): SResult[Node] =
    val ((name, price, weight), in1) = readCommonItemFields(in)
    val (vol, in2) = askPositiveDouble("Объём (литры):")(in1)
    val (gaz, in3) = askYesNo("Газированный?")(in2)
    (DrinkItem(name, price, weight, vol, gaz), in3)

  private def readBox(in: LazyList[String]): SResult[Node] =
    val (boxName, in1) = askNonEmpty("Название коробки:", "box name")(in)
    val (n, in2) = askNonNegativeInt("Сколько элементов внутри коробки?")(in1)
    val (children, in3) = readNodeList(n, in2)
    (Box(boxName, children), in3)

  //чтение списка Node
  @tailrec
  def readNodeList(n: Int, in: LazyList[String], acc: List[Node] = Nil): SResult[List[Node]] =
    if n <= 0 then (acc.reverse, in)
    else
      val (node, inNext) = readNode(in)
      readNodeList(n - 1, inNext, node :: acc)
