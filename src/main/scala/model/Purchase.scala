package shop.model

final case class Purchase(
                           customer: Customer,
                           nodes: List[Node]   // было: List[Item]
                         ):

  def totalPrice: BigDecimal =
    nodes.foldLeft(BigDecimal(0))((acc, n) => acc + n.totalPrice)

  def totalWeight: Double =
    nodes.foldLeft(0.0)((acc, n) => acc + n.totalWeight)

  def totalVolume: Double =
    nodes.foldLeft(0.0) {
      case (acc, DrinkItem(_, _, _, volume, _)) => acc + volume
      case (acc, Box(_, children))              => // если хотим по коробкам ходить
        acc + children.foldLeft(0.0) {
          case (a, d: DrinkItem)   => a + d.volume
          case (a, b: Box)         => a + b.totalVolume   // если сделать метод
          case (a, _)              => a
        }
      case (acc, _) => acc
    }

  override def toString: String =
    def render(node: Node, indent: String = ""): List[String] =
      node match
        case r: RegularItem =>
          List(s"$indent- Обычный товар: ${r.name}, цена: ${r.price}, вес: ${r.weight}")
        case f: FoodItem =>
          List(s"$indent- Продукт: ${f.name}, цена: ${f.price}, вес: ${f.weight}, годен до: ${f.expirationDate}")
        case d: DrinkItem =>
          List(s"$indent- Напиток: ${d.name}, цена: ${d.price}, вес: ${d.weight}, объём: ${d.volume}, газированный: ${d.isCarbonated}")
        case Box(name, children) =>
          val header = s"$indent+ Коробка: $name (итоговая цена: ${node.totalPrice}, вес: ${node.totalWeight})"
          val childs = children.flatMap(ch => render(ch, indent + "  "))
          header :: childs

    val header = s"Покупатель: ${customer.name}"
    val body   = nodes.flatMap(n => render(n))
    val footer = f"ИТОГО: цена = $totalPrice%.2f, вес = $totalWeight%.2f кг"
    (header :: body) :+ footer mkString "\n"
