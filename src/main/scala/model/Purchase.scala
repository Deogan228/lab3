package shop.model

final case class Purchase(
                           customer: Customer, //кто покупает
                           items: List[Item] //список купленных товаров
                         ):

  // считает общую стоимость покупки
  def totalPrice: BigDecimal =
    items.foldLeft(BigDecimal(0))((acc, it) => acc + it.price)

  // общий вес
  def totalWeight: Double =
    items.foldLeft(0.0)((acc, it) => acc + it.weight)

  // общий объём только напитков
  def totalVolume: Double =
    items.foldLeft(0.0) {
      case (acc, DrinkItem(_, _, _, volume, _)) => acc + volume
      case (acc, _)                             => acc
    }

  override def toString: String =
    val lines = items.map { it =>
      f"${it.name}%-15s  цена: ${it.price}%-7s  вес: ${it.weight}%.2f кг"
    }
    (s"Покупатель: ${customer.name}" :: lines ::: List(
      f"ИТОГО: цена = $totalPrice%.2f, вес = $totalWeight%.2f кг"
    )).mkString("\n")
