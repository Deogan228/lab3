package shop.model

object PrettyPrint:

  def renderPurchase(p: Purchase): String =
    val header = s"Покупатель: ${p.customer.name}"

    val body = p.roots.flatMap(n => renderNode(n, ""))

    val footer =
      f"ИТОГО: цена = ${p.totalPrice}%.2f, вес = ${p.totalWeight}%.2f кг, объём = ${p.totalVolume}%.2f л"

    (List(header) ++ body ++ List(footer)).mkString("\n")

  private def renderNode(node: Node, indent: String): List[String] =
    node match
      case RegularItem(name, price, weight, category) =>
        List(f"$indent- Regular: $name (категория: $category, цена: $price, вес: $weight%.2f)")

      case FoodItem(name, price, weight, exp) =>
        List(f"$indent- Food: $name (годен до: $exp, цена: $price, вес: $weight%.2f)")

      case DrinkItem(name, price, weight, volume, carbonated) =>
        List(f"$indent- Drink: $name (объём: $volume%.2f л, газ: $carbonated, цена: $price, вес: $weight%.2f)")

      case Box(name, children) =>
        val head = f"$indent+ Box: $name (цена: ${node.totalPrice}%.2f, вес: ${node.totalWeight}%.2f, объём: ${node.totalVolume}%.2f)"
        val kids = children.flatMap(ch => renderNode(ch, indent + "  "))
        head :: kids
