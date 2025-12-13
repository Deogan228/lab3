package shop.model

final case class Purchase(
                           customer: Customer,
                           roots: List[Node]
                         ):
  def totalPrice: BigDecimal =
    roots.foldLeft(BigDecimal(0))((acc, n) => acc + n.totalPrice)

  def totalWeight: Double =
    roots.foldLeft(0.0)((acc, n) => acc + n.totalWeight)

  def totalVolume: Double =
    roots.foldLeft(0.0)((acc, n) => acc + n.totalVolume)
