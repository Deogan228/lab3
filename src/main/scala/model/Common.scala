package shop.model

// Общее свойство "есть цена"
trait HasPrice:
  def price: BigDecimal

// Общее свойство "есть вес"
trait HasWeight:
  def weight: Double
