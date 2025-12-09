package shop.model

trait HasPrice:
  def price: BigDecimal

trait HasWeight:
  def weight: Double
