package shop.model

import java.time.LocalDate

// Узел дерева покупки: либо товар, либо коробка с узлами
sealed trait Node:
  def totalPrice: BigDecimal
  def totalWeight: Double
  def totalVolume: Double

// Лист дерева: конкретный товар
sealed trait Item extends Node:
  def name: String
  def price: BigDecimal
  def weight: Double

  final override def totalPrice: BigDecimal = price
  final override def totalWeight: Double    = weight
  override def totalVolume: Double          = 0.0

final case class RegularItem(
                              name: String,
                              price: BigDecimal,
                              weight: Double,
                              category: String
                            ) extends Item

final case class FoodItem(
                           name: String,
                           price: BigDecimal,
                           weight: Double,
                           expirationDate: LocalDate
                         ) extends Item

final case class DrinkItem(
                            name: String,
                            price: BigDecimal,
                            weight: Double,
                            volume: Double,
                            isCarbonated: Boolean
                          ) extends Item:
  override def totalVolume: Double = volume

final case class Box(
                      name: String,
                      children: List[Node]
                    ) extends Node:
  override def totalPrice: BigDecimal =
    children.foldLeft(BigDecimal(0))((acc, n) => acc + n.totalPrice)

  override def totalWeight: Double =
    children.foldLeft(0.0)((acc, n) => acc + n.totalWeight)

  override def totalVolume: Double =
    children.foldLeft(0.0)((acc, n) => acc + n.totalVolume)

final case class Customer(name: String)
