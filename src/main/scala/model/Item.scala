package shop.model

import java.time.LocalDate

// Алгебраический тип данных: один sealed trait + фиксированный набор case class

sealed trait Item extends HasPrice, HasWeight:
  def name: String
  def price: BigDecimal
  def weight: Double

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
                            volume: Double,          // литры
                            isCarbonated: Boolean
                          ) extends Item

final case class Customer(name: String) //модель покупателя
