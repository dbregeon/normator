package com.digitalbrikes.normator

import scala.util.{Failure, Success}

case object Amount extends Property
case object PayerId extends Property
case object PayeeId extends Property

case class Party(id : String)
object NoParty extends Party("None")
case class Bill(payer: Party, payee: Party, amount: Double)

case object PayerProperty extends Property
case object PayeeProperty extends Property

class PayerSource extends Source[Party] {
  override def inputProperties: Set[Property] = Set(PayerId)
  override def outputProperty: Property = PayerProperty.asInstanceOf[Property]

  override def resolve(inputs: Set[PropertyValue[_]]): PropertyValue[Party] = inputs.find(p => p.property == PayerId).get.value match {
    case Success(value) => PropertyValue(outputProperty, Success(Party(value.asInstanceOf[String])))
    case Failure(error) => PropertyValue(outputProperty, Failure(MissingInputException("PayerId is missing")))
  }
}

class PayeeSource extends Source[Party] {
  override def inputProperties: Set[Property] = Set(PayeeId)
  override def outputProperty: Property = PayeeProperty.asInstanceOf[Property]

  override def resolve(inputs: Set[PropertyValue[_]]): PropertyValue[Party] = PropertyValue(outputProperty, Success(Party(inputs.find(p => p.property == PayeeId).get.value.get.asInstanceOf[String])))
}

case object AmountNormalizer extends Normalizer[Double] {
  def outputProperty = Amount

  override def normalize(value: PropertyValue[Double]): PropertyOutput[Double] =  PropertyOutput(outputProperty, value.value.toOption, "Validated!")
}

case object PayerIdNormalizer extends Normalizer[String] {
  def outputProperty = PayerId

  override def normalize(value: PropertyValue[String]): PropertyOutput[String] =  PropertyOutput(outputProperty, value.value.toOption, "Validated!")
}

case object PayerNormalizer extends Normalizer[Party] {
  def outputProperty = PayerProperty

  override def normalize(value: PropertyValue[Party]): PropertyOutput[Party] =  PropertyOutput(outputProperty, value.value.toOption, "Validated!")
}

case object PayeeIdNormalizer extends Normalizer[String] {
  def outputProperty = PayeeId

  override def normalize(value: PropertyValue[String]): PropertyOutput[String] =   PropertyOutput(outputProperty, value.value.toOption, "Validated!")
}

case object PayeeNormalizer extends Normalizer[Party] {
  def outputProperty = PayeeProperty

  override def normalize(value: PropertyValue[Party]): PropertyOutput[Party] =   PropertyOutput(outputProperty, value.value.toOption, "Validated!")
}



class CreateBillActivity extends Activity[Bill] {
  val resolvers : Set[Source[_]] = Set(new PayerSource)

  val normalizers : Set[Normalizer[_]] = Set(AmountNormalizer, PayerNormalizer, PayeeNormalizer)

  val output: Set[Property] = Set(PayerProperty, PayeeProperty, Amount)

  override def materialize() = ???


//  override def update(inputs: Set[PropertyInput[_]]): Set[PropertyOutput[_]] = {
//    val values = inputs.map(i => i.source.resolve(Set.empty))
//    val outProperties
//
//    val valuesMap : Map[Property, PropertyValue[_]] = sources.map(s => (s.outputProperty, s.resolve(values))).toMap
//    normalizers.map(n => n.normalize(valuesMap(n.property).asInstanceOf))
//  }
}