package com.digitalbrikes.normator

import com.digitalbrikes.normator.AmountNormalizer.outputProperty
import com.digitalbrikes.normator.PayerIdNormalizer.outputProperty
import com.digitalbrikes.normator.PayerNormalizer.outputProperty

case object Amount extends Property
case object PayerId extends Property
case object PayeeId extends Property

class Party(id : String)
object NoParty extends Party("None")
class Bill(payer: Party, payee: Party, amount: Double)

case object PayerProperty extends Property
case object PayeeProperty extends Property

class PayerSource extends Source[Party] {
  override def inputProperties: Set[Property] = Set(PayerId)
  override def outputProperty: Property = PayerProperty.asInstanceOf[Property]

  override def resolve(inputs: Set[PropertyValue[_]]): PropertyValue[Party] = PropertyValue(outputProperty, new Party(inputs.find(p => p.property == PayerId).get.value.asInstanceOf[String]))
}

class PayeeSource extends Source[Party] {
  override def inputProperties: Set[Property] = Set(PayeeId)
  override def outputProperty: Property = PayeeProperty.asInstanceOf[Property]

  override def resolve(inputs: Set[PropertyValue[_]]): PropertyValue[Party] = PropertyValue(outputProperty, new Party(inputs.find(p => p.property == PayeeId).get.value.asInstanceOf[String]))
}

case object AmountNormalizer extends Normalizer[Double] {
  def outputProperty = Amount

  override def normalize(value: Option[PropertyValue[Double]]): PropertyOutput[Double] =  PropertyOutput(outputProperty, value.map(v => v.value).getOrElse(0.0), "Validated!")
}

case object PayerIdNormalizer extends Normalizer[String] {
  def outputProperty = PayerId

  override def normalize(value: Option[PropertyValue[String]]): PropertyOutput[String] =  PropertyOutput(outputProperty, value.map(v => v.value).getOrElse(""), "Validated!")
}

case object PayerNormalizer extends Normalizer[Party] {
  def outputProperty = PayerProperty

  override def normalize(value: Option[PropertyValue[Party]]): PropertyOutput[Party] =  PropertyOutput(outputProperty, value.map(v => v.value).getOrElse(NoParty), "Validated!")
}

case object PayeeIdNormalizer extends Normalizer[String] {
  def outputProperty = PayeeId

  override def normalize(value: Option[PropertyValue[String]]): PropertyOutput[String] =   PropertyOutput(outputProperty, value.map(v => v.value).getOrElse(""), "Validated!")
}

case object PayeeNormalizer extends Normalizer[Party] {
  def outputProperty = PayeeProperty

  override def normalize(value: Option[PropertyValue[Party]]): PropertyOutput[Party] =   PropertyOutput(outputProperty, value.map(v => v.value).getOrElse(NoParty), "Validated!")
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