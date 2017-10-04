package com.digitalbrikes.normator

import scala.util.{Failure, Success}

case object Amount extends Property
case object PayerId extends Property
case object PayeeId extends Property
case object BillProperty extends Property

case class Party(id : String)
object NoParty extends Party("None")
case class Bill(payer: Party, payee: Party, amount: Double)

case object PayerProperty extends Property
case object PayeeProperty extends Property

class BillContext

class PayerSource extends Source[Party, BillContext] {
  override def inputProperties: Set[Property] = Set(PayerId)
  override def outputProperty: Property = PayerProperty.asInstanceOf[Property]

  override def resolve(inputs: Set[PropertyValue[_]])(implicit context : BillContext): PropertyValue[Party] = inputs.find(p => p.property == PayerId).get.value match {
    case Success(value) => PropertyValue(outputProperty, Success(Party(value.asInstanceOf[String])))
    case Failure(error) => PropertyValue(outputProperty, Failure(MissingInputException("PayerId is missing")))
  }
}

class PayeeSource extends Source[Party, BillContext] {
  override def inputProperties: Set[Property] = Set(PayeeId)
  override def outputProperty: Property = PayeeProperty.asInstanceOf[Property]

  override def resolve(inputs: Set[PropertyValue[_]])(implicit context : BillContext): PropertyValue[Party] = PropertyValue(outputProperty, Success(Party(inputs.find(p => p.property == PayeeId).get.value.get.asInstanceOf[String])))
}

class BillSource extends Source[Bill, BillContext] {
    override def inputProperties: Set[Property] = Set(PayerProperty, PayeeProperty, Amount)
    override def outputProperty: Property = BillProperty.asInstanceOf[Property]

    override def resolve(inputs: Set[PropertyValue[_]])(implicit context : BillContext): PropertyValue[Bill] = PropertyValue(
      outputProperty,
      Success(Bill(
        inputs.find(p => p.property == PayerProperty).get.value.get.asInstanceOf[Party],
        inputs.find(p => p.property == PayeeProperty).get.value.get.asInstanceOf[Party],
        inputs.find(p => p.property == Amount).get.value.get.asInstanceOf[Double]
      )))
}

case object AmountNormalizer extends Normalizer[Double, BillContext] {
  def outputProperty: Amount.type = Amount

  override def normalize(value: PropertyValue[Double])(implicit context : BillContext): PropertyOutput[Double] =  PropertyOutput(outputProperty, value.value.toOption, "Validated!")
}

case object PayerIdNormalizer extends Normalizer[String, BillContext] {
  def outputProperty: PayerId.type = PayerId

  override def normalize(value: PropertyValue[String])(implicit context : BillContext): PropertyOutput[String] =  PropertyOutput(outputProperty, value.value.toOption, "Validated!")
}

case object PayerNormalizer extends Normalizer[Party, BillContext] {
  def outputProperty: PayerProperty.type = PayerProperty

  override def normalize(value: PropertyValue[Party])(implicit context : BillContext): PropertyOutput[Party] =  PropertyOutput(outputProperty, value.value.toOption, "Validated!")
}

case object PayeeIdNormalizer extends Normalizer[String, BillContext] {
  def outputProperty: PayeeId.type = PayeeId

  override def normalize(value: PropertyValue[String])(implicit context : BillContext): PropertyOutput[String] =   PropertyOutput(outputProperty, value.value.toOption, "Validated!")
}

case object PayeeNormalizer extends Normalizer[Party, BillContext] {
  def outputProperty: PayeeProperty.type = PayeeProperty

  override def normalize(value: PropertyValue[Party])(implicit context : BillContext): PropertyOutput[Party] =   PropertyOutput(outputProperty, value.value.toOption, "Validated!")
}

case object BillNormalizer extends Normalizer[Bill, BillContext] {
  def outputProperty: BillProperty.type = BillProperty

  override def normalize(value: PropertyValue[Bill])(implicit context : BillContext): PropertyOutput[Bill] =   PropertyOutput(outputProperty, value.value.toOption, "Validated!")
}


class CreateBillActivity extends Activity[Bill, BillContext] {
  val resolvers : Set[Source[_, BillContext]] = Set(new PayerSource, new PayeeSource, new BillSource)

  val normalizers : Set[Normalizer[_, BillContext]] = Set(BillNormalizer, PayerNormalizer, PayeeNormalizer)
}