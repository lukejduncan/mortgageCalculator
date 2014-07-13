package com.lukejduncan.mortgage.calc

case class HomePayment(
  val monthlyPI: Int,
  val monthlyPropertyTax: Int,
  val monthlyInsurance: Int,
  val monthlyPMI: Int,
  val monthlyHOA: Int) {

  def monthlyPayments = monthlyPI +
    monthlyPropertyTax +
    monthlyInsurance +
    monthlyPMI +
    monthlyHOA

  override def toString() = csv(List(monthlyPI, monthlyPropertyTax, monthlyInsurance, monthlyPMI, monthlyHOA, monthlyPayments))
  private def csv(l: List[Int]) = l.foldRight("")((left, right) => left + "," + right)
}