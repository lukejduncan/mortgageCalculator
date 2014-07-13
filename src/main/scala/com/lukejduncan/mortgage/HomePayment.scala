package com.lukejduncan.mortgage

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
}