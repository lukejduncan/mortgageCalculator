package com.lukejduncan.mortgage.calc

/**
 * @author lukejduncan
 */
class MortgageCalculator {
  private val fixedPMIRates: Map[Int, Double] = Map(80 -> 0,
    85 -> 0.0044,
    90 -> 0.0059,
    95 -> 0.0076,
    100 -> 0.0098)
  private val armPMIRates: Map[Int, Double] = Map(80 -> 0,
    85 -> 0.0068,
    90 -> 0.0081,
    95 -> 0.0101,
    100 -> 0.0117)

  def requiredIncome(payment: HomePayment, dti: Int) = {
    (payment.monthlyPayments / (dti / 100.0)) * 12;
  }

  def calcMonthlyPayments(months: Int,
    homePrice: Int,
    downPayment: Int,
    rate: Double,
    propertyTaxRate: Option[Double],
    annualHomeownersInsurance: Option[Int],
    optMonthlyHOA: Option[Int],
    typ: LoanType.Value): HomePayment = {

    val principal = homePrice - downPayment
    val monthlyPI = calcMonthlyPI(principal, rate, months)
    val monthlyHOA = optMonthlyHOA.getOrElse(0)
    val monthlyInsurance = annualHomeownersInsurance match { case None => 0 case Some(x) => x / 12 }
    val monthlyPropertyTax = propertyTaxRate match { case None => 0 case Some(x) => ((x / 100.0) * homePrice) / 12 }
    val monthlyPMI = calcPMI(principal, homePrice, typ)

    HomePayment(monthlyPI.toInt,
      monthlyPropertyTax.toInt,
      monthlyInsurance,
      monthlyPMI.toInt,
      monthlyHOA)
  }

  /**
   * Equation similar to ehow's
   * How to calculate a month interest rate.
   *
   * http://www.ehow.com/how_6498488_do-calculate-monthly-loan-payment_.html
   */
  def calcMonthlyPI(principal: Int, rate: Double, term: Int): Double = {
    val dailyRate = rate match {
      case 0 => principal / term
      case _ => {

        val monthlyRate = (rate / 1200.0)
        val adjustedRate = Math.pow(1 + monthlyRate, term)
        val dividedRate = adjustedRate / (adjustedRate - 1)
        dividedRate * monthlyRate
      }
    }

    principal * dailyRate // Money or two decimals?
  }

  def calcPMI(principal: Int, homePrice: Int, typ: LoanType.Value) = {
    val percentBorrowed = principal.toDouble / homePrice.toDouble * 100
    val rates = typ match {
      case LoanType.Fixed => fixedPMIRates
      case LoanType.Arm => armPMIRates
    }

    val percentBorrowedBucket = percentBorrowed match {
      case x if x <= 80 => 80
      case x if x <= 85 => 85
      case x if x <= 90 => 90
      case x if x <= 95 => 95
      case x if x <= 100 => 100
    }

    val pmiRate = rates(percentBorrowedBucket)
    (pmiRate * principal) / 12
  }

  def calMonthlyPropertyTaxes(homePrice: Int, taxRate: Double): Double = (homePrice * taxRate) / 12
}