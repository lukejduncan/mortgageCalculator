package unit

import org.specs2.matcher.ShouldMatchers
import org.specs2.mutable._
import unit.DoublePrecision._
import com.lukejduncan.mortgage.LoanType
import com.lukejduncan.mortgage.MortgageCalculator

object TestMortgageCalculator extends Specification with ShouldMatchers {
  implicit val precision = Precision(0.5)
  def million(n: Int) = n * 1000000
  def thousand(n: Int) = n * 1000
  def years(n: Int) = n * 12

  val calculator = new MortgageCalculator()

  "Mortgage Calculator" should {
    "Calculate with PMI for Fixed" in {
      val term = years(30)
      val homePrice = million(1)
      val downPayment = thousand(160)
      val rate = 4
      val propertyTaxRate = Some(1.2)
      val annualHomeownersInsurance = Some(800)
      val optMonthlyHOA = Some(400)
      val typ = LoanType.Fixed

      val payments = calculator.calcMonthlyPayments(term,
        homePrice,
        downPayment,
        rate,
        propertyTaxRate,
        annualHomeownersInsurance,
        optMonthlyHOA,
        typ)

      payments.monthlyPayments == 5784
    }

    "Calculate with PMI for Arm" in {
      val term = years(30)
      val homePrice = million(1)
      val downPayment = thousand(160)
      val rate = 4
      val propertyTaxRate = Some(1.2)
      val annualHomeownersInsurance = Some(800)
      val optMonthlyHOA = Some(400)
      val typ = LoanType.Arm

      val payments = calculator.calcMonthlyPayments(term,
        homePrice,
        downPayment,
        rate,
        propertyTaxRate,
        annualHomeownersInsurance,
        optMonthlyHOA,
        typ)

      payments.monthlyPayments == 5952
    }

    "Know required income" in {
      val term = years(30)
      val homePrice = million(1)
      val downPayment = thousand(160)
      val rate = 4
      val propertyTaxRate = Some(1.2)
      val annualHomeownersInsurance = Some(800)
      val optMonthlyHOA = Some(400)
      val typ = LoanType.Fixed

      val payments = calculator.calcMonthlyPayments(term,
        homePrice,
        downPayment,
        rate,
        propertyTaxRate,
        annualHomeownersInsurance,
        optMonthlyHOA,
        typ)

      calculator.requiredIncome(payments, 30) == 231360
    }
  }

  "PI Calculations" should {
    "Be accurate" in {
      calculator.calcMonthlyPI(thousand(800), 4.0, 30 * 12) ~= 3819
    }
  }

  "PMI Calculations" should {
    "know PMI is 0 for Fixed with < 80% loan" in {
      calculator.calcPMI(thousand(70), million(1), LoanType.Arm) ~= 0
    }

    "know PMI for Fixed with 80% loan" in {
      calculator.calcPMI(thousand(800), million(1), LoanType.Arm) ~= 0.0
    }

    "know PMI for Fixed with 85% loan" in {
      calculator.calcPMI(thousand(850), million(1), LoanType.Fixed) ~= 312
    }

    "know PMI for Fixed with 90% loan" in {
      calculator.calcPMI(thousand(900), million(1), LoanType.Fixed) ~= 443
    }

    "know PMI for Fixed with 95% loan" in {
      calculator.calcPMI(thousand(900), million(1), LoanType.Fixed) ~= 443
    }

    "know PMI for Fixed with 100% loan" in {
      calculator.calcPMI(million(1), million(1), LoanType.Fixed) ~= 817
    }

    "know PMI for ARM with any percent loan" in {
      calculator.calcPMI(thousand(850), million(1), LoanType.Arm) ~= 482
    }
  }
}