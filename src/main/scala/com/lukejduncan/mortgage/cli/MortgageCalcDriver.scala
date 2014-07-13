package com.lukejduncan.mortgage.cli

import com.lukejduncan.mortgage.calc.MortgageCalculator
import com.lukejduncan.mortgage.calc.LoanType
import javax.annotation.Resource
import java.io.PrintWriter
import java.io.File

object MortgageCalcDriver {
  private val toInt: (String) => Int = java.lang.Integer.parseInt
  private val toDouble: (String) => Double = java.lang.Double.parseDouble

  def main(args: Array[String]) = {
    if (args.size != 2) {
      println("Expected: <infile> <outfile>")
      System.exit(0)
    }

    val calc = new MortgageCalculator()

    val inLoc = args(0)
    val outLoc = args(1)

    val inFile = scala.io.Source.fromFile(inLoc)
    val writer = new PrintWriter(new File(outLoc))

    inFile.getLines.foreach { line =>
      val input = line.split(",")

      val months = input(0).toInt
      val homePrice = input(1).toInt
      val downPayment = input(2).toInt
      val rate = input(3).toDouble
      val propertyTaxRate = parseOptional(input(4), toDouble)
      val annualHomeownersInsurance = parseOptional(input(5), toInt)
      val optMonthlyHOA = parseOptional(input(6), toInt)
      val typ = parseType(input(7))

      val t = (s: String) => Double
      val payment = calc.calcMonthlyPayments(months,
        homePrice,
        downPayment,
        rate,
        propertyTaxRate,
        annualHomeownersInsurance,
        optMonthlyHOA,
        typ)

      writer.write(payment.toString + "\n")
    }

    inFile.close
    writer.close
  }

  def parseType(s: String): LoanType.Value = s match {
    case "fixed" => LoanType.Fixed
    case "arm" => LoanType.Arm
    case _ => LoanType.Fixed
  }

  def parseOptional[T](s: String, transformer: (String) => T) = {
    s match {
      case "" => None
      case a => Some(transformer(a))
    }
  }
}