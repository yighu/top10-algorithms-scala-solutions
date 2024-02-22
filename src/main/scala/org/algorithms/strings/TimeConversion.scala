package org.algorithms.strings

object TimeConversion extends App {
  /**
   * 0<=hour<12 and "AM"   -> print it
   * 0<=HOUR<12 and "PM"   -> add 12 to hour and print
   * hour = 12  and "AM"   -> make hour to "00" and print
   * hour = 12  and "PM"   -> print
   *
   */

  def timeConversion(s: String): String = {
    val str = s.split(":")
    val hour = str(0).toInt
    val min = str(1)
    val secPeriod = str(2)
    val sec = str(2).substring(0, secPeriod.length-2)
    val period = str(2).substring(secPeriod.length -2, secPeriod.length)
    if ((0 <= hour && hour <12) && period=="AM") String.format("%0d", hour)+":"+min+":"+sec
    else if((0<=hour && hour <12) && period == "PM") (12+hour)+":"+min+":"+sec
    else if(hour==12 && period =="AM") "00"+":"+min+":"+sec
    else if (hour==12 && period=="PM") hour+":"+min+":"+sec
    else ""
  }
  println(timeConversion("07:05:45PM"))

}
