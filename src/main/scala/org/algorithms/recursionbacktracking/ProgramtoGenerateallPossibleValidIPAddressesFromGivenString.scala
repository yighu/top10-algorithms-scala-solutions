package org.algorithms.recursionbacktracking

object ProgramtoGenerateallPossibleValidIPAddressesFromGivenString extends App {
  /**
   * https://www.geeksforgeeks.org/program-generate-possible-valid-ip-addresses-given-string/
   * Program to generate all possible valid IP addresses from given string
   *
   * Given a string containing only digits, restore it by returning all possible valid IP address combinations.
     A valid IP address must be in the form of A.B.C.D, where A, B, C, and D are numbers from 0-255. The numbers
     cannot be 0 prefixed unless they are 0.


   */

    def restoreIPAddresses(s: String): List[String] = {
      def backtrack(start: Int, parts: List[String]): List[String] = {
        if (start == s.length && parts.length == 4) {
          parts.reverse.mkString("::") :: Nil
        } else if (start < s.length && parts.length < 4) {
          (1 to 3).flatMap(len => {
            val end = start + len
            if (end <= s.length) {
              val segment = s.substring(start, end)
              if ((segment.length == 1 || segment.head != '0') && segment.toInt <= 255) {
                backtrack(end, segment :: parts)
              } else if (segment == "0") {
                backtrack(end, segment :: parts)
              } else {
                Nil
              }
            } else {
              Nil
            }
          }).toList
        } else {
          Nil
        }
      }

      backtrack(0, Nil)
    }


      val input = "25525511135"
      restoreIPAddresses(input) foreach println

}
