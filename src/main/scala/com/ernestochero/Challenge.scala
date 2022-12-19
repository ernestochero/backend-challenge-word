package com.ernestochero

object Challenge extends App {
  val input =
    "In 1991, while studying computer science at University of Helsinki, Linus Torvalds began a project that later became the Linux kernel. He wrote the program specifically for the hardware he was using and independent of an operating system because he wanted to use the functions of his new PC with an 80386 processor. Development was done on MINIX using the GNU C Compiler."
  def format(input: String, limit: Int): List[String] =
    val processedInput = input.split(" ")
    val output = processedInput
      .foldLeft((List.empty[String], "", 0)) { case ((lines, line, counter), word) =>
        if (counter + word.length >= limit) (lines :+ line, word, word.length)
        else if (line.isEmpty) (lines, word, counter + word.length)
        else (lines, line + " " + word, counter + word.length + 1)
      }
    val (lines, lastLine) = (output._1, output._2)
    lines :+ lastLine
  format(input, 40).foreach(println)
}
