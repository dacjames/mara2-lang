package io.dac.mara


/**
  * Created by dcollins on 8/2/16.
  */
object CLI extends MaraLanguage with App {
  def debug(text: String) = s"${show(text)} => ${eval(text)}"

  println(debug("3^(7*4)<=3^7*4||1&&1<=1"))
  println(debug("3+~1+1"))
  println(debug("if1{3}"))
  println(debug("if0{3}else{4}"))
  println(debug("0&&1"))
  println(debug("0~&1"))
  println(debug("0@1$2%3^4^5%6$7@8"))
  println(debug("valx:Int"))
  println(debug("valx:Int=10"))
  println(debug("x"))
  println(debug("do{valx:Int=20;x}"))
  println(debug("do{valx=30;x}"))
  println(debug("___T"))
}
