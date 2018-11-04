import WireframeGuiParser.{Button, EditBox, Element, PlainText}

object HtmlRenderer {

  def main(args: Array[String]): Unit = {

    import WireframeGuiParser.Parse.parse

    // todo   Password | "****     "
    // todo   Password | "****     "
    // todo   Password | "****     "
    // todo   Password | "****     "
    // todo   Password | "****     "
    // todo   Password | "****     "

    val startSaltEndSalt =
      """@startsalt
        |{
        |  Login    | "MyName   "
        |  Password | " sok csillag "
        |  [Cancel] | [  OK   ]
        |}
        |@endsalt""".stripMargin

    println()
    println()
    println()
    println()
    println(startSaltEndSalt)
    println()
    println(
      render(
        parse(startSaltEndSalt)
      )
    )

    val saltWireframe =
      """@startuml
        |salt
        |{
        |  Just plain text
        |  [This is my button]
        |  ()  Unchecked radio
        |  (X) Checked radio
        |  []  Unchecked box
        |  [X] Checked box
        |  "Enter text here   "
        |  ^This is a droplist^
        |}
        |@enduml""".stripMargin

    println()
    println()
    println()
    println()
    println(saltWireframe)
    println()
    println(
      render(
        parse(saltWireframe)

      )
    )
  }


  def render(value: List[Element]) = {
    val q = value map {

      case PlainText(s) => s"""<p>$s</p>"""
      case Button(s) => s"""<button>$s</button>"""
      case EditBox(s) => s"""<input placeholder="$s"></input>"""

      case x => "" //x.toString // todo https://guide.udash.io/ext/bootstrap

    }
    q.mkString("<div>\n", "<p>\n", "\n</div>\n")
  }
}