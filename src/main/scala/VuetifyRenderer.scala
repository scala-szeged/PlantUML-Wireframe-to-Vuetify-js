import WireframeGuiParser._

object VuetifyRenderer {

  def main(args: Array[String]): Unit = {

    import WireframeGuiParser.Parse.parse

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

    val login =
      """@startsalt
        |{
        |  plain text
        |  "Login   "
        |  "Password"
        |  [Cancel] | [  OK   ]
        |}
        |@endsalt""".stripMargin

    println()
    println()
    println()
    println()
    println(login)
    println()
    println(
      render(
        parse(login)
      )
    )
  }


  def render(value: List[Element]) = {
    "<v-card-text>\n<v-form>\n" +
      renderInside(value) +
      "\n</v-form>\n</v-card-text>\n"
  }

  def renderInside(value: List[Element]): String = {
    val q = value map {

      case ParentElement(children) =>
        "\n<v-content>\n" + renderInside(children) + "\n</v-content>\n"

      case PlainText(s) => s"""<v-toolbar dark color="primary">
<v-toolbar-title>$s</v-toolbar-title>
</v-toolbar>"""

      case Button(s) => s"""<v-btn color="primary">$s</v-btn>"""
      case EditBox(s) => s"""<v-text-field name="a-name" label="$s"></v-text-field>"""

      case x => "" //x.toString

    }
    q.mkString("\n")
  }
}