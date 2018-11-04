import scala.util.parsing.combinator.JavaTokenParsers


object WireframeGuiParser {


  sealed trait Element

  case class ParentElement(children: List[Element]) extends Element

  case class PlainText(s: String) extends Element

  case class Button(s: String) extends Element

  case class UncheckedRadio(s: String) extends Element

  case class CheckedRadio(s: String) extends Element

  case class UncheckedBox(s: String) extends Element

  case class CheckedBox(s: String) extends Element

  case class EditBox(s: String) extends Element

  case class DropList(s: String) extends Element // todo how to handle the possible list items with PlantUML Salt?


  object Parse extends JavaTokenParsers {

    override protected val whiteSpace = """[\t ]+""".r


    def parse(text: String) = {
      val result = parseAll(saltUml, text)
      result match {
        case Success(succesfulResult, _) =>
          succesfulResult

        case _ =>
          System.err.println(result)
          List.empty
      }
    }

    def saltUml = saltUml1 | saltUml2 // -
    def saltUml1 = "@startuml" ~ nl ~ "salt" ~ nl ~ "{" ~ nl ~> inside <~ nl ~ "}" ~ nl ~ "@enduml" // -
    def saltUml2 = "@startsalt" ~ nl ~ "{" ~ nl ~> inside <~ nl ~ "}" ~ nl ~ "@endsalt"

    def someNewLine = """\n+""".r

    def nl = someNewLine

    def inside = rep1sep(parentElement | element, nl)

    def parentElement = element ~ "|" ~ rep1sep(element, "|") ^^ {
      case element ~ pipe ~ elementList => ParentElement(element :: elementList)
    }

    def element =
      checkedRadio | uncheckedRadio | checkedBox | uncheckedBox | button | editBox | dropList | plainText

    def wordList: Parser[List[String]] = rep1("""\w+""".r)

    def checkedRadio: Parser[CheckedRadio] = "(X)" ~> wordList ^^ (wordList => CheckedRadio(wordList.mkString(" "))) // -
    def uncheckedRadio: Parser[UncheckedRadio] = "()" ~> wordList ^^ (wordList => UncheckedRadio(wordList.mkString(" ")))

    def checkedBox: Parser[CheckedBox] = "[X]" ~> wordList ^^ (wordList => CheckedBox(wordList.mkString(" "))) // -
    def uncheckedBox: Parser[UncheckedBox] = "[]" ~> wordList ^^ (wordList => UncheckedBox(wordList.mkString(" ")))

    def button: Parser[Button] = "[" ~> wordList <~ "]" ^^ (wordList => Button(wordList.mkString(" ")))

    def editBox: Parser[EditBox] = "\"" ~> wordList <~ "\"" ^^ (wordList => EditBox(wordList.mkString(" "))) // todo szóköz kezelése, lásd "Enter text here   "

    def dropList: Parser[DropList] = "^" ~> wordList <~ "^" ^^ (wordList => DropList(wordList.mkString(" ")))

    def plainText: Parser[PlainText] = wordList ^^ (wordList => PlainText(wordList.mkString(" ")))


  }

}