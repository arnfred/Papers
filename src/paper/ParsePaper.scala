package paper

import scala.collection.immutable.Stream
import scala.util.parsing.input._

object ParsePaper {

  val input = StreamReader(new java.io.InputStreamReader(System.in))

  def getText(in: StreamReader): Stream[Char] = in.atEnd match {
    case true    => Stream.Empty
    case false   => in.first #:: getText(in.drop(1))
  }

  def text = getText(input)

  //def isitTitle(line : String) : Title  = Title(line)
  //def getLine(s: Stream) : Stream = (s.takeWhile(_!='\n').mkString)
  //def slice(start: String, end: String)

  //def parseISIT(text: Stream) : Paper = {
  //  // Get input at hand
  //  var text = getText(input)

  //  // Get rid of the heading which is the same for all of the papers
  //  text = test.dropWhile(_!='\n')

  //  // Then fetch title
  //  var title = Title(getTitle(text))
  //}






  def main(args: Array[String]): Unit = {

    // Match text
    (dropLines(len(100)))(text) match {
      case Success(result, rest)    => println(result.mkString)
      case Failure(msg, rest)       => println(msg)
    }
    //parser(tokens) match {
    //  case Success(result, _) => println(result)
    //  case e =>  println(e)
    //}    
  }




  type Input    = Stream[Char]
  sealed abstract class Result[+T]
  case class ~ [+T, +U](r1 : T, r2 : U)
  case class Success[T](result: T, in : Input) extends Result[T]
  case class Failure(msg : String, in : Input) extends Result[Nothing]

  abstract class Parser[+T] extends (Input => Result[T]) {
    p =>

    def ~ [U](q: => Parser[U]) = new Parser[T~U] {
      def apply(in: Input) = p(in) match {
        case Success(x, in1) => q(in1) match {
          case Success(y, in2)  => Success(new ~(x, y), in2)
          case Failure(msg, in) => Failure(msg, in)
        }
        case Failure(msg, in) => Failure(msg, in)
      }
    }

    def | [U >: T](q: => Parser[U]) = new Parser[U] {
      def apply(in: Input) = p(in) match {
        case s @ Success(x, rest)   => s
        case Failure(_,_)           => q(in)
      }
    }

    def ^^ [U](f: T => U) : Parser[U] = new Parser[U] {
      def apply(in: Input) = p(in) match {
        case Success(x, rest)       => Success(f(x), rest)
        case Failure(msg, in)       => Failure(msg, in)
      }
    }

    def ^^^ [U](v: U) = p ^^ { case _ => v }
    def ~> [U](q: => Parser[U]) = (p ~ q) ^^ { case a~b => b }
    def <~ [U](q: => Parser[U]) = (p ~ q) ^^ { case a~b => a }

  }

  //def inLine[T](q : => Parser[T]) : Parser[T] = line ^^ { (l : Stream[Char]) => q(l) }

  def line : Parser[Stream[Char]] = new Parser[Stream[Char]] {
    override def apply(in: Input) : Result[Stream[Char]] = {
      val (h,t) = in.span(_!='\n')
      if (in.isEmpty) Failure("End of document",in)
      else Success(h,t.tail)
    }
  }

  def len(n : Int) : Parser[Stream[Char]] = new Parser[Stream[Char]] {
    override def apply(in: Input) : Result[Stream[Char]] = {
      if (in.take(n).mkString.length < n) Failure("Input too small",in)
      else Success(in.take(n),in.drop(n))
    }
  }

  //def startsWith[T](t: String) : Parser[T] = str(t) ~ line ^^ { case a~b => a ++ b }

  //def toLine[T](q: Parser[U]) : Parser[T] = new Parser[T] {
  //  override def apply(in: Input) : Result[T] = q(in) match {
  //    case s @ Success(_,_) => line(in)
  //    case Failure(_,_) => (dropLine ~> toLine(q))(in)
  //  }
  //}

  def dropLine : Parser[Stream[Char]] = line ~> success(Stream.Empty)

  def dropLines(p : Parser[Stream[Char]]) : Parser[Stream[Char]] = new Parser[Stream[Char]] {
    override def apply(in: Input) : Result[Stream[Char]] = line(in) match {
      case s @ Success(result,rest) => p(result) match {
        case Success(_,_)               => s
        case Failure(_,_)               => dropLines(p)(in.dropWhile(_!='\n'))
      }
      case f @ Failure(_,_)             => f
    }
  }

  def dropLines(n : Int) : Parser[Stream[Char]] = n match {
    case 0      => success(Stream.Empty)
    case n      => line ~> dropLines(n-1)
  }

  def opt [T](q : => Parser[T]) : Parser[Option[T]] = (
    q ^^ { case a => Some(a) }
    | success(None))

  def rep [T](p : => Parser[T]) : Parser[List[T]] = (
      p ~ rep(p) ^^ { case a~b => a::b }
    | success(List()))

  def repsep [T, U](p : => Parser[T], sep : => Parser[U]) : Parser[List[T]] = (
      p ~ rep(sep ~> p) ^^ { case a~b => a::b }
    | success(List()))

  def success[T](v: T) : Parser[T] = new Parser[T] {
    override def apply(in: Input) : Result[T] = Success(v,in)
  }

  def fail(msg : String) : Parser[Nothing] = new Parser[Nothing] {
    override def apply(in: Input) : Result[Nothing] = Failure(msg,in)
  }


  implicit def str[T](t: String) : Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val word = in.take(t.length).mkString
      val rest  = in.drop(t.length)
      if (in.isEmpty) Failure("Expected " + t + " but reached end of file", in)
      else (word == t) match {
        case true     => { Success(word ,rest) }
        case false    => { Failure("Couldn't match expected: " + t + " given " + in.head, in.tail) }
      }
    }
  }

}

