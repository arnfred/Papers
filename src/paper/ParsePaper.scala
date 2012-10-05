package paper

import scala.collection.immutable.Stream
import scala.collection.immutable.StringOps
import scala.util.parsing.input._
import scala.io.Source
import java.io.File

abstract class Parsers {
  def parse(file : Source) : Option[Paper]
}

trait ParsePaper {

  def getText(in: Source): Stream[Char] = in.hasNext match {
    case false    => in.close(); Stream.Empty
    case true   => in.next #:: getText(in)
  }


  object Isit extends Parsers {

    def paper : Parser[Paper] = (
      //title ~ authors ~ dropLinesUntil("R EFERENCES") ~ references
      dropLinesUntil("R EFERENCES") ~ references
        ^^ { case b~r => Paper(0,0,Title(""),Nil,Abstract("Not saved"),Body("Not saved"),r,Map.empty,List()) }
     | dropLinesUntil("References") ~ references
        ^^ { case b~r => Paper(0,0,Title(""),Nil,Abstract("Not saved"),Body("Not saved"),r,Map.empty,List()) })


    def title : Parser[Title] = (
      line && ("Title:" ~> rest) 
        ^^ trim ^^ (s => Title(s.init.mkString)))

    def authors : Parser[List[Author]] = (
      line && "Author:        " ~> split(", ") 
      ^^ (as => {
          if (as.length == 0) Nil
          else {
            var names = as.init.map(a => a.mkString) ::: List(as.last.mkString.init)
            names.map(a => Author(formatAuthor(a.mkString)))
          }
        }))

    def abstr : Parser[Abstract] = (
      dropLinesUntil("Abstract") ~> takeLinesUntil("I. ") 
        ^^ (t => Abstract(t.mkString)))

    def body : Parser[Body] = (
      takeLinesUntil("R EFERENCES")
        ^^ (t => Body(t.mkString)))

    def refBracket : Parser[Input] = (
      "[" ~ rep(number) ~ "] " ^^^ Stream.Empty)

    def refLine : Parser[Input] = (
      takeLinesUntil("\n" | refBracket))

    def refAuthors : Parser[List[Author]] = (
        until(", “") && split(", and " | " and " | ", ") 
          ^^ { x => x.init.map { a => Author(a.mkString) } } )

    def refTitle : Parser[Title] = (
        until(",\"" | "\"," | "\"." | ".") ^^ (s => Title(s.mkString))			// Character modification. Possible errors in the future !!!
      | success(Title("")))

    def reference : Parser[Reference] = (
      refLine && (refAuthors ~ refTitle)
        ^^ { case a~t => Reference(a, t) })

    def references : Parser[List[Reference]] = (
      dropLinesUntil(refBracket) ~> rep(reference) ^^ cleanRefs)


    // The function for actually parsing a paper
    def parse(file : Source) : Option[Paper] = {
      paper(getText(file)) match {
        case Failure(msg, rest)       => println("Failure: " + msg); None
        case Success(result, rest)    => Some(result.setMeta("parsed" -> "yes"))
      }
    }
  }


  def cleanRefs(refs : List[Reference]) : List[Reference] = {
    val ret = for (r <- refs if r.title.t.stripMargin != "" && r.authors.length > 0) yield {
      // Clean authors
      var authors = for (a <- r.authors if a.name.stripMargin.length > 2) yield Author(a.name.stripMargin)
      Reference(authors, r.title)
    }
    return ret
  }

  def formatAuthor(name : String) : String = {
    var result = ""
    var names = name.split(" ").filter(n => n.length > 0)
    if (names.length > 0) {
      result = names.init.filter(n => n.length > 0).map(n => n.head).mkString("",". ",". ") + names.last
    }
    return result
  }
    


  type Input    = Stream[Char]
  sealed abstract class Result[+T]
  case class ~ [+T, +U](r1 : T, r2 : U)
  case class Success[T](result: T, in : Input) extends Result[T]
  case class Failure(msg : String, in : Input) extends Result[Nothing]

  val lineSep : List[Char] = List('\n','\r')
  val tokenSep : List[Char] = List(' ',',','.',':','[',']','-') ::: lineSep

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

    def ^ [U](f: (T, Input) => U, g: (T, Input) => Input) : Parser[U] = new Parser[U] {
      def apply(in: Input) = p(in) match {
        case Success(x, rest)       => Success(f(x, rest), g(x, rest))
        case Failure(msg, in)       => Failure(msg, in)
      }
    }

    def ^^ [U](f: T => U) : Parser[U] = p ^ ({ case (x, _) => f(x) }, { case (_,r) => r })
    def ^^^ [U](v: U) = p ^^ { case _ => v }
    def ~> [U](q: => Parser[U]) = (p ~ q) ^^ { case a~b => b }
    def <~ [U](q: => Parser[U]) = (p ~ q) ^^ { case a~b => a }

    def &&[U](q: => Parser[U]) = new Parser[U] {
      def apply(in: Input) = p(in) match {
        case f @ Failure(msg, in)   => f
        case Success(x1, rest)      => {
          // Because I don't know the result of parser q, I'm backtracing to figure out what was matched
          var matched = in.zipWithIndex.takeWhile{case (x,i) => !in.drop(i).equals(rest)}.unzip._1
          q(matched) match {
            case Success(x2, rest2)     => Success(x2, rest)
            case Failure(msg, rest2)    => Failure(msg, rest)
          }
        }
      }
    }
  }

  //def inLine[U](q : => Parser[U]) : Parser[U] = q <~ "\n"

  def line : Parser[Input] = next(lineSep)
  def token : Parser[Input] = next(tokenSep)

  def number : Parser[Input] = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0"

  def trim(s : Input) : Input = s.dropWhile(c => c==' ' || c=='\t')
  def line(p : Parser[Input]) : Parser[Input] = line && p && rest


  // Splits input by letting the parser p be the delimiter
  def split(p : Parser[Input]) : Parser[List[Input]] = rep(until(p))

  def until[U](p : => Parser[U]) : Parser[Input] = new Parser[Input] {
    override def apply(in: Input) : Result[Input] = {

      def gather(s : Input, soFar : Input) : Result[Input] = p(s) match {
        case Success(result, rest)          => Success(soFar, rest)
        case Failure(msg, Stream.Empty)     => Success(soFar, Stream.Empty)
        case Failure(_, _)                  => gather(s.drop(1), soFar ++ s.take(1))
      }

      if (in == Stream.Empty) return Failure("[Until] Reached end of input", in) 
      else return gather(in, Stream.Empty)
    }
  }

  // Much faster than the naive version, and also avoids a stackoverflow
  def takeLinesUntil[U](p : => Parser[U]) : Parser[Input] = new Parser[Input] {
    override def apply(in: Input) : Result[Input] = {

      def gather(s : Input, soFar : Input) : Result[Input] = {
        p(s) match {
          case Success(result, rest)          => Success(soFar, rest)
          case Failure(msg, Stream.Empty)     => Success(soFar, Stream.Empty)
          case Failure(_, _)                  => gather(s.dropWhile(c => c != '\n').drop(1), soFar ++ s.takeWhile(c => c != '\n').drop(1))
        }
      }

      if (in == Stream.Empty) return Failure("[takeLinesUntil] Reached end of input", in) 
      else return gather(in, Stream.Empty)
    }
  }

  //def takeLinesUntil(p : Parser[Input]) = rep(line(p)) 
  def dropLinesUntil[U](p : Parser[U]) = takeLinesUntil(p) ~> success(Stream.Empty)


  def not(s: String) : Parser[Input] = new Parser[Input] {
    override def apply(in: Input) : Result[Input] = in.take(s.length).mkString == s match {
      case true     => Failure("Failed: " + s + " was matched", in)
      case false    => Success(Stream.Empty, in)
    }
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

  def rest : Parser[Input] = new Parser[Input] {
    override def apply(in: Input) : Result[Input] = {
      in.length match {
        case 0    => Failure("[Rest] Reached end of input",in)
        case n    => Success(in, Stream.Empty)
      }
    }
  }

  def next(sep : List[Char]) : Parser[Input] = new Parser[Input] {
    def apply(in: Input) = in.span(c => !sep.contains(c)) match {
      case (Stream.Empty, Stream.Empty) => Failure("[Next] Reached end of input", in)
      case (head, rest)                 => Success(head.init, rest.drop(1))
    }
  }

  implicit def predicate(p: String => Boolean) : Parser[Input] = new Parser[Input] {
    def apply(in: Input) = {
      p(in.mkString) match { 
        case true     => Success(Stream.Empty, in)
        case false    => Failure("Couldn't match predicate on:\n" + in.mkString, Stream.Empty)
      }
    }
  }
    
  implicit def str(t: String) : Parser[Input] = new Parser[Stream[Char]] {
    def apply(in: Input) = in.take(t.length).mkString == t match {
      case true      => Success(in.take(t.length), in.drop(t.length))
      case _         => Failure("Couldn't match expected: '" + t + "' given " + in.mkString, in)
    }
  }

  implicit def resultToStream(r: Result[Input]) : Input = r match {
    case Success(x, rest)       => x
    case Failure(_,_)           => Stream.Empty
  }

  //    //implicit def Stream2Str(s : Stream[Char]) : String = s.mkString

}

