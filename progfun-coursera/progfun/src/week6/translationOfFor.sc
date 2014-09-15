package week6

/**
 * Lecture 6.5
 *
 * Translation of For expression into higher order functions:
 * map, flatMap and filter
 */
object translationOfFor
{
  case class Book( title: String, authors: List[String] )
  
  val books: List[Book] = List( Book( title = "Structure and Interpretation of Computer Programs",
  																		authors = List( "Abelson, Harald", "Sussman, Gerald J." ) ),
  															Book( title = "Introduction of Functional Programming",
  																		authors = List( "Bird, Richard", "Wadler, Phil" ) ),
  															Book( title = "Effective Java",
  																		authors = List( "Bloch, Joshua" ) ),
  															Book( title = "Java Puzzlers",
  																		authors = List( "Bloch, Joshua", "Gaftner, Neal" ) ),
  															Book( title = "Programming in Scala",
  																		authors = List( "Odersky, Martin", "Spoon, Lex", "Venners, Bill" ) ) )
                                                  //> books  : List[week6.translationOfFor.Book] = List(Book(Structure and Interpr
                                                  //| etation of Computer Programs,List(Abelson, Harald, Sussman, Gerald J.)), Boo
                                                  //| k(Introduction of Functional Programming,List(Bird, Richard, Wadler, Phil)),
                                                  //|  Book(Effective Java,List(Bloch, Joshua)), Book(Java Puzzlers,List(Bloch, Jo
                                                  //| shua, Gaftner, Neal)), Book(Programming in Scala,List(Odersky, Martin, Spoon
                                                  //| , Lex, Venners, Bill)))


	//Find the title of books whose author's name is Bird
	for( b <- books; a <- b.authors if( a.startsWith( "Bird," ) ) )
	yield b.title                             //> res0: List[String] = List(Introduction of Functional Programming)
	
	
	//Translation of this query into higher order functions
	//TODO: check why this code isn't working
	//books flatMap( b => b.authors withFilter( a => a startsWith "Bird," ) ) map ( y => y.title )
}