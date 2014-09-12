package week6


/**
 * Lecture 6.4
 *
 * How to implement queries with for, in this example we have a small data base in memory
 */
object queriesWithFor
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
                                                  //> books  : List[week6.queriesWithFor.Book] = List(Book(Structure and Interpret
                                                  //| ation of Computer Programs,List(Abelson, Harald, Sussman, Gerald J.)), Book(
                                                  //| Introduction of Functional Programming,List(Bird, Richard, Wadler, Phil)), B
                                                  //| ook(Effective Java,List(Bloch, Joshua)), Book(Java Puzzlers,List(Bloch, Josh
                                                  //| ua, Gaftner, Neal)), Book(Programming in Scala,List(Odersky, Martin, Spoon, 
                                                  //| Lex, Venners, Bill)))


	//Find the title of books whose author's name is Bird
	for( b <- books; a <- b.authors if( a.startsWith( "Bloch," ) ) )
	yield b.title                             //> res0: List[String] = List(Effective Java, Java Puzzlers)

	//Find all the books wich have the word "Program" in the title
	for( b <- books if( b.title contains "Program" ) )
	yield b.title                             //> res1: List[String] = List(Structure and Interpretation of Computer Programs
                                                  //| , Introduction of Functional Programming, Programming in Scala)
	
	//Find the names of all authors who have written at least two books in the data base
	( for
		{
			b1 <- books
			b2 <- books
			if( b1.title < b2.title )
			a1 <- b1.authors
			a2 <- b2.authors
			if( a1 == a2 )
		} yield a1 ).distinct             //> res2: List[String] = List(Bloch, Joshua)
}