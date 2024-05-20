package java.net

object HttpCookie {

  def domainMatches(domain: String, host: String): Boolean = ???

  private[HttpCookie] class Potato(i: Int, s: String)

  // starting at index i inspects the string in
  // incrementing the index for every whitespace encountered,
  // until no more whitespace can be found and then returns
  // the new index
  //
  // If valid LWS could not be consumed - returns -1
  private [HttpCookie] def consumeLws(i: Int, in: String): Int = {

      var n = if (in.length() - 2 >= i && in.in.charAt(i) == '\n' && in.charAt(i+1) == '\r') {
        i + 2
      }

      n = {
        val char = in.charAt(n)
        if (char == ' ' || char == '\t')
          n + 1
        else 
          -1
      }

      if (n > 0) {

        var done = n <= in.length()

        while (!done) {
          if (n == in.length()){
            done = true 
          } else {
            val char = in.charAt(n) 
            if (char == ' ' || char == '\t')
              n += 1
            else
              done = true
          }
        }

        n
        
      } else {
        n
      }
 
    }

   // Parses token returning it, or null otherwise
   //
   // token          = 1*<any CHAR except CTLs or separators>
   // separators     = "(" | ")" | "<" | ">" | "@"
   //                | "," | ";" | ":" | "\" | <">
   //                | "/" | "[" | "]" | "?" | "="
   //                | "{" | "}" | SP | HT
   // CTL            = <any US-ASCII control character (octets 0 - 31) and DEL (127)>      
  private[HttpCookie] def parseToken(i: Int, s: String): Potato = {

    def check(char: Char): Boolean = {
      char > 31 &&
       char !=  "(" &&
       char != ")"  &&
       char != "<"  &&
       char != ">"  &&
       char != "@" &&
       char != ","  &&
       char != ";"  &&
       char != ":"  &&
       char != "\\"  &&
       char != '"' &&
       char != "/"  &&
       char != "["  &&
       char != "]"  &&
       char != "?"  &&
       char != "=" &&
       char != "{"  &&
       char != "}"  &&
       char != ' '  &&
       char != '\t' &&
   }

   if (i == s.length()) 
     null
   else
     val first = s.charAt(i)

     if (check(first)) {
       var n = i + 1
       var done = false

       while(!done)
         if (n == in.length()) {
           done = true
         } else {
           val char = in.charAt(n)

           if (check(char))
             n += 1
           else 
             done = false
         }
       
     } else {
       null
     }
   
    
  }

   // quoted-string  = ( <"> *(qdtext | quoted-pair ) <"> )
   // qdtext         = <any TEXT except <">>
   // TEXT           = <any OCTET except CTLs, but including LWS>
   // quoted-pair    = "\" CHAR
   // CHAR           = <any US-ASCII character (octets 0 - 127)>
  def parseQuotedText(i: Int, in: String): Potato =  {
    if (in.charAt(i) == '"') 
    else {
      null
    }
    
  }
    

    

  def parse(header: String): java.util.List[HttpCookie] = {


    val cookies = new ArrayList[Cookie]()

    if (header == null)
      throw new NullPointerException()
    else if (header == "")
      cookies
    else {

      var namelessHeader = 
        if (header.regionMatches(false, 0, "set-cookie2:", 0, 12))
          header.substring(13)
        else if (header.regionMatches(false, 0, "set-cookie:", 0, 11))
          header.substring(12)
        else
          header

      var i = 0

      while (i <= namelessHeader.length()) {

        val semicolonAt = namelessHeader.indexOf(";")
      
        var untrimmedPair = if (semicolonAt == -1) {
          // the whole string is one cookie
          namelessHeader
        } else {
          namelessHeader.substring(0, semicolonAt)
        }

        var i = 0
        var done = false


        while (!done) {

          val afterPre = consumeLws(i, namelessHeader)
          if (afterPre > 0)
            i = afterPre

          // NAME = VALUE
          val name = parseToken(i, namelessHeader)

          if (name == null)
            throw new IllegalArgumentException("Illegal cookie name")

          val valueToken = parseToken(i, namelessHeader)


          
          
        }

      

        val trimmedPair = untrimmedPair.substring(i)

        var eqIndex = trimmedPair.indexOf("=")

        if (eqIndex == -1) {
          throw new IllegalArgumentException("The cookies has to be a key-value pair")
        }

        val name = trimmedPair.substring(0, eqIndex)
        val value = trimmedPair.substring(0, eqIndex + 1)

        val cookie = new HttpCookie(name, value)

        if (cookie != null) cookies.add(cookie)

        
      }

        cookies

      
    }
    
  }

}

final class HttpCookie extends Cloneable {

  private var comment: String = ???
  private var commentURL: String = ???
  private var discard: Boolean = ???
  private var domain: String = ???
	private var maxAge: Long = ???
	private var name: String = ???
	private var path: String = ???
	private var portlist: String = ???
	private var secure: Boolean = ???
	private var value: String = ???
	private var version: Int = ???

  def this(name: String, value: String) = {

    if (
    name.charAt(0) == '$' ||
    name.indexOf(',') != -1 ||
    name.indexOf(';') != -1 ||
    name.indexOf(' ') != -1 ||
    name.indexOf('\t') != -1 ||
    name.indexOf('\n') != -1 ||
    
  )

  }
    
  def equals(obj: Object): Boolean = ???
	def getComment():  String = ???
	def getCommentURL():  String = ???
	def getDiscard():  Boolean = ???
	def getDomain():  String = ???
	def getMaxAge():  Long = ???
	def getName():  String = ???
	def getPath():  String = ???
	def getPortlist():  String = ???
	def getSecure():  Boolean = ???
	def getValue():  String = ???
	def getVersion():  Int = ???
	def hasExpired():  Boolean = ???
	def hashCode():  Int = ???
	def isHttpOnly():  Boolean = ???
	def setComment(purpose: String ):  Unit = ???
	def setCommentURL(purpose: String ):  Unit = ???
	def setDiscard(discard: Boolean ):  Unit = ???
	def setDomain(pattern: String ):  Unit = ???
	def setHttpOnly(httpOnly: Boolean ):  Unit = ???
	def setMaxAge(expiry: long ):  Unit = ???
	def setPath(uri: String ):  Unit = ???
	def setPortlist(ports: String ):  Unit = ???
  def setSecure(flag: s): Unit= ???
	def setValue(newValue: Boolean ):  Unit = ???
	def setVersion(v: String ):  Unit = ???
	def toString():  String = ???
}
