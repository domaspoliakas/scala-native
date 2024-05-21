package java.net

object HttpCookie {

  def domainMatches(domain: String, host: String): Boolean = ???

  private[HttpCookie] val Discard = "Discard"
  private[HttpCookie] val Secure = "Secure"
  private[HttpCookie] val Comment = "Comment"
  private[HttpCookie] val CommentURL = "CommentURL"
  private[HttpCookie] val Discard = "Discard"
  private[HttpCookie] val Domain = "Domain"
  private[HttpCookie] val Max-Age = "Max-Age"
  private[HttpCookie] val Path = "Path"
  private[HttpCookie] val Port = "Port"
  private[HttpCookie] val Secure = "Secure"
  private[HttpCookie] val Version = "Version"


  def parse(header: String): java.util.List[HttpCookie] = {

   // set-cookie      =       "Set-Cookie2:" cookies
   // cookies         =       1#cookie
   // cookie          =       NAME "=" VALUE *(";" set-cookie-av)
   // NAME            =       attr
   // VALUE           =       value
   // set-cookie-av   =       "Comment" "=" value
   //                 |       "CommentURL" "=" <"> http_URL <">
   //                 |       "Discard"
   //                 |       "Domain" "=" value
   //                 |       "Max-Age" "=" value
   //                 |       "Path" "=" value
   //                 |       "Port" [ "=" <"> portlist <"> ]
   //                 |       "Secure"
   //                 |       "Version" "=" 1*DIGIT
   // portlist        =       1#portnum
   // portnum         =       1*DIGIT

    val cookies = new ArrayList[Cookie]()

    if (header == null)
      throw new NullPointerException()
    else if (header == "")
      cookies
    else {

      var str = 
        if (header.regionMatches(false, 0, "set-cookie2:", 0, 12))
          header.substring(13)
        else if (header.regionMatches(false, 0, "set-cookie:", 0, 11))
          header.substring(12)
        else
          header


      val keyValueSeparator = str.indexOf("=")
      val name = validateName(str.substring(0, keyValueSeparator).trim())
      val rest = str.substring(keyValueSeparator + 1)
      
      var commaIndex = rest.indexOf(",")
      var semicolonIndex = rest.indexOf(";")

      var value = 
        (if (commaIndex > 0 && (semicolonIndex < 0 || semicolonIndex > commaIndex)) {
          rest.substring(0, commaIndex)
        } else if (semicolonIndex > 0 && (commaIndex < 0 || commaIndex > semicolonIndex)) {
          rest.substring(0, semicolonIndex)
        } else {
          // rest of string
          rest
        }).trim


      
    }
    
  }

  //    VALUE           =       value
  //    value       =     token | quoted-string
  // token = 1*<any CHAR except CTLs or separators>
  // separators = "(" | ")" | "<" | ">" | "@"
  //                | "," | ";" | ":" | "\" | <">
  //                | "/" | "[" | "]" | "?" | "="
  //                | "{" | "}" | SP | HT
  // CHAR = <any US-ASCII character (octets 0 - 127)>
  // CTL = <any US-ASCII control character (octets 0 - 31) and DEL (127)>
  // 
  // quoted-string  = ( <"> *(qdtext | quoted-pair ) <"> )
  // qdtext         = <any TEXT except <">>
  // quoted-pair    = "\" CHAR  
  // 
   // TEXT           = <any OCTET except CTLs,
   //                  but including LWS>  
   private def processValue(value: String): String = {
    if (value.length() == 0 || 
        // this next bit is kinda weird, but that's how graal behaves 
        value == "\"\"")
      value
    else {

      // graal only strips quotes if both are present and there's something
      // inbetween
      if (value.getChar(0) == '"' && value.getChar(value.length() - 1) == '"') {
        // quoted string path
        var res = ""
        var i = 1


        while (i < (value.length() - 1)) {

          val char = value.charAt(i)

          if (char == '\')

          i++
          
        }

        
      }

          
    }

    
  }

  // NAME = attr
  // attr = token
  // token = 1*<any CHAR except CTLs or separators>
  // separators = "(" | ")" | "<" | ">" | "@"
  //                | "," | ";" | ":" | "\" | <">
  //                | "/" | "[" | "]" | "?" | "="
  //                | "{" | "}" | SP | HT
  // CHAR = <any US-ASCII character (octets 0 - 127)>
  // CTL = <any US-ASCII control character (octets 0 - 31) and DEL (127)>
  private def validateName(name: String) = {
    if (name.length == 0)
      throw new IllegalArgumentException("Illegal cookie name")

    var i = 0
    while(i < name.length()) {
      val char = name.charAt(i)
      if (
        char < 32 || // CTL
        char == 127 || // CTL
        char == '(' ||
        char == ')' ||
        char == '<' ||
        char == '>' ||
        char == '@'||
        char == ',' ||
        char == ';' ||
        char == ':' ||
        char == '\\' ||
        char == '"'||
        char == '/' ||
        char == '[' ||
        char == ']' ||
        char == '?' ||
        char == '='||
        char == '{' ||
        char == '}' ||
        char == ' ' ||
        char == '\t'
          ) {
              throw new IllegalArgumentException("Illegal cookie name")
            }

      i++
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
