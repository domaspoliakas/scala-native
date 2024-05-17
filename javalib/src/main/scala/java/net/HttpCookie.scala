package java.net

object HttpCookie {

  def domainMatches(domain: String, host: String): Boolean = ???

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

        while (!done && i < untrimmedPair.length()) {
          if (c == '\t' || c == '\n' || c == 0x0b || c == '\f'
                  || c == '\r' || c == ' ' || c == ',' || c == ';') {
            i++;
          } else {
            done = true
          }
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
