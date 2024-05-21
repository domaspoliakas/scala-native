package java.net

import java.util.ArrayList

object HttpCookie {

  def domainMatches(domain: String, host: String): Boolean = ???

  private[HttpCookie] val Discard = "Discard"
  private[HttpCookie] val Comment = "Comment"
  private[HttpCookie] val CommentURL = "CommentURL"
  private[HttpCookie] val Domain = "Domain"
  private[HttpCookie] val `Max-Age` = "Max-Age"
  private[HttpCookie] val Path = "Path"
  private[HttpCookie] val Port = "Port"
  private[HttpCookie] val Secure = "Secure"
  private[HttpCookie] val Version = "Version"

      def parseQuotedString(in: String): String = if (in.charAt(0) == '"') {
        var endquote = -1
        var i = 1
        var escaping = false
        var done = false
        while (!done) {
          if (in.length() == i) {
            done = true
          } else {
            
            val char = in.charAt(i)

            if (char == '\\') {
              escaping = true
              i += 1
            } else if (char == '"' && !escaping){
              done = true
              endquote = i
            } else {
              escaping = false
              i += 1
            }
          }
        }
        in.substring(1, endquote)
      } else null


  // NAME = attr
  // attr = token
  // token = 1*<any CHAR except CTLs or separators>
  // separators = "(" | ")" | "<" | ">" | "@"
  //                | "," | ";" | ":" | "\" | <">
  //                | "/" | "[" | "]" | "?" | "="
  //                | "{" | "}" | SP | HT
  // CHAR = <any US-ASCII character (octets 0 - 127)>
  // CTL = <any US-ASCII control character (octets 0 - 31) and DEL (127)>
  private def validateName(name: String): String = {
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

      i += 1
    }

    name

  }

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

    val cookies = new ArrayList[HttpCookie]()

    if (header == null)
      throw new NullPointerException()
    else if (header == "")
      cookies
    else {

      var version = 1

      var str = 
        if (header.regionMatches(false, 0, "set-cookie2:", 0, 12)) {
          version = 2
          header.substring(12)
        } else if (header.regionMatches(false, 0, "set-cookie:", 0, 11))
          header.substring(11)
        else
          header

      val keyValueSeparator = str.indexOf("=")
      val name = validateName(str.substring(0, keyValueSeparator).trim())
      var rest = str.substring(keyValueSeparator + 1).trim()

      var value: String = parseQuotedString(rest)

      if (value == null) {
          
        var commaIndex = rest.indexOf(',')
        var semicolonIndex = rest.indexOf(';')

        value = (if (commaIndex > 0 && (semicolonIndex < 0 || semicolonIndex > commaIndex)) {

          val res = rest.substring(0, commaIndex)
          rest = rest.substring(commaIndex)
          res
        } else if (semicolonIndex > 0 && (commaIndex < 0 || commaIndex > semicolonIndex)) {
          val res = rest.substring(0, semicolonIndex)
          rest = rest.substring(semicolonIndex)

          res
        } else {

          // rest of string
          val res = rest
          rest = ""
          res
        }).trim
      } else {
        rest = rest.substring(value.length + 2) // +2 for both quotes
      }

      val cookie = new HttpCookie(name, value)

      // parse props
      //
      rest = rest.trim()
      if (rest.charAt(0) == ';') {
        // there's props
        rest = rest.substring(1)

        var done = false
        while (!done) {

          if (rest == "") {
            done
          } else {
            val semicolonIndex = rest.indexOf(';')
            val commaIndex = rest.indexOf(',')

            var prop: String = null

            if (semicolonIndex >= 0 && (semicolonIndex < commaIndex || commaIndex == -1)) {
              prop = rest.substring(0, semicolonIndex).trim()
              rest = rest.substring(semicolonIndex + 1)

            } else if (commaIndex >= 0 && (commaIndex < semicolonIndex || semicolonIndex == -1)) {
              prop = rest.substring(0, commaIndex).trim()
              rest = rest.substring(commaIndex + 1)
              done = true
            } else {
              prop = rest.trim()
              rest = ""
              done = true
            }

            def propValue =  {
              val eqIndex = prop.indexOf('=')
              if (eqIndex > 0) {
                prop.substring(eqIndex + 1)
              } else null
            }

            def quotedStringPropValue = {
              val p = propValue
              if (propValue != null) {
                parseQuotedString(propValue.trim())
              } else null
            }


            if (prop.regionMatches(true, 0, Discard, 0, Discard.length()))
              cookie.setDiscard(true)
            else if (prop.regionMatches(true, 0, Secure, 0, Secure.length())) {
              cookie.setSecure(true) 
            } else if (prop.regionMatches(true, 0, CommentURL, 0, CommentURL.length())) {
              // TODO I think this might need to be different
              val commentUrl = quotedStringPropValue
              if (commentUrl != null)
                cookie.setCommentURL(commentUrl)
            } else if (prop.regionMatches(true, 0, Comment, 0, Comment.length())) {
              val comment = quotedStringPropValue
              if (comment != null)
                cookie.setComment(comment)
            } else if (prop.regionMatches(true, 0, Domain, 0, Domain.length())) {
              val domain = quotedStringPropValue
              if (domain != null)
                cookie.setDomain(domain)
            } else if (prop.regionMatches(true, 0, `Max-Age`, 0, `Max-Age`.length())) {
              val p = propValue
              if (propValue != null) {
                try {
                  val l = java.lang.Long.valueOf(propValue.trim())
                  cookie.setMaxAge(l)
                } catch case (_: NumberFormatException) => {}
              }
            } else if (prop.regionMatches(true, 0, Path, 0, Path.length())) {
              val path = quotedStringPropValue
              if (path != null)
                cookie.setPath(path)
            } else if (prop.regionMatches(true, 0, Port, 0, Port.length())) {
              val port = quotedStringPropValue
              if (port != null)
                cookie.setPortlist(port)
            } else if (prop.regionMatches(true, 0, Version, 0, Version.length())) {

              val p = propValue
              if (propValue != null) {
                try {
                  println("asd")
                  val l = java.lang.Integer.valueOf(propValue.trim())
                  cookie.setVersion(l)
                } catch case (_: NumberFormatException) => {}
              }

            }


          }

        }

      }
      cookies.add(cookie)

      cookies
    }
    
  }



}

final class HttpCookie private(
  private var _comment: String = null,
  private var _commentURL: String = null,
  private var _httpOnly: Boolean = false,
  private var _discard: Boolean = false,
  private var _domain: String = null,
	private var _maxAge: Long = -1,
	private var _name: String = null,
	private var _path: String = null,
	private var _portlist: String = null,
	private var _secure: Boolean = false,
	private var _value: String = null,
	private var _version: Int = 1
) extends Cloneable {

  def this(name: String, value: String) = {
    // TODO validate name
    // TODO validate value
    this(_name = name, _value = value)
  }
    
  override def equals(obj: Any): Boolean = ???
	def getComment(): String = this._comment
	def getCommentURL(): String = this._commentURL
	def getDiscard(): Boolean = this._discard
	def getDomain(): String = this._domain
	def getMaxAge(): Long = this._maxAge
	def getName(): String = this._name
	def getPath(): String = this._path
	def getPortlist(): String = this._portlist
	def getSecure(): Boolean = this._secure
	def getValue(): String = this._value
	def getVersion(): Int = this._version
	def hasExpired(): Boolean = ???
	override def hashCode(): Int = ???
	def isHttpOnly(): Boolean = ???
	def setComment(purpose: String): Unit = this._comment = purpose
	def setCommentURL(purpose: String): Unit = this._commentURL = purpose
	def setDiscard(discard: Boolean): Unit = this._discard = discard
	def setDomain(pattern: String): Unit = this._domain = pattern
	def setHttpOnly(httpOnly: Boolean): Unit = this._httpOnly = httpOnly
	def setMaxAge(expiry: Long): Unit = this._maxAge = expiry
	def setPath(uri: String): Unit = this._path = uri
	def setPortlist(ports: String):  Unit = this._portlist = ports
  def setSecure(flag: Boolean): Unit= this._secure = flag
	def setValue(newValue: String):  Unit = this._value = newValue
	def setVersion(v: Int):  Unit = this._version = v
	override def toString():  String = ???
}
