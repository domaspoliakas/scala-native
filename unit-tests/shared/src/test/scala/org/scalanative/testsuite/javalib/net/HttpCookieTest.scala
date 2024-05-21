package org.scalanative.testsuite.javalib.net

import java.net._

import org.junit.Test
import org.junit.Assert._

import org.scalanative.testsuite.utils.AssertThrows.assertThrows

class HttpCookieTest {

  // TODO non-backspaced CTL in quoted text
  // TODO backspace before the end of quoted text
  // TODO no backspaces after unwrapping quoted strings

  @Test
  def basicParseTest(): Unit = {

    val out = HttpCookie.parse("set-cookie2:potato=tomato")
    assertEquals(out.size(), 1)
    val cookie = out.get(0)
    assertEquals("potato", cookie.getName())
    assertEquals("tomato", cookie.getValue())
    
  }

}
