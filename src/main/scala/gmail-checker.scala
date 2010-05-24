package org.reliant.gmail

import java.io.Console
import org.apache.http.auth.{AuthScope,AuthState,UsernamePasswordCredentials}
import org.apache.http.client.{CredentialsProvider,HttpClient}
import org.apache.http.client.protocol.ClientContext
import org.apache.http.client.methods.HttpGet
import org.apache.http.HttpResponse
import org.apache.http.impl.client.{DefaultTargetAuthenticationHandler,DefaultHttpClient,BasicCredentialsProvider}
import org.apache.http.protocol.BasicHttpContext
import org.apache.http.util.EntityUtils
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.xml.{XML,Elem}

object GmailChecker {
  
  def getOutput(resp: HttpResponse): Option[Elem] = 
    resp.getEntity() match {
      case null => None
      case entity => Some(XML.loadString(EntityUtils.toString(entity)))
    }

  def readConsole(prompt: String): Option[String] = {
    print(prompt)
    scala.Console.readLine() match {
      case null => None
      case line => Some(line)
    }
  }

  def getPwd(msg: String): Option[String] = { 
    System.console().readPassword(msg) match {
      case null => None
      case passwd: Array[Char] => Some(new String(passwd))
    }
  }

  def getCredentials(): UsernamePasswordCredentials = {
    val username = readConsole("gmail username: ")
    if (username.isEmpty)
      System.exit(1)
    val password = getPwd("Enter password: ")
    if (password.isEmpty)
      System.exit(1)
    new UsernamePasswordCredentials(username.get, password.get)
  }

  def main(args: Array[String]) {
    val client = new DefaultHttpClient
    val credsProvider = new BasicCredentialsProvider
    val ctx = new BasicHttpContext
    val authScope = new AuthScope("mail.google.com", 443, "New mail feed", "BASIC")
    val creds = getCredentials()
    credsProvider.setCredentials(authScope, creds)
    client.setCredentialsProvider(credsProvider)
    val get = new HttpGet("https://mail.google.com/mail/feed/atom/unread")
    val output = getOutput(client.execute(get, ctx)) match {
      case None => throw new Exception("No output received")
      case Some(xml) => xml
    }
    val nbUnreadMsgs = (output \ "fullcount").text
    println("Nb unread messages: " + nbUnreadMsgs)
    output match {
      case <feed>{ entries @  _* }</feed> => {
        // Print out summary for every received email
        for {
          <entry>{ email @ _* }</entry> <- entries
          <title>{ title }</title> <- email
          name <- email \\ "name"
        } println(title + "\nfrom " + name.text)
      }
      case _ => throw new Exception("Couldn't find email entries")
    }
  }
}

// vim: set ts=2 sw=2 et:
