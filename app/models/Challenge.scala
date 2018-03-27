package models

import java.nio.charset.StandardCharsets
import java.security.{MessageDigest, SecureRandom}
import java.util.Base64

case class Challenge(secret: String, hash: String)

object Challenge {
  val secureRandom = new SecureRandom()
  val encoder = Base64.getUrlEncoder
  def apply(): Challenge ={

    val buffer = new Array[Byte](128)
    secureRandom.nextBytes(buffer)

    val secret = encoder.encodeToString(buffer)

    val hashedSecret = MessageDigest.getInstance("SHA-256").digest(secret.getBytes(StandardCharsets.UTF_8))
    // The documentation is ambiguous as to what it wants the hash to be of.
    // It wants the hash of the base64 encoded secret.

    val hash = encoder.encodeToString(hashedSecret)
    Challenge(secret,hash)

  }

}
