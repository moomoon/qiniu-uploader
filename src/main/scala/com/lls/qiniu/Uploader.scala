package com.lls.qiniu

import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}

import argonaut._, Argonaut._

import scala.collection.JavaConverters._
import com.qiniu.http.Response
import com.qiniu.storage.Configuration
import com.qiniu.util.Auth
import org.rogach.scallop.{ScallopConf, Subcommand}

import scala.util.Try

object Uploader extends App {

  object Config extends ScallopConf(args) {

    object config extends Subcommand("config") {
      val clear = opt[Boolean]("clear", required = false)
      val list = opt[Boolean]("list", required = false)
      val accessKey = opt[String]("access-key", required = false)
      val secretKey = opt[String]("secret-key", required = false)
      val domain = opt[String]("domain", required = false)
      val bucket = opt[String]("bucket-name", required = false)
    }

    addSubcommand(config)

    object upload extends Subcommand("upload") {
      val accessKey = opt[String]("access-key", required = false)
      val secretKey = opt[String]("secret-key", required = false)
      val domain = opt[String]("domain", required = false)
      val bucket = opt[String]("bucket-name", required = false)
      val file = trailArg[String]("file")
    }

    addSubcommand(upload)
    verify()
  }

  def expand(path: String): String = path
    .replaceFirst("^~", System.getProperty("user.home"))
    .replaceFirst("^\\.", System.getProperty("user.dir"))

  def configPath: File = {
    val path = expand("~/.qiniu/config")
    val file = new File(path)
    val parent = file.getParentFile
    if (!parent.exists()) parent.mkdirs()
    if (!file.exists()) file.createNewFile()
    file
  }

  import java.util.Properties

  def readProperty = tryWith(new FileInputStream(configPath)) { s ⇒
    val p = new Properties
    p.load(s)
    p
  }

  def saveProperty(prop: Properties) = tryWith(new FileOutputStream(configPath)) { s ⇒
    prop.store(s, null)
  }

  def exit(msg: String): Nothing = {
    println(msg)
    System.exit(1)
    throw new RuntimeException
  }

  case class Ret(hash: Option[String], key: Option[String])

  object Ret {
    implicit def codecJson: CodecJson[Ret] = casecodec2(Ret.apply, Ret.unapply)("hash", "key")
  }

  Config.subcommand match {
    case Some(Config.upload) ⇒
      lazy val properties = readProperty.get
      val accessKey = Config.upload.accessKey.orElse(Option(properties.getProperty("access-key")).filter(_.nonEmpty)).getOrElse(exit("no access key"))
      val secretKey = Config.upload.secretKey.orElse(Option(properties.getProperty("secret-key")).filter(_.nonEmpty)).getOrElse(exit("no secret key"))
      val bucket = Config.upload.bucket.orElse(Option(properties.getProperty("bucket")).filter(_.nonEmpty)).getOrElse(exit("no bucket"))
      val domain = Config.upload.domain.orElse(Option(properties.getProperty("domain"))).toOption
      val auth = Auth.create(accessKey, secretKey).uploadToken(bucket)
      import com.qiniu.common.Zone
      import com.qiniu.storage.UploadManager

      val cfg = new Configuration(Zone.zone0)
      val uploadManager = new UploadManager(cfg)
      val file = Config.upload.file.toOption.get
        .replaceFirst("^~", System.getProperty("user.home"))
        .replaceFirst("^~", System.getProperty("user.dir"))
      val response: Response = uploadManager.put(file, file.substring(file.lastIndexOf("/") + 1), auth)

      domain.fold(
        println(response.bodyString())
      ) { domain ⇒

        response.bodyString().decodeOption[Ret].foreach { ret ⇒
          ret.key.foreach(key ⇒ println(s"$domain/$key"))
          ret.hash.foreach(println)
        }
      }
    case Some(Config.config) ⇒
      if (Config.config.list getOrElse false) {
        val properties = readProperty.get
        properties.propertyNames().asScala.map(_.toString).foreach { name ⇒
          println(s"$name = ${properties.get(name)}")
        }
      } else if (Config.config.clear getOrElse false) {
        configPath.delete()
      } else {
        val properties = readProperty.get
        Config.config.accessKey.foreach(properties.setProperty("access-key", _))
        Config.config.secretKey.foreach(properties.setProperty("secret-key", _))
        Config.config.bucket.foreach(properties.setProperty("bucket", _))
        Config.config.domain.foreach(properties.setProperty("domain", _))
        saveProperty(properties)
      }
    case _ ⇒
  }

}
