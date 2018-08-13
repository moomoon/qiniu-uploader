package com.lls.qiniu

import java.io._

import com.lls.qiniu.Uploader.Config.set.SetOp
import com.qiniu.common.Zone
import com.qiniu.http.Response
import com.qiniu.storage.model.BatchStatus
import com.qiniu.storage.{BucketManager, Configuration}
import com.qiniu.util.Auth
import org.rogach.scallop.{ScallopConf, Subcommand}
import upickle.default.{macroRW, read, writeTo, ReadWriter => RW}

import scala.collection.JavaConverters._
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

    object delete extends Subcommand("delete") {
      val accessKey = opt[String]("access-key", required = false)
      val secretKey = opt[String]("secret-key", required = false)
      val bucket = opt[String]("bucket-name", required = false)
      val fileKey = trailArg[String]("file-key", required = true)
    }

    addSubcommand(delete)

    object upload extends Subcommand("upload") {
      val accessKey = opt[String]("access-key", noshort = true, required = false)
      val secretKey = opt[String]("secret-key", noshort = true, required = false)
      val set = opt[String]("set", short = 's', required = false)
      val domain = opt[String]("domain", required = false)
      val bucket = opt[String]("bucket-name", required = false)
      val file = trailArg[String]("file")
    }

    addSubcommand(upload)

    object set extends Subcommand("set") {
      val list = opt[Boolean]("list", required = false)
      val listContent = opt[String]("list-content", short = 'c', required = false)
      val delete = opt[String]("delete", required = false)
      val accessKey = opt[String]("access-key", required = false)
      val secretKey = opt[String]("secret-key", required = false)
      val bucket = opt[String]("bucket-name", required = false)


      sealed trait SetOp

      object SetOp {

        object list extends SetOp

        case class listContent(name: String) extends SetOp

        case class delete(name: String, accessKey: Option[String], secretKey: Option[String], bucket: Option[String]) extends SetOp

      }

      def unapply(ignored: Unit.type): Option[SetOp] = {
        if (set.list getOrElse false) Some(SetOp.list)
        else (set.listContent.toOption, set.delete.toOption) match {
          case (Some(content), _) ⇒ Some(SetOp.listContent(content))
          case (_, Some(d)) ⇒ Some(SetOp.delete(d, set.accessKey.toOption, set.secretKey.toOption, set.bucket.toOption))
          case _ ⇒ exit("unrecognized usage of set command, see `set --help`")
        }
      }
    }

    addSubcommand(set)

    verify()
  }

  def expand(path: String): String = path
    .replaceFirst("^~", System.getProperty("user.home"))
    .replaceFirst("^\\.", System.getProperty("user.dir"))

  def ensureFile(path: String): File = {
    val expanded = expand(path)
    val file = new File(expanded)
    val parent = file.getParentFile
    if (!parent.exists()) parent.mkdirs()
    if (!file.exists()) file.createNewFile()
    file
  }

  def configFile(name: String) = ensureFile(s"~/.qiniu/$name")

  def configPath: File = configFile("config")

  def setPath: File = configFile("sets")

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

  case class Ret(hash: String, key: String)

  object Ret {
    implicit def rw: RW[Ret] = macroRW
  }

  case class RawSet(name: String, values: List[String])

  object RawSet {
    implicit def rw: RW[RawSet] = macroRW
  }

  case class Sets(values: String Map List[String]) {
    def toRaw: List[RawSet] = values.toList.map { case (n, v) ⇒ RawSet(n, v) }

    def save =
      tryWith(new FileOutputStream(setPath)) { os ⇒
        tryWith(new OutputStreamWriter(os)) { writer ⇒
          writeTo(toRaw, writer)
        }
      }.flatten
  }

  object Sets {
    def apply(rawSets: List[RawSet]): Sets = Sets(rawSets.groupBy(_.name).mapValues(_.flatMap(_.values)))

    def apply(): Sets = Sets(Try(read[List[RawSet]](setPath)).toOption.getOrElse(List.empty))
  }

  implicit class UserPref(val properties: Properties) extends AnyVal {
    def accessKey: Option[String] = Option(properties.getProperty("access-key")).filter(_.nonEmpty)

    def secretKey: Option[String] = Option(properties.getProperty("secret-key")).filter(_.nonEmpty)

    def bucket: Option[String] = Option(properties.getProperty("bucket")).filter(_.nonEmpty)

    def domain: Option[String] = Option(properties.getProperty("domain"))
  }

  Config.subcommand match {
    case Some(Config.upload) ⇒
      import Config.upload
      lazy val properties = readProperty.get
      val accessKey = upload.accessKey orElse properties.accessKey getOrElse exit("no access key")
      val secretKey = upload.secretKey orElse properties.secretKey getOrElse exit("no secret key")
      val bucket = upload.bucket orElse properties.bucket getOrElse exit("no bucket")
      val domain = upload.domain.toOption orElse properties.domain
      val set = upload.set.toOption
      val auth = Auth.create(accessKey, secretKey).uploadToken(bucket)
      import com.qiniu.storage.UploadManager

      lazy val sets = Sets()

      val cfg = new Configuration(Zone.zone0)
      val uploadManager = new UploadManager(cfg)
      val file = Config.upload.file.toOption.get
        .replaceFirst("^~", System.getProperty("user.home"))
        .replaceFirst("^~", System.getProperty("user.dir"))
      val response: Response = uploadManager.put(file, file.substring(file.lastIndexOf("/") + 1), auth)

      domain.fold(
        println(response.bodyString())
      ) { domain ⇒
        val ret = read[Ret](response.bodyString())
        val url = s"$domain/${ret.key}"
        println(url)
        set.foreach { set ⇒
          val newValues: List[String] = sets.values.getOrElse(set, List()) :+ url
          val newMap = sets.values + (set → newValues.distinct)
          sets.copy(newMap).save
          println(s"added to set: $set")
        }
      }

    case Some(Config.delete) ⇒
      import Config.delete
      lazy val properties = readProperty.get
      val accessKey = delete.accessKey orElse properties.accessKey getOrElse exit("no access key")
      val secretKey = delete.secretKey orElse properties.secretKey getOrElse exit("no secret key")
      val bucket = delete.bucket orElse properties.bucket getOrElse exit("no bucket")
      val fileKey = delete.fileKey getOrElse exit("no file key")

      val cfg = new Configuration(Zone.zone0)
      val auth = Auth.create(accessKey, secretKey)
      val bucketManager = new BucketManager(auth, cfg)
      val response: Response = bucketManager.delete(bucket, fileKey)
      if (response.isOK)
        println(s"deleted $fileKey")

    case Some(Config.set) ⇒
      import Config.set
      lazy val sets = Sets()
      val set(op) = Unit
      op match {
        case SetOp.list ⇒
          sets.values.foreach { case (n, v) ⇒
            println(s"$n ⇒ ${v.size}")
          }
        case SetOp.listContent(name) ⇒
          sets.values.get(name).fold(
            println("empty")
          ) { v ⇒
            println(s"$name ⇒ ${v.size}")
            v.zipWithIndex.foreach { case (s, i) ⇒ println(s"${i + 1} - $s") }
          }

        case SetOp.delete(name, dAccessKey, dSecretKey, dBucket) ⇒
          lazy val properties = readProperty.get
          val accessKey = dAccessKey orElse properties.accessKey getOrElse exit("no access key")
          val secretKey = dSecretKey orElse properties.secretKey getOrElse exit("no secret key")
          val bucket = dBucket orElse properties.bucket getOrElse exit("no bucket")
          val names = sets.values.getOrElse(name, exit(s"empty set: $name"))

          val cfg = new Configuration(Zone.zone0)
          val auth = Auth.create(accessKey, secretKey)
          val bucketManager = new BucketManager(auth, cfg)
          val batchOperations = new BucketManager.BatchOperations
          batchOperations.addDeleteOp(bucket, names.map { s ⇒ s.substring(s.lastIndexOf('/') + 1) }: _*)
          val response = bucketManager.batch(batchOperations)
          val batchStatusList = response.jsonToObject(classOf[Array[BatchStatus]])
          if (batchStatusList.zip(names).map { case (status, n) ⇒
            if (status.code != 200) {
              println(s"failed to delete $n: ${status.data.error}")
              false
            } else true
          }.forall(identity)) {
            println(s"delete all ${names.size} files")
          }
          sets.copy(values = sets.values - name).save
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
