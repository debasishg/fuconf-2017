package patterns.m4

import cats._
import cats.data._
import cats.instances.all._
import cats.syntax.either._

import scala.util.Try
import com.typesafe.config.Config

/**
 * This object wraps the native Java config APIs into a monadic
 * interpreter
 */ 
object AppConfig {

  private[AppConfig] case class KafkaSettings(
    brokers: String, 
    zk: String, 
    fromTopic: String, 
    toTopic: String, 
    errorTopic: String
  )

  private[AppConfig] case class DataLoaderSettings(
    sourceTopic: String,
    directoryToWatch: String
  )

  case class ConfigData(ks: KafkaSettings, dls: DataLoaderSettings) {
    def brokers = ks.brokers
    def zk = ks.zk
    def fromTopic = ks.fromTopic
    def toTopic = ks.toTopic
    def errorTopic = ks.errorTopic
    def sourceTopic = dls.sourceTopic
    def directoryToWatch = dls.directoryToWatch
  }

  type ConfigReader[A] = ReaderT[Try, Config, A]

  private def fromKafkaConfig: ConfigReader[KafkaSettings] = Kleisli { (config: Config) =>
    Try {
      KafkaSettings(
        config.getString("dcos.kafka.brokers"),
        config.getString("dcos.kafka.zookeeper"),
        config.getString("dcos.kafka.fromtopic"),
        config.getString("dcos.kafka.totopic"),
        config.getString("dcos.kafka.errortopic")
      )
    }
  }

  private def fromDataLoaderConfig: ConfigReader[DataLoaderSettings] = Kleisli { (config: Config) =>
    Try {
      DataLoaderSettings(
        config.getString("dcos.kafka.loader.sourcetopic"),
        config.getString("dcos.kafka.loader.directorytowatch")
      )
    }
  }

  type ErrorOr[A] = Either[Exception, A]

  private def readString(path: String, config: Config): ErrorOr[String] = try {
    Either.right(config.getString(path))
  } catch {
    case ex: Exception => Either.left(ex)
  }

  private def fromKafkaConfigA(config: Config): ErrorOr[KafkaSettings] = for {
    b <- readString("dcos.kafka.brokers", config)
    z <- readString("dcos.kafka.zookeeper", config)
    f <- readString("dcos.kafka.fromtopic", config)
    t <- readString("dcos.kafka.totopic", config)
    e <- readString("dcos.kafka.errortopic", config)
  } yield KafkaSettings(b, z, f, t, e)

  private def fromKafkaConfigC: Config => ErrorOr[KafkaSettings] = (config: Config) => for {
    b <- readString("dcos.kafka.brokers", config)
    z <- readString("dcos.kafka.zookeeper", config)
    f <- readString("dcos.kafka.fromtopic", config)
    t <- readString("dcos.kafka.totopic", config)
    e <- readString("dcos.kafka.errortopic", config)
  } yield KafkaSettings(b, z, f, t, e)

  type ConfigReaderX[A] = ReaderT[ErrorOr, Config, A]

  private def readString(path: String): ConfigReaderX[String] = Kleisli{ (config: Config) =>
    try {
      Either.right(config.getString(path))
    } catch {
      case ex: Exception => Either.left(ex)
    }
  }

  private def fromKafkaConfigD: ReaderT[ErrorOr, Config, KafkaSettings] = for {
    b <- readString("dcos.kafka.brokers")
    z <- readString("dcos.kafka.zookeeper")
    f <- readString("dcos.kafka.fromtopic")
    t <- readString("dcos.kafka.totopic")
    e <- readString("dcos.kafka.errortopic")
  } yield KafkaSettings(b, z, f, t, e)

  private def fromKafkaConfigB: ConfigReaderX[KafkaSettings] = for {
    b <- readString("dcos.kafka.brokers")
    z <- readString("dcos.kafka.zookeeper")
    f <- readString("dcos.kafka.fromtopic")
    t <- readString("dcos.kafka.totopic")
    e <- readString("dcos.kafka.errortopic")
  } yield KafkaSettings(b, z, f, t, e)


  def fromConfig: ConfigReader[ConfigData] = for {
    k <- fromKafkaConfig
    d <- fromDataLoaderConfig
  } yield ConfigData(k, d)
}

