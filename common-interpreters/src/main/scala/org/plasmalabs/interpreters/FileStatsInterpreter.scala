package org.plasmalabs.interpreters

import cats.Applicative
import cats.effect.implicits.*
import cats.effect.kernel.{Resource, Sync}
import io.circe.Json
import org.plasmalabs.algebras.Stats

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}

object FileStatsInterpreter {

  object Eval {
    private val openOptions = List(StandardOpenOption.WRITE, StandardOpenOption.CREATE, StandardOpenOption.APPEND)

    def make[F[_]: Sync](path: String): Resource[F, Stats[F]] =
      for {
        filePath <- Sync[F].blocking(Paths.get(path)).toResource
      } yield (new Stats[F] {

        def writeFile(statName: String, data: Json): F[Unit] = {
          val contents = data.asObject.fold(data.toString())(d => d.toList.map(_._2.toString).mkString(",")) + "\n"
          Sync[F].blocking {
            if (!Files.exists(filePath)) {
              val header = data.asObject.fold("")(d => d.toList.map(_._1).mkString(",")) + "\n"
              Files.write(filePath, (header + contents).getBytes(StandardCharsets.UTF_8), openOptions*)
            } else {
              Files.write(filePath, contents.getBytes(StandardCharsets.UTF_8), openOptions*)
            }
          }
        }

        def incrementCounter(statName: String, description: String, attributes: Map[String, Json]): F[Unit] =
          Applicative[F].unit

        def decrementCounter(statName: String, description: String, attributes: Map[String, Json]): F[Unit] =
          Applicative[F].unit

        def recordGauge(statName: String, description: String, attributes: Map[String, Json], value: Json): F[Unit] =
          Applicative[F].unit

        def recordHistogram(statName: String, description: String, attributes: Map[String, Json], value: Json)
          : F[Unit] =
          Applicative[F].unit
      })
  }
}
