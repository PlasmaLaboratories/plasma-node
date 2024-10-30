package org.plasmalabs.algebras

import cats.MonadThrow
import cats.implicits._
import org.plasmalabs.models._

object StoreOps {

  implicit class epochToTOps[F[_]: MonadThrow, T](storage: Store[F, Epoch, Set[T]]) {

    def addIdToEpoch(epoch: Epoch, id: T): F[Unit] =
      for {
        currentIds <- storage.get(epoch)
        _          <- storage.put(epoch, currentIds.fold(Set(id))(_ + id))
      } yield ()

    def removeIdFromEpoch(epoch: Epoch, id: T): F[Unit] =
      for {
        currentIds <- storage.get(epoch)
        newIds     <- currentIds.fold(Set.empty[T])(_ - id).pure[F]
        _          <- storage.put(epoch, newIds)
      } yield ()
  }

  implicit class proposalVotingOps[F[_]: MonadThrow](storage: Store[F, (Epoch, ProposalId), Long]) {

    def tryToAddOneVote(vote: (Epoch, ProposalId)): F[Unit] =
      for {
        currentVoteOpt <- storage.get(vote)
        _              <- currentVoteOpt.traverse(currentVote => storage.put(vote, currentVote + 1))
      } yield ()

    def addOneVote(vote: (Epoch, ProposalId)): F[Unit] =
      for {
        currentVote <- storage.getOrRaise(vote)
        _           <- storage.put(vote, currentVote + 1)
      } yield ()

    def createVoteAndAddOneVote(vote: (Epoch, ProposalId)): F[Unit] =
      for {
        currentVote <- storage.get(vote).flatMap {
          case Some(actualVote) => actualVote.pure[F]
          case None             => 0L.pure[F]
        }
        _ <- storage.put(vote, currentVote + 1)
      } yield ()

    def tryToRemoveOneVote(vote: (Epoch, ProposalId)): F[Unit] =
      for {
        currentVoteOpt <- storage
          .get(vote)
          .ensure(new IllegalStateException("Negative vote counter"))(_.getOrElse(Long.MaxValue) > 0)
        _ <- currentVoteOpt.traverse(currentVote => storage.put(vote, currentVote - 1))
      } yield ()

    def removeOneVote(vote: (Epoch, ProposalId)): F[Unit] =
      for {
        currentVote <- storage.getOrRaise(vote).ensure(new IllegalStateException("Negative vote counter"))(_ > 0)
        _           <- storage.put(vote, currentVote - 1)
      } yield ()

    def deleteVoting(vote: (Epoch, ProposalId)): F[Unit] =
      for {
        _ <- storage.getOrRaise(vote).ensure(new IllegalStateException("Try remove non negative vote"))(_ == 0)
        _ <- storage.remove(vote)
      } yield ()
  }

  implicit class VersionCounterOps[F[_]: MonadThrow](storage: Store[F, Unit, VersionId]) {

    def decrementVersion: F[Unit] =
      for {
        currentVersion <- storage.getOrRaise(())
        _              <- storage.put((), currentVersion - 1)
      } yield ()

    def getFreeVersion(): F[Int] =
      for {
        currentFreeVersion <- storage.getOrRaise(())
        _                  <- storage.put((), currentFreeVersion + 1)
      } yield currentFreeVersion
  }
}
