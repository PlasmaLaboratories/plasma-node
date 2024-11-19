package org.plasmalabs.mpt

import monocle.Prism
import org.web3j.rlp.{RlpDecoder, RlpEncoder, RlpList, RlpString, RlpType}
import scala.annotation.nowarn

private[mpt] trait Optics[T: RLPPersistable] {

  import cats.implicits._

  val vEv = implicitly[RLPPersistable[T]]

  val rlpStringPrism = Prism[RlpType, RlpString] {
    case rlpString: RlpString => Some(rlpString)
    case _                    => None
  }(identity)

  @nowarn("msg=.*cannot be checked at runtime because its type arguments can't be determined from.*")
  val nodeBranchPrism = Prism[Node, BranchNode[T]] {
    case branch: BranchNode[T] => Some(branch)
    case _                     => None
  }(identity)

  val rlpListPrism = Prism[RlpType, RlpList] {
    case rlpList: RlpList => Some(rlpList)
    case _ =>
      None
  } { case (list) =>
    list
  }

  val rlpLeaf = Prism[(RlpType, RlpType), LeafNode[T]] {
    case (encodedKey, encodedValue) =>
      for {
        key   <- rlpStringPrism.getOption(encodedKey) if hpFlag(key.getBytes())
        value <- rlpStringPrism.getOption(encodedValue)
      } yield LeafNode(key.getBytes(), vEv.decode(value))
    case null => None
  }(node => (RlpString.create(node.key), vEv.encode(node.value)))

  val rlpExtension = Prism[(RlpType, RlpType), ExtensionNode[T]] {
    case (encodedKey, encodedValue) =>
      for {
        key   <- rlpStringPrism.getOption(encodedKey) if !hpFlag(key.getBytes())
        value <- rlpStringPrism.getOption(encodedValue)
        node  <- rlpTypeToNode(value)
      } yield ExtensionNode(key.getBytes(), node)
    case null => None
  }(node => (RlpString.create(node.key), nodeToRlp(node.node)))

  def rlpTypeToNode(rlpType: RlpType): Option[Node] = {
    val singleElt = rlpStringPrism
      .getOption(rlpType)
      .flatMap { x =>
        Option
          .when(x.getBytes().length == 32)(RefNode(x.getBytes().toTreeRoot))
          .orElse(Option.when(x.getBytes().isEmpty)(EmptyNode))
      }
    val leafOrExt = for {
      list <- rlpListPrism.getOption(rlpType)
      pair <- listToPairPrism.getOption(list)
      res  <- rlpLeaf.getOption(pair).orElse(rlpExtension.getOption(pair))
    } yield res
    singleElt.orElse(leafOrExt).orElse(rlpListPrism.andThen(rlpBranch).getOption(rlpType))
  }

  @nowarn("msg=.*cannot be checked at runtime because its type arguments can't be determined from.*")
  def nodeToRlp(node: Node): RlpType = node match {
    case e: BranchNode[T] =>
      rlpBranch.reverseGet(e)
    case e: ExtensionNode[T] =>
      rlpListPrism.andThen(listToPairPrism).andThen(rlpExtension).reverseGet(e)
    case e: LeafNode[T] => rlpListPrism.andThen(listToPairPrism).andThen(rlpLeaf).reverseGet(e)
    case RefNode(hash)  => rlpStringPrism.reverseGet(RlpString.create(hash.toByteArray))
    case EmptyNode      => RlpString.create(Array.emptyByteArray)
  }

  def nodeToRef(node: Node): Option[RefNode] = node match {
    case ref: RefNode => Some(ref)
    case _            => None
  }

  val rlpBranch: Prism[RlpList, BranchNode[T]] = Prism[RlpList, BranchNode[T]] { list =>
    val size = list.getValues().size()
    if (size == 17) {
      import scala.jdk.CollectionConverters._
      val someChildren = list
        .getValues()
        .subList(0, 16)
        .asScala
        .toVector
        .map(rlpTypeToNode)
        .sequence
      val value = list.getValues().get(16)
      val someValue = rlpStringPrism
        .getOption(value)
        .flatMap(rlpStringPrism.getOption)
        .map(x => vEv.decode(x))
      someChildren.map(BranchNode(_, someValue))
    } else {
      None
    }
  } { node =>
    new RlpList(
      (node.children.map(nodeToRlp) :+ node.value.map(vEv.encode).getOrElse(RlpString.create(Array.emptyByteArray)))*
    )
  }

  val listToPairPrism = Prism[RlpList, (RlpType, RlpType)] { list =>
    val size = list.getValues().size()
    if (size == 2) {
      val encodedKey = list.getValues().get(0)
      val encodedValue = list.getValues().get(1)
      Some((encodedKey, encodedValue))
    } else {
      None
    }
  } { case (encodedKey, encodedValue) =>
    new RlpList(encodedKey, encodedValue)
  }

  val bytesRlpList = Prism[Array[Byte], RlpType] { bytes =>
    if (bytes.isEmpty) Some(RlpDecoder.decode(bytes))
    else
      Some(RlpDecoder.decode(bytes).getValues().get(0))
  } { case (elt) =>
    RlpEncoder.encode(elt)
  }

  val byteToLeafPrism = bytesRlpList
    .andThen(rlpListPrism)
    .andThen(listToPairPrism)
    .andThen(rlpLeaf)

  val byteToBranch = bytesRlpList
    .andThen(rlpListPrism)
    .andThen(rlpBranch)

  val byteToExtension = bytesRlpList
    .andThen(rlpListPrism)
    .andThen(listToPairPrism)
    .andThen(rlpExtension)

}
