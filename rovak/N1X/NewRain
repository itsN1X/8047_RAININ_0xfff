package io.payonics.services

import akka.actor.FSM.->
import io.payonics.models.{Platform, Tip, PlatformWallet, WalletTransfer}
import io.payonics.telegram.models.{TelegramGroupUser, TelegramGroupUserRepository}
import io.payonics.utils
import scala.util.Random

class AirdropService {

  def buildRainDrop(from: PlatformWallet, users: List[TelegramGroupUser], amount: Long, token: String) = {

    val (distributedAmount, amountLeftFromDistribution) = Random.shuffle(users).foldLeft((Map[Int, Double](), amount)) {
      case ((userList, amountLeft), nextUser) =>
        if (amountLeft > 1) {
          (userList ++ Map(nextUser.userId -> 1D), amountLeft - 1)
        } else {
          (userList ++ Map(nextUser.userId -> 0D), amountLeft)
        }
    }

    var initialDistribution = distributedAmount
    val highestValue = Math.floor(amount.toDouble / users.size).toInt
    val userValues = initialDistribution.keys.toList
    var remainingAirdrop = amountLeftFromDistribution

    for  {
      i <- 0 to (userValues.size * 3)
      if remainingAirdrop > 0
    } {
      val randomUser = utils.Random.getRandomElement(userValues)
      val currentAmount = initialDistribution(randomUser)
      val highestAmountToDrop = if (highestValue < remainingAirdrop) highestValue else remainingAirdrop
      val randomAmount = utils.Random.nextInt(0, highestAmountToDrop.toInt)
      remainingAirdrop -= randomAmount
      initialDistribution += randomUser -> (currentAmount + randomAmount)
    }

    initialDistribution.map {
      case (userId, rainAmount) =>
        WalletTransfer(from, PlatformWallet(Platform.telegram, userId.toString), Tip(rainAmount, token))
    }.toList
  }

  def buildAirdrop(from: PlatformWallet, users: List[TelegramGroupUser], amount: Long, token: String) = {
    val userAmount = Math.floor(amount.toDouble / users.size)
    users.map(user => WalletTransfer(from, PlatformWallet(Platform.telegram, user.userId.toString), Tip(userAmount, token)))
  }
}