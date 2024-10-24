package org.plasmalabs

package object typeclasses {

  object implicits
      extends ContainsTransactions.Instances
      with ContainsTransactions.ToContainsTransactionsOps
      with ContainsTransactionIds.Instances
      with ContainsTransactionIds.ToContainsTransactionIdsOps
      with ShowInstances
      with EqInstances
      with JsonInstances
      with TransactionsOps
}
