package com.engitano.dynamof

import cats.free.Free.liftF
import cats.free.FreeApplicative.lift
import cats.data.NonEmptyList

object Transactionally {
    
    def writeOp(item: WriteItem, items: WriteItem*) = TransactWriteRequest(NonEmptyList(item, items.toList))
    def writeP(item: WriteItem, items: WriteItem*)  = lift[DynamoOpA, Unit](writeOp(item, items: _*))
    def write(item: WriteItem, items: WriteItem*)   = liftF(writeP(item, items: _*))
}