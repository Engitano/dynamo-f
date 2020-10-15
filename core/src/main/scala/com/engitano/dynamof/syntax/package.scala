package com.engitano.dynamof 

package object syntax {

  object freeApplicative    extends FreeApplicativeOpsSyntax
  object dynamoOps          extends DynamoOpsSyntax
  object table              extends TableSyntax
  object isPrimaryKey       extends IsPrimaryKeySyntax
  object filter             extends FilterSyntax
  object scalarAttributes   extends ToScalarAttrInstances
  object all                extends AllSyntax
}
