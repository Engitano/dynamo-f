package com.engitano.dynamof

package object syntax {
    
    trait AllSyntax extends TableSyntax with ToKeySyntax with HasScalarAttributeReprInstances

    object table extends TableSyntax
    object toKey extends ToKeySyntax
    object scalarAttributes extends HasScalarAttributeReprInstances
    object completableFutures extends CompletableFutureSyntax
    object all extends AllSyntax
}

