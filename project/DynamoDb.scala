import sbt.Keys._
import sbt._
import com.localytics.sbt.dynamodb.DynamoDBLocalKeys._

object Dynamo {
    val settings = Seq(
      startDynamoDBLocal := startDynamoDBLocal.dependsOn(compile in IntegrationTest).value,
      test in IntegrationTest := (test in IntegrationTest).dependsOn(startDynamoDBLocal).value,
      testOnly in IntegrationTest := (testOnly in IntegrationTest).dependsOn(startDynamoDBLocal).evaluated,
      testOptions in IntegrationTest += dynamoDBLocalTestCleanup.value
    )
  }
  