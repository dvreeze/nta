/*
 * Copyright 2011-2018 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eu.cdevreeze.nta.common.validator

/**
 * Individual validation result.
 *
 * TODO Improve, especially the message.
 *
 * @author Chris de Vreeze
 */
final case class Result(ruleName: String, code: String, level: Level, message: String)

object Result {

  def makeErrorResult(ruleName: String, code: String, message: String): Result = {
    apply(ruleName, code, Level.Error, message)
  }

  def makeWarningResult(ruleName: String, code: String, message: String): Result = {
    apply(ruleName, code, Level.Warning, message)
  }

  def makeOkResult(ruleName: String, code: String, message: String): Result = {
    apply(ruleName, code, Level.Ok, message)
  }
}
