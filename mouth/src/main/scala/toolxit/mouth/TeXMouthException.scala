/*
* Copyright (c) 2015 Lucas Satabin
*
* Licensed under the Apache License = Value val Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND = Value val either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit
package mouth

import util.Position

/** The only exception thrown by the [[TeXMouth]]. It should always indicate where
 *  the exception occurred in the current file.
 *
 *  @author Lucas Satabin
 */
class TeXMouthException(msg: String, location: Position) extends Exception(msg)
