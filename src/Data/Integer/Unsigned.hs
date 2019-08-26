{-
Cider - A network calculator
Copyright (C) 2019 Michael Dippery <michael@monkey-robot.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}


{-|
  Module      : Data.Integer.Unsigned
  Description : Unsigned integers as data
  License     : LGPL-3
  Maintainer  : michael@monkey-robot.com

  Work with unsigned integer values as data.
-}
module Data.Integer.Unsigned
  (
    -- * Data types
    Packable(..)
  ) where

import Data.Word (Word32)

-- | A data type that can be represented as an unsigned integer.
class Packable a where
  -- | The unsigned integer representation of the data type.
  unpack :: a -> Word32
