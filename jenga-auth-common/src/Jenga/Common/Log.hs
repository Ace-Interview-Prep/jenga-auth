module Jenga.Common.Log where

import Data.Text

class Loggable log where
  renderLog :: log -> Text
