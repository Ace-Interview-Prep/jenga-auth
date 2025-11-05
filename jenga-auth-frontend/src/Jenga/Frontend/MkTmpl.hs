module Jenga.Frontend.MkTmpl where

-- | TODO: use this for creating Login+Auth templates
data MkTemplate domCfg events t m a = MkTemplate
  { _domBuilder :: domCfg t m -> m (events t m)
  , _FRP :: events t m -> m (domCfg t m, a)
  -- ^ a represents an effect from the FRP that escapes the
  --   scope of this template
  }
