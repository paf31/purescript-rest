-- | Helper methods for dealing with JSON data.
module REST.JSON where

-- | Pretty-print JSON with spaces and new-lines
foreign import prettyJSON :: forall a. a -> String
