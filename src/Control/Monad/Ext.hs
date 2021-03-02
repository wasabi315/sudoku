{-# LANGUAGE LambdaCase #-}

module Control.Monad.Ext (foldZipWithM) where

foldZipWithM :: (Monad m, Monoid c) => (a -> b -> m c) -> [a] -> [b] -> m c
foldZipWithM f =
  foldr step (const (pure mempty))
  where
    step a k =
      \case
        [] -> pure mempty
        b : bs -> do
          c1 <- f a b
          c2 <- k bs
          pure $! c1 <> c2
{-# INLINE foldZipWithM #-}
