module Text.PrettyPrint.Leijen.Text (Pretty (..)) where

data Doc

class Pretty a where
  pretty :: a -> Doc
