module Lang where

import Data.Proxy

class Lang (a :: LangS) where
  demote :: Proxy a -> LangS
instance Lang CPP where
  demote _ = CPP
instance Lang Python where
  demote _ = Python
instance Lang CSharp where
  demote _ = CSharp
instance Lang JS where
  demote _ = JS

data LangS = CPP | Python | CSharp | JS

cpp :: Proxy CPP
cpp = Proxy

python :: Proxy Python
python = Proxy

csharp :: Proxy CSharp
csharp = Proxy

js :: Proxy JS
js = Proxy
