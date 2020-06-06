module Lang where

import Data.Proxy

class Lang a
instance Lang CPP
instance Lang Python
instance Lang CSharp
instance Lang JS

data CPP
data Python
data CSharp
data JS

cpp :: Proxy CPP
cpp = Proxy

python :: Proxy Python
python = Proxy

csharp :: Proxy CSharp
csharp = Proxy

js :: Proxy JS
js = Proxy
