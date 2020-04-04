module Lang where

import Data.Proxy

class Lang a
instance Lang CPP
instance Lang Python

data CPP
data Python

cpp :: Proxy CPP
cpp = Proxy

python :: Proxy Python
python = Proxy
