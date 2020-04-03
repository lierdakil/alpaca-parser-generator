module Lang where

import Data.Proxy

data CPP
data Python

cpp :: Proxy CPP
cpp = Proxy

python :: Proxy Python
python = Proxy
