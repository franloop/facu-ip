module TestStock where

import Stock
import Test.HUnit

runTestStock = runTestTT testStock

testStock = test [
    " Casobase 1: getMercaderia 1" ~: (getMercaderia 1) ~?= ["Mesa", "Silla", "Tablet"]
    ]

