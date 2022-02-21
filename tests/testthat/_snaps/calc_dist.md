# ct_dist() output has not changed

    Code
      ct_dist()
    Output
                     p   vac   inf  symp  test detect
       1: 0.0035416667  TRUE  TRUE  TRUE  TRUE   TRUE
       2: 0.0006250000  TRUE  TRUE  TRUE  TRUE  FALSE
       3: 0.0000000000  TRUE  TRUE  TRUE FALSE   TRUE
       4: 0.0000000000  TRUE  TRUE  TRUE FALSE  FALSE
       5: 0.0000000000  TRUE  TRUE FALSE  TRUE   TRUE
       6: 0.0000000000  TRUE  TRUE FALSE  TRUE  FALSE
       7: 0.0000000000  TRUE  TRUE FALSE FALSE   TRUE
       8: 0.0125000000  TRUE  TRUE FALSE FALSE  FALSE
       9: 0.0000000000  TRUE FALSE  TRUE  TRUE   TRUE
      10: 0.0000000000  TRUE FALSE  TRUE  TRUE  FALSE
      11: 0.0000000000  TRUE FALSE  TRUE FALSE   TRUE
      12: 0.0000000000  TRUE FALSE  TRUE FALSE  FALSE
      13: 0.0000000000  TRUE FALSE FALSE  TRUE   TRUE
      14: 0.0000000000  TRUE FALSE FALSE  TRUE  FALSE
      15: 0.0000000000  TRUE FALSE FALSE FALSE   TRUE
      16: 0.4833333333  TRUE FALSE FALSE FALSE  FALSE
      17: 0.0070833333 FALSE  TRUE  TRUE  TRUE   TRUE
      18: 0.0012500000 FALSE  TRUE  TRUE  TRUE  FALSE
      19: 0.0000000000 FALSE  TRUE  TRUE FALSE   TRUE
      20: 0.0000000000 FALSE  TRUE  TRUE FALSE  FALSE
      21: 0.0030357143 FALSE  TRUE FALSE  TRUE   TRUE
      22: 0.0005357143 FALSE  TRUE FALSE  TRUE  FALSE
      23: 0.0000000000 FALSE  TRUE FALSE FALSE   TRUE
      24: 0.0214285714 FALSE  TRUE FALSE FALSE  FALSE
      25: 0.0000000000 FALSE FALSE  TRUE  TRUE   TRUE
      26: 0.0000000000 FALSE FALSE  TRUE  TRUE  FALSE
      27: 0.0000000000 FALSE FALSE  TRUE FALSE   TRUE
      28: 0.0000000000 FALSE FALSE  TRUE FALSE  FALSE
      29: 0.0000000000 FALSE FALSE FALSE  TRUE   TRUE
      30: 0.0666666667 FALSE FALSE FALSE  TRUE  FALSE
      31: 0.0000000000 FALSE FALSE FALSE FALSE   TRUE
      32: 0.4000000000 FALSE FALSE FALSE FALSE  FALSE
                     p   vac   inf  symp  test detect

# ct_dist() works with partial args

    Code
      data
    Output
                     p   vac   inf  symp  test detect
       1: 0.0069416667  TRUE  TRUE  TRUE  TRUE   TRUE
       2: 0.0012250000  TRUE  TRUE  TRUE  TRUE  FALSE
       3: 0.0000000000  TRUE  TRUE  TRUE FALSE   TRUE
       4: 0.0000000000  TRUE  TRUE  TRUE FALSE  FALSE
       5: 0.0042145833  TRUE  TRUE FALSE  TRUE   TRUE
       6: 0.0007437500  TRUE  TRUE FALSE  TRUE  FALSE
       7: 0.0000000000  TRUE  TRUE FALSE FALSE   TRUE
       8: 0.0148750000  TRUE  TRUE FALSE FALSE  FALSE
       9: 0.0002016000  TRUE FALSE  TRUE  TRUE   TRUE
      10: 0.0065184000  TRUE FALSE  TRUE  TRUE  FALSE
      11: 0.0000000000  TRUE FALSE  TRUE FALSE   TRUE
      12: 0.0000000000  TRUE FALSE  TRUE FALSE  FALSE
      13: 0.0049896000  TRUE FALSE FALSE  TRUE   TRUE
      14: 0.1613304000  TRUE FALSE FALSE  TRUE  FALSE
      15: 0.0000000000  TRUE FALSE FALSE FALSE   TRUE
      16: 0.4989600000  TRUE FALSE FALSE FALSE  FALSE
      17: 0.0059500000 FALSE  TRUE  TRUE  TRUE   TRUE
      18: 0.0010500000 FALSE  TRUE  TRUE  TRUE  FALSE
      19: 0.0000000000 FALSE  TRUE  TRUE FALSE   TRUE
      20: 0.0000000000 FALSE  TRUE  TRUE FALSE  FALSE
      21: 0.0020642857 FALSE  TRUE FALSE  TRUE   TRUE
      22: 0.0003642857 FALSE  TRUE FALSE  TRUE  FALSE
      23: 0.0000000000 FALSE  TRUE FALSE FALSE   TRUE
      24: 0.0145714286 FALSE  TRUE FALSE FALSE  FALSE
      25: 0.0000828000 FALSE FALSE  TRUE  TRUE   TRUE
      26: 0.0026772000 FALSE FALSE  TRUE  TRUE  FALSE
      27: 0.0000000000 FALSE FALSE  TRUE FALSE   TRUE
      28: 0.0000000000 FALSE FALSE  TRUE FALSE  FALSE
      29: 0.0011710286 FALSE FALSE FALSE  TRUE   TRUE
      30: 0.0378632571 FALSE FALSE FALSE  TRUE  FALSE
      31: 0.0000000000 FALSE FALSE FALSE FALSE   TRUE
      32: 0.2342057143 FALSE FALSE FALSE FALSE  FALSE
                     p   vac   inf  symp  test detect

---

    Code
      attrs
    Output
      $names
      [1] "p"      "vac"    "inf"    "symp"   "test"   "detect"
      
      $class
      [1] "data.table" "data.frame"
      
      $row.names
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
      [26] 26 27 28 29 30 31 32
      
      $params
      $params$vac
      $params$vac$p_comm
      [1] 0.5
      
      $params$vac$eff
      [1] 0.5
      
      $params$vac$p_org
      [1] 0.7
      
      
      $params$inf
      $params$inf$p_incid
      [1] 0.005
      
      $params$inf$t_presymp
      [1] 5
      
      $params$inf$t_symp
      [1] 7
      
      
      $params$symp
      $params$symp$p_inf_vac
      [1] 0.5
      
      $params$symp$p_inf_unvac
      [1] 0.5
      
      $params$symp$p_uninf
      [1] 0.01
      
      
      $params$test
      $params$test$p_symp
      [1] 1
      
      $params$test$p_asymp_unvac
      [1] 0.1428571
      
      $params$test$p_asymp_vac
      [1] 0.25
      
      
      $params$detect
      $params$detect$sens
      [1] 0.85
      
      $params$detect$spec
      [1] 0.97
      
      
      

