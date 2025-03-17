I have a project call geomates
the pdf is the project request


Name               Att  Loc               VALUE       SIZE        KIND     SIDES  REGULAR  COLOR  WIDTH  HEIGHT  DIAMONDS  ROTATION  OVAL  RADIUS  
-----------------  ---  ----------------  ----------  ----------  -------  -----  -------  -----  -----  ------  --------  --------  ----  ------
POLYGON-FEATURE16  NEW  ( 0.5 20.5 1080)  "platform"  0.11        POLYGON  4      TRUE     BLACK  1      39                                        
OVAL1              NEW  (10.0 22.5 1080)  "disc"      1.0                                                        0                   T     1.5     
POLYGON-FEATURE18  NEW  (15.5 20.5 1080)  "platform"  0.08        POLYGON  4      TRUE     BLACK  29     1                                         
POLYGON-FEATURE11  NEW  (20.0 26.0 1080)  "diamond"   1.0         POLYGON                                                                          
POLYGON-FEATURE13  NEW  (35.0 10.0 1080)  "diamond"   1.0         POLYGON                                                                          
POLYGON-FEATURE14  NEW  (40.0  0.5 1080)  "platform"  0.22999999  POLYGON  4      TRUE     BLACK  80     1                                         
POLYGON-FEATURE10  NEW  (40.0 22.0 1080)  "rect"      0.17999999  POLYGON  4      TRUE     RED    32.0   2.0     0         -0.0                    
POLYGON-FEATURE15  NEW  (40.0 39.5 1080)  "platform"  0.22999999  POLYGON  4      TRUE     BLACK  80     1                                         
POLYGON-FEATURE19  NEW  (59.5 20.5 1080)  "platform"  0.11        POLYGON  4      TRUE     BLACK  39     1                                         
POLYGON-FEATURE12  NEW  (60.0 26.0 1080)  "diamond"   1.0         POLYGON                                                                          
POLYGON-FEATURE17  NEW  (79.5 20.5 1080)  "platform"  0.11        POLYGON  4      TRUE     BLACK  1      39                                        

right now I am block and sometimes when I move rect, the rect fall down to the platfrom

here is the step you could help me:
1 how to use visual and visual-location module to get the platfroms ?
2 where to save this inforamtion ?
3 when the rect could be extand witdh to cross the gap of the platforms ?


and my code is in model-dummy.lisp and navigation-functions , which I paste below index ``` 


and act-R visicon  is wrapped in ####

```

```

####
Name               Att  Loc               VALUE       SIZE        KIND     SIDES  REGULAR  COLOR  WIDTH  HEIGHT  DIAMONDS  ROTATION  OVAL  RADIUS  
-----------------  ---  ----------------  ----------  ----------  -------  -----  -------  -----  -----  ------  --------  --------  ----  ------
POLYGON-FEATURE16  NEW  ( 0.5 20.5 1080)  "platform"  0.11        POLYGON  4      TRUE     BLACK  1      39                                        
OVAL1              NEW  (10.0 22.5 1080)  "disc"      1.0                                                        0                   T     1.5     
POLYGON-FEATURE18  NEW  (15.5 20.5 1080)  "platform"  0.08        POLYGON  4      TRUE     BLACK  29     1                                         
POLYGON-FEATURE11  NEW  (20.0 26.0 1080)  "diamond"   1.0         POLYGON                                                                          
POLYGON-FEATURE13  NEW  (35.0 10.0 1080)  "diamond"   1.0         POLYGON                                                                          
POLYGON-FEATURE14  NEW  (40.0  0.5 1080)  "platform"  0.22999999  POLYGON  4      TRUE     BLACK  80     1                                         
POLYGON-FEATURE10  NEW  (40.0 22.0 1080)  "rect"      0.17999999  POLYGON  4      TRUE     RED    32.0   2.0     0         -0.0                    
POLYGON-FEATURE15  NEW  (40.0 39.5 1080)  "platform"  0.22999999  POLYGON  4      TRUE     BLACK  80     1                                         
POLYGON-FEATURE19  NEW  (59.5 20.5 1080)  "platform"  0.11        POLYGON  4      TRUE     BLACK  39     1                                         
POLYGON-FEATURE12  NEW  (60.0 26.0 1080)  "diamond"   1.0         POLYGON                                                                          
POLYGON-FEATURE17  NEW  (79.5 20.5 1080)  "platform"  0.11        POLYGON  4      TRUE     BLACK  1      39                                        
####