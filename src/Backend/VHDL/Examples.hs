module Backend.VHDL.Examples where

import Backend.VHDL.Syntax

--------------------------------------------------------------------------------
-- * Parity checker

entity :: EntityDeclaration
entity =
  (EntityDeclaration
    (Ident "even")
    (EntityHeader
      (Nothing)
      (Just
        (PortClause
          (InterfaceList
            [(InterfaceSignalDeclaration
              [(Ident "a"), (Ident "b"), (Ident "c"), (Ident "o")]
              (Just (In))
              (SubtypeIndication
                (Nothing)
                (TMType
                  (NSimple
                    (Ident "STD_LOGIC")
                  )
                )
                (Nothing)
              )
              (False)
              (Nothing)
            )]
          )
        )
      )
    )
    (EntityDeclarativePart
      []
    )
    (Nothing)
  )

architecture1 :: ArchitectureBody
architecture1 =
  (ArchitectureBody
    (Ident "Behavioural")
    (NSimple
     (Ident "even")
    )
    []
    [(ConSignalAss
      (CSASCond
        (Nothing)
        (False)
        (ConditionalSignalAssignment
          (TargetName
            (NSimple (Ident "o"))
          )
          (Options
            (False)
            (Nothing)
          )
          (ConditionalWaveforms
            (WaveElem
              [(WaveEExp
                (EXor
                  [(Relation
                    (ShiftExpression
                      (SimpleExpression
                        (Nothing)
                        (Term
                          (FacPrim
                            (PrimName
                              (NSimple
                                (Ident "a")
                              )
                            )
                            (Nothing)
                          )
                          (Nothing)
                        )
                        (Nothing)
                      )
                      (Nothing)
                    )
                    (Nothing)
                   )
                  ,(Relation
                    (ShiftExpression
                      (SimpleExpression
                        (Nothing)
                        (Term
                          (FacPrim
                            (PrimName
                              (NSimple
                                (Ident "b")
                              )
                            )
                            (Nothing)
                          )
                          (Nothing)
                        )
                        (Nothing)
                      )
                      (Nothing)
                    )
                    (Nothing)
                   )
                  ,(Relation
                    (ShiftExpression
                      (SimpleExpression
                        (Nothing)
                        (Term
                          (FacPrim
                            (PrimName
                              (NSimple
                                (Ident "c")
                              )
                            )
                            (Nothing)
                          )
                          (Nothing)
                        )
                        (Nothing)
                      )
                      (Nothing)
                    )
                    (Nothing)
                   )
                  ]
                )
                (Nothing)
              )]
            , (Nothing)
            )
            []
          )
        )
      ))
    ]
  )
