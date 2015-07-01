module Backend.VHDL.Examples where

import Backend.VHDL.Syntax
import Backend.VHDL.Pretty

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
    ([])
    (Nothing)
  )

body :: ArchitectureBody
body =
  (ArchitectureBody
    (Ident "Behavioural")
    (NSimple
      (Ident "even")
    )
    []
    [ (ConSignalAss
        (CSASCond
          (Nothing)
          (False)
          (ConditionalSignalAssignment
            (TargetName
              (NSimple
                (Ident "o")
              )
            )
            (Options
              (False)
              (Nothing)
            )
            (ConditionalWaveforms
              []
              ( (WaveElem
                  [(WaveEExp
                     (ENand
                       (Relation
                         (ShiftExpression
                           (SimpleExpression
                             (Nothing)
                             (Term
                               (FacNot
                                 (PrimExp
                                   (xors)
                                 )
                               )
                               []
                             )
                             []
                           )
                           (Nothing)
                         )
                         (Nothing)
                       )
                       (Nothing)
                     )                     
                     (Nothing)
                  )]
                )
              , (Nothing)
              )
            )
          )
        )
      )
    ]
  )
  where
    xors :: Expression
    xors =
      (EXor
        [ (Relation
            (shifte "a")
            (Nothing)
          )
        , (Relation
            (shifte "b")
            (Nothing)
          )
        , (Relation
            (shifte "c")
            (Nothing)
          )
        ]
      )

    shifte :: String -> ShiftExpression
    shifte name =
      (ShiftExpression
        (SimpleExpression
          (Nothing)
          (Term
            (FacPrim
              (PrimName
                (NSimple
                  (Ident name)
                )
              )
              (Nothing)
            )
            []
          )
          []
        )
        (Nothing)
      )

--------------------------------------------------------------------------------
--

















body2 :: ArchitectureBody
body2 = undefined

{-



-}
