

package object ttfi {
  /*

  Initial and final, deep and shallow: the first-order case

  Intro1
  Algebraic data type/initial representation of expressions
  Constructor functions: the intimation of the final representation (or, shallow embedding)

  Intro2
  Symantics: parameterization of terms by interpreters

  Intro3
  Initial and Final, Deep and Shallow, First-class

  ExtI
  Algebraic data types are indeed not extensible

  ExtF
  Adding a new expression form to the final view: solving the expression problem

  Serialize
  Serialization and de-serialization

  SerializeExt
  De-serializing the extended language

  PushNegI
  Pushing the negation down: the initial implementation

  PushNegF
  Pushing the negation down: the final implementation

  PushNegFExt
  Pushing the negation down for extended tagless-final terms

  FlatI
  FlatF
  Flattening of additions, the initial and the final implementations

  PushNegFI

   */
  val modules = List(
    Intro1
    , Intro2
    , Intro3
    , ExtI
    , ExtF
    , Serialize
    , SerializeExt
    , PushNegI
    , PushNegF
    , PushNegFExt
    , FlatI
    , FlatF
    , PushNegFI
    , IntroHOT
    , IntroHOIF
    , TTFdB
    /*
    , TTF
    , TTIF
    , TTFdBHO
    , TypeCheck
    , Typ
    , CPS
    , TDPE
    , ToTDPE
    , LinearLC
    , HOCCG
    , LG
    , CBAny
    , CB98
    , PrintScanF
    */
  )
}

