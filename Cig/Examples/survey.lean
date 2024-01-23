import Cig.Generator.generator

open Concepts

def survey: Concept := {
  name:= "survey",
  purpose := "survey's purpose",
  operationalPrinciple := "op",
  actions := #[
    {name:="action1", ret:=CTypes.Bool, args:=#[("mybool", CTypes.Bool)]}
  ]
}

def survey2 (α: CTypes): Concept := {
  name:= "survey",
  purpose := "survey's purpose",
  operationalPrinciple := "op",
  actions := #[
    {name:="action1", ret:=α, args:=#[("mybool", α)]}
  ]
}

#eval (ToConceptImpl.toOpenAPISpec survey)
#eval (ToConceptImpl.toOpenAPISpec (survey2 CTypes.Nat))
