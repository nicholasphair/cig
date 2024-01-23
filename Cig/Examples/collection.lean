import Cig.Generator.generator

open Lean Json ToJson FromJson
open Concepts
open ToConceptImpl

-- def Reading: (Concept Type) := {
--     name := "collection",
--     purpose := "collection",
--     operationalPrinciple := "",
--     actions := #[
--       {
--         name := "add",
--         args := #[{name:= "r", type := r}]
--         ret := r
--       },
--       {
--         name := "remove",
--         args := #[{name:= "r", type := r}]
--         ret := r
--       },
--       {
--         name := "contains",
--         args := #[{name:= "r", type := r}]
--         ret := r
--       },
--       {
--         name := "init",
--         args := #[]
--         ret := r
--       }
--     ]
--   }

def Collection (R: CTypes): Concept := {
    name := "collection",
    purpose := "collection",
    operationalPrinciple := "",
    actions := #[
      {
        name := "add",
        args := #[("r", R)]
        ret := CTypes.Unit
      },
      {
        name := "remove",
        args := #[("r", R)]
        ret := R
      },
      {
        name := "contains",
        args := #[("r", R)]
        ret := CTypes.Bool
      },
      {
        name := "init",
        args := #[]
        ret := CTypes.Unit
      }
    ]
  }

def CollectionR := Collection CTypes.Bool
#eval IO.println (toOpenAPISpec CollectionR)
