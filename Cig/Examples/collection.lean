import Cig.Generator.generator

open Lean Json ToJson FromJson
open Concepts
open ToConceptImpl

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


def ItemType: CTypes := CTypes.UserDefined [("val", CTypes.Nat)]
def CollectionItems := Collection ItemType
#eval IO.println (toOpenAPISpec CollectionItems)

def ReportType: CTypes := CTypes.UserDefined [("score", CTypes.Nat), ("ts", CTypes.Nat), ("id", CTypes.String)]
def CollectionReports := Collection ReportType
#eval IO.println (toOpenAPISpec CollectionReports)
