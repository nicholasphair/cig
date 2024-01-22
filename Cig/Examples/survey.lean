import Cig.Generator.generator

open Lean Json ToJson FromJson
open Concepts

-- NB (nphair): Our type signatures are too strict. Not every action should require α input.
def Thermometer { R: Type} [ToConceptImpl R] (r: R): (Concept R) := {
    name := "thermometer",
    purpose := "collect sensor readings",
    operationalPrinciple := "after submit(r), and then r in reports",
    actions := #[
      {
        name := "submit",
        args := #[{name:= "r", type := r}]
        ret := r
      },
      {
        name := "init",
        args := #[]
        ret := r
      }
    ]
  }

def ThermometerNumber := Thermometer ConceptPrimitiveTypes.Number
#eval IO.println (toJson ThermometerNumber)


structure MDT where
  (timestamp: Nat)
  (submitterId: String)
  (temperature: Nat)

instance: ToConceptImpl MDT where
  toSchema (_: MDT) := Json.mkObj [
    ("type", "object"),
    ("properties", Json.mkObj [
      ("submitterId", Json.mkObj [("type", "number")]),
      ("temperature", Json.mkObj [("type", "string")]),
      ("timestamp", Json.mkObj [("type", "number")])
    ])
  ]

-- NB (nphair): Oof, this is not the interface I was after.
-- I think my vars need to be bumped a universe.
def ThermometerMDT := Thermometer (MDT.mk 1 "" 2)
#eval IO.println (toJson ThermometerMDT)
