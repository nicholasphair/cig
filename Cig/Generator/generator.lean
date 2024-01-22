import Lean
import Lean.Data.Json.Basic
import Lean.Data.Json.Parser
import Lean.Data.Json.Printer
-- can I invoke openapi directly for demo tomorrow?

open Lean Json ToJson FromJson

def newline := "\n"

namespace Concepts

class ToConceptImpl (α : Type) where
  toSchema : α → Lean.Json  -- need JSON Schema repr for openAPI.

inductive ConceptPrimitiveTypes
| Number : ConceptPrimitiveTypes
| Boolean : ConceptPrimitiveTypes
| String : ConceptPrimitiveTypes

instance: ToConceptImpl ConceptPrimitiveTypes where
  toSchema : ConceptPrimitiveTypes → Json
  | ConceptPrimitiveTypes.Number => Json.mkObj [("type", "number")]
  | ConceptPrimitiveTypes.Boolean => Json.mkObj [("type", "boolean")]
  | ConceptPrimitiveTypes.String => Json.mkObj [("type", "string")]

instance [ToConceptImpl α]: ToConceptImpl (Array α) where
  toSchema x := Json.arr (Array.map ToConceptImpl.toSchema x)

-- NB (nphair) I do not care the argument type so long as I can represent it with a JSON schema.
--  Not just as JSON, but as JSON Schema.
structure Var (α : Type) [ToConceptImpl α] where
  name: String
  type: α

instance [ToConceptImpl α]: ToJson (Var α)  where
  toJson : (Var α) → Json
  | {name:= n, type:=t}=> Json.mkObj [(n, ToConceptImpl.toSchema t)]

-- NB (nphair) I think I want to be chaining instances. That is, when needed, first convert a structure to a JSON Schema
--  then convert that to JSON. Don't yet know how to do that in Lean though.
structure AAction (α : Type) [ToConceptImpl α] where
  name: String
  args: Array (Var α)
  ret: α


def toPair [ToConceptImpl α] (v : Var α) : (String × Json) := (v.name, ToConceptImpl.toSchema v.type)

instance [ToConceptImpl α]: ToJson (AAction α)  where
  toJson : (AAction α) -> Json
  | {name:=n, args:=a, ret:=r} =>
    Json.mkObj [(
      s!"/{n}", Json.mkObj [(
        "/get", Json.mkObj [(
          "requestBody", Json.mkObj [(
            "content", Json.mkObj [(
              "application/json", Json.mkObj [(
                "schema", Json.mkObj [(
                  "properties", (Json.mkObj (Array.map toPair a).toList)
                )]
              )]
            )]
          )]
      ), (
          "responses", Json.mkObj [(
            "'200'", Json.mkObj [(
              "content", Json.mkObj [(
                "application/json", Json.mkObj [(
                  "schema", ToConceptImpl.toSchema r
                )]
              )]
            )]
          )]
        )])
    ]
    )]

def actionExample: (AAction ConceptPrimitiveTypes):= {
  name := "name",
  args := #[{name:= "foo", type := ConceptPrimitiveTypes.Number}]
  ret := ConceptPrimitiveTypes.Number
}
#eval toJson actionExample

def actionArrayExample: Array (AAction ConceptPrimitiveTypes) := #[ actionExample ]
#eval toJson actionArrayExample


structure Concept (α : Type) [ToConceptImpl α]where
  (name : String)
  (purpose : String)
  (actions : Array (AAction α))
  (operationalPrinciple : String)

instance [ToConceptImpl α]: ToJson (Concept α)  where
  toJson: (Concept α) -> Json
  | {name:=n,purpose:=p, actions:=a, operationalPrinciple:=op} =>
    Json.mkObj [
      ("openapi", "3.0.1"),
      ("info", Json.mkObj [
        ("name", n ),
        ("description", p ++ newline ++ op)
      ]),
      ("paths", toJson a)
    ]
end Concepts
