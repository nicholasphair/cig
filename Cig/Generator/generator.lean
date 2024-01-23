import Lean
import Lean.Data.Json.Basic
import Lean.Data.Json.Parser
import Lean.Data.Json.Printer

open Lean Json ToJson FromJson

namespace Concepts

class ToSchema (a: Type u) where
  toJsonSchema : a → Json

class ToConceptImpl (a: Type u) where
  toOpenAPISpec : a → Json

-- A language of types.
inductive CTypes
| Nat: CTypes
| Bool: CTypes
| String: CTypes
| Unit: CTypes
| Ptr (c: CTypes) : CTypes
| Func (arg : CTypes) (ret: CTypes): CTypes
| UserDefined (adt : Type) : CTypes

instance: ToSchema CTypes where
  toJsonSchema : CTypes → Json
  | CTypes.Nat => Json.mkObj [("type", "number")]
  | CTypes.Bool => Json.mkObj [("type", "boolean")]
  | CTypes.String => Json.mkObj [("type", "string")]
  | _ => Json.mkObj [("error", "NotImplemented")]

structure Action where
  name: String
  args: Array (String × CTypes)
  ret: CTypes

def toPair: (String × CTypes) → (String × Json)
| (name, type) => (name, ToSchema.toJsonSchema type)

instance: ToConceptImpl Action where
  toOpenAPISpec : Action -> Json
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
                  "schema", ToSchema.toJsonSchema r
                )]
              )]
            )]
          )]
        )])
    ]
    )]

-- NB (nphair): This might be too agressive.
instance [ToConceptImpl α]: ToConceptImpl (Array α) where
  toOpenAPISpec x := Json.arr (Array.map ToConceptImpl.toOpenAPISpec x)

structure Concept where
  (name : String)
  (purpose : String)
  (actions : Array Action)
  (operationalPrinciple : String)

instance: ToConceptImpl Concept where
  toOpenAPISpec: Concept -> Json
  | {name:=n,purpose:=p, actions:=a, operationalPrinciple:=op} =>
    Json.mkObj [
      ("openapi", "3.0.1"),
      ("info", Json.mkObj [
        ("name", n ),
        ("description", p ++ "\n" ++ op)
      ]),
      ("paths", ToConceptImpl.toOpenAPISpec a)
    ]

end Concepts
