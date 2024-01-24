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
| UserDefined (kvs: List (String × CTypes)) : CTypes

-- Need `cytpeToJsonSchemaAux` and `toProperties` to show Lean our function terminates.
def ctypeToJsonSchemaAux : CTypes → Json
| CTypes.Nat => Json.mkObj [("type", "number")]
| CTypes.Bool => Json.mkObj [("type", "boolean")]
| CTypes.String => Json.mkObj [("type", "string")]
| _ => Json.mkObj [("error", "NotImplemented")]

def toProperties: List (String × CTypes) → Json
| (s, t)::tail => Json.mergeObj (Json.mkObj [(s, ctypeToJsonSchemaAux t)]) (toProperties tail)
| [] => Json.mkObj []

def ctypeToJsonSchema: CTypes → Json
| CTypes.UserDefined (kvs: List (String × CTypes)) => Json.mkObj [("type", "object"), ("properties", toProperties kvs)]
| x => ctypeToJsonSchemaAux x

instance: ToSchema CTypes :=
 ⟨ctypeToJsonSchema⟩

structure Action where
  name: String
  args: Array (String × CTypes)
  ret: CTypes

def toPair: (String × CTypes) → (String × Json)
| (name, type) => (name, ToSchema.toJsonSchema type)

def responseForRet(t: CTypes): Json :=
  Json.mkObj [
    ("responses", match t with
      | CTypes.Unit => Json.mkObj [
          ("200", Json.mkObj [("description", "200")])]
      | ret => Json.mkObj [
          ("200", Json.mkObj [
            ("description", "200"),
            ("content", Json.mkObj [
              ("application/json", Json.mkObj [
                ("schema", ToSchema.toJsonSchema ret)
              ])
            ])
          ])
        ]
    )
  ]

def requestBodyForArgs: Array (String × CTypes) → Json
| #[] => Json.mkObj []
| args => Json.mkObj [
  ("requestBody", Json.mkObj [
    ("content", Json.mkObj [
      ("application/json", Json.mkObj [
        ("schema", Json.mkObj [
          ( "properties", (Json.mkObj (Array.map toPair args).toList))
        ])
      ])
    ])
  ])
]

instance: ToConceptImpl Action where
  toOpenAPISpec : Action -> Json
  | {name:=n, args:=a, ret:=r} =>
    Json.mkObj [
      (s!"/{n}", Json.mkObj [
        ("post", Json.mergeObj
          (requestBodyForArgs a)
          (responseForRet r)
        )
      ])
    ]

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
        ("title", n ),
        ("description", p ++ "\n" ++ op),
        ("version", "latest")
      ]),
      ("paths", Array.foldl Json.mergeObj (Json.mkObj []) (Array.map ToConceptImpl.toOpenAPISpec a))
    ]

end Concepts
