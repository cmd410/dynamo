## This package provides a Variant type.
## This type implements a dynamic JavaScript-like (although more strict) typing system in Nim.
## 
## Primary usecase of this library being the creation of scripting languages
## in Nim, dynamo takes care of all the boilerplate code for supporting
## dynamic objects and their interactions.
runnableExamples:
  # Create some variant objects
  let n = nil.toVariant
  let i = 42.toVariant
  let f = 12.0.toVariant
  let b = true.toVariant
  
  # Lists and objects
  var l = @[1, 2, 3].toVariant
  var d = {"key": @[1, 2, 3]}.toVariant

  # Lists can contain any kind of variants
  l.add n
  doAssert (nil.toVariant in l).isTruthy()

  l.add i
  l.add f
  l.add (f + i)
  l.add b
  echo l  # @[1, 2, 3, null, 42, 12.0, 54.0, true]

  # Objects too
  d.key2 = l
  echo d  # {"key": @[1, 2, 3], "key2": @[1, 2, 3, null, 42, 12.0, 54.0, true]}

  # Implement custom operators easily
  func someOp[T: float | int](a, b: T): T = a + b * (a + b)
  
  binOps:
    someOp:
      # left side accepts argument Variant argument `a` of types float and int and assigns it's value to `x`
      left as a from [float, int] as x:
        int:  # when x is int
          when y is float:
            someOp(x.toFloat(),y)
          else:
            someOp(x,y)
      # right side accepts argument Variant argument `right` of types float and int and assigns it's value to `y`
      right from [float, int] as y:
        int:  # when y is int
          when x is float:
            someOp(x, y.toFloat())
          else:
            someOp(x,y)

  echo someOp(42.toVariant, 15.0)  # 897.0

import std/tables
import std/enumutils
import std/macros
import std/setutils
import std/sequtils
import std/strutils

type
  VariantCompatible* = concept v
    ## VariantCompatible type is something that can be converted to Variant
    v.toVariant() is Variant

  VariantType* = enum
    varNull = "Null"
    varInt = "Int"
    varFloat = "Float"
    varBool = "Bool"
    varStr = "Str"
    varList = "List"
    varRange = "Range"
    varObj = "Object"

  RangeTuple = tuple[rStart: int, rEnd: int]

  Null* = object
    ## A special value

  Variant* = object
    ## Single object type to represent
    ## all the possible object types
    case kind*: VariantType
    of varNull: discard
    of varInt:
      intValue: int
    of varFloat:
      floatValue: float
    of varBool:
      boolValue: bool
    of varStr:
      strValue: string
    of varList:
      listValue: seq[Variant]
    of varRange:
      rangeValue: RangeTuple
    of varObj:
      objValue: Table[string, Variant]
  
  DynamicTypeError* = object of ValueError

func toVariant*[T](v: T): Variant =
  ## Convert Nim value to Variant.
  ## 
  ## List of Compatible types:
  ## 
  ## - `nil`, `Null` - result in Null Variant
  ## - `int`, `float`, `bool`, `string` - converter to their respective variant types
  ## - `array` - converted to list or object depending on element type (i.e. key-value pairs)
  ## - `HSlice` and `RangeTuple` - converted to range Variant
  ## - `Table` - converted to object Variant
  ## - `Variant` - returned unchaged

  template objFromPairs(pairsIt: untyped): untyped =
    result = Variant(kind: varObj)
    for i in pairsIt:
      let key = when typeof(i[0]) is string: i[0]
                else: $i[0]
      result.objValue[key] = i[1].toVariant()

  when T is typeof(nil) or T is Null:
    result = Variant(kind: varNull)
  elif T is int:
    result = Variant(kind: varInt, intValue: v)
  elif T is float:
    result = Variant(kind: varFloat, floatValue: v)
  elif T is bool:
    result = Variant(kind: varBool, boolValue: v)
  elif T is string:
    result = Variant(kind: varStr, strValue: v)
  elif T is seq or T is array:
    when typeof(v[0]) is (string, auto):
      objFromPairs(v)
    else:
      result = Variant(kind: varList)
      for i in v:
        result.listValue.add i.toVariant()
  elif T is HSlice:
    result = Variant(kind: varRange, rangeValue: (rStart: v.a, rEnd: v.b))
  elif T is RangeTuple:
    result = Variant(kind: varRange, rangeValue: v)
  elif T is Table:
    objFromPairs(v.pairs)
  elif T is Variant:
    result = v

  else: {.error: "Can't convert to variant: " & $typeof(v).}

func value*[T](v: Variant): T =
  ## Get value of Variant as given type `T`.
  ## Makes assertion of variant kind, so only
  ## do this if you know the kind already.
  when T is int:
    assert v.kind == varInt
    result = v.intValue
  elif T is float:
    assert v.kind == varFloat
    result = v.floatValue
  elif T is bool:
    assert v.kind == varBool
    result = v.boolValue
  elif T is string:
    assert v.kind == varStr
    result = v.strValue
  elif T is seq[Variant]:
    assert v.kind == varList
    result = v.listValue
  elif T is RangeTuple:
    assert v.kind == varRange
    result = v.rangeValue
  elif T is Table[string, Variant]:
    assert v.kind == varObj
    result = v.objValue
  else: {.error: "Unsupported type conversion: " & $T.}

template asType*(v: Variant, kind: static[VariantType], name, body: untyped): untyped =
  ## Assign value of variant to `name` and do `body`. Used to generate minimal code 
  ## for cases when variant kind is known at compile-time.
  when kind == varNull:
    var name = Null()
    body
  elif kind == varInt:
    var name = v.intValue
    body
  elif kind == varFloat:
    var name = v.floatValue
    body
  elif kind == varBool:
    var name = v.boolValue
    body
  elif kind == varStr:
    var name = v.strValue
    body
  elif kind == varList:
    var name = v.listValue
    body
  elif kind == varRange:
    var name = v.rangeValue
    body
  elif kind == varObj:
    var name = v.objValue
    body
  else: {.error: "Can't get as kind " & $kind.}

macro onlyTypes*(v: Variant, kinds: static[set[VariantType]], name, body, others: untyped) =
  ## Generate switch case only for given variant kinds.
  ## Each `of` branch assigns value to `name` and runs `body`.
  ## If variant doesn't match given kinds, runs `others`
  name.expectKind(nnkIdent)
  body.expectKind(nnkStmtList)
  others.expectKind(nnkStmtList)
  
  result = newStmtList()
  var caseStmt = nnkCaseStmt.newTree(
    newDotExpr(
      v, ident("kind")
    )
  )

  for i in kinds:
    var branch = nnkOfBranch.newTree(ident(i.symbolName))
    var branchStmtList = newStmtList()
    
    branchStmtList.add quote do:
      `v`.asType(`i`.VariantType, `name`):
        `body`
    branch.add branchStmtList
    caseStmt.add branch

  if kinds != VariantType.fullSet():
    caseStmt.add nnkElse.newTree(others)
  result.add caseStmt

template everyType*(v: Variant, name, body, onNull: untyped): untyped =
  ## Generate case for each VariantType.
  ## Each of branch assigns value of variant to `name` and runs `body`.
  ## When VariantType == varNull, runs `onNull`. 
  v.onlyTypes({varInt..VariantType.high}, name):
    body
  do: onNull

func `$`*(v: Variant): string =
  v.everyType(x):
    result = $x
  do:
    result = "null"

func `-`*(v: Variant): Variant =
  ## Implements unary `-` for int and float variants
  v.onlyTypes({varInt, varFloat}, x):
    result = (-x).toVariant()
  do:
    raise DynamicTypeError.newException:
      "Unsupported operation for type " & $v.kind

func `+`*(v: Variant): Variant =
  ## Implements unary `+` for int and float variants
  v.onlyTypes({varInt, varFloat}, x):
    result = (+x).toVariant()
  do:
    raise DynamicTypeError.newException:
      "Unsupported operation for type " & $v.kind

func `not`*(v: Variant): Variant =
  ## Implements unary `not` for bool variants
  v.onlyTypes({varBool}, x):
    result = (not x).toVariant()
  do:
    raise DynamicTypeError.newException:
      "Unsupported operation for type " & $v.kind

macro binOps*(body: untyped) =
  ## A shortcut macro for creating binary operators
  ## between Variant types. Implements a sort of DSL
  ## for that purpose. As a result it generates a function
  ## with the most efficient case and when statements
  ## possible for variants provided.
  ## 
  ## Note that in `from [types] as name` and `type:` branches
  ## types are not exactly named as Nim types, for brewity purposes.
  ## Here is a full list of type abbreviations:
  ## - `nil`: for `varNull` Variants underlying value is `Null`
  ## - `int`: for `varInt` Variants underlying value is `int`
  ## - `float:` for `varFloat` Variants underlying value is `float`
  ## - `bool`: for `varFloat` Variants underlying value is `bool`
  ## - `string`: for `varStr` Variants underlying value is `string`
  ## - `list`: for `varList` Variants underlying value is `seq[Variant]`
  ## - `range`: for `varRange` Variants underlying value is `RangeTuple`
  ## - `object`: for `varObj` Variants underlying value is `Table[string, Variant]`
  ## - `any`: for any of the above. Note that if this branch is present, all others will be ignored.
  ## 
  ## Parameters of generated func will be named as `left` and `right` by default,
  ## but names can be overriden by replacing `left` with `left as anyNewNameYouLike`.
  runnableExamples:
    binOps:
      `+`:
        ## Implements `+` operation for Variant type.
        ## Implemented for any combination of int and float
        ## values. If one of operands is float, result is also float.
        
        # left side of operator accepts integer or float, assigns value to x
        left from [int, float] as x:
          int: # when x is integer
            # expression to get the result follows
            when y is float: x.toFloat() + y
            else:            x           + y
        
        # right side of operator accepts integer or float, assigns value to y
        right from [int, float] as y:
          int:# when y is integer
            # expression to get the result follows
            when x is float: x + y.toFloat()
            else:            x + y

  result = newStmtList()

  # Some lookup tables
  const idsToVarKind = {
    "nil": varNull,
    "int": varInt,
    "float": varFloat,
    "bool": varBool,
    "string": varStr,
    "list": varList,
    "range": varRange,
    "object": varObj
  }.toTable()

  const idsToNimType = {
    "nil": "Null",
    "int": "int",
    "float": "float",
    "bool": "bool",
    "string": "string",
    "list": "seq[Variant]",
    "range": "RangeTuple",
    "object": "Table[string, Variant]"
  }.toTable()
  
  body.expectKind(nnkStmtList)

  # Iterate over operator definitions
  for opDef in body:
    var commentsSeq: seq[NimNode] # collect comments here
    opDef.expectKind(nnkCall)
    opDef.expectLen(2)
    let opId = opDef[0]

    if opId.kind notin {nnkIdent, nnkAccQuoted}:
      error("Expected identifier", opId)

    let opBody = opDef[1]
    opBody.expectKind(nnkStmtList)

    # Collecting some data for func generation
    var
      # names to assign value to from each side of operator
      sideToAssgnName: Table[string, NimNode]
      # accepted types for each side
      sideAccTypes: Table[string, seq[NimNode]]
      #                           side          type    result expr
      sideTypeToResultExpr: Table[string, Table[string, NimNode        ]]
      # new names for func parameters, if any were overriden with `as`
      sideToRename: Table[string, NimNode]

    for sideBody in opBody:
      if sideBody.kind == nnkDiscardStmt: continue # ignore discards
      elif sideBody.kind == nnkCommentStmt:
        commentsSeq.add sideBody  # collect doc comments
        continue
      
      sideBody.expectKind(nnkInfix)
      sideBody.expectLen(4)
      sideBody[0].expectIdent("as")

      let fromInfix = sideBody[1]
      fromInfix.expectKind(nnkInfix)
      fromInfix.expectLen(3)
      fromInfix[0].expectIdent("from")
      

      var key: string   # either `left` or `right`
      case fromInfix[1].kind
      of nnkIdent:
        key = $fromInfix[1]
      of nnkInfix:    # Can be `as` infix for param renaming
        fromInfix[1][0].expectIdent("as")
        key = $fromInfix[1][1]
        sideToRename[key] = fromInfix[1][2]
      else:
        error("Unexpected node kind " & $fromInfix[1].kind, fromInfix[1])
      
      if key notin ["left", "right"]:
        error("Expected `left` or `right`", fromInfix[1])

      sideToAssgnName[key] = (sideBody[2].expectKind(nnkIdent);sideBody[2])
      
      # Now we handle the [..types..] part
      let typesNode = fromInfix[2]
      typesNode.expectKind(nnkBracket)
      typesNode.expectMinLen(1)
      
      for typeId in typesNode:
        # get type NimNode and force it be be ident
        var typeName: string
        case typeId.kind
        of nnkIdent: typeName = $typeId
        of nnkObjectTy: typeName = "object"
        of nnkNilLit: typeName = "nil"
        else:
          error("Unknown type. Expected one of " & $idsToVarKind.keys.toSeq, typeId)
        
        # add it to side to types table
        if key in sideAccTypes:
          sideAccTypes[key].add ident(typeName)
        else:
          sideAccTypes[key] = @[ident(typeName)]
      
      # Now parse `type: expr` branches
      let exprs = sideBody.last
      exprs.expectKind(nnkStmtList)
      for exprBranch in exprs:
        if exprBranch.kind == nnkDiscardStmt: # ignore discards
          continue
        exprBranch.expectKind(nnkCall)
        exprBranch.expectLen(2)
        let typ = 
          case exprBranch[0].kind
          of nnkObjectTy: ident("object")
          else: exprBranch[0]

        let exprBody = exprBranch[1]
        typ.expectKind(nnkIdent)
        exprBody.expectKind(nnkStmtList)
        if key in sideTypeToResultExpr:
          sideTypeToResultExpr[key][$typ] = exprBody
        else:
          sideTypeToResultExpr[key] = {$typ: exprBody}.toTable()
    
    # HERE IT GOES, We generate a function

    let
      # Lookup parameter name overrides, if none given, fallback to `left` and `right`
      leftVar = sideToRename.getOrDefault("left", ident("left"))
      rightVar = sideToRename.getOrDefault("right", ident("right"))
      
      # Lookup value assignments name overrides, if none, generate new Sym of `{side}Value`
      assgnLeft  = sideToAssgnName.getOrDefault("left",  genSym(NimSymKind.nskLet, "leftValue"))
      assgnRight = sideToAssgnName.getOrDefault("right", genSym(NimSymKind.nskLet, "leftValue"))
    
    leftVar.expectKind(nnkIdent)
    rightVar.expectKind(nnkIdent)

    # create NimNode of `VariantType` sets to feed into `onlyTypes` macro
    var leftKinds = nnkCurly.newTree()
    var rightKinds = nnkCurly.newTree()

    for typeIdent in sideAccTypes["left"]:
      if $typeIdent == "any":
        leftKinds = nnkCurly.newTree()
        for i in idsToVarKind.values:
          leftKinds.add ident(i.symbolName)
        break
      else:
        leftKinds.add ident(idsToVarKind[$typeIdent].symbolName)
  
    for typeIdent in sideAccTypes["right"]:
      if $typeIdent == "any":
        rightKinds = nnkCurly.newTree()
        for i in idsToVarKind.values:
          rightKinds.add ident(i.symbolName)
        break
      else:
        rightKinds.add ident(idsToVarKind[$typeIdent].symbolName)

    # create func definition
    let funcDef = nnkFuncDef.newTree(
      postfix(opId, "*"),
      newEmptyNode(), newEmptyNode(),
      nnkFormalParams.newTree(
        ident("Variant"),
        newIdentDefs(
          leftVar,
          ident("Variant"),
          newEmptyNode()
        ),
        newIdentDefs(
          rightVar,
          ident("VariantCompatible"),
          newEmptyNode()
        )
      ),
      newEmptyNode(), newEmptyNode(),
      newStmtList()
    )
    result.add funcDef
    let funcBody = funcDef.last
    for i in commentsSeq:
      funcBody.add i  # insert doc comments into func body right after defintion

    # Create when statement for right operand auto-conversion to Variant if it's not already
    funcBody.add nnkWhenStmt.newTree(
      nnkElifBranch.newTree(
        infix(rightVar, "isnot", ident("Variant")),
        newStmtList(
          newLetStmt(
            rightVar,
            newCall(newDotExpr(rightVar,ident("toVariant")))
          )
        )
      )
    )

    let raiseStmt = nnkRaiseStmt.newTree(
      newCall(
        newDotExpr(ident("DynamicTypeError"), ident("newException")),
        newStrLitNode("Incompatible types for operation")
      )
    )

    # Create `onlyTypes` matryoshka in the func body
    funcBody.add newCall(
      newDotExpr(leftVar,ident("onlyTypes")),
      leftKinds,
      assgnLeft,
      newStmtList(
        newCall(
          newDotExpr(rightVar, ident("onlyTypes")),
          rightKinds,
          assgnRight,
          newStmtList(
            nnkBlockStmt.newTree(
              newEmptyNode(),
              newStmtList()
            )
          ),
          newStmtList(
            nnkRaiseStmt.newTree(
              newCall(
                newDotExpr(ident("DynamicTypeError"), ident("newException")),
                newCall(
                  ident("&"),
                  newStrLitNode("Right operand is not suitable for this operation: "),
                  newCall(ident("$"), newDotExpr(rightVar, ident("kind")))
                )
              )
            )
          )
        )
      ),
      newStmtList(
        nnkRaiseStmt.newTree(
          newCall(
            newDotExpr(ident("DynamicTypeError"), ident("newException")),
            newCall(
              ident("&"),
              newStrLitNode("Left operand is not suitable for this operation: "),
              newCall(ident("$"), newDotExpr(leftVar, ident("kind")))
            )
          )
        )
      )
    )
    # code above created the
    #
    # left.onlyTypes(...):
    #   right.onlyTypes(...):
    #     block:
    #       [This is what we are getting now and assigning to `actionStmtList` variable]
    # ...
    let actionStmtList = funcBody.last[3][0][3][0][1]

    # NOW then, the result expressions
    if "left" in sideTypeToResultExpr:
      # Here we convert left side `type:` breanhces to when statements
      var paramBranches = nnkWhenStmt.newTree()
      for typToBody in sideTypeToResultExpr["left"].pairs:
        if typToBody[0] == "any":
          paramBranches = nnkAsgn.newTree(
            ident("result"),
            newDotExpr(
              typToBody[1],  # Insert user provided expression
              ident("toVariant")
            )
          )
          break
        paramBranches.add nnkElifBranch.newTree(
          nnkInfix.newTree(
            ident("is"),
            assgnLeft,
            idsToNimType[typToBody[0]].parseExpr
          ),
          nnkAsgn.newTree(
            ident("result"),
            newDotExpr(
              typToBody[1],   # Insert user provided expression
              ident("toVariant")
            )
          )
        )
      if sideTypeToResultExpr["left"].len > 0:
        actionStmtList.add paramBranches
    
    if "right" in sideTypeToResultExpr:
      # Here we convert right side `type:` breanhces to when statements
      var paramBranches = nnkWhenStmt.newTree()
      for typToBody in sideTypeToResultExpr["right"].pairs:
        if typToBody[0] == "any":
          paramBranches = nnkAsgn.newTree(
            ident("result"),
            newDotExpr(
              typToBody[1],  # Insert user provided expression
              ident("toVariant")
            )
          )
          break
        paramBranches.add nnkElifBranch.newTree(
          nnkInfix.newTree(
            ident("is"),
            assgnRight,
            idsToNimType[typToBody[0]].parseExpr
          ),
          nnkAsgn.newTree(
            ident("result"),
            newDotExpr(
              typToBody[1],  # Insert user provided expression
              ident("toVariant")
            )
          )
        )
      if sideTypeToResultExpr["right"].len > 0:
        actionStmtList.add paramBranches

func isTruthy*(v: Variant): bool =
  ## Check if variant is truthy.
  ## Roughtly follows python convebtions.
  ## - ints and floats are truthy if not equal to zero
  ## - bools returned as-is
  ## - string, list, object are truthy if theri len is > 0
  ## - range is truthy if (end - start) > 0
  v.everyType(x):
    when x is int:
      result = x != 0
    elif x is float:
      result = x != 0.0
    elif x is bool:
      result = x
    elif x is string:
      result = x.len > 0
    elif x is seq[Variant]:
      result = x.len > 0
    elif x is RangeTuple:
      result = (x.rEnd - x.rStart) > 0
    elif x is Table[string, Variant]:
      result = x.len > 0
    else: {.error: "unsupported type: " & typeof(x).}
  do:
    result = false

func `==`*(left: Variant, right: VariantCompatible): Variant
func `==`*(a, b: seq[Variant]): bool =
  if a.len != b.len:
    result = false
  else:
    result = true
    for i in a.low..a.high:
      (a[i] == b[i]).asType(varBool, x):
        if not x:
          result = false
          break

func `==`*(a, b: Table[string, Variant]): bool =
  if a.len != b.len:
    result = false
  else:
    result = true
    for pair in a.pairs:
      if pair[0] notin b: return false
      (pair[1] == b[pair[0]]).asType(varBool, x):
        if not x:
          result = false
          break

proc add*(v: var Variant, other: VariantCompatible) =
  ## Implements `add` operation for list variants
  v.onlyTypes({ varList }, x):
    v.listValue.add other.toVariant()
  do:
    raise DynamicTypeError.newException:
      "Unimplemented operation for type: " & $v.kind

proc pop*(v: var Variant): Variant =
  ## Implements `pop` operation for list variants
  v.onlyTypes({ varList }, x):
    result = v.listValue.pop()
  do:
    raise DynamicTypeError.newException:
      "Unimplemented operation for type: " & $v.kind

proc find*(v: Variant, other: VariantCompatible): Variant =
  ## Return index as int Variant of element if it's found inside a list variant.
  ## If value is not in the list, returns
  v.onlyTypes({ varList }, x):
    result = -1.toVariant()
    var i = 0
    for el in x:
      if (el == other).isTruthy():
        return i.toVariant()
      i.inc()
  do:
    raise DynamicTypeError.newException:
      "Unimplemented operation for type: " & $v.kind

proc contains*(v: Variant, other: VariantCompatible): bool =
  ## Checks if list or object variant contains `other`.
  ## - lists take any value of other
  ## - objects expect a string value and checks it against keys, otherwise always false.
  when other isnot Variant:
    let other = other.toVariant()
  case v.kind
  of varList:
    if value[int](v.find(other)) != -1:
      result = true
    else:
      result = false
  of varObj:
    case other.kind
    of varStr:
      result = other.strValue in v.objValue
    else:
      result = false
  else:
    raise DynamicTypeError.newException:
      "Unimplemented operation for type: " & $v.kind


func `==`*(a, b: Null): bool {.inline.} = true
func `!=`*(a, b: Null): bool {.inline.} = false
func `<=`*(a, b: Null): bool {.inline.} = true
func `>=`*(a, b: Null): bool {.inline.} = true
func `<`* (a, b: Null): bool {.inline.} = false
func `>`* (a, b: Null): bool {.inline.} = false


binOps:
  `+`:
    ## Implements `+` operation for Variant type.
    ## Implemented for any combination of int and float
    ## values. If one of operands is float, result is also float.
    left from [int, float] as x:
      int:
        when y is float: x.toFloat() + y
        else:            x           + y
    right from [int, float] as y:
      int:
        when x is float: x + y.toFloat()
        else:            x + y
  
  `-`:
    ## Implements `-` operation for Variant type.
    ## Implemented for any combination of int and float
    ## values. If one of operands is float, result is also float.
    left from [int, float] as x:
      int:
        when y is float: x.toFloat() - y
        else:            x           - y
    right from [int, float] as y:
      int:
        when x is float: x - y.toFloat()
        else:            x - y
  
  `*`:
    ## Implements `*` operation for Variant type.
    ## Implemented for any combination of int and float
    ## values. If one of operands is float, result is also float.
    left from [int, float] as x:
      int:
        when y is float: x.toFloat() * y
        else:            x           * y
    right from [int, float] as y:
      int:
        when x is float: x * y.toFloat()
        else:            x * y
  
  `/`:
    ## Implements `/` operation for Variant type.
    ## Implemented for any combination of int and float
    ## values. If one of operands is float, result is also float.
    left from [int, float] as x:
      int:
        when y is float: x.toFloat() / y
        else:            x           / y
    right from [int, float] as y:
      int:
        when x is float: x / y.toFloat()
        else:            x / y
  
  `[]`:
    ## Access a key of `list` or `object` Variant.
    ## Key can be `string` or `int`.
    ## - When accessing `object` with `int` key, key will be implicitly converted to string.
    ## - When accessing `list` with `string` key, it will be implicitly parsed as integer. 
    left as container from [object, list] as x:
      object:
        when y is int:
          x[$y]
        else:
          x[y]
      list:
        when y is string:
          x[y.parseInt]
        else:
          x[y]
    right as key from [int, string] as y: discard

  `&`:
    ## Concantenate two lists or string.
    ## Or add element to list or string if `right` is not list or string.
    ## When adding elements to string, they are implicitly converted to string.
    left from [list, string] as arr: discard
    right from [any] as other:
      any:
        when arr is string:
          arr & $right
        elif other is seq[Variant]:
          arr & other
        else:
          arr & @[right]

  `==`:
    ## Check equality of two Variants.
    ## Variants are never equal if their type is different.
    left  from [any] as x: discard
    right from [any] as y:
      any:
        when typeof(x) is typeof(y):
          x == y
        else:
          false
  
  `!=`:
    ## Check un-equality of two Variants.
    ## Variants are never equal if their type is different.
    left  from [any] as x: discard
    right from [any] as y:
      any:
        when typeof(x) is typeof(y):
          x != y
        else:
          false
  
  `<=`:
    ## Check if left Variant is lesser or equal to the right one.
    ## Always false when types are different.
    left  from [int, float, string, bool, nil] as x: discard
    right from [int, float, string, bool, nil] as y:
      any:
        when typeof(x) is typeof(y):
          x <= y
        else:
          false
  
  `>=`:
    ## Check if left Variant is greater or equal to the right one.
    ## Always false when types are different.
    left  from [int, float, string, bool, nil] as x: discard
    right from [int, float, string, bool, nil] as y:
      any:
        when typeof(x) is typeof(y):
          x >= y
        else:
          false

  `<`:
    ## Check if left Variant is lesser that the right one.
    ## Always false when types are different.
    left  from [int, float, string, bool, nil] as x: discard
    right from [int, float, string, bool, nil] as y:
      any:
        when typeof(x) is typeof(y):
          x < y
        else:
          false
  
  `>`:
    ## Check if left Variant is greater that the right one.
    ## Always false when types are different.
    left  from [int, float, string, bool, nil] as x: discard
    right from [int, float, string, bool, nil] as y:
      any:
        when typeof(x) is typeof(y):
          x > y
        else:
          false

  `in`:
    ## Implements `in` operator for list and object variants
    left from  [any] as x:
      any:
        right.contains(left)
    right from [list, object] as y: discard
  
  `notin`:
    ## Implements `notin` operator for list and object variants
    left from  [any] as x:
      any:
        not right.contains(left)
    right from [list, object] as y: discard
  
  `and`:
    left  from [bool, int] as x:
      bool:
        when y isnot bool:
          x and right.isTruthy()
        else:
          x and y
      int:
        when y isnot int:
          let y =
            case y
            of true: 1
            of false: 0
          x and y
        else:
          x and y

    right from [bool, int] as y: discard


func `[]=`*(container: var Variant, key: VariantCompatible, value: VariantCompatible) =
  ## Set the key of list or object variant
  ## ## Key can be `string` or `int`.
  ## - When accessing `object` with `int` key, key will be implicitly converted to string.
  ## - When accessing `list` with `string` key, it will be implicitly parsed as integer. 
  when key isnot Variant:
    let key = key.toVariant()
  when value isnot Variant:
    let value = value.toVariant()
  
  case container.kind
  of varObj:
    key.onlyTypes({varStr, varInt}, k):
      block:
        when k is int:
          let k = $k
        container.objValue[k] = value
    do:
      raise DynamicTypeError.newException:
        "Cannot index a container with " & $key.kind
  of varList:
    key.onlyTypes({varStr, varInt}, k):
      block:
        when k is string:
          let k = k.parseInt()
        container.listValue[k] = value
    do:
      raise DynamicTypeError.newException:
        "Cannot index a container with " & $key.kind
  else:
    raise DynamicTypeError.newException:
      "Operation not supported for type " & $container.kind


{.experimental: "dotOperators".}

template `.`*(v: Variant, field: untyped): untyped =
  ## Attempt to access field of Variant object.
  ## If not exists, returns Null.
  ## If Variant is not an object, raises Value Error
  v.onlyTypes({varObj}, x):
    x.getOrDefault(astToStr(field), nil.toVariant())
  do:
    raise DynamicTypeError.newException:
      "Can't access field " & astToStr(field) & " of " & $v.kind

template `.=`*[T](v: var Variant, field: untyped, value: T): untyped =
  ## Attempt to assign field of Variant object.
  ## If Variant is not an object, raises Value Error
  v.onlyTypes({varObj}, x):
    v.objValue[astToStr(field)] = value.toVariant()
  do:
    raise DynamicTypeError.newException:
      "Can't access field " & astToStr(field) & " of " & $v.kind
