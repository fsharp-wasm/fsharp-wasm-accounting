namespace Accounting

open System.Collections.Generic
open FSharp.Wasm.Frame
open FSharp.Wasm.Formula
open FSharp.Wasm.Graph
open FSharp.Wasm.Live

/// The plan of the budget, and the corrections of the ledger, as a graph.  What a person writes into the budget of an account in a month, and the booked amount that they correct of a journal line, is a step of a document (`Doc`), so it is on the tape, can be undone, and is a patch that can travel;
/// the cells are a `Sheet` over that text (a formula reads other cells and the ledger by name: `Actual`, `Budget`), and the cube is a calc: the cube of the facts for the two dimensions on the axes
/// with the cells of the plan written over its Budget measure.  A step runs what it reaches, once each, and only writes the cells whose number came out different into the cube.
///
/// The graph is one mutable store: the models of a run share it (an earlier model sees the later steps), which is fine for a loop that only moves forward; the tape and the timeline are what go back.
[<ReferenceEquality>]
type Planning =
    { Doc: Doc
      Timeline: Timeline
      /// the node of the plan; its map `cells` has the text of every cell, by the key of the cell
      Node: int
      Sheet: Sheet
      /// the dimensions on the rows and on the columns
      Axes: Source<int * int>
      /// the ledger of accounts × months as it is, with the corrections of its lines: what the names Actual and Budget of a formula stand for (a calc, so that a formula that reads them runs again when a line is corrected)
      Base: Calc<Cube>
      /// the corrections of journal lines of the document, written into the facts: it says by another number that it wrote, and the cubes depend on that
      Ledger: Calc<int>
      /// the booked amounts as the ledger had them before any line was corrected
      Original: float[]
      /// the cube of the facts for the axes, and the same with the plan written over the Budget measure
      Raw: Calc<Cube>
      Cube: Calc<Cube>
      /// the cells whose value changed in the last step, in the order they were calculated
      Changed: ResizeArray<Change> }

module Planning =

    let private samePair (a: int * int) (b: int * int) : bool = fst a = fst b && snd a = snd b

    /// The key of the cell of an account and a month: the column of a cell is its month, the row its account.
    let private key (account: int) (period: int) : string = string (CellRef.key (CellRef.create period account))

    let private atKey (k: string) : CellRef = CellRef.ofKey (System.Int32.Parse k)

    let private textOf (a: Atom) : string =
        match a with
        | Atom.Text s -> s
        | _ -> ""

    /// The plan over the facts, empty, with the accounts on the rows and the months on the columns (the ledger that `Actual` and `Budget` read is that one).
    let create (facts: Facts) (formulas: (string * FSharp.Wasm.Frame.Expr)[]) : Planning =
        let doc = Doc(1)
        let mutable node = 0
        // the plan itself is not a step to undo
        doc.Edit("plan", fun tx -> node <- tx.Create "Plan") |> ignore
        let engine = doc.Engine
        let axes = Source<int * int>(engine, (Ledger.Accounts, Ledger.Periods), samePair)
        // A correction of a line is an entry of the document ("lines": the line's number, the amount).  This calc makes the facts say what the document says, and says by another number that it
        // did; the cubes read that number, so one flush of the engine runs the correction, the cubes, the formulas that read the ledger and the plan written over the budget, once each.
        let original = Array.copy facts.Values.[0]
        let applied = Dictionary<int, float>()
        let mutable version = 0
        let ledger =
            Calc<int>(engine, Same.int, 0, (fun _ _ -> ()), fun () ->
                let now = Dictionary<int, float>()
                for k in doc.Keys(node, "lines") do
                    match doc.Entry(node, "lines", k) with
                    | Atom.Num n -> now.[System.Int32.Parse k] <- n
                    | _ -> ()
                let booked = facts.Values.[0]
                let mutable changed = false
                for kv in now do
                    match applied.TryGetValue kv.Key with
                    | true, v when v = kv.Value -> ()
                    | _ ->
                        booked.[kv.Key] <- kv.Value
                        changed <- true
                for kv in applied do
                    if not (now.ContainsKey kv.Key) then
                        booked.[kv.Key] <- original.[kv.Key]
                        changed <- true
                applied.Clear()
                for kv in now do applied.[kv.Key] <- kv.Value
                if changed then version <- version + 1
                version)
        let revision () : int = ledger.Value
        let raw = Cubes.pivotOver engine facts axes revision formulas
        // the ledger of accounts × months, whichever way round the table is turned
        let ledgerAxes = Source<int * int>(engine, (Ledger.Accounts, Ledger.Periods), samePair)
        let baseCalc = Cubes.pivotOver engine facts ledgerAxes revision formulas
        let text (at: CellRef) : string = textOf (doc.Entry(node, "cells", string (CellRef.key at)))
        let resolve (at: CellRef) (name: string) : Value option =
            match name with
            | "Actual" -> Some (Value.Number (Cube.leaf baseCalc.Value 0 at.Row at.Col))
            | "Budget" -> Some (Value.Number (Cube.leaf baseCalc.Value 1 at.Row at.Col))
            | _ -> None
        let sheet = Sheet(engine, text, resolve, fun _ -> None)
        let changed = ResizeArray<Change>()
        sheet.OnChange <- fun c -> changed.Add c
        // what the plan gives the cube: a number per cell that is one, at the leaf of the account and the leaf of the month, whichever way round the table is
        let cells () : (int * int * float)[] =
            let (r, c) = axes.Value
            let byAccountFirst = r = Ledger.Accounts && c = Ledger.Periods
            if byAccountFirst || (r = Ledger.Periods && c = Ledger.Accounts) then
                [| for k in doc.Keys(node, "cells") do
                     let at = atKey k
                     match sheet.Read at with
                     | Value.Number n -> yield (if byAccountFirst then (at.Row, at.Col, n) else (at.Col, at.Row, n))
                     | _ -> () |]
            else [||]
        let cube = Cubes.overlay engine raw 1 cells
        { Doc = doc; Timeline = Timeline(doc); Node = node; Sheet = sheet; Axes = axes; Base = baseCalc; Ledger = ledger; Original = original; Raw = raw; Cube = cube; Changed = changed }

    /// What was written into the cell of an account and a month ("" when nothing was).
    let source (p: Planning) (account: int) (period: int) : string = textOf (p.Doc.Entry(p.Node, "cells", key account period))

    /// How many cells hold something.
    let count (p: Planning) : int = (p.Doc.Keys(p.Node, "cells")).Length

    /// The cell of an account and a month, for the sheet.
    let cellAt (account: int) (period: int) : CellRef = CellRef.create period account

    /// What a step did: its tape entry and how many cells came out with another value.
    let private done' (p: Planning) (a: Applied) : Applied * int = (a, p.Changed.Count)

    /// Write what was typed into the cell of an account and a month, as one step on the tape; an empty text clears the cell.
    let write (p: Planning) (label: string) (account: int) (period: int) (text: string) : Applied * int =
        p.Changed.Clear()
        // the cell is there (blank) before it is written, so that what is written is a change of its value
        p.Sheet.Peek(cellAt account period) |> ignore
        let value = if text.Trim() = "" then Nil else Atom.Text text
        done' p (p.Timeline.Do(label, fun tx -> tx.Put(p.Node, "cells", key account period, value)))

    /// The key of a journal line in the document.
    let private lineKey (line: int) : string = string line

    /// Whether the booked amount of a journal line has been corrected.
    let isCorrected (p: Planning) (line: int) : bool = p.Doc.Entry(p.Node, "lines", lineKey line) <> Nil

    /// Correct the booked amount of a journal line, as one step on the tape.  The step reaches the cubes and the formulas that read the ledger in the same flush.
    let correct (p: Planning) (label: string) (line: int) (amount: float) : Applied * int =
        p.Changed.Clear()
        done' p (p.Timeline.Do(label, fun tx -> tx.Put(p.Node, "lines", lineKey line, Atom.Num amount)))

    /// How many journal lines have a corrected amount.
    let corrections (p: Planning) : int = (p.Doc.Keys(p.Node, "lines")).Length

    /// Take the correction of a line back, as a step: the ledger's amount stands again.
    let uncorrect (p: Planning) (label: string) (line: int) : Applied * int =
        p.Changed.Clear()
        done' p (p.Timeline.Do(label, fun tx -> tx.Put(p.Node, "lines", lineKey line, Nil)))

    let undo (p: Planning) : (Applied * int) option =
        p.Changed.Clear()
        p.Timeline.Undo() |> Option.map (done' p)

    let redo (p: Planning) : (Applied * int) option =
        p.Changed.Clear()
        p.Timeline.Redo() |> Option.map (done' p)

    /// Put other dimensions on the axes: the cube is built again from the facts, and the plan is written over it.
    let turn (p: Planning) (rowDim: int) (colDim: int) : unit =
        p.Axes.Set((rowDim, colDim)) |> ignore
        p.Doc.Engine.Flush() |> ignore

    /// The steps so far, the last one last.
    let tape (p: Planning) : Entry[] = p.Timeline.Tape
