module Accounting.Tests.Tests

open FSharp.Math
open FSharp.Wasm.Frame
open FSharp.Wasm.Formula
open FSharp.Graphics.Core
open FSharp.Graphics.Scene
open FSharp.Graphics.Widgets
open Wasmecto
open Accounting

let private clock () = 0.0
let private lines = 20000
let private init () = fst (Model.init lines clock)
let private send (msgs: Msg list) (m: Model) = msgs |> List.fold (fun m msg -> fst (Model.update clock m msg)) m

let private labelsOf (m: Model) : string list = [ for i in 0 .. m.Rows.Length - 1 -> (Model.Table.rowLine m i).Label ]
let private rowOf (m: Model) (label: string) : int = Seq.findIndex (fun l -> l = label) (labelsOf m)

let ledger =
    testList "Ledger" [
        testCase "the same seed makes the same lines, another seed does not" <| fun () ->
            let a = Ledger.facts 500 7u
            let b = Ledger.facts 500 7u
            let c = Ledger.facts 500 8u
            Expect.equal a.Values.[0] b.Values.[0] "the same amounts"
            Expect.equal a.Dimensions.[0].Leaf b.Dimensions.[0].Leaf "the same accounts"
            Expect.notEqual a.Values.[0] c.Values.[0] "another seed, other amounts"
        testCase "the chart has 65 accounts in 13 groups of 4 classes, the periods 24 months, the centres 7" <| fun () ->
            let f = Ledger.facts 10 1u
            Expect.equal f.Dimensions.[Ledger.Accounts].Tree.Leaves 65 "accounts"
            Expect.equal (f.Dimensions.[Ledger.Accounts].Tree.Nodes |> Array.filter (fun n -> n.Level = 0)).Length 4 "classes"
            Expect.equal (f.Dimensions.[Ledger.Accounts].Tree.Nodes |> Array.filter (fun n -> n.Level = 1)).Length 13 "groups"
            Expect.equal f.Dimensions.[Ledger.Periods].Tree.Leaves 24 "months"
            Expect.equal f.Dimensions.[Ledger.Centres].Tree.Leaves 7 "centres"
        testCase "revenue counts for the result and costs against it; a line is in whole cents" <| fun () ->
            let f = Ledger.facts 3000 3u
            let accounts = f.Dimensions.[Ledger.Accounts]
            let mutable wrongSign = 0
            let mutable wrongCents = 0
            for i in 0 .. f.Count - 1 do
                let revenue = accounts.Tree.Nodes.[accounts.Tree.LeafNodes.[accounts.Leaf.[i]]].Parent |> fun g -> accounts.Tree.Nodes.[accounts.Tree.Nodes.[g].Parent].Label = "Revenue"
                if (revenue && f.Values.[0].[i] <= 0.0) || (not revenue && f.Values.[0].[i] >= 0.0) then wrongSign <- wrongSign + 1
                let cents = f.Values.[0].[i] * 100.0
                if abs (cents - floor (cents + 0.5)) > 1e-6 then wrongCents <- wrongCents + 1
            Expect.equal wrongSign 0 "the sign by class"
            Expect.equal wrongCents 0 "whole cents"
        testCase "dates are real: a year of the ledger, a month from 1 to 12, a day from 1 to 28" <| fun () ->
            let f = Ledger.facts 2000 5u
            for i in 0 .. f.Count - 1 do
                let d = Ledger.dateOf f i
                let year = int (d.Substring(0, 4))
                let month = int (d.Substring(5, 2))
                let day = int (d.Substring(8, 2))
                Expect.isTrue (year = 2025 || year = 2026) ("year of " + d)
                Expect.isTrue (month >= 1 && month <= 12 && day >= 1 && day <= 28) ("month and day of " + d)
    ]

let model =
    testList "Model" [
        testCase "it starts with the total, the classes and the groups, and with the years and the quarters" <| fun () ->
            let m = init ()
            Expect.equal m.Rows.Length 18 "1 total + 4 classes + 13 groups"
            Expect.equal m.Cols.Length 11 "1 total + 2 years + 8 quarters"
            Expect.equal (Model.Table.rowLine m 0) { Label = "Total"; Level = 0; Group = true; Open = true } "the total, open"
            Expect.equal (Model.Table.rowLine m 1).Label "Revenue" "the first class"
            Expect.isTrue (Model.Table.rowLine m 1).Group "a class is a group"
        testCase "a total is the sum of what it is made of, in every cell" <| fun () ->
            let m = init () |> send [ Fold true ]
            let col = 0
            let value r = Cube.value m.Cube 0 m.Rows.[r] m.Cols.[col]
            // the groups of Revenue add up to Revenue
            let revenue = rowOf m "Revenue"
            let groups = [ for i in 0 .. m.Rows.Length - 1 do if m.Cube.RowTree.Nodes.[m.Rows.[i]].Parent = m.Rows.[revenue] then yield value i ]
            Expect.equal groups.Length 3 "three groups"
            Expect.approxEqual (List.sum groups) (value revenue) 1e-6 "the sum of the groups is the class"
            // the classes add up to the total
            let classes = [ for i in 0 .. m.Rows.Length - 1 do if m.Cube.RowTree.Nodes.[m.Rows.[i]].Level = 0 then yield value i ]
            Expect.approxEqual (List.sum classes) (value 0) 1e-6 "the classes are the total"
        testCase "opening a group shows what it is made of, and folding it takes it away" <| fun () ->
            let m0 = init ()
            let revenue = rowOf m0 "Revenue"
            let sales = rowOf m0 "Product sales"
            let m1 = m0 |> send [ ToggleRow sales ]
            Expect.equal m1.Rows.Length (m0.Rows.Length + 5) "five accounts appear"
            Expect.equal (Model.Table.rowLine m1 (sales + 1)).Label "4000 Domestic" "right after the group"
            Expect.equal (Model.Table.rowLine m1 (sales + 1)).Level 3 "set in by its depth"
            let m2 = m1 |> send [ ToggleRow sales ]
            Expect.equal m2.Rows m0.Rows "folded again: as it was"
            Expect.isTrue (revenue < sales) "the class comes before its groups"
        testCase "the total folds everything to one line, and a column folds to its total" <| fun () ->
            let m = init () |> send [ ToggleRow 0 ]
            Expect.equal m.Rows [| 0 |] "one line"
            Expect.equal (Model.Table.rowLine m 0).Open false "folded"
            let c = init () |> send [ ToggleCol 0 ]
            Expect.equal c.Cols [| 0 |] "one column: the total"
        testCase "Open all and Fold all" <| fun () ->
            let all = init () |> send [ Fold true ]
            Expect.equal all.Rows.Length (1 + 4 + 13 + 65) "every node of the chart"
            Expect.equal all.Cols.Length (1 + 2 + 8 + 24) "every node of the periods"
            let none = all |> send [ Fold false ]
            Expect.equal none.Rows.Length 5 "the total and the classes"
            Expect.equal none.Cols.Length 3 "the total and the years"
        testCase "the measures: Budget and Actual differ, Variance is their difference, Variance % is relative to the budget" <| fun () ->
            let m = init ()
            let at i = Cube.value m.Cube i 0 0
            Expect.notEqual (at 0) (at 1) "actual and budget"
            Expect.approxEqual (at 2) (at 0 - at 1) 1e-6 "variance"
            Expect.approxEqual (at 3) ((at 0 - at 1) * 100.0 / at 1) 1e-9 "variance as a percentage of the budget"
            let cell i = (Model.Table.cell (m |> send [ SetMeasure i ]) 0 0).Text
            Expect.equal (cell 0) (Format.amount (at 0)) "the text is the formatted value"
        testCase "turning the table builds it from other dimensions and takes the tabs away" <| fun () ->
            let m0 = init () |> send [ OpenCell(1, 1) ]
            Expect.equal m0.Details.Length 1 "a tab"
            let m = m0 |> send [ SetPivot 1 ]
            Expect.equal m.Pivot 1 "cost centres by periods"
            Expect.equal m.Tab "pivot" "back on the table"
            Expect.equal m.Details.Length 0 "the tabs belonged to the other pivot"
            Expect.equal (Model.Table.rowLine m 1).Label "EMEA" "regions now"
            Expect.approxEqual (Cube.value m.Cube 0 0 0) (Cube.value m0.Cube 0 0 0) 1e-6 "the grand total is the same whichever way the table is turned"
            let back = m |> send [ SetPivot 0 ]
            Expect.equal back.Rows m0.Rows "and back again"
            Expect.equal (m |> send [ SetPivot 1 ]).Details.Length 0 "choosing the pivot that is chosen changes nothing"
        testCase "a cell opens the lines behind it in a tab; the same cell opens the same tab; closing returns to the table" <| fun () ->
            let m0 = init ()
            let row = rowOf m0 "Revenue"
            let m1 = m0 |> send [ OpenCell(row, 1) ]
            Expect.equal m1.Details.Length 1 "one tab"
            Expect.equal m1.Tab m1.Details.Head.Id "and it is the one on show"
            let d = m1.Details.Head
            Expect.equal d.Entries.Length (Facts.entries m0.Facts 0 m0.Rows.[row] 2 m0.Cols.[1]).Length "the lines of that group in that column"
            Expect.isTrue (d.Entries.Length > 0) "there are some"
            Expect.approxEqual (d.Entries |> Array.sumBy (fun i -> m0.Facts.Values.[0].[i])) (Cube.value m0.Cube 0 m0.Rows.[row] m0.Cols.[1]) 1e-6 "they add up to the cell"
            let m2 = m1 |> send [ Select "pivot"; OpenCell(row, 1) ]
            Expect.equal m2.Details.Length 1 "no second tab for the same cell"
            Expect.equal m2.Tab d.Id "the tab is selected again"
            let m3 = m2 |> send [ Close d.Id ]
            Expect.equal m3.Details.Length 0 "closed"
            Expect.equal m3.Tab "pivot" "the table is on show"
        testCase "the cells of a tab are the journal lines: date, account, cost centre, reference and the two amounts" <| fun () ->
            let m = init () |> send [ OpenCell(1, 1) ]
            let d = m.Details.Head
            Expect.equal (Layout.detailColumns |> Array.length) 5 "five columns"
            let first = d.Entries.[0]
            Expect.equal (Model.Table.detailRow m d 0).Label (Ledger.dateOf m.Facts first) "the date is the row's"
            Expect.equal (Model.Table.detailCell m d 0 3).Text (Format.amount m.Facts.Values.[0].[first]) "actual"
            Expect.equal (Model.Table.detailCell m d 0 4).Text (Format.amount m.Facts.Values.[1].[first]) "budget"
            Expect.isTrue (Model.Table.detailCell m d 0 0).Left "text is set at the left"
            Expect.isFalse (Model.Table.detailCell m d 0 3).Left "numbers at the right"
            Expect.stringStarts (Model.Table.detailCell m d 0 2).Text "INV-" "a reference"
        testCase "at most six tabs are kept: the oldest goes" <| fun () ->
            let m = init () |> send [ for c in 1 .. 8 -> OpenCell(1, c) ]
            Expect.equal m.Details.Length 6 "six"
            Expect.isFalse (m.Details |> List.exists (fun d -> d.Id.EndsWith(":" + string m.Cols.[1]))) "the first one is gone"
        testCase "the keys move the selection, open and fold, and act on the table that is on show" <| fun () ->
            let m0 = init ()
            let m1 = m0 |> send [ Key "Down" ]
            Expect.equal m1.Main.Selected (Some(0, 0)) "the first key selects the first cell"
            let m2 = m1 |> send [ Key "Down"; Key "Right"; Key "Right" ]
            Expect.equal m2.Main.Selected (Some(1, 2)) "moved"
            let m3 = m2 |> send [ Key "Space" ]
            Expect.notEqual m3.Rows.Length m2.Rows.Length "Space on a class that is open folds it, closed opens it: the number of rows changes"
            let m4 = m3 |> send [ Key "Enter" ]
            Expect.equal m4.Details.Length 1 "Enter opens the cell"
            let before = m4.Main
            let m5 = m4 |> send [ Key "Down" ]
            Expect.equal m5.Main before "with a tab on show the keys move that table, not the pivot"
            Expect.notEqual m5.Details.Head.Grid.Selected None "the tab's selection moved"
            Expect.equal (m5 |> send [ Key "nonsense" ]) m5 "a key that means nothing changes nothing"
        testCase "a smaller window holds the table inside it" <| fun () ->
            let m = init () |> send [ Fold true ]
            let scrolled = m |> send [ GridState("pivot", { Grid.start with OffsetY = 1.0e6f }) ]
            let frame = Model.mainFrame scrolled
            Expect.isTrue (scrolled.Main.OffsetY > 0.0f) "the offset was taken"
            let small = scrolled |> send [ Resize(Vec2.create 600.0f 500.0f) ]
            Expect.isTrue (small.Main.OffsetY <= float32 frame.Rows * Grid.metrics.RowHeight) "inside the table"
            Expect.equal small.Viewport (Vec2.create 600.0f 500.0f) "the size is kept"
    ]

// ── the plan: the Budget measure of the accounts × periods table, written by hand and by formula ──

/// The grid with every leaf shown, on the Budget measure: the one where cells can be written.
let private planModel () = init () |> send [ Fold true; SetMeasure 1 ]

/// The place in the table of the leaf `leaf` of a tree among the lines (or columns) that are shown.
let private placeOf (tree: Hierarchy) (shown: int[]) (leaf: int) : int =
    shown |> Array.findIndex (fun n -> tree.Nodes.[n].Children.Length = 0 && tree.Nodes.[n].Lo = leaf)

/// Select the cell of an account and a month (accounts × periods).
let private select (account: int) (period: int) (m: Model) : Model =
    m |> send [ GridState("pivot", { m.Main with Selected = Some(placeOf m.Cube.RowTree m.Rows account, placeOf m.Cube.ColTree m.Cols period) }) ]

/// Write `text` into a cell of the plan the way a person does: select it, type, Enter.
let private put (account: int) (period: int) (text: string) (m: Model) : Model =
    m |> select account period |> send [ BeginEdit(Some text); CommitEdit ]

let private budget (m: Model) (account: int) (period: int) = Cube.leaf m.Cube 1 account period
let private total (m: Model) (measure: int) = Cube.value m.Cube measure 0 0

let plan =
    testList "Plan" [
        testCase "a number typed into a budget cell is the budget of that account and month, and every total above it moves by the difference" <| fun () ->
            let m0 = planModel ()
            let m = m0 |> put 0 0 "900"
            Expect.approxEqual (budget m 0 0) 900.0 1e-9 "the cell"
            Expect.approxEqual (total m 1) (total m0 1 - budget m0 0 0 + 900.0) 1e-6 "the grand total of the budget moved by exactly that"
            Expect.approxEqual (total m 0) (total m0 0) 1e-9 "what was booked is not touched"
            Expect.approxEqual (total m 2) (total m 0 - total m 1) 1e-6 "the variance follows the budget"
            Expect.equal (m.LastRecalc |> Option.map (fun r -> r.Changed)) (Some 1) "one cell changed"
            Expect.equal m.Editing None "the editing is over"
            let r = placeOf m.Cube.RowTree m.Rows 0
            let c = placeOf m.Cube.ColTree m.Cols 0
            Expect.stringStarts (Model.Table.cell m r c).Text "• " "a number that was typed is marked"
            Expect.isTrue (Model.Plan.leafAt m r c |> Option.isSome) "it is a cell of the plan"
        testCase "keys write a cell: a character starts it, Backspace takes one back, Enter keeps it, Escape throws it away" <| fun () ->
            let m0 = planModel () |> select 3 5
            let typing = m0 |> send [ Key "1"; Key "2"; Key "3"; Key "Backspace" ]
            Expect.equal (typing.Editing |> Option.map (fun e -> e.Text)) (Some "12") "what was typed"
            Expect.equal (budget typing 3 5) (budget m0 3 5) "nothing is written before Enter"
            let kept = typing |> send [ Key "Enter" ]
            Expect.approxEqual (budget kept 3 5) 12.0 1e-9 "Enter keeps it"
            let thrown = kept |> send [ Key "9"; Key "9"; Key "Escape" ]
            Expect.equal thrown.Editing None "Escape ends it"
            Expect.approxEqual (budget thrown 3 5) 12.0 1e-9 "and the cell is as it was"
            Expect.equal thrown.Undo.Length 1 "a thrown away text is no step to undo"
        testCase "F2 opens a cell with what is in it: the ledger's budget as a number, or the text that was written" <| fun () ->
            let m0 = planModel () |> select 2 4
            let e0 = (m0 |> send [ Key "F2" ]).Editing.Value
            Expect.equal e0.Text (Value.numberText (Cube.leaf m0.PlanBase 1 2 4)) "the budget of the ledger"
            Expect.equal (e0.Account, e0.Period) (2, 4) "the account and the month"
            let e1 = (m0 |> send [ BeginEdit(Some "=Actual*2"); CommitEdit; Key "F2" ]).Editing.Value
            Expect.equal e1.Text "=Actual*2" "the formula, not its value"
        testCase "a formula reads the ledger by name: Actual and Budget are the numbers of that account and month" <| fun () ->
            let m0 = planModel ()
            let m = m0 |> put 4 7 "=Actual*1.1" |> put 4 8 "=Budget+100"
            Expect.approxEqual (budget m 4 7) (Cube.leaf m0.PlanBase 0 4 7 * 1.1) 1e-6 "ten percent over what was booked"
            Expect.approxEqual (budget m 4 8) (Cube.leaf m0.PlanBase 1 4 8 + 100.0) 1e-6 "the ledger's budget and a hundred"
            let r = placeOf m.Cube.RowTree m.Rows 4
            Expect.stringStarts (Model.Table.cell m r (placeOf m.Cube.ColTree m.Cols 7)).Text "ƒ " "a formula is marked"
            Expect.approxEqual (total m 1) (total m0 1 + (budget m 4 7 - budget m0 4 7) + (budget m 4 8 - budget m0 4 8)) 1e-6 "the total moved by both"
        testCase "a cell that reads another is calculated again when it changes, and only what changed is written into the cube" <| fun () ->
            // account 0 is row 1, period 0 is column A, period 1 is column B
            let m0 = planModel () |> put 0 0 "100" |> put 0 1 "=A1*2"
            Expect.approxEqual (budget m0 0 1) 200.0 1e-9 "B1 is twice A1"
            let m = m0 |> put 0 0 "150"
            Expect.approxEqual (budget m 0 1) 300.0 1e-9 "and follows it"
            Expect.equal (m.LastRecalc |> Option.map (fun r -> r.Changed)) (Some 2) "A1 and B1: two cells changed, the rest was left alone"
            let same = m |> put 0 0 "150.0"
            Expect.equal (same.LastRecalc |> Option.map (fun r -> r.Changed)) (Some 0) "the same value written another way changes nothing downstream"
        testCase "an error is shown as the error it is, and the ledger's budget stays in the totals" <| fun () ->
            let m0 = planModel ()
            let m = m0 |> put 1 1 "=1/0" |> put 1 2 "=nonsense" |> put 1 3 "=C2"
            let text period = (Model.Table.cell m (placeOf m.Cube.RowTree m.Rows 1) (placeOf m.Cube.ColTree m.Cols period)).Text
            Expect.stringContains (text 1) "#DIV/0!" "division by zero"
            Expect.stringContains (text 2) "#NAME?" "a name that nothing knows"
            Expect.stringContains (text 3) "#NAME?" "an error is read on"
            Expect.approxEqual (total m 1) (total m0 1) 1e-6 "none of them is a number, so the totals are the ledger's"
            let loop = m0 |> put 2 0 "=B3" |> put 2 1 "=A3"
            Expect.stringContains (Model.Table.cell loop (placeOf loop.Cube.RowTree loop.Rows 2) (placeOf loop.Cube.ColTree loop.Cols 0)).Text "#CYCLE!" "a loop is named"
            Expect.approxEqual (total loop 1) (total m0 1) 1e-6 "and costs the totals nothing"
        testCase "an error that is corrected puts its number into the totals" <| fun () ->
            let m0 = planModel ()
            let m = m0 |> put 1 1 "=1/0" |> put 1 1 "=Actual"
            Expect.approxEqual (budget m 1 1) (Cube.leaf m0.PlanBase 0 1 1) 1e-6 "the number"
            Expect.approxEqual (total m 1) (total m0 1 - budget m0 1 1 + budget m 1 1) 1e-6 "in the total"
        testCase "undo and redo go back and forward over the steps, also across cells that read each other" <| fun () ->
            let m0 = planModel ()
            let m1 = m0 |> put 0 0 "100" |> put 0 1 "=A1*2" |> put 0 0 "150"
            Expect.approxEqual (budget m1 0 1) 300.0 1e-9 "after three steps"
            let u1 = m1 |> send [ Key "Undo" ]
            Expect.approxEqual (budget u1 0 0) 100.0 1e-9 "A1 is as it was"
            Expect.approxEqual (budget u1 0 1) 200.0 1e-9 "and B1 followed it back"
            let u3 = u1 |> send [ Undo; Undo ]
            Expect.approxEqual (total u3 1) (total m0 1) 1e-6 "all three undone: the totals of the ledger"
            Expect.equal u3.Plan.Count 0 "the plan is empty"
            Expect.equal (u3 |> send [ Undo ]) u3 "nothing more to undo changes nothing"
            let r = u3 |> send [ Key "Redo"; Redo; Redo ]
            Expect.approxEqual (budget r 0 1) 300.0 1e-9 "all three again"
            Expect.approxEqual (total r 1) (total m1 1) 1e-6 "the same totals"
            let branched = u1 |> put 5 5 "7"
            Expect.equal branched.Redo [] "a new step ends what could be redone"
        testCase "Delete empties the cell of the plan that is selected, and that is a step that can be undone" <| fun () ->
            let m0 = planModel ()
            let m1 = m0 |> put 6 6 "5000"
            let m2 = m1 |> select 6 6 |> send [ Key "Delete" ]
            Expect.equal (m2.Plan.Source(6, 6)) "" "empty"
            Expect.approxEqual (budget m2 6 6) (budget m0 6 6) 1e-9 "the ledger's budget is back"
            Expect.approxEqual (total m2 1) (total m0 1) 1e-6 "and so are the totals"
            Expect.approxEqual (budget (m2 |> send [ Undo ]) 6 6) 5000.0 1e-9 "undone"
            // the book of the plan is one mutable store that the models of a run share, so the empty cell is looked at in a fresh model
            let fresh = planModel () |> select 6 6
            Expect.equal (fresh |> send [ Key "Delete" ]) fresh "an empty cell has nothing to empty"
        testCase "moving to another cell keeps what was written; Tab keeps it and moves right" <| fun () ->
            let m0 = planModel () |> select 7 2
            let moved = m0 |> send [ BeginEdit(Some "321"); GridState("pivot", { m0.Main with Selected = Some(fst m0.Main.Selected.Value, snd m0.Main.Selected.Value + 1) }) ]
            Expect.approxEqual (budget moved 7 2) 321.0 1e-9 "kept"
            Expect.equal moved.Editing None "and the editing is over"
            let tabbed = m0 |> send [ BeginEdit(Some "654"); Key "Tab" ]
            Expect.approxEqual (budget tabbed 7 2) 654.0 1e-9 "kept"
            Expect.equal tabbed.Main.Selected (Some(fst m0.Main.Selected.Value, snd m0.Main.Selected.Value + 1)) "one to the right"
        testCase "only a leaf by a leaf on the Budget measure of the accounts and months can be written" <| fun () ->
            let actual = init () |> send [ Fold true ] |> select 0 0
            Expect.equal (actual |> send [ Key "5" ]).Editing None "Actual is the ledger's: nothing to write"
            let m = planModel ()
            Expect.equal (m |> send [ GridState("pivot", { m.Main with Selected = Some(0, 0) }); Key "5" ]).Editing None "the grand total is no cell of the plan"
            let group = Array.findIndex (fun n -> m.Cube.RowTree.Nodes.[n].Children.Length > 0 && n <> 0) m.Rows
            Expect.equal (m |> send [ GridState("pivot", { m.Main with Selected = Some(group, 2) }); Key "5" ]).Editing None "a group is a sum, not a cell"
            let centres = planModel () |> send [ SetPivot 1; Fold true ]
            Expect.equal (centres |> send [ GridState("pivot", { centres.Main with Selected = Some(centres.Rows.Length - 1, centres.Cols.Length - 1) }); Key "5" ]).Editing None "cost centres have no plan"
            Expect.equal (m |> select 0 0 |> send [ BeginEdit(Some "5"); SetMeasure 0 ]).Editing None "changing the measure ends an editing"
        testCase "turning the table keeps the plan: the months by the accounts show the same cells, the cost centres the ledger" <| fun () ->
            let m0 = planModel () |> put 0 0 "900" |> put 9 9 "=Actual/2"
            let turned = m0 |> send [ SetPivot 2; Fold true ]
            Expect.approxEqual (total turned 1) (total m0 1) 1e-6 "the same grand total"
            Expect.approxEqual (Cube.leaf turned.Cube 1 0 0) 900.0 1e-9 "the cell is the same: period 0 of account 0"
            Expect.approxEqual (Cube.leaf turned.Cube 1 9 9) (budget m0 9 9) 1e-6 "and the formula"
            let edited = turned |> send [ GridState("pivot", { turned.Main with Selected = Some(placeOf turned.Cube.RowTree turned.Rows 3, placeOf turned.Cube.ColTree turned.Cols 4) }); BeginEdit(Some "11"); CommitEdit ]
            Expect.approxEqual (Cube.leaf edited.Cube 1 3 4) 11.0 1e-9 "written in the turned table: row = month 3, column = account 4"
            let back = edited |> send [ SetPivot 0 ]
            Expect.approxEqual (budget back 4 3) 11.0 1e-9 "and it is account 4, month 3 when turned back"
            Expect.approxEqual (total back 1) (total edited 1) 1e-6 "the same total again"
            let centres = back |> send [ SetPivot 1 ]
            Expect.approxEqual (total centres 1) (Cube.value (init () |> send [ SetPivot 1 ]).Cube 1 0 0) 1e-6 "by cost centre the ledger's own budget"
        testCase "the cube of an edited plan is the one that is built from the whole plan at once" <| fun () ->
            let m = planModel () |> put 0 0 "100" |> put 0 1 "=A1+Actual" |> put 12 3 "=Budget*3" |> put 0 0 "=Actual" |> put 12 3 "" |> send [ Undo ]
            let whole = Model.Plan.apply m.Pivot m.Plan m.Raw
            for measure in 0 .. Model.measureCount - 1 do
                for (r, c) in [ (0, 0); (0, 1); (1, 0); (2, 5); (4, 10); (m.Cube.RowTree.Nodes.Length - 1, m.Cube.ColTree.Nodes.Length - 1) ] do
                    Expect.approxEqual (Cube.value m.Cube measure r c) (Cube.value whole measure r c) 1e-6 ("measure " + string measure + " at " + string r + "," + string c)
    ]

let scene =
    testList "Scene" [
        testCase "the page is built: a table with its name and the selected cell, a strip of tabs, the buttons" <| fun () ->
            let text = TextEngine.approximate
            let app = App(text, 5000, clock)
            let build () = app.Build(0.0f, Vec2.create 1280.0f 800.0f)
            let nodes = SemanticTree.walk (build ())
            let grid = nodes |> Array.find (fun s -> s.Role = Role.Grid)
            Expect.stringContains grid.Name "Accounts × Periods" "named after the pivot"
            Expect.isTrue (nodes |> Array.exists (fun s -> s.Role = Role.TabList)) "a strip of tabs"
            Expect.isTrue (nodes |> Array.exists (fun s -> s.Role = Role.Tab && s.Name = "Pivot" && s.Selected)) "the pivot tab is selected"
            Expect.isTrue ((nodes |> Array.filter (fun s -> s.Role = Role.Button) |> Array.length) >= 10) "the buttons of the pivots, the measures and Open all and Fold all"
            app.Dispatch(Key "Down")
            let after = SemanticTree.walk (build ()) |> Array.find (fun s -> s.Role = Role.Grid)
            Expect.stringContains after.Value "Total" "the selected cell is said"
            app.Dispatch(OpenCell(1, 1))
            let tabs = SemanticTree.walk (build ()) |> Array.filter (fun s -> s.Role = Role.Tab)
            Expect.equal tabs.Length 2 "the pivot and the lines of the cell"
            Expect.isTrue (tabs |> Array.exists (fun s -> s.Selected && s.Name <> "Pivot")) "the new tab is the one on show"
        testCase "a canvas of another size is told to the model" <| fun () ->
            let app = App(TextEngine.approximate, 1000, clock)
            app.Build(0.0f, Vec2.create 900.0f 600.0f) |> ignore
            Expect.equal app.Model.Viewport (Vec2.create 900.0f 600.0f) "the size"
    ]

let suites = [ ledger; model; plan; scene ]
