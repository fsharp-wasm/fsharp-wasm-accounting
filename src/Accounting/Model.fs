namespace Accounting

open FSharp.Math
open FSharp.Wasm.Frame
open FSharp.Wasm.Formula
open FSharp.Graphics.Tea
open FSharp.Graphics.Widgets

/// Two dimensions of the ledger set against each other.
type Pivot = { Title: string; RowDim: int; ColDim: int }

module Pivots =
    let all : Pivot[] =
        [| { Title = "Accounts × Periods"; RowDim = Ledger.Accounts; ColDim = Ledger.Periods }
           { Title = "Cost centres × Periods"; RowDim = Ledger.Centres; ColDim = Ledger.Periods }
           { Title = "Periods × Accounts"; RowDim = Ledger.Periods; ColDim = Ledger.Accounts }
           { Title = "Accounts × Cost centres"; RowDim = Ledger.Accounts; ColDim = Ledger.Centres } |]

/// The lines behind one cell of the pivot, in a tab of their own.
type Detail =
    { Id: string
      Title: string
      /// the positions of the journal lines
      Entries: int[]
      Grid: Grid.State }

/// A cell of the plan being written: the budget of an account in a month (their leaves), and the text typed so far.
type Edit = { Account: int; Period: int; Text: string }

/// What a step of the plan did, for the status line: the cell, how many cells came out with another value (the cell itself, and what reads it), and what the engine did for it: how many
/// calcs the step reached and how many of those it ran (the rest came out the same and were left alone).
type Recalc = { Cell: string; Changed: int; Reached: int; Ran: int }

type Model =
    { Facts: Facts
      Pivot: int
      /// the cube as the table shows it: the facts for the two dimensions with the plan written over the Budget measure (a snapshot of `G.Cube` after the last step)
      Cube: Cube
      /// the plan as a graph: the text of the cells, the formulas, the cube, the tape and the undo.  It is changed only by `update`.
      G: Planning
      /// the cell being written, when there is one
      Editing: Edit option
      /// how many steps are on the tape: what tells a model after a step from one before it
      Steps: int
      LastRecalc: Recalc option
      /// the index of the measure shown: Actual, Budget, Variance, Variance %
      Measure: int
      /// which nodes of the two trees are open, by node number (node 0, the total, included)
      RowOpen: bool[]
      ColOpen: bool[]
      /// the nodes that are shown, in order
      Rows: int[]
      Cols: int[]
      Main: Grid.State
      /// the table of the steps on the tape
      HistoryGrid: Grid.State
      Details: Detail list
      /// "pivot", or the id of a detail
      Tab: string
      Viewport: Vec2
      Ui: UiState
      /// what making the lines and building the cube took, in milliseconds (the host measures)
      GenMs: float
      CubeMs: float }

type Msg =
    | UiMsg of UiMsg
    | Resize of Vec2
    | SetPivot of int
    | SetMeasure of int
    /// a row or a column that is a total, by its place among the lines that are shown: open it or fold it
    | ToggleRow of int
    | ToggleCol of int
    /// open every group (true), or fold everything to the first level (false)
    | Fold of bool
    | GridState of tab: string * Grid.State
    | OpenCell of row: int * col: int
    | Select of string
    | Close of string
    | Key of string
    /// start writing the plan cell that is selected (with its text, or with this text instead)
    | BeginEdit of string option
    | EditText of string
    | CommitEdit
    | CancelEdit
    | Undo
    | Redo

module Layout =
    let pad = 16.0f
    let titleY = 12.0f
    let pivotsY = 54.0f
    let measuresY = 98.0f
    /// the bar that shows the plan cell that is selected and the text being typed into it
    let formulaY = 142.0f
    let tabsY = 186.0f
    let statusH = 30.0f

    /// Where the table is, for a canvas of `viewport`.
    let gridRect (viewport: Vec2) : Rect =
        let y = tabsY + 38.0f
        Rect.create pad y (max 200.0f (viewport.X - 2.0f * pad)) (max 120.0f (viewport.Y - y - statusH - pad))

    /// The totals of a million lines are nine digits and two decimals: a column wide enough for them.
    let mainMetrics : Grid.Metrics = { Grid.metrics with ColWidth = 136.0f }

    /// The lines behind a cell are wider columns and a narrow row header (the date).
    let detailMetrics : Grid.Metrics = { Grid.metrics with ColWidth = 150.0f; HeaderWidth = 120.0f }
    let detailColumns = [| "Account"; "Cost centre"; "Reference"; "Actual"; "Budget" |]

    /// The steps on the tape: the step is the line header, so it is wide, and four small columns say what the engine did for it.
    let historyMetrics : Grid.Metrics = { Grid.metrics with ColWidth = 110.0f; HeaderWidth = 340.0f }
    let historyColumns = [| "Reached"; "Ran"; "Left alone"; "Changed" |]

module Model =

    let private measureNames = [| "Actual"; "Budget"; "Variance"; "Variance %" |]
    let measureName (i: int) : string = measureNames.[i]
    let measureCount = measureNames.Length

    /// Open as far as the levels given: the total and the first `levels` levels.
    let private openTo (tree: Hierarchy) (levels: int) : bool[] = tree.Nodes |> Array.map (fun n -> n.Level < levels)

    /// The nodes that are shown: the total, and below it what is open.  A total that is folded is one line.
    let private visible (tree: Hierarchy) (isOpen: bool[]) : int[] =
        if isOpen.[0] then Array.append [| 0 |] (Hierarchy.visible tree (fun i -> isOpen.[i])) else [| 0 |]

    let mainFrame (m: Model) : Grid.Frame =
        let r = Layout.gridRect m.Viewport
        { Rows = m.Rows.Length; Cols = m.Cols.Length; Width = r.W; Height = r.H; Metrics = Layout.mainMetrics }

    /// The steps on the tape, the newest first.
    let historyFrame (m: Model) : Grid.Frame =
        let r = Layout.gridRect m.Viewport
        { Rows = m.Steps; Cols = Layout.historyColumns.Length; Width = r.W; Height = r.H; Metrics = Layout.historyMetrics }

    let detailFrame (m: Model) (d: Detail) : Grid.Frame =
        let r = Layout.gridRect m.Viewport
        { Rows = d.Entries.Length; Cols = Layout.detailColumns.Length; Width = r.W; Height = r.H; Metrics = Layout.detailMetrics }

    let private nodeLabel (tree: Hierarchy) (id: int) : string = if id = 0 then "Total" else tree.Nodes.[id].Label

    // ── the plan: what a person writes into the budget of an account in a month ──
    module Plan =
        /// The grids that show the plan are the ones of accounts and months, either way round; the others show the budget of the ledger.
        let pivotHasPlan (pivot: int) : bool = pivot = 0 || pivot = 2

        /// The account and the month (their leaves) of the cell at a place of the grid, when that is a cell of the plan: the Budget measure, a leaf on both axes.
        let leafAt (m: Model) (r: int) (c: int) : (int * int) option =
            if m.Tab = "pivot" && m.Measure = 1 && pivotHasPlan m.Pivot && r >= 0 && c >= 0 && r < m.Rows.Length && c < m.Cols.Length && m.Rows.[r] <> 0 && m.Cols.[c] <> 0 then
                let rn = m.Cube.RowTree.Nodes.[m.Rows.[r]]
                let cn = m.Cube.ColTree.Nodes.[m.Cols.[c]]
                if rn.Children.Length = 0 && cn.Children.Length = 0 then (if m.Pivot = 0 then Some (rn.Lo, cn.Lo) else Some (cn.Lo, rn.Lo)) else None
            else None

        /// The cell of the plan that is selected, when there is one.
        let selected (m: Model) : (int * int) option =
            match m.Main.Selected with
            | Some (r, c) -> leafAt m r c
            | None -> None

        let cellName (m: Model) (account: int) (period: int) : string =
            Ledger.leafLabel m.Facts Ledger.Accounts account + " · " + Ledger.leafLabel m.Facts Ledger.Periods period

    // ── the lines and the cells, as the grid asks for them ──
    module Table =
        let rowLine (m: Model) (i: int) : Grid.Line =
            let n = m.Cube.RowTree.Nodes.[m.Rows.[i]]
            { Label = nodeLabel m.Cube.RowTree n.Id; Level = n.Level + 1; Group = n.Children.Length > 0; Open = m.RowOpen.[n.Id] }

        let colLine (m: Model) (i: int) : Grid.Line =
            let n = m.Cube.ColTree.Nodes.[m.Cols.[i]]
            { Label = nodeLabel m.Cube.ColTree n.Id; Level = n.Level + 1; Group = n.Children.Length > 0; Open = m.ColOpen.[n.Id] }

        let cell (m: Model) (r: int) (c: int) : Grid.Cell =
            match Plan.leafAt m r c with
            | Some (account, period) when Planning.source m.G account period <> "" ->
                // a cell of the plan: its value (a formula is marked ƒ, a number that was typed •), or the error it is
                let at = Planning.cellAt account period
                let marked = (if m.G.Sheet.Formula(at).IsSome then "ƒ " else "• ")
                (match m.G.Sheet.Peek(at) with
                 | Number n -> { Text = marked + Format.amount n; Muted = false; Left = false }
                 | v -> { Text = marked + Value.toText v; Muted = false; Left = false })
            | _ ->
                let v = Cube.value m.Cube m.Measure m.Rows.[r] m.Cols.[c]
                { Text = Format.amount v; Muted = (v = 0.0); Left = false }

        /// The step in line `i` of the history (the newest is the first): its number and what was done.
        let historyRow (tape: FSharp.Wasm.Graph.Entry[]) (i: int) : Grid.Line =
            let e = tape.[tape.Length - 1 - i]
            { Label = string e.Number + "  " + e.Label + (if e.Remote then "  (from elsewhere)" else ""); Level = 0; Group = false; Open = false }

        let historyCol (c: int) : Grid.Line = { Label = Layout.historyColumns.[c]; Level = 0; Group = false; Open = false }

        /// What the engine did for a step: the calcs it reached, how many of them ran, how many it left alone because they came out the same, and how many came out with another value.
        let historyCell (tape: FSharp.Wasm.Graph.Entry[]) (r: int) (c: int) : Grid.Cell =
            let f = tape.[tape.Length - 1 - r].Flushed
            let n = match c with 0 -> f.Reached | 1 -> f.Ran | 2 -> f.Reached - f.Ran | _ -> f.Changed
            { Text = string n; Muted = (n = 0); Left = false }

        let detailRow (m: Model) (d: Detail) (i: int) : Grid.Line =
            { Label = Ledger.dateOf m.Facts d.Entries.[i]; Level = 0; Group = false; Open = false }

        let detailCol (c: int) : Grid.Line = { Label = Layout.detailColumns.[c]; Level = 0; Group = false; Open = false }

        let detailCell (m: Model) (d: Detail) (r: int) (c: int) : Grid.Cell =
            let line = d.Entries.[r]
            let f = m.Facts
            match c with
            | 0 -> { Text = Ledger.leafLabel f Ledger.Accounts f.Dimensions.[Ledger.Accounts].Leaf.[line]; Muted = false; Left = true }
            | 1 -> { Text = Ledger.leafLabel f Ledger.Centres f.Dimensions.[Ledger.Centres].Leaf.[line]; Muted = false; Left = true }
            | 2 -> { Text = "INV-" + string (100000 + line); Muted = true; Left = true }
            | 3 -> { Text = Format.amount f.Values.[0].[line]; Muted = false; Left = false }
            | _ -> { Text = Format.amount f.Values.[1].[line]; Muted = true; Left = false }

    let private clampSelection (rows: int) (cols: int) (s: Grid.State) : Grid.State =
        match s.Selected with
        | Some(r, c) when rows > 0 && cols > 0 -> { s with Selected = Some(min r (rows - 1), min c (cols - 1)) }
        | Some _ -> { s with Selected = None }
        | None -> s

    /// The lines and columns that are shown again, after something was opened, folded or turned; the table keeps its place where it still has one.
    let private reflow (m: Model) : Model =
        let m1 = { m with Rows = visible m.Cube.RowTree m.RowOpen; Cols = visible m.Cube.ColTree m.ColOpen }
        { m1 with Main = Grid.clamp (mainFrame m1) (clampSelection m1.Rows.Length m1.Cols.Length m1.Main) }

    /// A table is held inside its window: after the window changed size, and after a tab was opened.
    let private fit (m: Model) : Model =
        let m1 = { m with Main = Grid.clamp (mainFrame m) m.Main; HistoryGrid = Grid.clamp (historyFrame m) m.HistoryGrid }
        { m1 with Details = m1.Details |> List.map (fun d -> { d with Grid = Grid.clamp (detailFrame m1 d) d.Grid }) }

    /// The table turned to another pivot: the graph has built the cube for the new axes (and written the plan over it), the lines are the first levels again.
    let private turned (m: Model) (pivot: int) : Model =
        let cube = m.G.Cube.Peek
        reflow
            { m with
                Pivot = pivot
                Cube = cube
                Editing = None
                RowOpen = openTo cube.RowTree 1
                ColOpen = openTo cube.ColTree 1
                Main = Grid.start
                Details = []
                Tab = "pivot" }

    /// A fresh application over `lines` journal lines.  `clock` reads milliseconds (the host's `Gfx.now`, a Stopwatch under .NET): it times the two steps that cost something.
    let init (lines: int) (clock: unit -> float) : Model * Cmd<Msg> =
        let t0 = clock ()
        let facts = Ledger.facts lines 20261123u
        let t1 = clock ()
        let g = Planning.create facts Ledger.formulas
        let cube = g.Cube.Peek
        let t2 = clock ()
        let m =
            { Facts = facts
              Pivot = 0
              Cube = cube
              G = g
              Editing = None
              Steps = 0
              LastRecalc = None
              Measure = 0
              RowOpen = openTo cube.RowTree 1
              ColOpen = openTo cube.ColTree 1
              Rows = [||]
              Cols = [||]
              Main = Grid.start
              HistoryGrid = Grid.start
              Details = []
              Tab = "pivot"
              Viewport = Vec2.create 1280.0f 800.0f
              Ui = UiState.empty
              GenMs = t1 - t0
              CubeMs = t2 - t1 }
        reflow m, Cmd.none

    let private detailTitle (m: Model) (rowNode: int) (colNode: int) : string =
        let short (s: string) = if s.Length > 18 then s.Substring(0, 17) + "…" else s
        short (nodeLabel m.Cube.RowTree rowNode) + " · " + short (nodeLabel m.Cube.ColTree colNode)

    /// The lines behind a cell, in a tab (the tab that is already open for it, when there is one).  At most six tabs are kept: the oldest goes.
    let private openCell (m: Model) (r: int) (c: int) : Model =
        if r >= m.Rows.Length || c >= m.Cols.Length then m
        else
            let p = Pivots.all.[m.Pivot]
            let rowNode = m.Rows.[r]
            let colNode = m.Cols.[c]
            let id = "d" + string m.Pivot + ":" + string rowNode + ":" + string colNode
            if m.Details |> List.exists (fun d -> d.Id = id) then { m with Tab = id }
            else
                let d = { Id = id; Title = detailTitle m rowNode colNode; Entries = Facts.entries m.Facts p.RowDim rowNode p.ColDim colNode; Grid = Grid.start }
                let kept = if m.Details.Length >= 6 then List.tail m.Details else m.Details
                { m with Details = kept @ [ d ]; Tab = id }

    let private closeTab (m: Model) (id: string) : Model =
        let rest = m.Details |> List.filter (fun d -> d.Id <> id)
        { m with Details = rest; Tab = (if m.Tab = id then "pivot" else m.Tab) }

    let private toggle (isOpen: bool[]) (node: int) : bool[] = isOpen |> Array.mapi (fun i o -> if i = node then not o else o)

    // ── writing the plan: every step is a step on the tape of the graph ──

    /// The text a cell starts with when it is opened for editing: what was written into it, or the budget of the ledger as a number.
    let private startText (m: Model) (account: int) (period: int) : string =
        let written = Planning.source m.G account period
        if written <> "" then written else Value.numberText (Cube.leaf m.G.Base 1 account period)

    let private beginEdit (m: Model) (initial: string option) : Model =
        match Plan.selected m with
        | Some (account, period) ->
            { m with Editing = Some { Account = account; Period = period; Text = (match initial with Some t -> t | None -> startText m account period) } }
        | None -> m

    /// The model after a step of the graph: the cube it made, the step on the tape, and what the engine did for it (the status line says it).
    let private stepped (m: Model) (cell: string) (result: (FSharp.Wasm.Graph.Applied * int) option) : Model =
        match result with
        | None -> m
        | Some (a, changed) ->
            { m with
                Cube = m.G.Cube.Peek
                Steps = (Planning.tape m.G).Length
                LastRecalc = Some { Cell = cell; Changed = changed; Reached = a.Flushed.Reached; Ran = a.Flushed.Ran } }

    /// Keep what was typed: one step on the tape (nothing when it is what the cell holds).
    let private commitEdit (m: Model) : Model =
        match m.Editing with
        | Some e ->
            let cell = Plan.cellName m e.Account e.Period
            if Planning.source m.G e.Account e.Period = e.Text then { m with Editing = None }
            else stepped { m with Editing = None } cell (Some (Planning.write m.G (cell + " = " + e.Text) e.Account e.Period e.Text))
        | None -> m

    /// The label of the last step on the tape: what undo and redo name in the status line.
    let private lastLabel (m: Model) : string =
        match Array.tryLast (Planning.tape m.G) with
        | Some entry -> entry.Label
        | None -> ""

    let private undoEdit (m: Model) : Model =
        let r = Planning.undo m.G
        stepped { m with Editing = None } (lastLabel m) r

    let private redoEdit (m: Model) : Model =
        let r = Planning.redo m.G
        stepped { m with Editing = None } (lastLabel m) r

    /// Empty the cell of the plan that is selected: the ledger's budget is there again.
    let private clearPlanCell (m: Model) : Model =
        match Plan.selected m with
        | Some (account, period) when Planning.source m.G account period <> "" ->
            let cell = Plan.cellName m account period
            stepped m cell (Some (Planning.write m.G (cell + " cleared") account period ""))
        | _ -> m

    /// What a key does while a cell is being written: the characters go into the text, Enter and Tab keep it, Escape throws it away.
    let private editKey (m: Model) (e: Edit) (k: string) : Model =
        match k with
        | "Enter" -> commitEdit m
        | "Escape" -> { m with Editing = None }
        | "Tab" ->
            let m1 = commitEdit m
            (match Grid.key (mainFrame m1) false "Right" m1.Main with
             | Grid.Moved s -> { m1 with Main = s }
             | _ -> m1)
        | "Backspace" -> { m with Editing = Some { e with Text = (if e.Text.Length > 0 then e.Text.Substring(0, e.Text.Length - 1) else "") } }
        | _ -> if k.Length = 1 then { m with Editing = Some { e with Text = e.Text + k } } else m

    /// What a key does to the table that is on show.
    let private gridKey (m: Model) (k: string) : Model =
        if m.Tab = "pivot" then
            match Grid.key (mainFrame m) m.Ui.Shift k m.Main with
            | Grid.Moved s -> { m with Main = s }
            | Grid.ToggleRow r -> reflow { m with RowOpen = toggle m.RowOpen m.Rows.[r] }
            | Grid.ToggleCol c -> reflow { m with ColOpen = toggle m.ColOpen m.Cols.[c] }
            | Grid.Open(r, c) -> openCell m r c
            | Grid.Ignored -> m
        elif m.Tab = "history" then
            match Grid.key (historyFrame m) m.Ui.Shift k m.HistoryGrid with
            | Grid.Moved s -> { m with HistoryGrid = s }
            | _ -> m
        else
            match m.Details |> List.tryFind (fun d -> d.Id = m.Tab) with
            | Some d ->
                (match Grid.key (detailFrame m d) m.Ui.Shift k d.Grid with
                 | Grid.Moved s -> { m with Details = m.Details |> List.map (fun x -> if x.Id = d.Id then { x with Grid = s } else x) }
                 | _ -> m)
            | None -> m

    /// A key: writing into the plan when a cell of it is being written or is selected (F2 or a character opens it, Delete empties it, Ctrl+Z and Ctrl+Y undo and redo), the table's own keys otherwise.
    let private key (m: Model) (k: string) : Model =
        match m.Editing with
        | Some e -> editKey m e k
        | None ->
            match k with
            | "Undo" -> undoEdit m
            | "Redo" -> redoEdit m
            | "F2" -> beginEdit m None
            | "Delete" when (Plan.selected m).IsSome -> clearPlanCell m
            | _ when k.Length = 1 && k <> " " && (Plan.selected m).IsSome -> beginEdit m (Some k)
            | _ -> gridKey m k

    /// The program's step.  `clock` reads milliseconds: a turn of the table is timed (it builds the cube again, one pass over the lines).
    let update (clock: unit -> float) (m: Model) (msg: Msg) : Model * Cmd<Msg> =
        let next =
            match msg with
            | UiMsg u -> { m with Ui = UiState.update m.Ui u }
            | Resize size -> fit { m with Viewport = size }
            | SetPivot p ->
                if p = m.Pivot then m
                else
                    let t0 = clock ()
                    let d = Pivots.all.[p]
                    Planning.turn m.G d.RowDim d.ColDim
                    let t1 = clock ()
                    { turned m p with CubeMs = t1 - t0 }
            | SetMeasure i -> { m with Measure = i; Editing = None }
            | ToggleRow i -> if i < m.Rows.Length then reflow { m with RowOpen = toggle m.RowOpen m.Rows.[i] } else m
            | ToggleCol i -> if i < m.Cols.Length then reflow { m with ColOpen = toggle m.ColOpen m.Cols.[i] } else m
            | Fold openAll ->
                let all (tree: Hierarchy) = tree.Nodes |> Array.mapi (fun i _ -> openAll || i = 0)
                reflow { m with RowOpen = all m.Cube.RowTree; ColOpen = all m.Cube.ColTree }
            | GridState(tab, s) ->
                // moving to another cell keeps what was written
                if tab = "pivot" then (let m1 = (if m.Editing.IsSome && s.Selected <> m.Main.Selected then commitEdit m else m) in { m1 with Main = s })
                elif tab = "history" then { m with HistoryGrid = s }
                else { m with Details = m.Details |> List.map (fun d -> if d.Id = tab then { d with Grid = s } else d) }
            | OpenCell(r, c) -> openCell m r c
            | Select id -> { m with Tab = id }
            | Close id -> closeTab m id
            | Key k -> key m k
            | BeginEdit initial -> beginEdit m initial
            | EditText t -> (match m.Editing with Some e -> { m with Editing = Some { e with Text = t } } | None -> m)
            | CommitEdit -> commitEdit m
            | CancelEdit -> { m with Editing = None }
            | Undo -> undoEdit m
            | Redo -> redoEdit m
        next, Cmd.none
