namespace Accounting

open FSharp.Math
open FSharp.Wasm.Frame
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

type Model =
    { Facts: Facts
      Pivot: int
      Cube: Cube
      /// the index of the measure shown: Actual, Budget, Variance, Variance %
      Measure: int
      /// which nodes of the two trees are open, by node number (node 0, the total, included)
      RowOpen: bool[]
      ColOpen: bool[]
      /// the nodes that are shown, in order
      Rows: int[]
      Cols: int[]
      Main: Grid.State
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

module Layout =
    let pad = 16.0f
    let titleY = 12.0f
    let pivotsY = 54.0f
    let measuresY = 98.0f
    let tabsY = 144.0f
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

module Model =

    let private measureNames = [| "Actual"; "Budget"; "Variance"; "Variance %" |]
    let measureName (i: int) : string = measureNames.[i]
    let measureCount = measureNames.Length

    let private buildCube (facts: Facts) (pivot: int) : Cube =
        let p = Pivots.all.[pivot]
        Facts.pivot facts p.RowDim p.ColDim Ledger.formulas

    /// Open as far as the levels given: the total and the first `levels` levels.
    let private openTo (tree: Hierarchy) (levels: int) : bool[] = tree.Nodes |> Array.map (fun n -> n.Level < levels)

    /// The nodes that are shown: the total, and below it what is open.  A total that is folded is one line.
    let private visible (tree: Hierarchy) (isOpen: bool[]) : int[] =
        if isOpen.[0] then Array.append [| 0 |] (Hierarchy.visible tree (fun i -> isOpen.[i])) else [| 0 |]

    let mainFrame (m: Model) : Grid.Frame =
        let r = Layout.gridRect m.Viewport
        { Rows = m.Rows.Length; Cols = m.Cols.Length; Width = r.W; Height = r.H; Metrics = Layout.mainMetrics }

    let detailFrame (m: Model) (d: Detail) : Grid.Frame =
        let r = Layout.gridRect m.Viewport
        { Rows = d.Entries.Length; Cols = Layout.detailColumns.Length; Width = r.W; Height = r.H; Metrics = Layout.detailMetrics }

    let private nodeLabel (tree: Hierarchy) (id: int) : string = if id = 0 then "Total" else tree.Nodes.[id].Label

    // ── the lines and the cells, as the grid asks for them ──
    module Table =
        let rowLine (m: Model) (i: int) : Grid.Line =
            let n = m.Cube.RowTree.Nodes.[m.Rows.[i]]
            { Label = nodeLabel m.Cube.RowTree n.Id; Level = n.Level + 1; Group = n.Children.Length > 0; Open = m.RowOpen.[n.Id] }

        let colLine (m: Model) (i: int) : Grid.Line =
            let n = m.Cube.ColTree.Nodes.[m.Cols.[i]]
            { Label = nodeLabel m.Cube.ColTree n.Id; Level = n.Level + 1; Group = n.Children.Length > 0; Open = m.ColOpen.[n.Id] }

        let cell (m: Model) (r: int) (c: int) : Grid.Cell =
            let v = Cube.value m.Cube m.Measure m.Rows.[r] m.Cols.[c]
            { Text = Format.amount v; Muted = (v = 0.0); Left = false }

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
        let m1 = { m with Main = Grid.clamp (mainFrame m) m.Main }
        { m1 with Details = m1.Details |> List.map (fun d -> { d with Grid = Grid.clamp (detailFrame m1 d) d.Grid }) }

    let private turned (m: Model) (pivot: int) (cube: Cube) : Model =
        reflow
            { m with
                Pivot = pivot
                Cube = cube
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
        let cube = buildCube facts 0
        let t2 = clock ()
        let m =
            { Facts = facts
              Pivot = 0
              Cube = cube
              Measure = 0
              RowOpen = openTo cube.RowTree 1
              ColOpen = openTo cube.ColTree 1
              Rows = [||]
              Cols = [||]
              Main = Grid.start
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

    /// What a key does to the table that is on show.
    let private key (m: Model) (k: string) : Model =
        if m.Tab = "pivot" then
            match Grid.key (mainFrame m) m.Ui.Shift k m.Main with
            | Grid.Moved s -> { m with Main = s }
            | Grid.ToggleRow r -> reflow { m with RowOpen = toggle m.RowOpen m.Rows.[r] }
            | Grid.ToggleCol c -> reflow { m with ColOpen = toggle m.ColOpen m.Cols.[c] }
            | Grid.Open(r, c) -> openCell m r c
            | Grid.Ignored -> m
        else
            match m.Details |> List.tryFind (fun d -> d.Id = m.Tab) with
            | Some d ->
                (match Grid.key (detailFrame m d) m.Ui.Shift k d.Grid with
                 | Grid.Moved s -> { m with Details = m.Details |> List.map (fun x -> if x.Id = d.Id then { x with Grid = s } else x) }
                 | _ -> m)
            | None -> m

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
                    let cube = buildCube m.Facts p
                    let t1 = clock ()
                    { turned m p cube with CubeMs = t1 - t0 }
            | SetMeasure i -> { m with Measure = i }
            | ToggleRow i -> if i < m.Rows.Length then reflow { m with RowOpen = toggle m.RowOpen m.Rows.[i] } else m
            | ToggleCol i -> if i < m.Cols.Length then reflow { m with ColOpen = toggle m.ColOpen m.Cols.[i] } else m
            | Fold openAll ->
                let all (tree: Hierarchy) = tree.Nodes |> Array.mapi (fun i _ -> openAll || i = 0)
                reflow { m with RowOpen = all m.Cube.RowTree; ColOpen = all m.Cube.ColTree }
            | GridState(tab, s) ->
                if tab = "pivot" then { m with Main = s }
                else { m with Details = m.Details |> List.map (fun d -> if d.Id = tab then { d with Grid = s } else d) }
            | OpenCell(r, c) -> openCell m r c
            | Select id -> { m with Tab = id }
            | Close id -> closeTab m id
            | Key k -> key m k
        next, Cmd.none
