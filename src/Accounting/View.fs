namespace Accounting

open FSharp.Math
open FSharp.Wasm.Frame
open FSharp.Graphics.Core
open FSharp.Graphics.Scene
open FSharp.Graphics.SceneDsl
open FSharp.Graphics.Widgets

module View =

    /// A row of buttons from `x`, each as wide as its text; the one that is chosen is the filled one.
    let private buttons (ui: Ui<Msg>) (x: float32) (y: float32) (items: (string * string * bool * (unit -> unit)) list) : Scene =
        let th = ui.Theme
        let _, placed =
            items
            |> List.fold (fun (at, acc) (id, text, chosen, onClick) ->
                let b = Button.view ui id { Button.make text onClick with Variant = (if chosen then Button.Primary else Button.Secondary) }
                (at + th.PadX * 2.0f + Ui.textWidth ui th.FontSize text + th.Gap, shift at y b :: acc)) (x, [])
        Group(List.rev placed)

    let private caption (ui: Ui<Msg>) (x: float32) (y: float32) (text: string) : Scene =
        shift x (y + (ui.Theme.ControlHeight - ui.Theme.FontSize * 1.2f) * 0.5f) (Label.view ui Label.Muted text)

    /// The table that is on show, in the tab that is chosen.
    let private table (ui: Ui<Msg>) (m: Model) : Scene =
        match m.Details |> List.tryFind (fun d -> d.Id = m.Tab) with
        | Some d ->
            Tabs.panel "tabs" d.Title
                (Grid.view ui "grid" ("Journal lines: " + d.Title)
                    { Frame = Model.detailFrame m d; State = d.Grid; Corner = "Date"
                      Row = Model.Table.detailRow m d; Col = Model.Table.detailCol; Cell = Model.Table.detailCell m d
                      OnState = fun s -> ui.Send(GridState(d.Id, s))
                      OnToggleRow = ignore; OnToggleCol = ignore; OnOpen = fun _ _ -> () })
        | None ->
            let p = Pivots.all.[m.Pivot]
            Tabs.panel "tabs" "Pivot"
                (Grid.view ui "grid" ("Ledger, " + p.Title + ", " + Model.measureName m.Measure)
                    { Frame = Model.mainFrame m; State = m.Main; Corner = p.Title
                      Row = Model.Table.rowLine m; Col = Model.Table.colLine m; Cell = Model.Table.cell m
                      OnState = fun s -> ui.Send(GridState("pivot", s))
                      OnToggleRow = fun i -> ui.Send(ToggleRow i)
                      OnToggleCol = fun i -> ui.Send(ToggleCol i)
                      OnOpen = fun r c -> ui.Send(OpenCell(r, c)) })

    let private status (m: Model) : string =
        let p = Pivots.all.[m.Pivot]
        Format.count (int64 m.Facts.Count) + " journal lines  ·  " + p.Title + " built in " + Format.millis m.CubeMs + " ms  ·  "
        + Format.count (int64 m.Cube.RowTree.Leaves) + " × " + Format.count (int64 m.Cube.ColTree.Leaves) + " leaves  ·  "
        + Format.count (int64 m.Rows.Length) + " × " + Format.count (int64 m.Cols.Length) + " shown"

    let view (ui: Ui<Msg>) (size: Vec2) (m: Model) : Scene =
        let th = ui.Theme
        let pad = Layout.pad
        // `size` is an operation of the builders below
        let width, height = size.X, size.Y
        let r = Layout.gridRect size
        let pivots =
            Pivots.all |> Array.mapi (fun i p -> ("pivot" + string i, p.Title, (i = m.Pivot), (fun () -> ui.Send(SetPivot i)))) |> List.ofArray
        let measures =
            [ for i in 0 .. Model.measureCount - 1 -> ("measure" + string i, Model.measureName i, (i = m.Measure), (fun () -> ui.Send(SetMeasure i))) ]
        let foldW = th.PadX * 2.0f + Ui.textWidth ui th.FontSize "Fold all" + th.PadX * 2.0f + Ui.textWidth ui th.FontSize "Open all" + th.Gap
        let tab (id: string) (title: string) (closable: bool) : Tabs.Tab = { Id = id; Title = title; Closable = closable }
        let tabs = tab "pivot" "Pivot" false :: (m.Details |> List.map (fun d -> tab d.Id d.Title true))
        let strip =
            Tabs.viewNamed ui "tabs" "Tables"
                { Tabs.make tabs m.Tab (fun id -> ui.Send(Select id)) with Width = r.W; OnClose = fun id -> ui.Send(Close id) }
        let page =
            Group [
                rectangle { bounds 0.0f 0.0f width height; fill th.Background }
                shift pad Layout.titleY (Label.view ui Label.Title "Accounting grid")
                caption ui pad Layout.pivotsY "Pivot"
                buttons ui (pad + 64.0f) Layout.pivotsY pivots
                caption ui pad Layout.measuresY "Measure"
                buttons ui (pad + 64.0f) Layout.measuresY measures
                buttons ui (width - pad - foldW) Layout.measuresY
                    [ ("fold-open", "Open all", false, (fun () -> ui.Send(Fold true))); ("fold-close", "Fold all", false, (fun () -> ui.Send(Fold false))) ]
                shift pad Layout.tabsY strip
                shift r.X r.Y (table ui m)
                shift pad (height - Layout.statusH - 4.0f) (Label.view ui Label.Caption (status m))
            ]
        Ui.root ui (fun k -> ui.Send(Key k)) page
