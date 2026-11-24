namespace Accounting

open FSharp.Wasm.Frame

/// A ledger to look at: a chart of accounts, cost centres, two years of months, and journal lines made from a seed.  The same lines under .NET and as WebAssembly GC, so a
/// number in a test is the number on the page.
module Ledger =

    /// The dimensions of the facts, by number.
    [<Literal>]
    let Accounts = 0
    [<Literal>]
    let Centres = 1
    [<Literal>]
    let Periods = 2

    /// A class of accounts, the way its lines count in a result (revenue for it, costs against it), its first account number, and its groups.
    type private Class = { Name: string; Sign: float; First: int; Groups: (string * string[])[] }

    let private chart : Class[] =
        [| { Name = "Revenue"; Sign = 1.0; First = 4000
             Groups = [| "Product sales", [| "Domestic"; "EU"; "US"; "Asia"; "Other" |]
                         "Services", [| "Consulting"; "Support"; "Training"; "Projects"; "Other" |]
                         "Licences", [| "Subscription"; "Perpetual"; "OEM"; "Royalties"; "Other" |] |] }
           { Name = "Cost of sales"; Sign = -1.0; First = 5000
             Groups = [| "Materials", [| "Raw"; "Components"; "Packaging"; "Tooling"; "Scrap" |]
                         "Freight", [| "Inbound"; "Outbound"; "Customs"; "Insurance"; "Other" |]
                         "Production labour", [| "Direct"; "Overtime"; "Agency"; "Bonus"; "Other" |] |] }
           { Name = "Operating expenses"; Sign = -1.0; First = 6000
             Groups = [| "People", [| "Salaries"; "Social security"; "Pensions"; "Training"; "Recruiting" |]
                         "Premises", [| "Rent"; "Utilities"; "Cleaning"; "Repairs"; "Insurance" |]
                         "Marketing", [| "Advertising"; "Events"; "Content"; "Tools"; "Agencies" |]
                         "IT", [| "Hosting"; "Licences"; "Hardware"; "Support"; "Security" |]
                         "Travel", [| "Flights"; "Hotels"; "Meals"; "Rail"; "Other" |] |] }
           { Name = "Financial"; Sign = -1.0; First = 7000
             Groups = [| "Interest", [| "Loans"; "Deposits"; "Leasing"; "Fees"; "Other" |]
                         "Currency", [| "Gains"; "Losses"; "Hedging"; "Revaluation"; "Other" |] |] } |]

    /// The paths of the accounts (class, group, account), in the order of the chart: the order of the leaves.
    let accountPaths : string[][] =
        let out = ResizeArray<string[]>()
        for c in chart do
            for g in 0 .. c.Groups.Length - 1 do
                let name, accounts = c.Groups.[g]
                for a in 0 .. accounts.Length - 1 do
                    out.Add [| c.Name; name; string (c.First + g * 10 + a) + " " + accounts.[a] |]
        out.ToArray()

    /// +1 for an account of revenue, -1 for the others: what a line of that account does to the result.
    let private signs : float[] =
        let out = ResizeArray<float>()
        for c in chart do
            for _, accounts in c.Groups do
                for _ in accounts do out.Add c.Sign
        out.ToArray()

    let centrePaths : string[][] =
        [| [| "EMEA"; "Berlin" |]; [| "EMEA"; "Paris" |]; [| "EMEA"; "London" |]
           [| "Americas"; "New York" |]; [| "Americas"; "Austin" |]
           [| "APAC"; "Singapore" |]; [| "APAC"; "Tokyo" |] |]

    let private monthNames = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
    let firstYear = 2025
    let years = 2

    /// The months of the years, as year, quarter and month ("FY 2026", "Q3 2026", "Sep 2026").
    let periodPaths : string[][] =
        let out = ResizeArray<string[]>()
        for y in 0 .. years - 1 do
            let year = string (firstYear + y)
            for m in 0 .. 11 do
                out.Add [| "FY " + year; "Q" + string (m / 3 + 1) + " " + year; monthNames.[m] + " " + year |]
        out.ToArray()

    /// The measures of a line: what was booked, and what was planned; both add up along the rows and the columns.
    let measures : Measure[] =
        [| { Name = "Actual"; OverRows = Sum; OverCols = Sum }
           { Name = "Budget"; OverRows = Sum; OverCols = Sum } |]

    /// Formulas over them: taken of the totals of a group, not of its lines.
    let formulas : (string * Expr)[] =
        [| "Variance", Sub(M "Actual", M "Budget")
           "Variance %", Div(Mul(Sub(M "Actual", M "Budget"), Num 100.0), M "Budget") |]

    /// `count` journal lines made from `seed`: an account, a cost centre, a month and an amount in whole cents (revenue counts for the result, costs against it), and what was
    /// planned for the line, 90 to 110 percent of it.
    let facts (count: int) (seed: uint32) : Facts =
        let accounts = Hierarchy.ofPaths accountPaths
        let centres = Hierarchy.ofPaths centrePaths
        let periods = Hierarchy.ofPaths periodPaths
        let account : int[] = Array.zeroCreate count
        let centre : int[] = Array.zeroCreate count
        let period : int[] = Array.zeroCreate count
        let actual : float[] = Array.zeroCreate count
        let budget : float[] = Array.zeroCreate count
        let mutable s = seed
        for i in 0 .. count - 1 do
            s <- s * 1664525u + 1013904223u
            let r1 = int (s >>> 8)
            s <- s * 1664525u + 1013904223u
            let r2 = int (s >>> 8)
            s <- s * 1664525u + 1013904223u
            let r3 = int (s >>> 8)
            let a = r1 % accounts.Leaves
            account.[i] <- a
            centre.[i] <- r2 % centres.Leaves
            period.[i] <- (r2 / 7 + r1 / 13) % periods.Leaves
            let cents = 2000 + r3 % 250000
            let line = float cents / 100.0 * signs.[a]
            actual.[i] <- line
            budget.[i] <- floor (line * (0.90 + float (r3 / 4096 % 21) / 100.0) * 100.0 + 0.5) / 100.0
        { Count = count
          Dimensions = [| { Name = "Account"; Tree = accounts; Leaf = account }
                          { Name = "Cost centre"; Tree = centres; Leaf = centre }
                          { Name = "Period"; Tree = periods; Leaf = period } |]
          Measures = measures
          Values = [| actual; budget |] }

    /// What a dimension calls one of its leaves (an account, a centre, a month).
    let leafLabel (f: Facts) (dim: int) (leaf: int) : string =
        let t = f.Dimensions.[dim].Tree
        t.Nodes.[t.LeafNodes.[leaf]].Label

    /// The date of a line: its month, and a day from 1 to 28 that its position decides.
    let dateOf (f: Facts) (line: int) : string =
        let p = f.Dimensions.[Periods].Leaf.[line]
        let month = p % 12 + 1
        let day = 1 + (line * 7) % 28
        string (firstYear + p / 12) + "-" + (if month < 10 then "0" else "") + string month + "-" + (if day < 10 then "0" else "") + string day
