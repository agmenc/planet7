
TODO
----

Push out latest validation changes
Update docs to show a basic usage, the full recipe, and links to all the other recipes
Update Scala version to latest minor versions
Given two locations (directories), diff the files there
Allow us to use other columns to create a mapping, e.g. if column X has no/bad data, we can use column Y+Z
Add apply(index: Int) to row, so that we can use row(3), instead of row.data(3)
Reduce duplication between Diff and NonSortingDiff
Illustrated diff, to help during creation of rec scripts. Show each item side-by-side.
NaiveRowDiffer -> document usage
Tart up the API, e.g. compare (left -> right) on myIdDiffer
Ability to set tolerances for numerical field comparisons
Identify duplicates in both lists

Done
----
Better CSV parsing, respecting quotes
Allow user to define Csv().columnStructure(ignore("Address", "ID"))
Update Scala version to 2.11

