# Thanos - advanced filtering capabilities for R/Shiny

## Nathan Siemers


# Project goals

Thanos is a small but advanced prototype to make interactive visualization and filtering of data possible.

The test prototype in app.R currently reads the "flights" data set,
offers several columns as default columns for visualizaiton and
filtering.  Other columns can be added by the user, and default
columns can always be removed.

The power of the application: graphs/histograms of the data content
for all selected columns are shown.  The app tries to handle
categorical as well as continuous data logically.  Depending on the
data type, the app offers checkboxes or sliders to filter the data.

Importantly, whenever any filtering operation is performed, the
histograms instantly update based on the new filtered data.

## A major goal here is performance

this prototype works, but we seek to
make this tool as fast as possible, making sure there aren't redundant
update events sent out to the shiny app, etc. One could even consider
forked or otherwise parallelized operations to update graphs, but this
might be too cumbersome.

## Another major goal is flexible connectivity to relational database
   data.

I often use tall/skinny databases in my work, and we need to adapt the
code to facilitate this.  The "flights" data used for the prototype is
a standard and single table.  After the first round of speed
optimizations, we should focus on this.  Create a tall/skinny table
like:

RowID   ColumnName   Value

for the flights data.  Then create a fetch routine driven by a pointer
to the new database schema/table.  reproduce the functionality of the
prototype but with a relational backend.  Compare/contrast performance
with the in-memory table.  Might not be too different, because this
isn't a huge database.


## Goal: Modularize this code

Filtering is nice, but really the power will be to use this code in
other shiny applications.  Given a pointer to an existing
schema/table, it should be easy to source the Thanos code into an
existing R/Shiny app, point it to the relevant schema/table, and
create all the filtering graphics and widgets within the other R/shiny
app. One important thing: there needs to be an additional function
within the code that returns some sort of pointer to the rows that
remain after filtering.  This is so the other app can then know which
unfiltered rows it should be using to do whatever it does.  One simple
example of another app would be a simple graphing application that
take column names as input x,y,color,size,etc (the same columns
available to Thanos) and plots the filtered data.  I would think that
a pointer to the right rownames would be more efficient than returning
some huge data set to the parent app.  Create a simple additional app
that does this type of graphing functionality, then insert this Thanos
module into the graphing app in as elegant a way as possible.

## Goal: test on large data sets

"flights" is not that big.  find some big (> 2gb) data set you can
pull off the web, create the tall/skinny version of the data in
sqlite, and start running tests on performance.  Optimize everything
you can: query speed, graphing performance, etc.  Let's try to make
this tool fly together.

## Notes: (I will update this area sometimes)

feel free to create new directories for testing things - that graphing
app can be in its own directory, etc

make highly informative comments in your git commits so both you and I
have really good memories about what we have done

You can clean up the current file and directory structure as you wish.
The important files are thanos.R and the current testing app.R

We might have a situation where a user removes a column from the "Filter Columns" 
selector.  What's the right action? I think any filtering for that column should
be removed so we don't have any ghost filtering going on.  Consider this 
and make a remedy.

