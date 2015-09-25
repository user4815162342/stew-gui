unit stew_query;

{$mode objfpc}{$H+}

interface

{
TODO: This is basically an infrastructure for large, batch operations:
1. Querying lists of documents
2. Updating values on lists of documents.

It's not going to be a fully parsable SQL or anything like that, it's going
to be the structures needed for this query stuff. It would be up to some other
layer to build these, probably using GUI mechanisms.


The dynamics fall into three parts:
- Selectors: These indicate "fields" that should be displayed
- Filters: These indicate fields and values for those fields which must be true
in order for an item to pass a filter.
- Updators: These indicate fields and the values which should be applied to them.

** We're not going to do joins and things like that, I don't think this data structure
lends itself to that ***

Fields which can be selected:
- Document Path Fields: Name, Path
- Property Fields: Category, Status, Publish, User
- Synopsis Fields: Synopsis

TSelector:
- RequiresProperties: Boolean
- RequiresSynopsis: Boolean
- GetValue(TDocument,TDocumentProperties,Synopsis): String
  The TQuery will pass the required data to GetValue in order to get
  the actual value. If RequiresProperties and Synopsis are false, these
  values might not be filled.
- Subclasses for TSelector will be created for each kind of field that can
  be selected.

Fields which can be filtered:
- Document Path Fields: Name, Path
- Property Fields: Category, Status, Publish, User
- Synopsis Fields: Synopsis

Additional filter operations:
- Or
- And
- Recurse

TFilter:
- RequiresProperties: Boolean
- RequiresSynopsis: Boolean
- Recursive: Boolean
- MatchesPathFilter(TDocument): Boolean
- MatchesPropertiesFilter(TDocumentProperties): Boolean
- MatchesSynopsisFilter(String): Boolean
- Subclasses for TFilter will be created for each type of field, and
  each type of operation.
- For the TFilter operations, the Requires* properties and Recursive property
  should consider their "terms" in determining the value.

Fields which can be updated:
- Property Fields: Category, Status, Publish, User
- Synopsis Fields: Synopsis

Methods for updating:
- set to literal value
- set based on concatenation from a selector combined with literal values
  (i.e. set Synopsis=Category + ': ' + Synopsis to combine category into the
  synopsis).
- Some simple string manipulation operations (i.e. Synopsis = Substr(Synopsis,IndexOf(':')))

TUpdator:
- RequiresProperties: Boolean
- ChangesProperties: Boolean
- RequiresSynopsis: Boolean
- ChangesSynopsis: Boolean
- UpdateProperties(TDocument,TDocumentProperties,Synopsis) - note that properties can be changed.
- UpdateSynopsis(TDocument,TDocumentProperties,var Synopsis)
- Subclasses for TUpdator will be created for each kind of field that can
  be updated and for the various updating methods.

TSelect: This would be a class which would be used for building up the query.
- AddSelector(TSelector):
- SetFilter(TFilter)
- Run(TDocument)
  - the parameter is the 'base' document to run against.
  - The algorithm looks like the following:
    1. Check with the filter and see if it's recursive.
    2. Go through Selectors and filter and find out whether any need properties.
    3. Go through selectors and filter and find out whether any need synopsis
    4. The remaining operations have a lot of async, so it's going to get complex.
       In general, what will happen is that we'll keep a list of promises that
       we are waiting for. As the promises completes, the result is placed into the
       list in place of the promise.
       1. Do a list of the base.
       2. For each document in the list:
          1. If the filter is recursive, then properties are required. Go get the
             properties.
             1. Once we have properties for the recursive call, build the list of
                children and pass it back to the 4.2 for further processing. Use
                the same document list building algorithm as the project does in
                ListDocumentsInFolder. We do this first because it's possible
                that the document itself doesn't match, but that a child does.
          2. If the filter.MatchesPathFilter(document) returns false, then continue
             onto the next one.
          3. If properties are required, and we don't already have them from recursion,
             then go get the properties.
          4. If the filter.MatchesProperties(properties) returns false, then continue
             onto the next one.
          5. If synopsis is required, go get the synopsis.
          6. If the filter.MatchesSynopsis(synopsos) returns false, then continue
             onto the next one.
          7. For each selector:
             1. Call 'GetValue' with the data we now have. Add it to the result list.

TUpdate: This would be a class which would be used for doing mass updates.
- AddUpdator(TUpdator)
- SetFilter(TFilter)
- Run(TDocument)
  - the parameter is the 'base' document to run against.
  - The algorithm looks very much like TQuery, except starting a 4.7:
          7. For each updator:
             1. Call 'UpdateProperties' and 'UpdateSynopsis' as necessary.
             -- Note that if, for some reason, the updators update the same field,
                they are updated in order.
          8. If the updators update properties, then write the properties.
          9. If the updators update the synopsis, then write the synopsis.

}

uses
  Classes, SysUtils;

implementation

end.

