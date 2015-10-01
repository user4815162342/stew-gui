unit stew_query;

{$mode objfpc}{$H+}

interface

{
TODO: This is basically an infrastructure for large, batch operations:
1. Querying lists of documents
2. Updating properties and some other attachments on lists of documents.

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
- MatchesPathFilter(TDocument): Boolean
- MatchesPropertiesFilter(TDocumentProperties): Boolean
- MatchesSynopsisFilter(String): Boolean
- RecursePathFilter(TDocument): Boolean
- RecursePropertiesFilter(TDocumentProperties): Boolean
- RecurseSynopsisFilter(String): Boolean
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

TQuery: This would be a general query class that gets the data given a filter.
- SetFilter(TFilter)
- MatchFound(TDocument,TDocumentProperties,Synopsis); abstract;
- Run(TDocument)
  - the parameter is the 'base' document to run against.
  - The algorithm looks like the following:
    1. Check with the filter and see if it's recursive.
    2. Go through Selectors and filter and find out whether any need properties.
    3. Go through selectors and filter and find out whether any need synopsis
    4. The remaining operations have a lot of async, so it's going to get complex.
       In general, what will probably happen is that we'll keep a list of promises
       that we are waiting for. As the promises completes, the result is placed
       into the list in place of the promise.
       1. Set result list to an empty list.
       2. Set candidate list to a list from the base
       3. While the candidate list is not empty:
          1. Grab the first item off of the list.
          2. If the filter, selector or updator requires the properties, then go get them, defer the
             rest of this until we do.
          3. If the filter, selector or updator  requires the synopsis, then go get them, defer the
             rest of this until we do.
          4. Set matched = filter.MatchesPathFilter and filter.MatchesPropertiesFilter
                           and filter.MatchesSynopsisFilter
          5. Set recurse = filter.RecursePathFilter and filter.RecursePropertiesFilter
                           and filter.RecurseSynopsisFilter
          6. If matched then call MatchFound(document,properties,synopsis)
          7. If recurse then get the list of sub-documents in this folder
             and *insert* them at the top of the candidate list (so the
             results will stay in the same order)

TSelect (TQuery): This would be a class which would be used for building up the query.
- AddSelector(TSelector):
- MatchFound:
  1. call 'GetValue' on each selector with the data we
     now have and put into an array. Append that array to the result list
     along with the document path.
- Run:
  1. Create a result list and then call inherited. Return a promise that resolves
     with that result list once the inherited Run resolves.

TUpdate (TQuery): This would be a class which would be used for doing mass updates.
- AddUpdator(TUpdator)
- MatchFound:
  1. for each updator, call UpdateProperties and UpdateSynopsis. If it is supposed
     to change either of them, save those values.
- Run:
  2. Return a promise that resolves after the inherited Run resolves and all 'saves'
     are complete from MatchFound.

}

uses
  Classes, SysUtils;

implementation

end.

