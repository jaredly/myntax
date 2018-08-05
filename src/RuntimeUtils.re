
module RP = PackTypes.Path;

let rec greedy = (~mergeErrors, loop, min, max, subr, i, path, greedyCount, isNegated) =>
  /* implements e* e+ e? */
  switch max {
  | Some(0) => (i, [], ((-1), []))
  | _ =>
    if (min > 0) {
      /* we must match at least min or fail */
      let (i', found, err) = loop(i, [subr], [RP.Iter(greedyCount), ...path], 0, isNegated);
      if (i' >= i) {
        let (i'', children, merr) =
          greedy(~mergeErrors, loop, min - 1, max, subr, i', path, greedyCount + 1, isNegated);
        (i'', List.concat([found, children]), mergeErrors(err, merr))
      } else {
        ((-1), [], err)
      }
    } else {
      /* try matching, doesn't matter if we fail */
      let (i', children, err) = loop(i, [subr], [RP.Iter(greedyCount), ...path], 0, isNegated);
      if (i' > i) {
        let max =
          switch max {
          | None => None
          | Some(n) => Some(n - 1)
          };
        let (i'', more, merr) = greedy(~mergeErrors, loop, 0, max, subr, i', path, greedyCount + 1, isNegated);
        (i'', List.concat([children, more]), mergeErrors(err, merr))
      } else {
        (
          i,
          [],
          err /* don't fail, return longest match */
        )
      }
    }
  };
