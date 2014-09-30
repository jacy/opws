-record(side_pot, {
          members,
          all_in
         }).

-record(pot, {
	      pending = gb_trees:empty(),
          active = [],
          inactive = [],
          current = pot:new_side_pot()
         }).