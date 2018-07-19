-module(ejsv_utils_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(mod, ejsv_utils).

merge_lists_test()->
  ?assertEqual([{test, override},
                {only_parent, true},
                {only_child, true}],
               ?mod:merge_lists(
                  [{test, override}, {only_parent, true}],
                  [{test, overridden}, {only_child, true}]
                 )).

combine_lists_test()->
  ?assertEqual([{lists_concat, [1,2]}],
               ?mod:combine_lists(
                  [{lists_concat, [1]}],
                  [{lists_concat, [2]}]
                 )),
  ?assertEqual([{parent_value, [1,2]}],
               ?mod:combine_lists(
                  [{parent_value, 1}],
                  [{parent_value, [2]}]
                 )),
  ?assertEqual([{child_value, [1,2]}],
               ?mod:combine_lists(
                  [{child_value, [1]}],
                  [{child_value, 2}]
                 )),
  ?assertEqual([{values, [1,2]}],
               ?mod:combine_lists(
                  [{values, 1}],
                  [{values, 2}]
                 )),
  ?assertEqual([{single_value, 1}],
               ?mod:combine_lists(
                  [{single_value, 1}],
                  []
                 )),
  ?assertThrow({additional_values_found,value,[1,2,3]},
               ?mod:combine_lists(
                  [{value, 1}, {value, 2}],
                  [{value, 3}]
                 )).

combine_maps_test()->
  ?assertEqual(#{ maps_concat => [1,2] },
               ?mod:combine_maps(
                  #{ maps_concat => [1] },
                  #{ maps_concat => [2] }
                 )),
  ?assertEqual(#{ parent_value => [1,2] },
               ?mod:combine_maps(
                  #{ parent_value => 1 },
                  #{ parent_value => [2] }
                 )),
  ?assertEqual(#{ child_value => [1,2] },
               ?mod:combine_maps(
                  #{ child_value => [1] },
                  #{ child_value => 2 }
                 )),
  ?assertEqual(#{ values => [1,2] },
               ?mod:combine_maps(
                  #{ values => 1 },
                  #{ values => 2 }
                 )),
  ?assertEqual(#{ single_value => 1 },
               ?mod:combine_maps(
                  #{ single_value => 1 },
                  #{}
                 )).
