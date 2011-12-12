-module(splitter).
-compile(export_all).



-include_lib("eunit/include/eunit.hrl").

test() ->

    ?assertEqual(
        [
            [[1],[2]],
            [[1,2]]
        ],
        split([1,2], 2)
    ),

    ?assertEqual(
        [
            [[1],[2]],
            [[1,2]]
        ],
        split([1,2], 3)
    ),


    ?assertEqual(
        [
            [
                [1],
                    [
                        [2],[3]
                    ],
                    [
                        [2,3]
                    ]
            ],
            [
                [1,2], [3]
            ]
        ],
        split([1,2,3], 2)
    ),

    ?assertEqual(
        [
            [[1],
                [[2],[3]],
                [[2,3]]
            ],
            [[1,2],[3]],
            [[1,2,3]]
        ],
        split([1,2,3], 3)
    ),

    ?assertEqual(
        [
            [[1],
                [[2],[3]],
                [[2,3]]
            ],
            [[1,2],[3]],
            [[1,2,3]]
        ],
        split([1,2,3], 4)
    ),

    ok.


split([X, Y, Z, W, U| Rest], 5) ->
    [
        [[X] | split([Y, Z, W, U | Rest], 5)],
        [[X, Y] | split([Z, W, U | Rest], 5)],
        [[X, Y, Z] | split([W, U | Rest], 5)],
        [[X, Y, Z, W] | split([U | Rest], 5)],
        [[X, Y, Z, W, U] | split(Rest, 5)]
    ];


split([X, Y, Z, W| Rest], 5) ->
    [
        [[X] | split([Y, Z, W | Rest], 5)],
        [[X, Y] | split([Z, W | Rest], 5)],
        [[X, Y, Z] | split([W | Rest], 5)],
        [[X, Y, Z, W] | split(Rest, 5)]
    ];

split([X, Y, Z| Rest], 5) ->
    [
        [[X] | split([Y, Z | Rest], 5)],
        [[X, Y] | split([Z | Rest], 5)],
        [[X, Y, Z] | split(Rest, 5)]
    ];

split([X, Y| Rest], 5) ->
    [
        [[X] | split([Y | Rest], 5)],
        [[X, Y] | split(Rest, 5)]
    ];

split([X| Rest], 5) ->
    [
        [X] | split(Rest, 5)
    ];

split([], 5) ->
    [];



split([X, Y, Z, W| Rest], 4) ->
    [
        [[X] | split([Y, Z, W | Rest], 4)],
        [[X, Y] | split([Z, W | Rest], 4)],
        [[X, Y, Z] | split([W | Rest], 4)],
        [[X, Y, Z, W] | split(Rest, 4)]
    ];

split([X, Y, Z| Rest], 4) ->
    [
        [[X] | split([Y, Z | Rest], 4)],
        [[X, Y] | split([Z | Rest], 4)],
        [[X, Y, Z] | split(Rest, 4)]
    ];

split([X, Y| Rest], 4) ->
    [
        [[X] | split([Y | Rest], 4)],
        [[X, Y] | split(Rest, 4)]
    ];

split([X| Rest], 4) ->
    [
        [X] | split(Rest, 4)
    ];

split([], 4) ->
    [];

split([X, Y, Z| Rest], 3) ->
    [
        [[X] | split([Y, Z | Rest], 3)],
        [[X, Y] | split([Z| Rest], 3)],
        [[X, Y, Z] | split(Rest, 3)]
    ];

split([X, Y| Rest], 3) ->
    [
        [[X] | split([Y | Rest], 3)],
        [[X, Y] | split(Rest, 3)]
    ];

split([X| Rest], 3) ->
    [
        [X] | split(Rest, 3)
    ];

split([], 3) ->
    [];



split([X, Y| Rest], 2) ->
    [
        [[X] | split([Y | Rest], 2)],
        [[X, Y] | split(Rest, 2)]
    ];

split([X | Rest], 2) ->
    [
        [X] | split(Rest, 2)
    ];

split([], 2) -> [];


split(List, 1) -> List.



