%% @copyright 2022 Guilherme Andrade
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy  of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(changelog_updater_tests_SUITE).

-ifndef(NO_CT_SUITE_BEHAVIOUR).
-behaviour(ct_suite).
-endif.

-include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% ct_suite Function Exports
%% ------------------------------------------------------------------

-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% ------------------------------------------------------------------
%% Test Case Function Exports
%% ------------------------------------------------------------------

-export([ok_empty_log_add/1,
         ok_empty_log_change/1,
         ok_empty_log_remove/1,
         ok_empty_log_fix/1,

         ok_not_newly_unreleased_log_add_first_before_change/1,
         ok_not_newly_unreleased_log_add_second/1,
         ok_not_newly_unreleased_log_add_second_before_change/1,
         ok_not_newly_unreleased_log_add_first_before_removal/1,
         ok_not_newly_unreleased_log_add_second_before_removal/1,
         ok_not_newly_unreleased_log_add_first_before_fix/1,
         ok_not_newly_unreleased_log_add_second_before_fix/1,

         ok_not_newly_unreleased_log_change_first_after_add/1,
         ok_not_newly_unreleased_log_change_first_before_removal/1,
         ok_not_newly_unreleased_log_change_first_before_fix/1,
         ok_not_newly_unreleased_log_change_first_after_add_but_before_removal/1,

         ok_newly_unreleased_log_after_1_version_add/1,
         ok_newly_unreleased_log_after_1_version_change/1,
         ok_newly_unreleased_log_after_1_version_remove/1,
         ok_newly_unreleased_log_after_1_version_fix/1,

         ok_newly_unreleased_log_after_2_versions_add/1,
         ok_newly_unreleased_log_after_2_versions_change/1,
         ok_newly_unreleased_log_after_2_versions_remove/1,
         ok_newly_unreleased_log_after_2_versions_fix/1,

         ok_not_newly_unreleased_log_after_1_version_add/1,
         ok_not_newly_unreleased_log_after_1_version_change/1,
         ok_not_newly_unreleased_log_after_1_version_remove/1,
         ok_not_newly_unreleased_log_after_1_version_fix/1
        ]).

%% ------------------------------------------------------------------
%% elvis Tweaks
%% ------------------------------------------------------------------

-elvis([
    {elvis_style, atom_naming_convention, disable},
    {elvis_style, function_naming_convention, disable},
    {elvis_style, god_modules, disable}
]).

%% ------------------------------------------------------------------
%% ct_suite Function Definitions
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    [{_Name = tests,
      _Opts = [], %parallel],
      _TestCases = [
            ok_empty_log_add,
            ok_empty_log_change,
            ok_empty_log_remove,
            ok_empty_log_fix,

            ok_not_newly_unreleased_log_add_first_before_change,
            ok_not_newly_unreleased_log_add_second,
            ok_not_newly_unreleased_log_add_second_before_change,
            ok_not_newly_unreleased_log_add_first_before_removal,
            ok_not_newly_unreleased_log_add_second_before_removal,
            ok_not_newly_unreleased_log_add_first_before_fix,
            ok_not_newly_unreleased_log_add_second_before_fix,

            ok_not_newly_unreleased_log_change_first_after_add,
            ok_not_newly_unreleased_log_change_first_before_removal,
            ok_not_newly_unreleased_log_change_first_before_fix,
            ok_not_newly_unreleased_log_change_first_after_add_but_before_removal,

            ok_newly_unreleased_log_after_1_version_add,
            ok_newly_unreleased_log_after_1_version_change,
            ok_newly_unreleased_log_after_1_version_remove,
            ok_newly_unreleased_log_after_1_version_fix,

            ok_newly_unreleased_log_after_2_versions_add,
            ok_newly_unreleased_log_after_2_versions_change,
            ok_newly_unreleased_log_after_2_versions_remove,
            ok_newly_unreleased_log_after_2_versions_fix,

            ok_not_newly_unreleased_log_after_1_version_add,
            ok_not_newly_unreleased_log_after_1_version_change,
            ok_not_newly_unreleased_log_after_1_version_remove,
            ok_not_newly_unreleased_log_after_1_version_fix
    ]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCases, Config) ->
    Config.

%% ------------------------------------------------------------------
%% Test Cases Function Definitions
%% ------------------------------------------------------------------

ok_empty_log_add(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Added\n"
        "\n"
        "- a few things that were added\n"
        "\n",

        do_add("a few things that were added",
               "")).

ok_empty_log_change(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Changed\n"
        "\n"
        "- a few things that were changed\n"
        "\n",

        do_change("a few things that were changed",
                  "")).

ok_empty_log_remove(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Removed\n"
        "\n"
        "- a few things that were removed\n"
        "\n",

        do_remove("a few things that were removed",
                  "")).

ok_empty_log_fix(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Fixed\n"
        "\n"
        "- a few things that were fixed\n"
        "\n",

        do_fix("a few things that were fixed",
               "")).

%%
ok_not_newly_unreleased_log_add_first_before_change(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Added\n"
        "\n"
        "- a few things that were added\n"
        "\n"
        "### Changed\n"
        "\n"
        "- a few things that were changed\n",

        do_add("a few things that were added",
               "## [Unreleased]\n"
               "\n"
               "### Changed\n"
               "\n"
               "- a few things that were changed\n")).

ok_not_newly_unreleased_log_add_second(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Added\n"
        "\n"
        "- a few things that were added\n"
        "- a few more things that were also added\n"
        "\n",

        do_add("a few more things that were also added",
               "## [Unreleased]\n"
               "\n"
               "### Added\n"
               "\n"
               "- a few things that were added\n")).

ok_not_newly_unreleased_log_add_second_before_change(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Added\n"
        "\n"
        "- a few things that were added\n"
        "- a few more things that were also added\n"
        "\n"
        "### Changed\n"
        "\n"
        "- a few things that were changed\n",

        do_add("a few more things that were also added",
               "## [Unreleased]\n"
               "\n"
               "### Added\n"
               "\n"
               "- a few things that were added\n"
               "\n"
               "### Changed\n"
               "\n"
               "- a few things that were changed\n")).

ok_not_newly_unreleased_log_add_first_before_removal(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Added\n"
        "\n"
        "- a few things that were added\n"
        "\n"
        "### Removed\n"
        "\n"
        "- a few things that were removed\n",

        do_add("a few things that were added",
               "## [Unreleased]\n"
               "\n"
               "### Removed\n"
               "\n"
               "- a few things that were removed\n")).

ok_not_newly_unreleased_log_add_second_before_removal(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Added\n"
        "\n"
        "- a few things that were added\n"
        "- a few more things that were also added\n"
        "\n"
        "### Removed\n"
        "\n"
        "- a few things that were removed\n",

        do_add("a few more things that were also added",
               "## [Unreleased]\n"
               "\n"
               "### Added\n"
               "\n"
               "- a few things that were added\n"
               "\n"
               "### Removed\n"
               "\n"
               "- a few things that were removed\n")).

ok_not_newly_unreleased_log_add_first_before_fix(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Added\n"
        "\n"
        "- a few things that were added\n"
        "\n"
        "### Fixed\n"
        "\n"
        "- a few things that were fixed\n",

        do_add("a few things that were added",
               "## [Unreleased]\n"
               "\n"
               "### Fixed\n"
               "\n"
               "- a few things that were fixed\n")).

ok_not_newly_unreleased_log_add_second_before_fix(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Added\n"
        "\n"
        "- a few things that were added\n"
        "- a few more things that were also added\n"
        "\n"
        "### Fixed\n"
        "\n"
        "- a few things that were fixed\n",

        do_add("a few more things that were also added",
               "## [Unreleased]\n"
               "\n"
               "### Added\n"
               "\n"
               "- a few things that were added\n"
               "\n"
               "### Fixed\n"
               "\n"
               "- a few things that were fixed\n")).
%%
ok_not_newly_unreleased_log_change_first_after_add(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Added\n"
        "\n"
        "- a few things that were added\n"
        "\n"
        "### Changed\n"
        "\n"
        "- a few things that were changed\n"
        "\n",

        do_change("a few things that were changed",
                  "## [Unreleased]\n"
                  "\n"
                  "### Added\n"
                  "\n"
                  "- a few things that were added")).

ok_not_newly_unreleased_log_change_first_before_removal(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Changed\n"
        "\n"
        "- a few things that were changed\n"
        "\n"
        "### Removed\n"
        "\n"
        "- a few things that were removed",

        do_change("a few things that were changed",
                  "## [Unreleased]\n"
                  "\n"
                  "### Removed\n"
                  "\n"
                  "- a few things that were removed")).

ok_not_newly_unreleased_log_change_first_before_fix(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Changed\n"
        "\n"
        "- a few things that were changed\n"
        "\n"
        "### Fixed\n"
        "\n"
        "- a few things that were fixed",

        do_change("a few things that were changed",
                  "## [Unreleased]\n"
                  "\n"
                  "### Fixed\n"
                  "\n"
                  "- a few things that were fixed")).

ok_not_newly_unreleased_log_change_first_after_add_but_before_removal(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Added\n"
        "\n"
        "- a few things that were added\n"
        "\n"
        "### Changed\n"
        "\n"
        "- a few things that were changed\n"
        "\n"
        "### Removed\n"
        "\n"
        "- a few things that were removed\n",

        do_change("a few things that were changed",
                  "## [Unreleased]\n"
                  "\n"
                  "### Added\n"
                  "\n"
                  "- a few things that were added"
                  "\n"
                  "### Removed\n"
                  "\n"
                  "- a few things that were removed\n")).

%%
ok_newly_unreleased_log_after_1_version_add(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Added\n"
        "\n"
        "- a few things that were added\n"
        "\n"
        "## [1.23.40] - 1970-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- nonsense\n",

        do_add("a few things that were added",
               "## [1.23.40] - 1970-01-01\n"
               "\n"
               "### Retrieved\n"
               "\n"
               "- nonsense\n")).

ok_newly_unreleased_log_after_1_version_change(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Changed\n"
        "\n"
        "- a few things that were changed\n"
        "\n"
        "## [1.23.40] - 1970-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- nonsense\n",

        do_change("a few things that were changed",
                  "## [1.23.40] - 1970-01-01\n"
                  "\n"
                  "### Retrieved\n"
                  "\n"
                  "- nonsense\n")).

ok_newly_unreleased_log_after_1_version_remove(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Removed\n"
        "\n"
        "- a few things that were removed\n"
        "\n"
        "## [1.23.40] - 1970-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- nonsense\n",

        do_remove("a few things that were removed",
                  "## [1.23.40] - 1970-01-01\n"
                  "\n"
                  "### Retrieved\n"
                  "\n"
                  "- nonsense\n")).

ok_newly_unreleased_log_after_1_version_fix(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Fixed\n"
        "\n"
        "- a few things that were fixed\n"
        "\n"
        "## [1.23.40] - 1970-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- nonsense\n",

        do_fix("a few things that were fixed",
               "## [1.23.40] - 1970-01-01\n"
               "\n"
               "### Retrieved\n"
               "\n"
               "- nonsense\n")).

%%
ok_newly_unreleased_log_after_2_versions_add(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Added\n"
        "\n"
        "- a few things that were added\n"
        "\n"
        "## [1.23.49] - 1980-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- more nonsense\n"
        "\n"
        "## [1.23.40] - 1970-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- nonsense\n",

        do_add("a few things that were added",
               "## [1.23.49] - 1980-01-01\n"
               "\n"
               "### Retrieved\n"
               "\n"
               "- more nonsense\n"
               "\n"
               "## [1.23.40] - 1970-01-01\n"
               "\n"
               "### Retrieved\n"
               "\n"
               "- nonsense\n")).

ok_newly_unreleased_log_after_2_versions_change(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Changed\n"
        "\n"
        "- a few things that were changed\n"
        "\n"
        "## [1.23.49] - 1980-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- more nonsense\n"
        "\n"
        "## [1.23.40] - 1970-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- nonsense\n",

        do_change("a few things that were changed",
                  "## [1.23.49] - 1980-01-01\n"
                  "\n"
                  "### Retrieved\n"
                  "\n"
                  "- more nonsense\n"
                  "\n"
                  "## [1.23.40] - 1970-01-01\n"
                  "\n"
                  "### Retrieved\n"
                  "\n"
                  "- nonsense\n")).

ok_newly_unreleased_log_after_2_versions_remove(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Removed\n"
        "\n"
        "- a few things that were removed\n"
        "\n"
        "## [1.23.49] - 1980-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- more nonsense\n"
        "\n"
        "## [1.23.40] - 1970-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- nonsense\n",

        do_remove("a few things that were removed",
                  "## [1.23.49] - 1980-01-01\n"
                  "\n"
                  "### Retrieved\n"
                  "\n"
                  "- more nonsense\n"
                  "\n"
                  "## [1.23.40] - 1970-01-01\n"
                  "\n"
                  "### Retrieved\n"
                  "\n"
                  "- nonsense\n")).

ok_newly_unreleased_log_after_2_versions_fix(_Config) ->
    ?assertEqual(
        "## [Unreleased]\n"
        "\n"
        "### Fixed\n"
        "\n"
        "- a few things that were fixed\n"
        "\n"
        "## [1.23.49] - 1980-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- more nonsense\n"
        "\n"
        "## [1.23.40] - 1970-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- nonsense\n",

        do_fix("a few things that were fixed",
               "## [1.23.49] - 1980-01-01\n"
               "\n"
               "### Retrieved\n"
               "\n"
               "- more nonsense\n"
               "\n"
               "## [1.23.40] - 1970-01-01\n"
               "\n"
               "### Retrieved\n"
               "\n"
               "- nonsense\n")).

%%
ok_not_newly_unreleased_log_after_1_version_add(_Config) ->
    ?assertEqual(
        "## [unreleased]  \n"
        "\n"
        "### Foobared\n"
        "\n"
        "- barfoos\n"
        "\n"
        "### Added\n"
        "\n"
        "- a few things that were added\n"
        "\n"
        "### Removed \n"
        "-stuff\n"
        "## [1.23.40] - 1970-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- nonsense\n",

        do_add("a few things that were added",
               "## [unreleased]  \n"
               "\n"
               "### Foobared\n"
               "\n"
               "- barfoos\n"
               "### Removed \n"
               "-stuff\n"
               "## [1.23.40] - 1970-01-01\n"
               "\n"
               "### Retrieved\n"
               "\n"
               "- nonsense\n")).

ok_not_newly_unreleased_log_after_1_version_change(_Config) ->
    ?assertEqual(
        "## [unreleased]\n"
        "\n"
        "### Foobared\n"
        "\n"
        "- barfoos\n"
        "\n"
        "### Changed\n"
        "\n"
        "- a few things that were changed\n"
        "\n"
        "### Removed \n"
        "-stuff\n"
        "## [1.23.40] - 1970-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- nonsense\n",

        do_change("a few things that were changed",
                  "## [unreleased]\n"
                  "\n"
                  "### Foobared\n"
                  "\n"
                  "- barfoos\n"
                  "### Removed \n"
                  "-stuff\n"
                  "## [1.23.40] - 1970-01-01\n"
                  "\n"
                  "### Retrieved\n"
                  "\n"
                  "- nonsense\n")).

ok_not_newly_unreleased_log_after_1_version_remove(_Config) ->
    ?assertEqual(
        "## [unreleased]\n"
        "\n"
        "### Foobared\n"
        "\n"
        "- barfoos\n"
        "### Removed \n"
        "-stuff\n"
        "- the thing that was removed\n"
        "\n"
        "## [1.23.40] - 1970-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- nonsense\n",

        do_remove("the thing that was removed",
                  "## [unreleased]\n"
                  "\n"
                  "### Foobared\n"
                  "\n"
                  "- barfoos\n"
                  "### Removed \n"
                  "-stuff\n"
                  "## [1.23.40] - 1970-01-01\n"
                  "\n"
                  "### Retrieved\n"
                  "\n"
                  "- nonsense\n")).

ok_not_newly_unreleased_log_after_1_version_fix(_Config) ->
    ?assertEqual(
        "## [unreleased]\n"
        "\n"
        "### Foobared\n"
        "\n"
        "- barfoos\n"
        "### Removed \n"
        "-stuff\n"
        "\n"
        "### Fixed\n"
        "\n"
        "- the thing that was fixed\n"
        "\n"
        "## [1.23.40] - 1970-01-01\n"
        "\n"
        "### Retrieved\n"
        "\n"
        "- nonsense\n",

        do_fix("the thing that was fixed",
               "## [unreleased]\n"
               "\n"
               "### Foobared\n"
               "\n"
               "- barfoos\n"
               "### Removed \n"
               "-stuff\n"
               "## [1.23.40] - 1970-01-01\n"
               "\n"
               "### Retrieved\n"
               "\n"
               "- nonsense\n")).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_add(Addition, LogWithoutPreamble) ->
    do(fun changelog_updater:insert_addition/2, Addition, LogWithoutPreamble).

do_change(Change, LogWithoutPreamble) ->
    do(fun changelog_updater:insert_change/2, Change, LogWithoutPreamble).

do_remove(Removal, LogWithoutPreamble) ->
    do(fun changelog_updater:insert_removal/2, Removal, LogWithoutPreamble).

do_fix(Fix, LogWithoutPreamble) ->
    do(fun changelog_updater:insert_fix/2, Fix, LogWithoutPreamble).

do(Fun, Change, LogWithoutPreamble) ->
    Preamble = preamble(),
    Log = [Preamble, LogWithoutPreamble],
    {ok, UpdatedLog} = Fun(Change, Log),
    UpdatedLogStr = unicode:characters_to_list(UpdatedLog),

    ?assert(lists:prefix(Preamble, UpdatedLogStr)),
    UpdatedWithoutPreamble
        = lists:sublist(UpdatedLogStr,
                        length(Preamble) + 1,
                        length(UpdatedLogStr) - length(Preamble)),

    UpdatedWithoutPreamble.

preamble() ->
    "# Changelog\n"
    "All notable changes to this project will be documented in this file.\n"
    "\n"
    "The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),\n"
    "and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).\n"
    "\n".
